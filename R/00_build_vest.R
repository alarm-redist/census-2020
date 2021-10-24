# libs ----
library(tidyverse)
library(geomander) # >= 1.1.0.9000
library(PL94171)
library(here)
library(sf)
library(tigris) # >= 1.4.1.9000
library(censable)
library(dataverse)

# See https://iqss.github.io/dataverse-client-r/ for more info:
# Sys.setenv("DATAVERSE_KEY" = "<YOUR KEY HERE>")

# helpers ----
log_time <- function(path, state, year) {
    if (!fs::file_exists(path)) {
        fs::file_create(path)
    }
    lines <- readLines(con = path)
    lines <- c(lines, paste0(Sys.time(),',', state, ',', year))
    writeLines(text = lines, con = path)
}

# prep ----
sf_use_s2(FALSE)
years <- c(2016, 2018, 2020)

cw_zip <- dataverse::get_file_by_name('block10block20_crosswalks.zip', '10.7910/DVN/T9VMJO')
cw_zip_path <- withr::local_tempfile(fileext = '.zip')
writeBin(cw_zip, cw_zip_path)
unz_path <- file.path(dirname(cw_zip_path), 'block1020_crosswalks')
utils::unzip(cw_zip_path, exdir = unz_path, overwrite = TRUE)

proc_raw_cw <- function(raw) {
  fields <- str_split(raw, ',')
  map_dfr(fields, function(x) {
    if (length(x) <= 1) return(tibble())
    tibble(
      GEOID_to = x[1],
      GEOID = x[seq(2, length(x), by = 2L)],
      int_land = parse_number(x[seq(3, length(x), by = 2L)])
    )
  })
}

# set up crosswalks ----
fs::dir_create('crosswalks')
for (state in state.abb) {
  if (!fs::file_exists('crosswalks', state, ext = 'rds')) {
  vest_cw_raw <- read_lines(glue::glue('{unz_path}/block1020_crosswalk_{match_fips(state)}.csv'))
  vest_cw <- proc_raw_cw(vest_cw_raw)
  cw <- pl_crosswalk(toupper(state))
  vest_cw <- left_join(vest_cw, select(cw, -int_land), by = c('GEOID', 'GEOID_to'))
  saveRDS(vest_cw, file = fs::path('crosswalks', state, ext = 'rds'), compress = 'xz')
  }
}


# run
for (year in years) {
  states <- vest_states(year)
  for (state in states) {
    fs::dir_create(here('vest-2020', state))

    if (!fs::file_exists(here(glue::glue('vest-2020/{state}/{state}_{year}_2020_block.csv'))) &
      !fs::file_exists(here(glue::glue('vest-2020/{state}/{state}_{year}_2020_vtd.csv')))) {


      # Step 1: Match VEST precincts to 2010 Census blocks ----
      block <- tigris::blocks(state, year = 2010)
      vest <- get_vest(state = state, year = year, clean_names = FALSE) %>%
        st_transform(st_crs(block)) %>%
        st_zm()
      match_list <- geo_match(from = block, to = vest, method = 'centroid')
      tb <- tibble(block10_vest = match_list, block_GEOID = block$GEOID10)

      # Step 2: Add populations to 2010 blocks ----
      dec <- build_dec('block', state = state, geometry = FALSE, groups = 'all', year = 2010) %>%
        rename(block = GEOID) %>%
        select(-NAME)

      # Step 3: Estimate election data down to 2010 blocks ----
      elec_at_2010 <- tibble(GEOID = block$GEOID10)
      elections <- names(vest)[str_detect(names(vest), str_c('G', year - 2000)) |
        str_detect(names(vest), str_c('R', year - 2000))]
      for (election in elections) {
        elec_at_2010 <- elec_at_2010 %>%
          mutate(!!election := estimate_down(
            value = vest[[election]], wts = dec[['vap']],
            group = match_list
          ))
      }

      # Step 4: Crosswalk election data to 2020 blocks by area ----
      vest_cw_raw <- read_lines(glue::glue('{unz_path}/block1020_crosswalk_{match_fips(state)}.csv'))
      vest_cw <- proc_raw_cw(vest_cw_raw)
      cw <- pl_crosswalk(toupper(state))
      vest_cw <- left_join(vest_cw, select(cw, -int_land), by = c('GEOID', 'GEOID_to'))
      rt <- pl_retally(elec_at_2010, crosswalk = vest_cw)

      # Step 5: Aggregate to 2020 VTDs by BAFs
      baf <- pl_get_baf(toupper(state), 'VTD')
      if (length(baf) > 0) {
        baf <- baf[[1]]
        baf <- baf %>%
          rename(GEOID = BLOCKID) %>%
          mutate(
            STATEFP = match_fips(state),
            GEOID20 = paste0(STATEFP, COUNTYFP, DISTRICT)
          )

        rt <- rt %>% left_join(baf, by = 'GEOID')

        # agg
        vtd <- rt %>%
          select(-GEOID, -area_land, -area_water) %>%
          group_by(GEOID20) %>%
          summarize(
            across(where(is.character), .fns = unique),
            across(where(is.numeric), .fns = sum)
          ) %>%
          relocate(GEOID20, .before = everything()) %>%
          relocate(STATEFP, .before = COUNTYFP) %>%
          mutate(across(where(is.numeric), round, 2))

        # Write to File ----
        write_csv(vtd, file = here(glue::glue('vest-2020/{state}/{state}_{year}_2020_vtd.csv')))
      } else {
        rt <- rt %>%
          mutate(across(where(is.numeric), round, digits=1))
        write_csv(rt, file = here(glue::glue('vest-2020/{state}/{state}_{year}_2020_block.csv')))
      }

      log_time(here("vest-2020/log_time.txt"), state, year)
    }
  }
}

withr::deferred_clear()
