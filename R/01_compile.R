# libs ----
library(tidyverse)
library(PL94171)
library(here)
library(censable)
library(dataverse)

# helpers ----
log_time <- function(path, state) {
  if (!fs::file_exists(path)) {
    fs::file_create(path)
  }
  lines <- readLines(con = path)
  lines <- c(lines, paste0(Sys.time(), ',', state))
  writeLines(text = lines, con = path)
}

states <- censable::stata %>%
  filter(!is.na(region)) %>%
  pull(abb)

vest_path <- here('vest-2020')
joined_path <- here('census-vest-2020')

for (s in states) {
  vest_files <- Sys.glob(str_glue('{vest_path}/{str_to_lower(s)}/*.csv'))

  # download and subset pl ----
  pl <- pl_read(pl_url(s, 2020)) %>%
    pl_select_standard(clean_names = TRUE)
  type <- 'vtd'
  state_d <- pl_subset(pl, '700')

  # check that the state has vtds ----
  if (any(str_detect(vest_files, '_block\\.csv')) || nrow(state_d) == 0) { # no VTDs
    type <- 'block'
    state_d <- pl_subset(pl, '750') %>%
      select(-vtd)
  }

  fips_d <- censable::fips_2020 %>%
    left_join(select(censable::stata, fips, abb), by = c('state' = 'fips')) %>%
    select(state = abb, county_code = county, county = name)

  state_d <- state_d %>%
    select(-.data$row_id, -.data$summary_level) %>%
    rename(county_code = county, GEOID20 = GEOID) %>%
    left_join(tigris::fips_codes, by = c('state', 'county_code')) %>%
    select(-county_code, -state_code, -state_name) %>%
    relocate(county, .after = state)

  if (length(vest_files) > 0) {
    spec <- cols(GEOID20 = 'c')
    if (type != 'vtd') {
      spec <- cols(GEOID = 'c')
    }

    # read files ----
    vest_d <- map(vest_files, read_csv, show_col_types = FALSE, col_types = spec)

    # select elections and dem & rep ----
    vest_d <- vest_d %>%
      map(~ rename_with(., ~'GEOID20', ends_with('GEOID'))) %>%
      map(~ select(., GEOID20, starts_with('G'), starts_with('R'))) %>% # gen/runoff election
      map(geomander::clean_vest) %>%
      map(~ select(., GEOID20, starts_with(c('pre', 'uss', 'gov', 'atg', 'sos')) &
        contains(c('_rep_', '_dem_')))) %>%
      reduce(left_join, by = 'GEOID20')

    # Average ----
    vest_d <- vest_d %>%
      mutate(
        arv_16 = rowMeans(select(., contains('_16_rep_')), na.rm = TRUE),
        adv_16 = rowMeans(select(., contains('_16_dem_')), na.rm = TRUE),
        arv_18 = rowMeans(select(., contains('_18_rep_')), na.rm = TRUE),
        adv_18 = rowMeans(select(., contains('_18_dem_')), na.rm = TRUE),
        arv_20 = rowMeans(select(., contains('_20_rep_')), na.rm = TRUE),
        adv_20 = rowMeans(select(., contains('_20_dem_')), na.rm = TRUE),
        nrv = rowMeans(select(., contains('_rep_')), na.rm = TRUE),
        ndv = rowMeans(select(., contains('_dem_')), na.rm = TRUE),
        ndv = round(ndv, 1),
        nrv = round(nrv, 1),
        across(c(starts_with("adv_"), starts_with("arv_")),
               round, digits=1)
      )

    # Round ----
    vest_d <- vest_d %>%
      mutate(across(where(is.numeric), .fns = \(x) round(x, 1)))

    state_d <- state_d %>% left_join(vest_d, by = 'GEOID20')
  }

  write_csv(state_d, str_glue('{joined_path}/{str_to_lower(s)}_2020_{type}.csv'))
  log_time(here('census-vest-2020/log_time.txt'), s)

  fs::file_delete(fs::dir_ls(fs::path(tempdir(),'PL-unzip')))
}
