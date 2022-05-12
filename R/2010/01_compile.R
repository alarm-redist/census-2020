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

download_2010_baf <- function(state) {
    url <- str_glue('https://www2.census.gov/geo/docs/maps-data/data/baf/BlockAssign_ST{match_fips(state)}_{toupper(match_abb(state))}.zip')
    tf <- fs::file_temp(ext = 'zip')
    httr::GET(url = url, httr::write_disk(tf))
    zip::unzip(zipfile = tf, exdir = fs::path_dir(tf))
    path_vtd <- fs::path(fs::path_dir(tf), str_glue('BlockAssign_ST{match_fips(state)}_{toupper(match_abb(state))}_VTD.txt'))
    if (fs::file_exists(path_vtd)) {
        read_csv(file = path_vtd)
    } else {
        NULL
    }
}

# globals ----
states <- censable::stata %>%
    filter(!is.na(region)) %>%
    pull(abb)

vest_path <- here('vest-2010')
joined_path <- here('census-vest-2010')

for (s in states) {
    vest_files <- Sys.glob(str_glue('{vest_path}/{str_to_lower(s)}/*.csv'))

    # check that the state has vtds ----
    vtds <- download_2010_baf(s)
    if (is.null(vtds)) break
    type <- 'vtd'
    vtds <- vtds %>%
        transmute(
            GEOID = as.character(BLOCKID),
            vtd = DISTRICT
        )

    # download and subset pl ----
    pl <- censable::build_dec(geography = 'block', state = s, year = 2010,
                              geometry = FALSE)
    pl <- breakdown_geoid(pl)

    fips_d <- censable::fips_2010 %>%
        left_join(select(censable::stata, fips, abb), by = c('state' = 'fips')) %>%
        select(state = abb, county_code = county, county = name)

    # state_d <- pl %>%
    #   select(-.data$row_id, -.data$summary_level) %>%
    #   rename(county_code = county, GEOID20 = GEOID) %>%
    #   left_join(tigris::fips_codes, by = c('state', 'county_code')) %>%
    #   select(-county_code, -state_code, -state_name) %>%
    #   relocate(county, .after = state)

    spec <- cols(GEOID = 'c')

    # read files ----
    vest_d <- map(vest_files, read_csv, show_col_types = FALSE, col_types = spec)

    # select elections and dem & rep ----
    vest_d <- vest_d %>%
        map(~ rename_with(., ~'GEOID10', ends_with('GEOID'))) %>%
        map(~ select(., GEOID10, starts_with('G'), starts_with('R'))) %>% # gen/runoff election
        map(geomander::clean_vest) %>%
        map(~ select(., GEOID10, starts_with(c('pre', 'uss', 'gov', 'atg', 'sos')) &
                         contains(c('_rep_', '_dem_')))) %>%
        reduce(left_join, by = 'GEOID10')

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

    vest_d <- vest_d %>% rename(GEOID = GEOID10)

    state_d <- pl %>% left_join(vest_d, by = 'GEOID')

    state_d <- state_d %>% left_join(vtds, by = c('GEOID'))

    state_d <- state_d %>%
        select(state, county, vtd, starts_with(c('pop', 'vap', 'pre', 'uss', 'gov', 'atg', 'sos', 'adv', 'arv'))) %>%
        group_by(county, vtd) %>%
        summarize(
            state = state[1],
            across(where(is.numeric), sum),
            .groups = 'drop'
        ) %>%
        relocate(state, .before = everything())

    write_csv(state_d, str_glue('{joined_path}/{str_to_lower(s)}_2010_{type}.csv'))
    log_time(here('census-vest-2010/log_time.txt'), s)
}
