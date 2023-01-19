# libs ----
library(tidyverse)
library(geomander)
library(PL94171)
library(here)
library(sf)
library(tigris)
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

# run
for (year in years) {
    states <- vest_states(year)
    states <- intersect(states, state.abb)
    for (state in states) {
        fs::dir_create(here('vest-2010', state))

        if (!fs::file_exists(here(glue::glue('vest-2010/{state}/{state}_{year}_2010_block_data.csv'))) &
            !fs::file_exists(here(glue::glue('vest-2010/{state}/{state}_{year}_2010_vtd_data.csv')))) {


            # Step 1: Get 2010 Census blocks ----
            block <- build_dec('block', state = state, groups = 'all', year = 2010) %>%
                #select(-NAME) %>%
                st_transform(3875)
            vest <- get_vest(state = state, year = year, clean_names = FALSE) %>%
                st_transform(st_crs(block))

            # Step 2: Match Blocks to VEST data ----
            match_list <- geo_match(from = block, to = vest, method = 'centroid')

            # Step 3: Estimate election data down to 2010 blocks ----
            elec_at_2010 <- block %>%
                as_tibble() %>%
                select(starts_with('GEOID'), -any_of('geometry'))
            elections <- names(vest)[str_detect(names(vest), str_c('G', year - 2000)) |
                                         str_detect(names(vest), str_c('R', year - 2000))]
            for (election in elections) {
                elec_at_2010 <- elec_at_2010 %>%
                    mutate(!!election := estimate_down(
                        value = vest[[election]], wts = block[['vap']],
                        group = match_list
                    ))
            }

            # Step 4: Save 2010 block-level outputs ----
            elec_at_2010 <- elec_at_2010 %>%
                mutate(across(where(is.numeric), round, digits=1))
            write_csv(elec_at_2010, file = here(glue::glue('vest-2010/{tolower(state)}/{tolower(state)}_{year}_2010_block_data.csv')))
        }

        log_time(here("vest-2010/log_time.txt"), state, year)
    }
}

withr::deferred_clear()

# lapply(fs::dir_ls('vest-2010', recurse = TRUE, glob = '*.csv'), function(f) {
#     read_csv(f) %>%
#         select(-starts_with('pop'), -starts_with('vap'), -any_of('geometry')) %>%
#         write_csv(file = f)
# })
