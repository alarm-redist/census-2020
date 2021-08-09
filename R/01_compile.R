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
    lines <- c(lines, paste0(Sys.time(),',', state))
    writeLines(text = lines, con = path)
}

states = drop_na(censable::stata, region)$abb

vest_path = here("vest-2020")
joined_path = here("census-vest-2020")

for (s in states) {
    vest_files = Sys.glob(str_glue("{vest_path}/{str_to_lower(s)}/*.csv"))

    pl = pl_read(pl_url(s, 2020)) %>%
        pl_select_standard(clean_names=TRUE)
    type = "vtd"
    state_d = pl_subset(pl, "700")

    if (any(str_detect(vest_files, "_block_")) || nrow(state_d) == 0) { # no VTDs
        type = "block"
        state_d = pl_subset(pl, "750") %>%
            select(-vtd)
    }

    fips_d = censable::fips_2020 %>%
        left_join(select(censable::stata, fips, abb), by=c("state"="fips")) %>%
        select(state=abb, county_code=county, county=name)
    state_d = state_d %>%
        select(-.data$row_id, -.data$summary_level) %>%
        rename(county_code=county) %>%
        left_join(tigris::fips_codes, by=c("state", "county_code")) %>%
        select(-county_code) %>%
        relocate(county, .after=state)

    if (length(vest_files) > 0) {
                 # read files
        vest_d = map(vest_files, read_csv) %>%
            map(~ select(., GEOID20, starts_with("G"))) %>% # gen election only
            reduce(left_join, by="GEOID20") %>%
            # pivot and extract info from standardized col names
            pivot_longer(-GEOID20, names_to="code", values_to="votes") %>%
            mutate(year = str_sub(code, 2, 3),
                   office = str_sub(code, 4, 6),
                   party = str_to_lower(str_sub(code, 7, 7))) %>%
            # only keep elections contested by both parties (but sum votes by party)
            filter(party %in% c("d", "r")) %>%
            group_by(GEOID20, year, office) %>%
            filter(all(c("d", "r") %in% party)) %>%
            group_by(GEOID20, year, office, party) %>%
            summarize(votes = sum(votes)) %>%
            # average by year and across years
            group_by(GEOID20, year, party) %>%
            summarize(av = mean(votes)) %>%
            group_by(GEOID20, party) %>%
            mutate(nv = mean(av)) %>%
            ungroup() %>%
            # pivot back and rename nicely
            pivot_wider(names_from=year, values_from=av, names_prefix="av") %>%
            pivot_wider(names_from=party, values_from=c(nv, starts_with("av"))) %>%
            rename(ndv=nv_d, nrv=nv_r) %>%
            rename_with(str_replace, .cols=starts_with("av"),
                        "av(\\d+)_([dr])", "a\\2v_\\1") %>%
            mutate(ndv = round(ndv, 1),
                   nrv = round(nrv, 1),
                   across(c(starts_with("adv_"), starts_with("arv_")),
                          round, digits=1))
        state_d = left_join(state_d, vest_d, by="GEOID20")
    }

    write_csv(state_d, str_glue("{joined_path}/{str_to_lower(s)}_2020_{type}.csv"))
    log_time(here("census-vest-2020/log_time.txt"), s)
}
