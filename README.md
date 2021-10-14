# 2020 Redistricting Data Files

*Christopher T. Kenny and Cory McCartan*

[![License: CC BY-SA 4.0](https://img.shields.io/badge/Data%20License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
[![License: MIT](https://img.shields.io/badge/Software%20License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The precinct-level demographic and election data from
the [2020 decennial census](https://www.census.gov/2020census) and the
[Voting and Election Science Team](https://dataverse.harvard.edu/dataverse/electionscience)
which have been tidied and joined together using 2020 precinct boundaries.
Where 2020 precinct boundaries are not available, Census block-level data is
provided instead, and where no VEST data is available, only demographic
information is provided. Code to generate the data from these sources is
included; the entire workflow is open-source and reproducible.

## Getting the data

The easiest way to get the data is to [download it from our website](https://alarm-redist.github.io/posts/2021-08-10-census-2020/).
You can also [download a ZIP of all the data here](https://github.com/alarm-redist/census-2020/archive/refs/heads/main.zip).

However, if you want to work with a specific set of states, or wish to join the data
to a precinct shapefile, we have created short scripts to help you do so.

### Downloading files programmatically
```r
# downloads data for state `abbr` to `folder/{abbr}_2020_*.csv` and returns path to file
download_redistricting_file = function(abbr, folder) {
    abbr = tolower(abbr)
    url_vtd = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                     "main/census-vest-2020/", abbr, "_2020_vtd.csv")
    url_block = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                       "main/census-vest-2020/", abbr, "_2020_block.csv")

    path = paste0(folder, "/", basename(url_vtd))
    resp = download(url_vtd, path)
    if (resp != 0) {
        path = paste0(folder, "/", basename(url_block))
        resp = download(url_block, path)
        if (resp != 0)  {
            stop("No files available for ", abbr)
        }
    }
    path
}

# download a single state
download_redistricting_file("WA", "data/")
# or multiple states
lapply(c("IA", "MT", "MA"), download_redistricting_file, folder="data/")
```

### Adding shapefile geometry to the data
```r
library(PL94171)
library(dplyr)
library(readr)
library(sf)

# adds precinct shapefile geometry to downloaded data 
# replace `pl_get_vtd()` with `tigris::blocks()` for block data
join_shapefile = function(data) {
    geom_d = pl_get_vtd(data$state[1]) %>%
        select(GEOID20, area_land=ALAND20, area_water=AWATER20, geometry)
    left_join(data, geom_d, by="GEOID20") %>%
        st_as_sf()
}

# download, read, and add shapefile
wa_d = download_redistricting_file("WA", "data/") %>%
    read_csv() %>%
    join_shapefile()
```

## Using the data

Please make sure to cite the
[Voting and Election Science Team](https://dataverse.harvard.edu/dataverse/electionscience)
and the [U.S. Census Bureau](https://www.census.gov/2020census).
Consult [the license](https://github.com/alarm-redist/census-2020/blob/main/LICENSE.md)
for information on modifying and sharing the data and/or code.

- For redistricting and voting rights analysis, we recommend the
  [`redist`](https://alarm-redist.github.io/redist/) package.
- For pre-processing and tidying data for redistricting analysis, we recommend the
  [`geomander`](https://christopherkenny.github.io/geomander/) package.
- For more custom tabulations of the 2020 census data, we recommend the
  [`PL94171`](https://corymccartan.github.io/PL94171/) package.
- For general-purpose census data processing, we recommend the 
  [`censable`](https://christopherkenny.github.io/censable/) package.
- For alternate data unaffected by Census differential privacy, you may want to
  consider FCC block-level estimates, available using the
  [`blockpop`](https://corymccartan.github.io/blockpop/) package.
  
### Data Format
Each data table contains several identification columns, a set of census-derived
demographic columns, and a set of VEST-derived election columns.

- `GEOID20` is the unique identifier for a precinct or Census block. 
  The `state` and `county` of the precinct or block are also provided.
- Census variables are prefixed with `pop_` or `vap_`, depending on whether
  they are for the entire population or the voting-age population.
  Suffixes refer to racial and ethnic categories, as follows:
  
    * `_hisp`: Hispanic or Latino (of any race)
    * `_white`: White alone, not Hispanic or Latino
    * `_black`: Black or African American alone, not Hispanic or Latino
    * `_aian`: American Indian and Alaska Native alone, not Hispanic or Latino
    * `_asian`: Asian alone, not Hispanic or Latino
    * `_nhpi`: Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino
    * `_other`: Some Other Race alone, not Hispanic or Latino
    * `_two`: Population of two or more races, not Hispanic or Latino
    
- Election variables consist of average vote counts for Democratic and
  Republican candidates. The `avg_##_dem` and `avg_##_rep` columns report the
  average vote count in the `##` election, across all statewide races
  contested by both parties. The `ndv` and `nrv` columns further average
  the vote counts across all available election years. Prefixes indicate offices, as follows:
  
    * `pre_`: President
    * `uss_`: United States Senate
    * `gov_`: Governor
    * `atg_`: Attorney General
    * `sos_`: Secretary of State
  
## Technical notes
To produce election data using 2020 precinct boundaries, election results were
projected down to the 2010 block level using voting-age population as weights.
Results for 2020 blocks were then estimated using 2010 blocks and the
[land-use-based crosswalk files](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T9VMJO)
from VEST. Finally, 2020 blocks were aggregated to 2020 precincts using the 
Census' 2020 [block assignment files](https://www.census.gov/geographies/reference-files/time-series/geo/block-assignment-files.html).
