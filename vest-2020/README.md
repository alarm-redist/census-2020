# Project VEST Data to 2020 VTDs

This folder contains intermediate data consisting of VEST election returns
projected from 2010-era VTDs to 2020 VTDs (where available) or 2020 Census
blocks (where not).

Steps:
1. Match VEST precincts to 2010 Census blocks
2. Add populations to 2010 blocks 
3. Estimate election data down to 2010 blocks
4. Crosswalk election data to 2020 blocks by VEST's land use area.
5. Aggregate to 2020 VTDs by BAFs
