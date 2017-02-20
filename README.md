# farsr

[![Travis-CI Build Status](https://travis-ci.org/vsemichev/farsr.svg?branch=master)](https://travis-ci.org/vsemichev/farsr)

farsr provides a set of functions to manipulate [Fatality Analysis Reporting System (FARS)](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars) report files.


The package can be installed using

```R
ghit::install_github("vsemichev/farsr")
```
#### List of functions

- fars_read - read a FARS report file in CVS format
- make_filename - generate FARS report file name for a given year
- fars_read_years - read multiple FARS reports for a list of years
- fars_summarize_years - produce a monthly summary report for a list of years
- fars_map_state - draw a state map showing accident locations

#### Usage Examples

```R
df <- fars_read("../data/accident_2013.csv.bz2")
nm <- make_filename(2017)
mon_year <- fars_read_years(c(2013,2014,2015))
smry <- fars_summarize_years(list(2013,2014))
fars_map_state(6,2015)
```

