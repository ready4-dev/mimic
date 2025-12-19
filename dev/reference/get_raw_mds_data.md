# Get raw Minimum Dataset data

get_raw_mds_data() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get raw minimum
dataset data. The function returns a Minimum Dataset data (an output
object of multiple potential types).

## Usage

``` r
get_raw_mds_data(
  path_to_raw_dir_1L_chr,
  divider_1L_chr = "\\",
  r_dir_1L_chr = "R",
  select_chr = character(0),
  type_1L_chr = c("csv", "rds"),
  what_1L_chr = c("data", "names")
)
```

## Arguments

- path_to_raw_dir_1L_chr:

  Path to raw directory (a character vector of length one)

- divider_1L_chr:

  Divider (a character vector of length one), Default: '\\

- r_dir_1L_chr:

  R directory (a character vector of length one), Default: 'R'

- select_chr:

  Select (a character vector), Default: character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("csv", "rds")

- what_1L_chr:

  What (a character vector of length one), Default: c("data", "names")

## Value

a Minimum Dataset data (an output object of multiple potential types)
