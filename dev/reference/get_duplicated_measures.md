# Get duplicated measures

get_duplicated_measures() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get
duplicated measures. The function returns Duplicates (an output object
of multiple potential types).

## Usage

``` r
get_duplicated_measures(
  data_tb,
  group_by_chr,
  type_1L_chr = c("table", "uid"),
  uid_1L_chr = "episode_key",
  ungroup_1L_lgl = FALSE
)
```

## Arguments

- data_tb:

  Data (a tibble)

- group_by_chr:

  Group by (a character vector)

- type_1L_chr:

  Type (a character vector of length one), Default: c("table", "uid")

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default:
  'episode_key'

- ungroup_1L_lgl:

  Ungroup (a logical vector of length one), Default: FALSE

## Value

Duplicates (an output object of multiple potential types)
