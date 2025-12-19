# Make Minimum Dataset unique measures

make_mds_unique_measures() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
minimum dataset unique measures. The function returns Data (a tibble).

## Usage

``` r
make_mds_unique_measures(
  data_tb,
  episodes_tb,
  review_target_dtm = lubridate::days(90),
  prefixes_chr = c("k10p_", "iar_dst_")
)
```

## Arguments

- data_tb:

  Data (a tibble)

- episodes_tb:

  Episodes (a tibble)

- review_target_dtm:

  Review target (a date vector), Default: lubridate::days(90)

- prefixes_chr:

  Prefixes (a character vector), Default: c("k10p\_", "iar_dst\_")

## Value

Data (a tibble)
