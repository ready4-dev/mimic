# Add cost effectiveness statistics

add_cost_effectiveness_stats() is an Add function that updates an object
by adding new values to new or empty fields. Specifically, this function
implements an algorithm to add cost effectiveness statistics. The
function returns Data (a tibble).

## Usage

``` r
add_cost_effectiveness_stats(
  data_tb,
  threshold_1L_dbl = 96000,
  timestamp_1L_chr = get_timestamp(),
  utilities_chr = c("AQoL6D", "CHU9D")
)
```

## Arguments

- data_tb:

  Data (a tibble)

- threshold_1L_dbl:

  Threshold (a double vector of length one), Default: 96000

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL6D", "CHU9D")

## Value

Data (a tibble)
