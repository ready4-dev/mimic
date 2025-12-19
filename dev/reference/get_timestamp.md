# Get timestamp

get_timestamp() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get timestamp.
The function returns Timestamp (a character vector of length one).

## Usage

``` r
get_timestamp(
  sensitivities_ls = make_sensitivities_ls(),
  prefix_1L_chr = "_",
  what_1L_chr = c("outcomes", "costs")
)
```

## Arguments

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: '\_'

- what_1L_chr:

  What (a character vector of length one), Default: c("outcomes",
  "costs")

## Value

Timestamp (a character vector of length one)
