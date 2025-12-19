# Calculate Assessment of Quality of Life Eight Dimension from K10

calculate_aqol8d_from_k10() is a Calculate function that performs a
numeric calculation. Specifically, this function implements an algorithm
to calculate assessment of quality of life eight dimension from k10. The
function returns an Assessment of Quality of Life Eight Dimension (a
double vector of length one).

## Usage

``` r
calculate_aqol8d_from_k10(
  age_1L_dbl,
  k10_1L_dbl,
  norway_1L_lgl = FALSE,
  source_1L_chr = c("10.1192/bjp.bp.113.136036")
)
```

## Arguments

- age_1L_dbl:

  Age (a double vector of length one)

- k10_1L_dbl:

  K10 (a double vector of length one)

- norway_1L_lgl:

  Norway (a logical vector of length one), Default: FALSE

- source_1L_chr:

  Source (a character vector of length one), Default:
  c("10.1192/bjp.bp.113.136036")

## Value

an Assessment of Quality of Life Eight Dimension (a double vector of
length one)
