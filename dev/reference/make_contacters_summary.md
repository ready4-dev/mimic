# Make contacters summary

make_contacters_summary() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
contacters summary. The function returns Contacters (a tibble).

## Usage

``` r
make_contacters_summary(
  processed_ls,
  as_tsibble_1L_lgl = FALSE,
  type_1L_chr = serious::make_temporal_vars()
)
```

## Arguments

- processed_ls:

  Processed (a list)

- as_tsibble_1L_lgl:

  As tsibble (a logical vector of length one), Default: FALSE

- type_1L_chr:

  Type (a character vector of length one), Default:
  serious::make_temporal_vars()

## Value

Contacters (a tibble)
