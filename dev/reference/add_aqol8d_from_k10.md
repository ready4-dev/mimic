# Add Assessment of Quality of Life Eight Dimension from K10

add_aqol8d_from_k10() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add assessment of quality of life eight
dimension from k10. The function returns Data (an output object of
multiple potential types).

## Usage

``` r
add_aqol8d_from_k10(
  data_xx,
  correspondences_r3 = ready4show::ready4show_correspondences(),
  norway_1L_lgl = FALSE,
  source_1L_chr = c("10.1192/bjp.bp.113.136036"),
  tidy_cols_1L_lgl = FALSE,
  var_1L_chr = "AQoL8D"
)
```

## Arguments

- data_xx:

  Data (an output object of multiple potential types)

- correspondences_r3:

  Correspondences (a ready4 submodule), Default:
  ready4show::ready4show_correspondences()

- norway_1L_lgl:

  Norway (a logical vector of length one), Default: FALSE

- source_1L_chr:

  Source (a character vector of length one), Default:
  c("10.1192/bjp.bp.113.136036")

- tidy_cols_1L_lgl:

  Tidy columns (a logical vector of length one), Default: FALSE

- var_1L_chr:

  Variable (a character vector of length one), Default: 'AQoL8D'

## Value

Data (an output object of multiple potential types)
