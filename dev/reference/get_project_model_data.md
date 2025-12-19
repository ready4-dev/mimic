# Get project model data

get_project_model_data() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get
project model data. The function is called for its side effects and does
not return a value.

## Usage

``` r
get_project_model_data(
  model_data_ls,
  what_1L_chr,
  type_1L_chr = c("imputed", "unimputed")
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- what_1L_chr:

  What (a character vector of length one)

- type_1L_chr:

  Type (a character vector of length one), Default: c("imputed",
  "unimputed")

## Value

X (A dataset and data dictionary pair.)
