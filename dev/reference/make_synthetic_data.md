# Make synthetic data

make_synthetic_data() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make synthetic
data. The function returns Output (a list).

## Usage

``` r
make_synthetic_data(
  model_data_ls,
  imputations_1L_int = 5L,
  seed_1L_int = 2001,
  size_1L_int = 1000,
  transform_gender_1L_lgl = TRUE
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- imputations_1L_int:

  Imputations (an integer vector of length one), Default: 5

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- size_1L_int:

  Size (an integer vector of length one), Default: 1000

- transform_gender_1L_lgl:

  Transform gender (a logical vector of length one), Default: TRUE

## Value

Output (a list)
