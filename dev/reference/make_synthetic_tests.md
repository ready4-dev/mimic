# Make synthetic tests

make_synthetic_tests() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make synthetic
tests. The function returns Synthetic tests (a list).

## Usage

``` r
make_synthetic_tests(
  population_ls,
  model_data_ls = NULL,
  original_tb = NULL,
  comparison_1L_chr = c("OutcomesJoinersImputed", "Joiners", "OutcomesJoiners",
    "Outcomes"),
  ...
)
```

## Arguments

- population_ls:

  Population (a list)

- model_data_ls:

  Model data (a list), Default: NULL

- original_tb:

  Original (a tibble), Default: NULL

- comparison_1L_chr:

  Comparison (a character vector of length one), Default:
  c("OutcomesJoinersImputed", "Joiners", "OutcomesJoiners", "Outcomes")

- ...:

  Additional arguments

## Value

Synthetic tests (a list)
