# Make outcomes tibble

make_outcomes_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make outcomes
tibble. The function returns Outcomes (a tibble).

## Usage

``` r
make_outcomes_tb(
  outcomes_chr = character(0),
  categories_chr = character(0),
  derivation_chr = character(0),
  subcategories_chr = character(0)
)
```

## Arguments

- outcomes_chr:

  Outcomes (a character vector), Default: character(0)

- categories_chr:

  Categories (a character vector), Default: character(0)

- derivation_chr:

  Derivation (a character vector), Default: character(0)

- subcategories_chr:

  Subcategories (a character vector), Default: character(0)

## Value

Outcomes (a tibble)
