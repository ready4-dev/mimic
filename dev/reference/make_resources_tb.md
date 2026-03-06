# Make resources tibble

make_resources_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make resources
tibble. The function returns Resources (a tibble).

## Usage

``` r
make_resources_tb(
  resources_chr = character(0),
  categories_chr = character(0),
  derivation_chr = character(0),
  measures_chr = character(0),
  subcategories_chr = character(0),
  totals_chr = character(0)
)
```

## Arguments

- resources_chr:

  Resources (a character vector), Default: character(0)

- categories_chr:

  Categories (a character vector), Default: character(0)

- derivation_chr:

  Derivation (a character vector), Default: character(0)

- measures_chr:

  Measures (a character vector), Default: character(0)

- subcategories_chr:

  Subcategories (a character vector), Default: character(0)

- totals_chr:

  Totals (a character vector), Default: character(0)

## Value

Resources (a tibble)
