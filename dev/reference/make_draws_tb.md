# Make draws tibble

make_draws_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make draws
tibble. The function returns Draws (a tibble).

## Usage

``` r
make_draws_tb(
  inputs_ls,
  drop_missing_1L_lgl = FALSE,
  drop_suffix_1L_chr = character(0),
  extra_draws_fn = NULL,
  iterations_int = 1:100,
  seed_1L_int = integer(0)
)
```

## Arguments

- inputs_ls:

  Inputs (a list)

- drop_missing_1L_lgl:

  Drop missing (a logical vector of length one), Default: FALSE

- drop_suffix_1L_chr:

  Drop suffix (a character vector of length one), Default: character(0)

- extra_draws_fn:

  Extra draws (a function), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: 1:100

- seed_1L_int:

  Seed (an integer vector of length one), Default: integer(0)

## Value

Draws (a tibble)
