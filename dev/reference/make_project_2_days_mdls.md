# Make project 2 days models

make_project_2_days_mdls() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project 2 days models. The function returns Tpm models (a list).

## Usage

``` r
make_project_2_days_mdls(
  X_Ready4useDyad,
  add_chr = character(0),
  family_2_1L_chr = "Gamma(link = 'log')",
  link_1_1L_chr = "logit",
  max_1L_dbl = Inf,
  x_part_1_ls = NULL,
  x_part_2_ls = NULL,
  y_1L_chr = "EpisodeDurationDays",
  use_1_int = integer(0),
  use_2_int = integer(0),
  ...
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- add_chr:

  Add (a character vector), Default: character(0)

- family_2_1L_chr:

  Family 2 (a character vector of length one), Default: 'Gamma(link =
  'log')'

- link_1_1L_chr:

  Link 1 (a character vector of length one), Default: 'logit'

- max_1L_dbl:

  Maximum (a double vector of length one), Default: Inf

- x_part_1_ls:

  X part 1 (a list), Default: NULL

- x_part_2_ls:

  X part 2 (a list), Default: NULL

- y_1L_chr:

  Y (a character vector of length one), Default: 'EpisodeDurationDays'

- use_1_int:

  Use 1 (an integer vector), Default: integer(0)

- use_2_int:

  Use 2 (an integer vector), Default: integer(0)

- ...:

  Additional arguments

## Value

Tpm models (a list)
