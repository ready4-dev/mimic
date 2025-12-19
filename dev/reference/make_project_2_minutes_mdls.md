# Make project 2 minutes models

make_project_2_minutes_mdls() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project 2 minutes models. The function returns Tpm models (a list).

## Usage

``` r
make_project_2_minutes_mdls(
  X_Ready4useDyad,
  disciplines_chr = make_disciplines(),
  family_2_1L_chr = "Gamma(link = 'log')",
  link_1_1L_chr = "logit",
  x_part_1_ls = NULL,
  x_part_2_ls = NULL,
  y_1L_chr = "TotalUseMins_change",
  use_1_int = integer(0),
  use_2_int = integer(0),
  ...
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines()

- family_2_1L_chr:

  Family 2 (a character vector of length one), Default: 'Gamma(link =
  'log')'

- link_1_1L_chr:

  Link 1 (a character vector of length one), Default: 'logit'

- x_part_1_ls:

  X part 1 (a list), Default: NULL

- x_part_2_ls:

  X part 2 (a list), Default: NULL

- y_1L_chr:

  Y (a character vector of length one), Default: 'TotalUseMins_change'

- use_1_int:

  Use 1 (an integer vector), Default: integer(0)

- use_2_int:

  Use 2 (an integer vector), Default: integer(0)

- ...:

  Additional arguments

## Value

Tpm models (a list)
