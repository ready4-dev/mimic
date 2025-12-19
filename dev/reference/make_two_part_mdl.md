# Make two part model

make_two_part_mdl() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make two part
model. The function returns Model (a model).

## Usage

``` r
make_two_part_mdl(
  data_tb,
  family_2_1L_chr = "Gamma(link = 'inverse')",
  link_1_1L_chr = "logit",
  x_part_1_chr,
  x_part_2_chr,
  y_1L_chr,
  ...
)
```

## Arguments

- data_tb:

  Data (a tibble)

- family_2_1L_chr:

  Family 2 (a character vector of length one), Default: 'Gamma(link =
  'inverse')'

- link_1_1L_chr:

  Link 1 (a character vector of length one), Default: 'logit'

- x_part_1_chr:

  X part 1 (a character vector)

- x_part_2_chr:

  X part 2 (a character vector)

- y_1L_chr:

  Y (a character vector of length one)

- ...:

  Additional arguments

## Value

Model (a model)
