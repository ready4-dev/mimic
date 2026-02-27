# Metamorphose a model module to a model module of a different (non-inheriting) class

metamorphose method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicConfiguration'
metamorphose(
  x,
  arm_1L_chr = NA_character_,
  batch_1L_int = integer(0),
  draws_tb = NULL,
  env_ls = list(),
  Y_Ready4Module = Ready4Module(),
  ...
)
```

## Arguments

- x:

  An object of class MimicConfiguration

- arm_1L_chr:

  Arm (a character vector of length one), Default: 'NA'

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- draws_tb:

  Draws (a tibble), Default: NULL

- env_ls:

  Environment list (a list of environments), Default: list()

- Y_Ready4Module:

  PARAM_DESCRIPTION, Default: Ready4Module()

- ...:

  Additional arguments

## Value

Y_Ready4Module (An object)
