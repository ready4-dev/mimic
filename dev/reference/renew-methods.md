# Renew (update) values

renew method applied to MimicPopulation

renew method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicPopulation'
renew(
  x,
  population_ls = NULL,
  type_1L_chr = c("default", "customise", "transform"),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)

# S4 method for class 'MimicConfiguration'
renew(
  x,
  arm_1L_chr = character(0),
  draws_tb = NULL,
  iterations_int = integer(0),
  tx_prefix_1L_chr = character(0),
  type_1L_chr = c("default", "form"),
  what_1L_chr = c("population"),
  ...
)
```

## Arguments

- x:

  An object of class MimicConfiguration

- population_ls:

  Population (a list), Default: NULL

- type_1L_chr:

  Type (a character vector of length one), Default: c("default", "form")

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- ...:

  Additional arguments

- arm_1L_chr:

  Arm (a character vector of length one), Default: character(0)

- draws_tb:

  Draws (a tibble), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: integer(0)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  character(0)

- what_1L_chr:

  What (a character vector of length one), Default: c("population")

## Value

x (An object of class MimicPopulation)

x (An object of class MimicConfiguration)
