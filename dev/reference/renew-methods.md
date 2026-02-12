# Renew (update) values

renew method applied to MimicPopulation

renew method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicPopulation'
renew(
  x,
  invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x == -Inf | x == Inf | x
    < 0),
  population_ls = NULL,
  schedule_args_ls = list(),
  schedule_fn = NULL,
  step_dtm = lubridate::days(0),
  type_1L_chr = c("default", "customise", "schedule", "transform"),
  use_1L_chr = c("Y", "Z"),
  validate_chr = character(0),
  what_1L_chr = character(0),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)

# S4 method for class 'MimicConfiguration'
renew(
  x,
  arm_1L_chr = character(0),
  batch_1L_int = integer(0),
  draws_tb = NULL,
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

- schedule_args_ls:

  Schedule arguments (a list), Default: list()

- schedule_fn:

  Schedule (a function), Default: NULL

- step_dtm:

  Step (a date vector), Default: lubridate::days(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("default", "form")

- use_1L_chr:

  Use (a character vector of length one), Default: c("Y", "Z")

- what_1L_chr:

  What (a character vector of length one), Default: c("population")

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- ...:

  Additional arguments

- arm_1L_chr:

  Arm (a character vector of length one), Default: character(0)

- batch_1L_int:

  Batch (an integer vector), Default: integer(0)

- draws_tb:

  Draws (a tibble), Default: NULL

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  character(0)

## Value

x (An object of class MimicPopulation)

x (An object of class MimicConfiguration)
