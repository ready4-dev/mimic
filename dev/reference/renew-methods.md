# Renew (update) values

renew method applied to MimicActive

renew method applied to MimicPopulation

renew method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicActive'
renew(
  x,
  batch_1L_int = integer(0),
  env_ls = list(),
  type_1L_chr = c("default", "customise", "schedule"),
  X_MimicConfiguration = MimicConfiguration(),
  X_MimicSchedule = MimicSchedule(),
  ...
)

# S4 method for class 'MimicPopulation'
renew(
  x,
  batch_1L_int = integer(0),
  env_ls = list(),
  population_ls = NULL,
  type_1L_chr = c("default", "customise", "schedule", "transform"),
  what_1L_chr = character(0),
  X_MimicConfiguration = MimicConfiguration(),
  X_MimicSchedule = MimicSchedule(),
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

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- env_ls:

  Environment (a list), Default: list()

- type_1L_chr:

  Type (a character vector of length one), Default: c("default", "form")

- X_MimicConfiguration:

  X_MimicConfiguration, Default: MimicConfiguration()

- X_MimicSchedule:

  X_MimicSchedule, Default: MimicSchedule()

- ...:

  Additional arguments

- population_ls:

  Population (a list), Default: NULL

- what_1L_chr:

  What (a character vector of length one), Default: c("population")

- arm_1L_chr:

  Arm (a character vector of length one), Default: character(0)

- draws_tb:

  Draws (a tibble), Default: NULL

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  character(0)

## Value

x (An object of class MimicActive)

x (An object of class MimicPopulation)

x (An object of class MimicConfiguration)
