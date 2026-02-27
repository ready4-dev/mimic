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
  event_1L_chr = character(0),
  type_1L_chr = c("trigger", "customise", "filter", "schedule"),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)

# S4 method for class 'MimicPopulation'
renew(
  x,
  batch_1L_int = integer(0),
  env_ls = list(),
  event_1L_chr = character(0),
  population_ls = NULL,
  type_1L_chr = c("trigger", "customise", "filter", "event", "reset", "schedule",
    "switch", "transform"),
  use_1L_chr = "Y",
  what_1L_chr = character(0),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)

# S4 method for class 'MimicConfiguration'
renew(x, env_ls = list(), what_1L_chr = c("legacy"), ...)
```

## Arguments

- x:

  An object of class MimicConfiguration

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- env_ls:

  Environment list (a list of environments), Default: list()

- event_1L_chr:

  Event (a character vector of length one), Default: character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("trigger",
  "customise", "filter", "event", "reset", "schedule", "switch",
  "transform")

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- ...:

  Additional arguments

- population_ls:

  Population (a list), Default: NULL

- use_1L_chr:

  Use (a character vector of length one), Default: 'Y'

- what_1L_chr:

  What (a character vector of length one), Default: c("legacy")

## Value

x (An object of class MimicActive)

x (An object of class MimicPopulation)

x (An object of class MimicConfiguration)
