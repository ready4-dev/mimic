# Ratify that input or output data meet validity criteria

ratify method applied to MimicRepos

renew method applied to MimicActive

renew method applied to MimicPopulation

renew method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicRepos'
ratify(
  x,
  batches_int = integer(0),
  gh_token_1L_chr = "",
  key_1L_chr = NULL,
  match_xx = NULL,
  prefix_1L_chr = character(0),
  suffix_1L_chr = "",
  remote_fls_chr = NA_character_,
  type_1L_chr = c("MimicInputs", "ParamDraws", "list", "element"),
  what_chr = character(0),
  ...
)

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

- batches_int:

  Batches (an integer vector), Default: integer(0)

- gh_token_1L_chr:

  GitHub token (a character vector of length one), Default: ""

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

- match_xx:

  Match (an object), Default: NULL

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: character(0)

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ""

- remote_fls_chr:

  Remote files (a character vector), Default: NA_character\_

- type_1L_chr:

  Type (a character vector of length one), Default: c("trigger",
  "customise", "filter", "event", "reset", "schedule", "switch",
  "transform")

- what_chr:

  What (a character vector), Default: character(0)

- ...:

  Additional arguments

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- env_ls:

  Environment list (a list of environments), Default: list()

- event_1L_chr:

  Event (a character vector of length one), Default: character(0)

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- population_ls:

  Population (a list), Default: NULL

- use_1L_chr:

  Use (a character vector of length one), Default: 'Y'

- what_1L_chr:

  What (a character vector of length one), Default: c("legacy")

## Value

x (An object of class MimicRepos)

x (An object of class MimicActive)

x (An object of class MimicPopulation)

x (An object of class MimicConfiguration)
