# Manufacture a new object

manufacture method applied to MimicArguments

manufacture method applied to MimicEligible

manufacture method applied to MimicConfiguration

manufacture method applied to MimicRepos

manufacture method applied to MimicPopulation

manufacture method applied to MimicInputs

manufacture method applied to MimicDerivations

manufacture method applied to MimicVariables

## Usage

``` r
# S4 method for class 'MimicArguments'
manufacture(
  x,
  batch_1L_int = integer(0),
  env_ls = list(),
  what_1L_chr = c("args_ls"),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)

# S4 method for class 'MimicEligible'
manufacture(
  x,
  append_ls = list(),
  type_1L_chr = c("filter", "reset"),
  what_1L_chr = "args_ls",
  ...
)

# S4 method for class 'MimicConfiguration'
manufacture(
  x,
  arm_1L_chr = NA_character_,
  batch_1L_int = integer(0),
  draws_tb = NULL,
  extras_ls = list(),
  include_chr = "Modelled",
  subset_1L_chr = character(0),
  target_1L_chr = character(0),
  total_1L_lgl = TRUE,
  type_1L_chr = c("current", "concept", "entry", "measure"),
  what_1L_chr = c("draws_tb", "append_ls", "args_all", "daystonever", "iterations",
    "modifiable", "outcomes", "population_ls", "resources", "utilities"),
  ...
)

# S4 method for class 'MimicRepos'
manufacture(
  x,
  prefix_1L_chr = character(0),
  return_1L_chr = c("default", "batches", "files"),
  suffix_1L_chr = "",
  type_1L_chr = c("all", "batch_to", "draw_to"),
  what_1L_chr = c("sim_ws_dirs_chr"),
  ...
)

# S4 method for class 'MimicPopulation'
manufacture(x, what_1L_chr = c("population_ls"), ...)

# S4 method for class 'MimicInputs'
manufacture(x, what_1L_chr = c("inputs_ls"))

# S4 method for class 'MimicDerivations'
manufacture(
  x,
  env_ls = list(),
  flatten_1L_lgl = FALSE,
  name_1L_chr = character(0),
  what_1L_chr = c("args_ls"),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)

# S4 method for class 'MimicVariables'
manufacture(
  x,
  include_chr = character(0),
  subset_1L_chr = character(0),
  target_1L_chr = character(0),
  total_1L_lgl = FALSE,
  type_1L_chr = c("measure", "concept"),
  what_1L_chr = c("both", "outcomes", "resources"),
  ...
)
```

## Arguments

- x:

  An object of class MimicVariables

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- env_ls:

  Environment list (a list of environments), Default: list()

- what_1L_chr:

  What (a character vector of length one), Default: c("both",
  "outcomes", "resources")

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- ...:

  Additional arguments

- append_ls:

  Append (a list), Default: list()

- type_1L_chr:

  Type (a character vector of length one), Default: c("measure",
  "concept")

- arm_1L_chr:

  Arm (a character vector of length one), Default: 'NA'

- draws_tb:

  Draws (a tibble), Default: NULL

- extras_ls:

  Extras (a list), Default: list()

- include_chr:

  Include (a character vector), Default: character(0)

- subset_1L_chr:

  Subset (a character vector of length one), Default: character(0)

- target_1L_chr:

  Target (a character vector of length one), Default: character(0)

- total_1L_lgl:

  Total (a logical vector of length one), Default: FALSE

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: character(0)

- return_1L_chr:

  Return (a character vector of length one), Default: c("default",
  "batches", "files")

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

- flatten_1L_lgl:

  Flatten (a logical vector of length one), Default: FALSE

- name_1L_chr:

  Name (a character vector of length one), Default: character(0)

## Value

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)
