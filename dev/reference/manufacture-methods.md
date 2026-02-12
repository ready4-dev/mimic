# Manufacture a new object

manufacture method applied to MimicConfiguration

manufacture method applied to MimicRepos

manufacture method applied to MimicPopulation

manufacture method applied to MimicInputs

## Usage

``` r
# S4 method for class 'MimicConfiguration'
manufacture(
  x,
  arm_1L_chr = NA_character_,
  batch_1L_int = integer(0),
  extras_ls = list(),
  what_1L_chr = c("draws_tb", "args_all", "iterations", "population_ls")
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
```

## Arguments

- x:

  An object of class MimicInputs

- arm_1L_chr:

  Arm (a character vector of length one), Default: 'NA'

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- extras_ls:

  Extras (a list), Default: list()

- what_1L_chr:

  What (a character vector of length one), Default: c("inputs_ls")

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: character(0)

- return_1L_chr:

  Return (a character vector of length one), Default: c("default",
  "batches", "files")

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

- type_1L_chr:

  Type (a character vector of length one), Default: c("all", "batch_to",
  "draw_to")

- ...:

  Additional arguments

## Value

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)
