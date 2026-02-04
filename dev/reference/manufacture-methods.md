# Manufacture a new object

manufacture method applied to MimicRepos

manufacture method applied to MimicInputs

## Usage

``` r
# S4 method for class 'MimicRepos'
manufacture(
  x,
  suffix_1L_chr = "",
  type_1L_chr = c("all", "batch_to", "draw_to"),
  what_1L_chr = c("sim_ws_dirs_chr"),
  ...
)

# S4 method for class 'MimicInputs'
manufacture(x, what_1L_chr = c("inputs_ls"))
```

## Arguments

- x:

  An object of class MimicInputs

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

- type_1L_chr:

  Type (a character vector of length one), Default: c("all", "batch_to",
  "draw_to")

- what_1L_chr:

  What (a character vector of length one), Default: c("inputs_ls")

- ...:

  Additional arguments

## Value

Object (an output object of multiple potential types)

Object (an output object of multiple potential types)
