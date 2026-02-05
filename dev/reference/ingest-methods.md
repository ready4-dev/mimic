# Ingest data

ingest method applied to MimicRepos

## Usage

``` r
# S4 method for class 'MimicRepos'
ingest(
  x,
  batches_int = integer(0),
  gh_token_1L_chr = "",
  key_1L_chr = NULL,
  prefix_1L_chr = character(0),
  remote_fls_chr = NA_character_,
  type_1L_chr = c("MimicInputs", "ParamDraws", "list", "element"),
  what_chr = character(0),
  ...
)
```

## Arguments

- x:

  An object of class MimicRepos

- batches_int:

  Batches (an integer vector), Default: integer(0)

- gh_token_1L_chr:

  Github token (a character vector of length one), Default: ”

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: character(0)

- remote_fls_chr:

  Remote files (a character vector), Default: 'NA'

- type_1L_chr:

  Type (a character vector of length one), Default: c("MimicInputs",
  "ParamDraws", "list", "element")

- what_chr:

  What (a character vector), Default: character(0)

- ...:

  Additional arguments

## Value

Ingest (an output object of multiple potential types)
