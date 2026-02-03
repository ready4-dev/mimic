# Ingest data

ingest method applied to MimicRepos

## Usage

``` r
# S4 method for class 'MimicRepos'
ingest(
  x,
  gh_token_1L_chr = "",
  key_1L_chr = NULL,
  remote_fls_chr = NA_character_,
  type_1L_chr = c("MimicInputs", "list", "element"),
  what_chr = character(0),
  ...
)
```

## Arguments

- x:

  An object of class MimicRepos

- gh_token_1L_chr:

  Github token (a character vector of length one), Default: ”

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

- remote_fls_chr:

  Remote files (a character vector), Default: 'NA'

- type_1L_chr:

  Type (a character vector of length one), Default: c("MimicInputs",
  "list", "element")

- what_chr:

  What (a character vector), Default: character(0)

- ...:

  Additional arguments

## Value

Ingest (an output object of multiple potential types)
