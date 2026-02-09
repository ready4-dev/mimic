# Prognosticate (make predictions) by solving a forward problem

prognosticate method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicConfiguration'
prognosticate(
  x,
  Y_MimicRepos = MimicRepos(),
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  draws_1L_chr = c("make", "make_batch", "read", "read_batch"),
  options_chr = c("Y", "N"),
  purge_1L_lgl = FALSE,
  suffix_1L_chr = "",
  type_1L_chr = c("NULL", "D", "AB", "C"),
  what_1L_chr = c("all", "batch"),
  unlink_1L_lgl = TRUE,
  ...
)
```

## Arguments

- x:

  An object of class MimicConfiguration

- Y_MimicRepos:

  PARAM_DESCRIPTION, Default: MimicRepos()

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- draws_1L_chr:

  Draws (a character vector of length one), Default: c("make",
  "make_batch", "read", "read_batch")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- purge_1L_lgl:

  Purge (a logical vector of length one), Default: FALSE

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

- type_1L_chr:

  Type (a character vector of length one), Default: c("NULL", "D", "AB",
  "C")

- what_1L_chr:

  What (a character vector of length one), Default: c("all", "batch")

- unlink_1L_lgl:

  Unlink (a logical vector of length one), Default: TRUE

- ...:

  Additional arguments

## Value

Errors (a list)
