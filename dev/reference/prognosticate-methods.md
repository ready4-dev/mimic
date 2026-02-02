# Prognosticate (make predictions) by solving a forward problem

prognosticate method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicConfiguration'
prognosticate(
  x,
  consent_1L_chr = "",
  purge_1L_lgl = FALSE,
  type_1L_chr = c("NULL", "D", "AB", "C"),
  unlink_1L_lgl = TRUE,
  ...
)
```

## Arguments

- x:

  An object of class MimicConfiguration

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- purge_1L_lgl:

  Purge (a logical vector of length one), Default: FALSE

- type_1L_chr:

  Type (a character vector of length one), Default: c("NULL", "D", "AB",
  "C")

- unlink_1L_lgl:

  Unlink (a logical vector of length one), Default: TRUE

- ...:

  Additional arguments

## Value

Errors (a list)
