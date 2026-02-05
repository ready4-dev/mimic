# Author and save files

author method applied to MimicRepos

author method applied to MimicConfiguration

## Usage

``` r
# S4 method for class 'MimicRepos'
author(
  x,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  suffix_1L_chr = "",
  what_1L_chr = c("sim_ws_dirs_chr"),
  ...
)

# S4 method for class 'MimicConfiguration'
author(
  x,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  unlink_1L_lgl = FALSE,
  what_1L_chr = c("draws"),
  Y_MimicRepos = MimicRepos()
)
```

## Arguments

- x:

  An object of class MimicConfiguration

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

- what_1L_chr:

  What (a character vector of length one), Default: c("draws")

- ...:

  Additional arguments

- unlink_1L_lgl:

  Unlink (a logical vector of length one), Default: FALSE

- Y_MimicRepos:

  PARAM_DESCRIPTION, Default: MimicRepos()

## Value

No return value, called for side effects.

No return value, called for side effects.
