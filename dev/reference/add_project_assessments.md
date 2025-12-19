# Add project assessments

add_project_assessments() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project assessments. The function returns
Regressions (a list).

## Usage

``` r
add_project_assessments(
  regressions_ls,
  what_1L_chr,
  colours_chr = character(0),
  confusion_1L_lgl = F,
  exclude_int = integer(0),
  group_ls = list(Treatments = c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged")),
  model_1L_int = integer(0),
  rank_1L_lgl = TRUE,
  residual_1L_chr = "normal",
  two_part_1L_lgl = FALSE,
  type_1L_chr = c("candidates", "tests", "models"),
  var_1L_chr = character(0),
  X_Ready4useDyad = ready4use::Ready4useDyad()
)
```

## Arguments

- regressions_ls:

  Regressions (a list)

- what_1L_chr:

  What (a character vector of length one)

- colours_chr:

  Colours (a character vector), Default: character(0)

- confusion_1L_lgl:

  Confusion (a logical vector of length one), Default: F

- exclude_int:

  Exclude (an integer vector), Default: integer(0)

- group_ls:

  Group (a list), Default: list(Treatments = c("Tx_Waitlist",
  "Tx_Treatment", "Tx_Discharged"))

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- rank_1L_lgl:

  Rank (a logical vector of length one), Default: TRUE

- residual_1L_chr:

  Residual (a character vector of length one), Default: 'normal'

- two_part_1L_lgl:

  Two part (a logical vector of length one), Default: FALSE

- type_1L_chr:

  Type (a character vector of length one), Default: c("candidates",
  "tests", "models")

- var_1L_chr:

  Variable (a character vector of length one), Default: character(0)

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

## Value

Regressions (a list)
