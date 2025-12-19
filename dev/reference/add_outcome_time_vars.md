# Add outcome time variables

add_outcome_time_vars() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add outcome time variables. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_outcome_time_vars(
  Y_Ready4useDyad,
  outcome_1L_chr,
  add_adjustments_1L_lgl = FALSE,
  fup_var_1L_chr = character(0),
  follow_up_1L_int = integer(0),
  maintain_for_1L_int = 0L
)
```

## Arguments

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- outcome_1L_chr:

  Outcome (a character vector of length one)

- add_adjustments_1L_lgl:

  Add adjustments (a logical vector of length one), Default: FALSE

- fup_var_1L_chr:

  Follow-up variable (a character vector of length one), Default:
  character(0)

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: integer(0)

- maintain_for_1L_int:

  Maintain for (an integer vector of length one), Default: 0

## Value

Y (A dataset and data dictionary pair.)
