# Add costs event

add_costs_event() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add costs event. The function is called for
its side effects and does not return a value.

## Usage

``` r
add_costs_event(
  X_Ready4useDyad,
  inputs_ls,
  add_logic_fn = identity,
  add_offsets_1L_lgl = FALSE,
  base_for_rates_int = 1L,
  offsets_chr = character(0),
  type_1L_chr = c("variable", "fixed", "both", "zero"),
  variable_unit_1L_chr = "Minutes"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- inputs_ls:

  Inputs (a list)

- add_logic_fn:

  Add logic (a function), Default: identity

- add_offsets_1L_lgl:

  Add offsets (a logical vector of length one), Default: FALSE

- base_for_rates_int:

  Base for rates (an integer vector), Default: 1

- offsets_chr:

  Offsets (a character vector), Default: character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("variable",
  "fixed", "both", "zero")

- variable_unit_1L_chr:

  Variable unit (a character vector of length one), Default: 'Minutes'

## Value

X (A dataset and data dictionary pair.)
