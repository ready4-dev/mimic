# Add Quality Adjusted Life Years sensitivities

add_qalys_sensitivities() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add quality adjusted life years
sensitivities. The function is called for its side effects and does not
return a value.

## Usage

``` r
add_qalys_sensitivities(
  X_Ready4useDyad,
  end_var_1L_chr = character(0),
  sensitivities_ls = make_sensitivities_ls(),
  start_var_1L_chr = character(0),
  utility_1L_chr = c("AQoL6D"),
  type_1L_chr = c("main", "legacy")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- end_var_1L_chr:

  End variable (a character vector of length one), Default: character(0)

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- start_var_1L_chr:

  Start variable (a character vector of length one), Default:
  character(0)

- utility_1L_chr:

  Utility (a character vector of length one), Default: c("AQoL6D")

- type_1L_chr:

  Type (a character vector of length one), Default: c("main", "legacy")

## Value

X (A dataset and data dictionary pair.)
