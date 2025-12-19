# Make conditional variables

make_conditional_vars() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make conditional
variables. The function returns Variable (a character vector of length
one).

## Usage

``` r
make_conditional_vars(
  outcome_1L_chr,
  follow_up_1L_int = integer(0),
  fup_var_1L_chr = character(0),
  type_1L_chr = c("end", "fup", "start", "years")
)
```

## Arguments

- outcome_1L_chr:

  Outcome (a character vector of length one)

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: integer(0)

- fup_var_1L_chr:

  Follow-up variable (a character vector of length one), Default:
  character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("end", "fup",
  "start", "years")

## Value

Variable (a character vector of length one)
