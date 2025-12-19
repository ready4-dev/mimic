# Update scheduled date

update_scheduled_date() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update scheduled date. The function is called
for its side effects and does not return a value.

## Usage

``` r
update_scheduled_date(
  X_Ready4useDyad,
  increment_1L_int = integer(0),
  target_1L_int = integer(0),
  variable_1L_chr = character(0),
  type_1L_chr = c("End", "Day")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- increment_1L_int:

  Increment (an integer vector of length one), Default: integer(0)

- target_1L_int:

  Target (an integer vector of length one), Default: integer(0)

- variable_1L_chr:

  Variable (a character vector of length one), Default: character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("End", "Day")

## Value

X (A dataset and data dictionary pair.)
