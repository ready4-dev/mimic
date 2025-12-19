# Add non helpseekers

add_non_helpseekers() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add non helpseekers. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_non_helpseekers(
  X_Ready4useDyad,
  arms_for_non_helpseeking_chr = character(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- arms_for_non_helpseeking_chr:

  Arms for non helpseeking (a character vector), Default: character(0)

## Value

X (A dataset and data dictionary pair.)
