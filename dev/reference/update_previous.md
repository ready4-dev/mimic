# Update previous

update_previous() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update previous. The function is called for
its side effects and does not return a value.

## Usage

``` r
update_previous(
  X_Ready4useDyad,
  modifiable_chr = character(0),
  pattern_1L_chr = "{col}_previous"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- pattern_1L_chr:

  Pattern (a character vector of length one), Default: 'col_previous'

## Value

X (A dataset and data dictionary pair.)
