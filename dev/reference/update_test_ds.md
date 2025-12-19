# Update test dataset

update_test_ds() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update test dataset. The function is called
for its side effects and does not return a value.

## Usage

``` r
update_test_ds(
  X_Ready4useDyad,
  modifiable_chr,
  pattern_1L_chr = "{col}_1_year",
  period_dtm = lubridate::years(1),
  type_1L_chr = c("all", "main", "change", "zero")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- modifiable_chr:

  Modifiable (a character vector)

- pattern_1L_chr:

  Pattern (a character vector of length one), Default: 'col_1_year'

- period_dtm:

  Period (a date vector), Default: lubridate::years(1)

- type_1L_chr:

  Type (a character vector of length one), Default: c("all", "main",
  "change", "zero")

## Value

X (A dataset and data dictionary pair.)
