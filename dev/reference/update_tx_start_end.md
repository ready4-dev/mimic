# Update treatment start end

update_tx_start_end() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update treatment start end. The function is
called for its side effects and does not return a value.

## Usage

``` r
update_tx_start_end(
  X_Ready4useDyad,
  prefix_1L_chr = "treatment",
  tx_duration_dtm = lubridate::weeks(12)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: 'treatment'

- tx_duration_dtm:

  Treatment duration (a date vector), Default: lubridate::weeks(12)

## Value

X (A dataset and data dictionary pair.)
