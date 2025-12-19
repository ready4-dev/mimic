# Update partial results

update_partial_results() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update partial results. The function is
called for its side effects and does not return a value.

## Usage

``` r
update_partial_results(
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  utilities_chr,
  update_fn = function(X_Ready4useDyad) {
     identity(X_Ready4useDyad)
 },
  combined_suffixes_chr = c("", "S01", "S02", "S10", "S11", "S12"),
  timestamp_1L_chr = get_timestamp(),
  ...
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- utilities_chr:

  Utilities (a character vector)

- update_fn:

  Update (a function), Default: function(X_Ready4useDyad)
  identity(X_Ready4useDyad)

- combined_suffixes_chr:

  Combined suffixes (a character vector), Default: c("", "S01", "S02",
  "S10", "S11", "S12")

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- ...:

  Additional arguments

## Value

X (A dataset and data dictionary pair.)
