# Update Minimum Dataset modelling dataset

update_mds_modelling_ds() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update minimum dataset modelling dataset. The
function is called for its side effects and does not return a value.

## Usage

``` r
update_mds_modelling_ds(
  X_Ready4useDyad,
  imputations_int = 1,
  sample_ls = NULL,
  filter_true_1L_chr = "FlexPsych"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- imputations_int:

  Imputations (an integer vector), Default: 1

- sample_ls:

  Sample (a list), Default: NULL

- filter_true_1L_chr:

  Filter true (a character vector of length one), Default: 'FlexPsych'

## Value

X (A dataset and data dictionary pair.)
