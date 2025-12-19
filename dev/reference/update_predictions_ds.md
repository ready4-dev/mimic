# Update predictions dataset

update_predictions_ds() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update predictions dataset. The function is
called for its side effects and does not return a value.

## Usage

``` r
update_predictions_ds(
  Y_Ready4useDyad,
  adjustment_1L_dbl = 0,
  do_int = 1:5,
  follow_up_1L_int = 12L,
  maintain_for_1L_int = 0L,
  sensitivities_ls = make_sensitivities_ls(),
  tfmn_1L_chr = "NTF",
  tfmn_ls = make_class_tfmns(),
  utility_1L_chr = c("AQoL6D"),
  var_1L_chr = character(0),
  with_1L_chr = "_sim_mean"
)
```

## Arguments

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: 0

- do_int:

  Do (an integer vector), Default: 1:5

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: 12

- maintain_for_1L_int:

  Maintain for (an integer vector of length one), Default: 0

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- tfmn_1L_chr:

  Transformation (a character vector of length one), Default: 'NTF'

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns()

- utility_1L_chr:

  Utility (a character vector of length one), Default: c("AQoL6D")

- var_1L_chr:

  Variable (a character vector of length one), Default: character(0)

- with_1L_chr:

  With (a character vector of length one), Default: '\_sim_mean'

## Value

Y (A dataset and data dictionary pair.)
