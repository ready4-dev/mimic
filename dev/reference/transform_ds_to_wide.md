# Transform dataset to wide

transform_ds_to_wide() is a Transform function that edits an object in
such a way that core object attributes - e.g. shape, dimensions,
elements, type - are altered. Specifically, this function implements an
algorithm to transform dataset to wide. The function is called for its
side effects and does not return a value.

## Usage

``` r
transform_ds_to_wide(
  X_Ready4useDyad,
  processed_ls,
  join_before_dtm = NULL,
  key_vars_chr = make_project_keys(),
  max_periods_1L_int = integer(0),
  max_tenure_1L_dbl = numeric(0),
  metric_var_1L_chr = "Minutes",
  patterns_ls = list(c("[[:space:]]", ""), c("NA", "MISSING")),
  period_var_1L_chr = "Year",
  separation_after_dbl = 3,
  service_var_1L_chr = "primary_purpose"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- processed_ls:

  Processed (a list)

- join_before_dtm:

  Join before (a date vector), Default: NULL

- key_vars_chr:

  Key variables (a character vector), Default: make_project_keys()

- max_periods_1L_int:

  Maximum periods (an integer vector of length one), Default: integer(0)

- max_tenure_1L_dbl:

  Maximum tenure (a double vector of length one), Default: numeric(0)

- metric_var_1L_chr:

  Metric variable (a character vector of length one), Default: 'Minutes'

- patterns_ls:

  Patterns (a list), Default: list(c("\[:space:\]", ""), c("NA",
  "MISSING"))

- period_var_1L_chr:

  Period variable (a character vector of length one), Default: 'Year'

- separation_after_dbl:

  Separation after (a double vector), Default: 3

- service_var_1L_chr:

  Service variable (a character vector of length one), Default:
  'primary_purpose'

## Value

Y (A dataset and data dictionary pair.)
