# Add project treatment change

add_project_treatment_change() is an Add function that updates an object
by adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project treatment change. The function
returns Dataset (a tibble).

## Usage

``` r
add_project_treatment_change(
  ds_tb,
  arrange_by_id_lgl = T,
  change_var_nm_1L_chr = "treatment_change",
  suffix_1L_chr = "_previous",
  timepoint_1L_chr = "MeasurementWeek",
  timepoint_bl_1L_chr = "Week0",
  uid_1L_chr = "UID",
  var_nm_1L_chr = "treatment_status",
  wide_1L_lgl = F
)
```

## Arguments

- ds_tb:

  Dataset (a tibble)

- arrange_by_id_lgl:

  Arrange by identity (a logical vector), Default: T

- change_var_nm_1L_chr:

  Change variable name (a character vector of length one), Default:
  'treatment_change'

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: '\_previous'

- timepoint_1L_chr:

  Timepoint (a character vector of length one), Default:
  'MeasurementWeek'

- timepoint_bl_1L_chr:

  Timepoint baseline (a character vector of length one), Default:
  'Week0'

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default: 'UID'

- var_nm_1L_chr:

  Variable name (a character vector of length one), Default:
  'treatment_status'

- wide_1L_lgl:

  Wide (a logical vector of length one), Default: F

## Value

Dataset (a tibble)
