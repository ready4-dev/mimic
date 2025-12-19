# Get raw parameters data

get_raw_params_data() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get raw
parameters data. The function returns Raw parameters data (a list).

## Usage

``` r
get_raw_params_data(
  path_to_param_data_1L_chr,
  program_fl_nm_1L_chr,
  unit_cost_fl_nm_1L_chr,
  cost_types_chr = character(0),
  divider_1L_chr = "\\",
  erp_fl_nm_1L_chr = character(0),
  mbs_fl_nm_1L_chr = character(0),
  mbs_sheet_1L_chr = "Table EXP.14",
  mbs_skip_1L_int = 4,
  no_care_fl_nm_1L_chr = "HILDA_K10.RDS",
  program_sheet_1L_chr,
  program_skip_1L_int = 0,
  r_dir_1L_chr = "R"
)
```

## Arguments

- path_to_param_data_1L_chr:

  Path to parameter data (a character vector of length one)

- program_fl_nm_1L_chr:

  Program file name (a character vector of length one)

- unit_cost_fl_nm_1L_chr:

  Unit cost file name (a character vector of length one)

- cost_types_chr:

  Cost types (a character vector), Default: character(0)

- divider_1L_chr:

  Divider (a character vector of length one), Default: '\\

- erp_fl_nm_1L_chr:

  Estimatedesident Population file name (a character vector of length
  one), Default: character(0)

- mbs_fl_nm_1L_chr:

  Medicare Benefits Schedule file name (a character vector of length
  one), Default: character(0)

- mbs_sheet_1L_chr:

  Medicare Benefits Schedule sheet (a character vector of length one),
  Default: 'Table EXP.14'

- mbs_skip_1L_int:

  Medicare Benefits Schedule skip (an integer vector of length one),
  Default: 4

- no_care_fl_nm_1L_chr:

  No care file name (a character vector of length one), Default:
  'HILDA_K10.RDS'

- program_sheet_1L_chr:

  Program sheet (a character vector of length one)

- program_skip_1L_int:

  Program skip (an integer vector of length one), Default: 0

- r_dir_1L_chr:

  R directory (a character vector of length one), Default: 'R'

## Value

Raw parameters data (a list)
