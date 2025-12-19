# Make project dictionary

make_project_dictionary() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project dictionary. The function returns Dictionary (an output object of
multiple potential types).

## Usage

``` r
make_project_dictionary(
  raw_data_ls,
  platform_1L_chr,
  dss_ls = NULL,
  financial_yrs_chr = c("FY 2021", "FY 2022", "FY 2023", "FY 2024"),
  gender_1L_chr = "gender",
  index_date_1L_chr = "onboarding_date",
  outcomes_chr = c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_utl"),
  price_indices_dbl = c(97.2, 100, 100.7796, 102.5514),
  price_ref_1L_int = 4L,
  recode_1L_lgl = T,
  recode_lup_r3 = make_project_recode_lup(),
  ref_1L_chr = "FY 2024",
  type_1L_chr = c("core", "all", "values"),
  what_chr = c("activity", "contacts", "costs_constant", "costs_current",
    "costs_adjusted", "costs_unit", "outcomes", "overview", "notes")
)
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- platform_1L_chr:

  Platform (a character vector of length one)

- dss_ls:

  Datasets (a list), Default: NULL

- financial_yrs_chr:

  Financial years (a character vector), Default: c("FY 2021", "FY 2022",
  "FY 2023", "FY 2024")

- gender_1L_chr:

  Gender (a character vector of length one), Default: 'gender'

- index_date_1L_chr:

  Index date (a character vector of length one), Default:
  'onboarding_date'

- outcomes_chr:

  Outcomes (a character vector), Default: c("gad2", "phq2", "gad7",
  "phq9", "k10", "chu9d_utl")

- price_indices_dbl:

  Price indices (a double vector), Default: c(97.2, 100, 100.7796,
  102.5514)

- price_ref_1L_int:

  Price reference (an integer vector of length one), Default: 4

- recode_1L_lgl:

  Recode (a logical vector of length one), Default: T

- recode_lup_r3:

  Recode (a ready4 submodule extension of lookup table), Default:
  make_project_recode_lup()

- ref_1L_chr:

  Reference (a character vector of length one), Default: 'FY 2024'

- type_1L_chr:

  Type (a character vector of length one), Default: c("core", "all",
  "values")

- what_chr:

  What (a character vector), Default: c("activity", "contacts",
  "costs_constant", "costs_current", "costs_adjusted", "costs_unit",
  "outcomes", "overview", "notes")

## Value

Dictionary (an output object of multiple potential types)
