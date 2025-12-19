# Make project dataset

make_project_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
dataset. The function returns Project (an output object of multiple
potential types).

## Usage

``` r
make_project_ds(
  raw_data_ls,
  platform_1L_chr,
  age_1L_chr = "Age",
  cleanse_1L_chr = c("itemdims", "all", "none", "dims", "items"),
  drop_1L_lgl = TRUE,
  date_of_birth_1L_chr = "date_of_birth",
  employment_1L_chr = "employment_status",
  financial_yrs_chr = c("FY 2021", "FY 2022", "FY 2023", "FY 2024"),
  gender_1L_chr = "gender",
  index_date_1L_chr = "onboarding_date",
  outcomes_chr = c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_utl"),
  price_indices_dbl = c(97.2, 100, 100.7796, 102.5514),
  recode_1L_lgl = T,
  recode_lup_r3 = make_project_recode_lup(),
  ref_1L_chr = "FY 2024",
  type_1L_chr = c("dyad", "tibble"),
  uid_1L_chr = "UID",
  what_1L_chr = c("all", "activity", "contacts", "costs_constant", "costs_current",
    "costs_adjusted", "costs_unit", "outcomes", "overview", "notes")
)
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- platform_1L_chr:

  Platform (a character vector of length one)

- age_1L_chr:

  Age (a character vector of length one), Default: 'Age'

- cleanse_1L_chr:

  Cleanse (a character vector of length one), Default: c("itemdims",
  "all", "none", "dims", "items")

- drop_1L_lgl:

  Drop (a logical vector of length one), Default: TRUE

- date_of_birth_1L_chr:

  Date of birth (a character vector of length one), Default:
  'date_of_birth'

- employment_1L_chr:

  Employment (a character vector of length one), Default:
  'employment_status'

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

- recode_1L_lgl:

  Recode (a logical vector of length one), Default: T

- recode_lup_r3:

  Recode (a ready4 submodule extension of lookup table), Default:
  make_project_recode_lup()

- ref_1L_chr:

  Reference (a character vector of length one), Default: 'FY 2024'

- type_1L_chr:

  Type (a character vector of length one), Default: c("dyad", "tibble")

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default: 'UID'

- what_1L_chr:

  What (a character vector of length one), Default: c("all", "activity",
  "contacts", "costs_constant", "costs_current", "costs_adjusted",
  "costs_unit", "outcomes", "overview", "notes")

## Value

Project (an output object of multiple potential types)
