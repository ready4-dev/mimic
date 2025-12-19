# Update with imputed

update_with_imputed() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update with imputed. The function returns
Imputed datasets (a list).

## Usage

``` r
update_with_imputed(
  project_dss_ls,
  age_1L_chr = "Age",
  employment_1L_chr = "employment_status",
  gender_1L_chr = "gender",
  imputation_args_ls = NULL,
  imputations_fn = mice::mice,
  platform_1L_chr = "platform",
  recode_lup_r3 = make_project_recode_lup(),
  uid_1L_chr = "case_number",
  type_1L_chr = c("modelled", "fixed"),
  what_chr = c("contacts", "outcomes", "overview")
)
```

## Arguments

- project_dss_ls:

  Project datasets (a list)

- age_1L_chr:

  Age (a character vector of length one), Default: 'Age'

- employment_1L_chr:

  Employment (a character vector of length one), Default:
  'employment_status'

- gender_1L_chr:

  Gender (a character vector of length one), Default: 'gender'

- imputation_args_ls:

  Imputation arguments (a list), Default: NULL

- imputations_fn:

  Imputations (a function), Default: mice::mice

- platform_1L_chr:

  Platform (a character vector of length one), Default: 'platform'

- recode_lup_r3:

  Recode (a ready4 submodule extension of lookup table), Default:
  make_project_recode_lup()

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default:
  'case_number'

- type_1L_chr:

  Type (a character vector of length one), Default: c("modelled",
  "fixed")

- what_chr:

  What (a character vector), Default: c("contacts", "outcomes",
  "overview")

## Value

Imputed datasets (a list)
