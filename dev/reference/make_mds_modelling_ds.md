# Make Minimum Dataset modelling dataset

make_mds_modelling_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset modelling dataset. The function returns Model data (a list).

## Usage

``` r
make_mds_modelling_ds(
  processed_ls,
  outcomes_ls,
  program_services_lup,
  provider_lup_tb,
  after_dtm = as.Date("2022-07-01"),
  add_programs_int = c(1, 4),
  age_max_1L_int = integer(0),
  age_min_1L_int = integer(0),
  disciplines_chr = make_disciplines(),
  distinct_orgs_1L_lgl = TRUE,
  filter_true_1L_chr = "FlexPsych",
  impute_below_1L_dbl = 40,
  imputations_1L_int = 1,
  intervention_1L_chr = "Intv",
  jurisdiction_1L_chr = "Jurisdiction",
  mature_only_1L_lgl = TRUE,
  max_iterations_1L_int = 2,
  missing_after_dtm = Sys.Date(),
  postcode_lup = NULL,
  program_true_1L_chr = "is_program",
  require_complete_chr = character(0)
)
```

## Arguments

- processed_ls:

  Processed (a list)

- outcomes_ls:

  Outcomes (a list)

- program_services_lup:

  Program services (a lookup table)

- provider_lup_tb:

  Provider lookup table (a tibble)

- after_dtm:

  After (a date vector), Default: as.Date("2022-07-01")

- add_programs_int:

  Add programs (an integer vector), Default: c(1, 4)

- age_max_1L_int:

  Age maximum (an integer vector of length one), Default: integer(0)

- age_min_1L_int:

  Age minimum (an integer vector of length one), Default: integer(0)

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines()

- distinct_orgs_1L_lgl:

  Distinct organisations (a logical vector of length one), Default: TRUE

- filter_true_1L_chr:

  Filter true (a character vector of length one), Default: 'FlexPsych'

- impute_below_1L_dbl:

  Impute below (a double vector of length one), Default: 40

- imputations_1L_int:

  Imputations (an integer vector of length one), Default: 1

- intervention_1L_chr:

  Intervention (a character vector of length one), Default: 'Intv'

- jurisdiction_1L_chr:

  Jurisdiction (a character vector of length one), Default:
  'Jurisdiction'

- mature_only_1L_lgl:

  Mature only (a logical vector of length one), Default: TRUE

- max_iterations_1L_int:

  Maximum iterations (an integer vector of length one), Default: 2

- missing_after_dtm:

  Missing after (a date vector), Default: Sys.Date()

- postcode_lup:

  Postcode (a lookup table), Default: NULL

- program_true_1L_chr:

  Program true (a character vector of length one), Default: 'is_program'

- require_complete_chr:

  Require complete (a character vector), Default: character(0)

## Value

Model data (a list)
