# Make Minimum Dataset costing dataset

make_mds_costing_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset costing dataset. The function returns COSTS (a ready4 module).

## Usage

``` r
make_mds_costing_ds(
  processed_ls,
  expenditure_tb,
  params_tb,
  provider_lup_tb,
  add_programs_int = integer(0),
  comparator_1L_chr = "Comp",
  disciplines_chr = make_disciplines(),
  end_dtm = as.Date("2024-06-30"),
  intervention_1L_chr = "Intv",
  intervention_long_1L_chr = "The intervention",
  jurisdiction_1L_chr = "Jurisdiction",
  mds_programs_lup = make_mds_program_lup(),
  start_dtm = as.Date("2023-07-01"),
  summary_1L_lgl = T
)
```

## Arguments

- processed_ls:

  Processed (a list)

- expenditure_tb:

  Expenditure (a tibble)

- params_tb:

  Parameters (a tibble)

- provider_lup_tb:

  Provider lookup table (a tibble)

- add_programs_int:

  Add programs (an integer vector), Default: integer(0)

- comparator_1L_chr:

  Comparator (a character vector of length one), Default: 'Comp'

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines()

- end_dtm:

  End (a date vector), Default: as.Date("2024-06-30")

- intervention_1L_chr:

  Intervention (a character vector of length one), Default: 'Intv'

- intervention_long_1L_chr:

  Intervention long (a character vector of length one), Default: 'The
  intervention'

- jurisdiction_1L_chr:

  Jurisdiction (a character vector of length one), Default:
  'Jurisdiction'

- mds_programs_lup:

  Minimum Dataset programs (a lookup table), Default:
  make_mds_program_lup()

- start_dtm:

  Start (a date vector), Default: as.Date("2023-07-01")

- summary_1L_lgl:

  Summary (a logical vector of length one), Default: T

## Value

COSTS (a ready4 module)
