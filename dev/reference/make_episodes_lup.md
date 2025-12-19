# Make episodes lookup table

make_episodes_lup() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make episodes
lookup table. The function returns Episodes lookup table (a tibble).

## Usage

``` r
make_episodes_lup(
  processed_ls,
  provider_lup_tb,
  add_programs_int = integer(0),
  distinct_orgs_1L_lgl = TRUE,
  drop_chr = character(0),
  filter_1L_lgl = TRUE,
  filter_fn = identity,
  keep_chr = c("client_key", "program_type", "referral_date"),
  phn_code_1L_chr = "PHN_code",
  phn_name_1L_chr = "PHN_area_name",
  program_services_lup = NULL,
  program_true_1L_chr = "is_program",
  program_true_int = integer(0),
  type_1L_chr = c("one", "two")
)
```

## Arguments

- processed_ls:

  Processed (a list)

- provider_lup_tb:

  Provider lookup table (a tibble)

- add_programs_int:

  Add programs (an integer vector), Default: integer(0)

- distinct_orgs_1L_lgl:

  Distinct organisations (a logical vector of length one), Default: TRUE

- drop_chr:

  Drop (a character vector), Default: character(0)

- filter_1L_lgl:

  Filter (a logical vector of length one), Default: TRUE

- filter_fn:

  Filter (a function), Default: identity

- keep_chr:

  Keep (a character vector), Default: c("client_key", "program_type",
  "referral_date")

- phn_code_1L_chr:

  Primary Health Network code (a character vector of length one),
  Default: 'PHN_code'

- phn_name_1L_chr:

  Primary Health Network name (a character vector of length one),
  Default: 'PHN_area_name'

- program_services_lup:

  Program services (a lookup table), Default: NULL

- program_true_1L_chr:

  Program true (a character vector of length one), Default: 'is_program'

- program_true_int:

  Program true (an integer vector), Default: integer(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("one", "two")

## Value

Episodes lookup table (a tibble)
