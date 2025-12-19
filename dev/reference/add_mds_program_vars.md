# Add Minimum Dataset program variables

add_mds_program_vars() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add minimum dataset program variables. The
function returns Data (a tibble).

## Usage

``` r
add_mds_program_vars(
  data_tb,
  processed_ls,
  program_services_lup,
  program_true_chr,
  program_type_ls,
  provider_lup_tb,
  add_start_date_1L_lgl = TRUE,
  filter_fn = identity,
  mature_after_dtm = lubridate::years(1)
)
```

## Arguments

- data_tb:

  Data (a tibble)

- processed_ls:

  Processed (a list)

- program_services_lup:

  Program services (a lookup table)

- program_true_chr:

  Program true (a character vector)

- program_type_ls:

  Program type (a list)

- provider_lup_tb:

  Provider lookup table (a tibble)

- add_start_date_1L_lgl:

  Add start date (a logical vector of length one), Default: TRUE

- filter_fn:

  Filter (a function), Default: identity

- mature_after_dtm:

  Mature after (a date vector), Default: lubridate::years(1)

## Value

Data (a tibble)
