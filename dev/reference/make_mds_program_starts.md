# Make Minimum Dataset program starts

make_mds_program_starts() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
minimum dataset program starts. The function returns Start up (a lookup
table).

## Usage

``` r
make_mds_program_starts(
  processed_ls,
  filter_fn,
  program_services_lup,
  program_true_chr,
  program_type_ls,
  provider_lup_tb,
  add_start_date_1L_lgl = FALSE,
  mature_after_dtm = lubridate::years(1)
)
```

## Arguments

- processed_ls:

  Processed (a list)

- filter_fn:

  Filter (a function)

- program_services_lup:

  Program services (a lookup table)

- program_true_chr:

  Program true (a character vector)

- program_type_ls:

  Program type (a list)

- provider_lup_tb:

  Provider lookup table (a tibble)

- add_start_date_1L_lgl:

  Add start date (a logical vector of length one), Default: FALSE

- mature_after_dtm:

  Mature after (a date vector), Default: lubridate::years(1)

## Value

Start up (a lookup table)
