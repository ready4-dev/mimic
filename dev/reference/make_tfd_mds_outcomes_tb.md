# Make transformed Minimum Dataset outcomes tibble

make_tfd_mds_outcomes_tb() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
transformed minimum dataset outcomes tibble. The function returns Data
(a tibble).

## Usage

``` r
make_tfd_mds_outcomes_tb(
  processed_ls,
  outcomes_ls,
  jurisdiction_1L_chr = "Jurisdiction",
  missing_after_dtm = Sys.Date(),
  postcode_lup = NULL,
  referrals_1L_lgl = FALSE,
  reviews_1L_lgl = FALSE
)
```

## Arguments

- processed_ls:

  Processed (a list)

- outcomes_ls:

  Outcomes (a list)

- jurisdiction_1L_chr:

  Jurisdiction (a character vector of length one), Default:
  'Jurisdiction'

- missing_after_dtm:

  Missing after (a date vector), Default: Sys.Date()

- postcode_lup:

  Postcode (a lookup table), Default: NULL

- referrals_1L_lgl:

  Referrals (a logical vector of length one), Default: FALSE

- reviews_1L_lgl:

  Reviews (a logical vector of length one), Default: FALSE

## Value

Data (a tibble)
