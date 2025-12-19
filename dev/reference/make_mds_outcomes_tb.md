# Make Minimum Dataset outcomes tibble

make_mds_outcomes_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset outcomes tibble. The function returns Outcomes (a tibble).

## Usage

``` r
make_mds_outcomes_tb(
  raw_mds_data_ls,
  processed_ls,
  provider_lup_tb,
  add_start_date_1L_lgl,
  filter_fn = identity,
  program_services_lup,
  program_true_chr,
  program_type_ls,
  as_wide_1L_lgl = FALSE,
  frequencies_chr = paste0("k10p_item", c(1:10, 14)),
  integers_chr = c("k10p_score", paste0("k10p_item", 11:13)),
  mature_after_dtm = lubridate::years(1),
  outcomes_ls = list(k10p = c("k10p_score", paste0("k10p_item", 11:14))),
  remove_duplicates_1L_lgl = TRUE,
  review_target_dtm = lubridate::days(90)
)
```

## Arguments

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- processed_ls:

  Processed (a list)

- provider_lup_tb:

  Provider lookup table (a tibble)

- add_start_date_1L_lgl:

  Add start date (a logical vector of length one)

- filter_fn:

  Filter (a function), Default: identity

- program_services_lup:

  Program services (a lookup table)

- program_true_chr:

  Program true (a character vector)

- program_type_ls:

  Program type (a list)

- as_wide_1L_lgl:

  As wide (a logical vector of length one), Default: FALSE

- frequencies_chr:

  Frequencies (a character vector), Default: paste0("k10p_item", c(1:10,
  14))

- integers_chr:

  Integers (a character vector), Default: c("k10p_score",
  paste0("k10p_item", 11:13))

- mature_after_dtm:

  Mature after (a date vector), Default: lubridate::years(1)

- outcomes_ls:

  Outcomes (a list), Default: list(k10p = c("k10p_score",
  paste0("k10p_item", 11:14)))

- remove_duplicates_1L_lgl:

  Remove duplicates (a logical vector of length one), Default: TRUE

- review_target_dtm:

  Review target (a date vector), Default: lubridate::days(90)

## Value

Outcomes (a tibble)
