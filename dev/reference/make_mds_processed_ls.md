# Make Minimum Dataset processed list

make_mds_processed_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset processed list. The function returns Processed (a list).

## Usage

``` r
make_mds_processed_ls(
  raw_mds_data_ls,
  add_start_date_1L_lgl,
  program_services_lup,
  program_true_chr,
  program_type_ls,
  provider_lup_tb,
  filter_fn = identity,
  frequencies_chr = c(paste0("k10p_item", c(1:10, 14))),
  integers_chr = c("k10p_score", paste0("k10p_item", 11:13),
    "iar_dst_practitioner_level_of_care"),
  mature_after_dtm = lubridate::years(1),
  outcomes_ls = make_project_2_outcomes_ls(),
  review_target_dtm = lubridate::days(90)
)
```

## Arguments

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- add_start_date_1L_lgl:

  Add start date (a logical vector of length one)

- program_services_lup:

  Program services (a lookup table)

- program_true_chr:

  Program true (a character vector)

- program_type_ls:

  Program type (a list)

- provider_lup_tb:

  Provider lookup table (a tibble)

- filter_fn:

  Filter (a function), Default: identity

- frequencies_chr:

  Frequencies (a character vector), Default: c(paste0("k10p_item",
  c(1:10, 14)))

- integers_chr:

  Integers (a character vector), Default: c("k10p_score",
  paste0("k10p_item", 11:13), "iar_dst_practitioner_level_of_care")

- mature_after_dtm:

  Mature after (a date vector), Default: lubridate::years(1)

- outcomes_ls:

  Outcomes (a list), Default: make_project_2_outcomes_ls()

- review_target_dtm:

  Review target (a date vector), Default: lubridate::days(90)

## Value

Processed (a list)
