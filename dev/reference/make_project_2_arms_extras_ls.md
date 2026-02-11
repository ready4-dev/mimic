# Make project 2 arms extras list

make_project_2_arms_extras_ls() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project 2 arms extras list. The function returns Arms extras (a list).

## Usage

``` r
make_project_2_arms_extras_ls(
  arms_chr,
  arms_for_iar_adjustment_chr = character(0),
  arms_for_intervention_costs_chr = character(0),
  arms_for_non_helpseeking_chr = character(0),
  arms_for_offsets_chr = character(0),
  treatment_ls = NULL,
  tx_duration_dtm = lubridate::weeks(12)
)
```

## Arguments

- arms_chr:

  Arms (a character vector)

- arms_for_iar_adjustment_chr:

  Arms for Initial Assessment andeferral adjustment (a character
  vector), Default: character(0)

- arms_for_intervention_costs_chr:

  Arms for intervention costs (a character vector), Default:
  character(0)

- arms_for_non_helpseeking_chr:

  Arms for non helpseeking (a character vector), Default: character(0)

- arms_for_offsets_chr:

  Arms for offsets (a character vector), Default: character(0)

- treatment_ls:

  Treatment (a list), Default: NULL

- tx_duration_dtm:

  Treatment duration (a date vector), Default: lubridate::weeks(12)

## Value

Arms extras (a list)
