# Write batch

write_batch() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write batch. The function is called for its side effects and does not
return a value.

## Usage

``` r
write_batch(
  batch_1L_int,
  arms_chr = character(0),
  comparator_fn = NULL,
  draws_tb = NULL,
  drop_missing_1L_lgl = FALSE,
  drop_suffix_1L_chr = FALSE,
  extra_draws_fn = NULL,
  horizon_dtm = lubridate::years(1),
  inputs_ls = NULL,
  intervention_fn = NULL,
  iterations_ls = NULL,
  modifiable_chr = character(0),
  prior_batches_1L_int = integer(0),
  seed_1L_int = 2001L,
  sensitivities_ls = NULL,
  start_dtm = Sys.Date(),
  tfmn_ls = NULL,
  utilities_chr = character(0),
  write_to_1L_chr,
  X_MimicConfiguration = MimicConfiguration(),
  ...
)
```

## Arguments

- batch_1L_int:

  Batch (an integer vector of length one)

- arms_chr:

  Arms (a character vector), Default: character(0)

- comparator_fn:

  Comparator (a function), Default: NULL

- draws_tb:

  Draws (a tibble), Default: NULL

- drop_missing_1L_lgl:

  Drop missing (a logical vector of length one), Default: FALSE

- drop_suffix_1L_chr:

  Drop suffix (a character vector of length one), Default: FALSE

- extra_draws_fn:

  Extra draws (a function), Default: NULL

- horizon_dtm:

  Horizon (a date vector), Default: lubridate::years(1)

- inputs_ls:

  Inputs (a list), Default: NULL

- intervention_fn:

  Intervention (a function), Default: NULL

- iterations_ls:

  Iterations (a list), Default: NULL

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- prior_batches_1L_int:

  Prior batches (an integer vector of length one), Default: integer(0)

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- sensitivities_ls:

  Sensitivities (a list), Default: NULL

- start_dtm:

  Start (a date vector), Default: Sys.Date()

- tfmn_ls:

  Transformation (a list), Default: NULL

- utilities_chr:

  Utilities (a character vector), Default: character(0)

- write_to_1L_chr:

  Write to (a character vector of length one)

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- ...:

  Additional arguments

## Value

No return value, called for side effects.
