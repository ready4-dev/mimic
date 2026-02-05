# Write batch

write_batch() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write batch. The function is called for its side effects and does not
return a value.

## Usage

``` r
write_batch(
  batch_1L_int,
  arms_chr,
  comparator_fn,
  draws_tb = NULL,
  drop_missing_1L_lgl,
  drop_suffix_1L_chr,
  extra_draws_fn,
  horizon_dtm,
  inputs_ls,
  intervention_fn,
  iterations_ls,
  modifiable_chr,
  prior_batches_1L_int,
  seed_1L_int,
  sensitivities_ls,
  start_dtm,
  tfmn_ls,
  utilities_chr,
  write_to_1L_chr,
  Y_MimicRepos = MimicRepos(),
  ...
)
```

## Arguments

- batch_1L_int:

  Batch (an integer vector of length one)

- arms_chr:

  Arms (a character vector)

- comparator_fn:

  Comparator (a function)

- draws_tb:

  Draws (a tibble), Default: NULL

- drop_missing_1L_lgl:

  Drop missing (a logical vector of length one)

- drop_suffix_1L_chr:

  Drop suffix (a character vector of length one)

- extra_draws_fn:

  Extra draws (a function)

- horizon_dtm:

  Horizon (a date vector)

- inputs_ls:

  Inputs (a list)

- intervention_fn:

  Intervention (a function)

- iterations_ls:

  Iterations (a list)

- modifiable_chr:

  Modifiable (a character vector)

- prior_batches_1L_int:

  Prior batches (an integer vector of length one)

- seed_1L_int:

  Seed (an integer vector of length one)

- sensitivities_ls:

  Sensitivities (a list)

- start_dtm:

  Start (a date vector)

- tfmn_ls:

  Transformation (a list)

- utilities_chr:

  Utilities (a character vector)

- write_to_1L_chr:

  Write to (a character vector of length one)

- Y_MimicRepos:

  PARAM_DESCRIPTION, Default: MimicRepos()

- ...:

  Additional arguments

## Value

No return value, called for side effects.
