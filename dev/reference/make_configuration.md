# Make configuration

make_configuration() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make
configuration. The function is called for its side effects and does not
return a value.

## Usage

``` r
make_configuration(
  arms_chr,
  drop_missing_1L_lgl,
  drop_suffix_1L_chr,
  extra_draws_fn,
  horizon_dtm,
  iterations_ls,
  modifiable_chr,
  seed_1L_int,
  sensitivities_ls,
  start_dtm,
  synthesis_fn,
  transformations_ls,
  utilities_chr,
  arms_extras_ls = list()
)
```

## Arguments

- arms_chr:

  Arms (a character vector)

- drop_missing_1L_lgl:

  Drop missing (a logical vector of length one)

- drop_suffix_1L_chr:

  Drop suffix (a character vector of length one)

- extra_draws_fn:

  Extra draws (a function)

- horizon_dtm:

  Horizon (a date vector)

- iterations_ls:

  Iterations (a list)

- modifiable_chr:

  Modifiable (a character vector)

- seed_1L_int:

  Seed (an integer vector of length one)

- sensitivities_ls:

  Sensitivities (a list)

- start_dtm:

  Start (a date vector)

- synthesis_fn:

  Synthesis (a function)

- transformations_ls:

  Transformations (a list)

- utilities_chr:

  Utilities (a character vector)

- arms_extras_ls:

  Arms extras (a list), Default: list()

## Value

X (Configuration details for a simulation run.)
