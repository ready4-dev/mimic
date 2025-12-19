# Add Initial Assessment andeferral parameters

add_iar_params() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add initial assessment andeferral parameters. The function
returns Parameters (a tibble).

## Usage

``` r
add_iar_params(
  params_tb,
  comparator_int,
  model_data_ls,
  processed_ls,
  raw_mds_data_ls,
  test_1L_chr,
  comparator_1L_chr = "Comparator",
  comparator_filter_fn = identity,
  cost_1L_dbl = 0,
  intervention_1L_chr = "Intervention",
  intervention_filter_fn = identity
)
```

## Arguments

- params_tb:

  Parameters (a tibble)

- comparator_int:

  Comparator (an integer vector)

- model_data_ls:

  Model data (a list)

- processed_ls:

  Processed (a list)

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- test_1L_chr:

  Test (a character vector of length one)

- comparator_1L_chr:

  Comparator (a character vector of length one), Default: 'Comparator'

- comparator_filter_fn:

  Comparator filter (a function), Default: identity

- cost_1L_dbl:

  Cost (a double vector of length one), Default: 0

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- intervention_filter_fn:

  Intervention filter (a function), Default: identity

## Value

Parameters (a tibble)
