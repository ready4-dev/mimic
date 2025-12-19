# Add cost offsets

add_cost_offsets() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add cost offsets. The function returns Data
(a tibble).

## Usage

``` r
add_cost_offsets(
  data_tb,
  inputs_ls,
  offsets_chr,
  add_logic_fn = identity,
  base_for_rates_int = 1L
)
```

## Arguments

- data_tb:

  Data (a tibble)

- inputs_ls:

  Inputs (a list)

- offsets_chr:

  Offsets (a character vector)

- add_logic_fn:

  Add logic (a function), Default: identity

- base_for_rates_int:

  Base for rates (an integer vector), Default: 1

## Value

Data (a tibble)
