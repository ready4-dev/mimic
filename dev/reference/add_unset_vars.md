# Add unset variables

add_unset_vars() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add unset variables. The function returns Data (a tibble).

## Usage

``` r
add_unset_vars(data_tb, var_names_chr, value_xx = 0)
```

## Arguments

- data_tb:

  Data (a tibble)

- var_names_chr:

  Variable names (a character vector)

- value_xx:

  Value (an output object of multiple potential types), Default: 0

## Value

Data (a tibble)
