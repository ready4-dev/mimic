# Add project outcomes data

add_project_outcomes_data() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project outcomes data. The function
returns Model data (a list).

## Usage

``` r
add_project_outcomes_data(model_data_ls, processed_ls, mdls_lup = NULL)
```

## Arguments

- model_data_ls:

  Model data (a list)

- processed_ls:

  Processed (a list)

- mdls_lup:

  Models (a lookup table), Default: NULL

## Value

Model data (a list)
