# Make project test dataset

make_project_test_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project test
dataset. The function is called for its side effects and does not return
a value.

## Usage

``` r
make_project_test_ds(model_data_ls, update_qalys_fn = identity)
```

## Arguments

- model_data_ls:

  Model data (a list)

- update_qalys_fn:

  Update Quality Adjusted Life Years (a function), Default: identity

## Value

X (A dataset and data dictionary pair.)
