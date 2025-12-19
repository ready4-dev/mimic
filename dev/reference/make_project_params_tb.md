# Make project parameters tibble

make_project_params_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
parameters tibble. The function returns Parameters (a tibble).

## Usage

``` r
make_project_params_tb(make_with_fn = make_project_1_params_tb)
```

## Arguments

- make_with_fn:

  Make with (a function), Default: make_project_1_params_tb

## Value

Parameters (a tibble)
