# Make project 1 results synthesis

make_project_1_results_synthesis() is a Make function that creates a new
R object. Specifically, this function implements an algorithm to make
project 1 results synthesis. The function is called for its side effects
and does not return a value.

## Usage

``` r
make_project_1_results_synthesis(
  inputs_ls,
  results_ls,
  modifiable_chr = c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D"),
  type_1L_chr = c("D", "AB", "C")
)
```

## Arguments

- inputs_ls:

  Inputs (a list)

- results_ls:

  Results (a list)

- modifiable_chr:

  Modifiable (a character vector), Default: c("treatment_status",
  "Minutes", "k10", "AQoL6D", "CHU9D")

- type_1L_chr:

  Type (a character vector of length one), Default: c("D", "AB", "C")

## Value

X (A dataset and data dictionary pair.)
