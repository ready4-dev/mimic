# Make project notes dataset

make_project_notes_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
notes dataset. The function returns Notes (a tibble).

## Usage

``` r
make_project_notes_ds(raw_data_ls, financial_yrs_chr)
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- financial_yrs_chr:

  Financial years (a character vector)

## Value

Notes (a tibble)
