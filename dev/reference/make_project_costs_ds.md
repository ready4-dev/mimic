# Make project costs dataset

make_project_costs_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
costs dataset. The function returns Costs (a tibble).

## Usage

``` r
make_project_costs_ds(
  dss_ls,
  end_dtm = NULL,
  financial_yrs_chr = c("FY 2021", "FY 2022", "FY 2023", "FY 2024"),
  price_indices_dbl = c(97.2, 100, 100.7796, 102.5514),
  ref_1L_chr = "FY 2024",
  start_dtm = NULL,
  sunk_ls = list(Base = make_project_sunk_tb(), S1 = make_project_sunk_tb(0)),
  type_1L_chr = c("constant", "current"),
  what_1L_chr = c("all", "fixed", "variable", "unit", "initial")
)
```

## Arguments

- dss_ls:

  Datasets (a list)

- end_dtm:

  End (a date vector), Default: NULL

- financial_yrs_chr:

  Financial years (a character vector), Default: c("FY 2021", "FY 2022",
  "FY 2023", "FY 2024")

- price_indices_dbl:

  Price indices (a double vector), Default: c(97.2, 100, 100.7796,
  102.5514)

- ref_1L_chr:

  Reference (a character vector of length one), Default: 'FY 2024'

- start_dtm:

  Start (a date vector), Default: NULL

- sunk_ls:

  Sunk (a list), Default: list(Base = make_project_sunk_tb(), S1 =
  make_project_sunk_tb(0))

- type_1L_chr:

  Type (a character vector of length one), Default: c("constant",
  "current")

- what_1L_chr:

  What (a character vector of length one), Default: c("all", "fixed",
  "variable", "unit", "initial")

## Value

Costs (a tibble)
