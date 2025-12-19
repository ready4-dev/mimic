# Update scenario names

update_scenario_names() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update scenario names. The function returns
Forecasts (a tibble).

## Usage

``` r
update_scenario_names(
  forecasts_tb,
  after_1L_chr = character(0),
  before_1L_chr = character(0),
  prefix_1L_chr = "scenario_",
  reference_1L_chr = "Status quo",
  others_chr = character(0),
  tfmn_1_fn = as.numeric,
  tfmn_2_fn = scales::percent
)
```

## Arguments

- forecasts_tb:

  Forecasts (a tibble)

- after_1L_chr:

  After (a character vector of length one), Default: character(0)

- before_1L_chr:

  Before (a character vector of length one), Default: character(0)

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: 'scenario\_'

- reference_1L_chr:

  Reference (a character vector of length one), Default: 'Status quo'

- others_chr:

  Others (a character vector), Default: character(0)

- tfmn_1_fn:

  Transformation 1 (a function), Default: as.numeric

- tfmn_2_fn:

  Transformation 2 (a function), Default: scales::percent

## Value

Forecasts (a tibble)
