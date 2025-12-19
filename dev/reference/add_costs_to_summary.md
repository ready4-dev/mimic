# Add costs to summary

add_costs_to_summary() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add costs to summary. The function returns
Summaries (a list).

## Usage

``` r
add_costs_to_summary(
  summaries_ls,
  processed_ls,
  periods_1L_int = 26,
  arrange_1L_chr = character(0),
  bind_to_tb = NULL,
  cost_tfmn_fn = identity,
  reference_1L_chr = "Status quo",
  tfmn_fn = identity,
  timestamp_1L_chr = get_timestamp(),
  type_1L_chr = c("change_scenario", "change_scen_sq", "change_sq", "scenario",
    "scen_sq", "sq", "summary_scenario"),
  unit_1L_chr = c("minutes", "clients")
)
```

## Arguments

- summaries_ls:

  Summaries (a list)

- processed_ls:

  Processed (a list)

- periods_1L_int:

  Periods (an integer vector of length one), Default: 26

- arrange_1L_chr:

  Arrange (a character vector of length one), Default: character(0)

- bind_to_tb:

  Bind to (a tibble), Default: NULL

- cost_tfmn_fn:

  Cost transformation (a function), Default: identity

- reference_1L_chr:

  Reference (a character vector of length one), Default: 'Status quo'

- tfmn_fn:

  Transformation (a function), Default: identity

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- type_1L_chr:

  Type (a character vector of length one), Default: c("change_scenario",
  "change_scen_sq", "change_sq", "scenario", "scen_sq", "sq",
  "summary_scenario")

- unit_1L_chr:

  Unit (a character vector of length one), Default: c("minutes",
  "clients")

## Value

Summaries (a list)
