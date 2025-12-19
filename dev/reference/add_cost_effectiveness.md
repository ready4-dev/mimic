# Add cost effectiveness

add_cost_effectiveness() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add cost effectiveness. The function returns
Data (a tibble).

## Usage

``` r
add_cost_effectiveness(
  data_tb,
  cost_1L_chr = "Cost",
  dominance_1L_chr = character(0),
  effect_1L_chr = "QALYs",
  icer_1L_chr = character(0),
  suffix_1L_chr = "",
  threshold_1L_dbl = 96000
)
```

## Arguments

- data_tb:

  Data (a tibble)

- cost_1L_chr:

  Cost (a character vector of length one), Default: 'Cost'

- dominance_1L_chr:

  Dominance (a character vector of length one), Default: character(0)

- effect_1L_chr:

  Effect (a character vector of length one), Default: 'QALYs'

- icer_1L_chr:

  Icer (a character vector of length one), Default: character(0)

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

- threshold_1L_dbl:

  Threshold (a double vector of length one), Default: 96000

## Value

Data (a tibble)
