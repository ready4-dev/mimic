# Add icer

add_icer() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add icer. The function returns Data (a tibble).

## Usage

``` r
add_icer(
  data_tb,
  cost_1L_chr = "Cost",
  effect_1L_chr = "QALYs",
  suffix_1L_chr = ""
)
```

## Arguments

- data_tb:

  Data (a tibble)

- cost_1L_chr:

  Cost (a character vector of length one), Default: 'Cost'

- effect_1L_chr:

  Effect (a character vector of length one), Default: 'QALYs'

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ”

## Value

Data (a tibble)
