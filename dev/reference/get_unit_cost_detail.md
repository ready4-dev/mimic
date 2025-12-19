# Get unit cost detail

get_unit_cost_detail() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get unit
cost detail. The function returns Detail (an output object of multiple
potential types).

## Usage

``` r
get_unit_cost_detail(
  unit_costs_tb,
  what_1L_chr = c("scenarios", "fixed", "names", "variable")
)
```

## Arguments

- unit_costs_tb:

  Unit costs (a tibble)

- what_1L_chr:

  What (a character vector of length one), Default: c("scenarios",
  "fixed", "names", "variable")

## Value

Detail (an output object of multiple potential types)
