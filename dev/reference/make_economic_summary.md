# Make economic summary

make_economic_summary() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make economic
summary. The function returns Economic (an output object of multiple
potential types).

## Usage

``` r
make_economic_summary(
  sim_results_ls,
  correspondences_r3 = ready4show::ready4show_correspondences(),
  costs_1L_chr = "Cost",
  effects_1L_chr = "QALYs",
  reference_1L_chr = "Intervention",
  threshold_1L_dbl = 96000,
  what_1L_chr = "total"
)
```

## Arguments

- sim_results_ls:

  Sim results (a list)

- correspondences_r3:

  Correspondences (a ready4 submodule), Default:
  ready4show::ready4show_correspondences()

- costs_1L_chr:

  Costs (a character vector of length one), Default: 'Cost'

- effects_1L_chr:

  Effects (a character vector of length one), Default: 'QALYs'

- reference_1L_chr:

  Reference (a character vector of length one), Default: 'Intervention'

- threshold_1L_dbl:

  Threshold (a double vector of length one), Default: 96000

- what_1L_chr:

  What (a character vector of length one), Default: 'total'

## Value

Economic (an output object of multiple potential types)
