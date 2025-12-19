# Plot treatment model confusion

plot_tx_mdl_confusion() is a Plot function that plots data.
Specifically, this function implements an algorithm to plot treatment
model confusion. The function returns Confusion (a plot).

## Usage

``` r
plot_tx_mdl_confusion(
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  tx_mdls_ls,
  model_1L_int,
  high_1L_chr = "#2E86C1",
  low_1L_chr = "#D6EAF8",
  treatment_vars_chr = c("treatment_status", "treatment_status_t2"),
  what_1L_chr = c("Waitlist", "Treatment", "Discharged")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- tx_mdls_ls:

  Treatment models (a list)

- model_1L_int:

  Model (an integer vector of length one)

- high_1L_chr:

  High (a character vector of length one), Default: '#2E86C1'

- low_1L_chr:

  Low (a character vector of length one), Default: '#D6EAF8'

- treatment_vars_chr:

  Treatment variables (a character vector), Default:
  c("treatment_status", "treatment_status_t2")

- what_1L_chr:

  What (a character vector of length one), Default: c("Waitlist",
  "Treatment", "Discharged")

## Value

Confusion (a plot)
