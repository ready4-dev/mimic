# Make treatment model confusion

make_tx_mdl_confusion() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make treatment
model confusion. The function returns Confusion (a list).

## Usage

``` r
make_tx_mdl_confusion(
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  tx_mdls_ls,
  model_1L_int,
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

- treatment_vars_chr:

  Treatment variables (a character vector), Default:
  c("treatment_status", "treatment_status_t2")

- what_1L_chr:

  What (a character vector of length one), Default: c("Waitlist",
  "Treatment", "Discharged")

## Value

Confusion (a list)
