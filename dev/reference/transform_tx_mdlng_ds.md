# Transform treatment modelling dataset

transform_tx_mdlng_ds() is a Transform function that edits an object in
such a way that core object attributes - e.g. shape, dimensions,
elements, type - are altered. Specifically, this function implements an
algorithm to transform treatment modelling dataset. The function returns
Data (a tibble).

## Usage

``` r
transform_tx_mdlng_ds(
  data_tb,
  treatment_vars_chr = c("treatment_status", "treatment_status_t2"),
  what_1L_chr = c("Waitlist", "Treatment", "Discharged")
)
```

## Arguments

- data_tb:

  Data (a tibble)

- treatment_vars_chr:

  Treatment variables (a character vector), Default:
  c("treatment_status", "treatment_status_t2")

- what_1L_chr:

  What (a character vector of length one), Default: c("Waitlist",
  "Treatment", "Discharged")

## Value

Data (a tibble)
