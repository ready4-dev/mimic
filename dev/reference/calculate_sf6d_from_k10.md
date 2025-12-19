# Calculate Short Form - Six Dimension from K10

calculate_sf6d_from_k10() is a Calculate function that performs a
numeric calculation. Specifically, this function implements an algorithm
to calculate short form - six dimension from k10. The function returns a
Short Form - Six Dimension (a double vector of length one).

## Usage

``` r
calculate_sf6d_from_k10(
  female_1L_lgl,
  k10_1L_dbl,
  beta_female_moderate_1L_dbl = -0.059,
  beta_female_high_1L_dbl = -0.124,
  beta_male_moderate_1L_dbl = -0.055,
  beta_male_high_1L_dbl = -0.123,
  beta_constant_1L_dbl = 0.805,
  source_1L_chr = c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036")
)
```

## Arguments

- female_1L_lgl:

  Female (a logical vector of length one)

- k10_1L_dbl:

  K10 (a double vector of length one)

- beta_female_moderate_1L_dbl:

  Beta female moderate (a double vector of length one), Default: -0.059

- beta_female_high_1L_dbl:

  Beta female high (a double vector of length one), Default: -0.124

- beta_male_moderate_1L_dbl:

  Beta male moderate (a double vector of length one), Default: -0.055

- beta_male_high_1L_dbl:

  Beta male high (a double vector of length one), Default: -0.123

- beta_constant_1L_dbl:

  Beta constant (a double vector of length one), Default: 0.805

- source_1L_chr:

  Source (a character vector of length one), Default:
  c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036")

## Value

a Short Form - Six Dimension (a double vector of length one)
