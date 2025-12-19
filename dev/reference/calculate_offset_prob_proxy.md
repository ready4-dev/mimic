# Calculate offset probability proxy

calculate_offset_prob_proxy() is a Calculate function that performs a
numeric calculation. Specifically, this function implements an algorithm
to calculate offset probability proxy. The function returns Probability
proxy (a double vector of length one).

## Usage

``` r
calculate_offset_prob_proxy(
  area_erp_1L_dbl,
  exposed_1L_dbl,
  rate_1L_dbl,
  risk_1L_dbl,
  denominator_1L_dbl = 100000,
  time_1L_dbl = 1
)
```

## Arguments

- area_erp_1L_dbl:

  Area Estimatedesident Population (a double vector of length one)

- exposed_1L_dbl:

  Exposed (a double vector of length one)

- rate_1L_dbl:

  Rate (a double vector of length one)

- risk_1L_dbl:

  Risk (a double vector of length one)

- denominator_1L_dbl:

  Denominator (a double vector of length one), Default: 100000

- time_1L_dbl:

  Time (a double vector of length one), Default: 1

## Value

Probability proxy (a double vector of length one)
