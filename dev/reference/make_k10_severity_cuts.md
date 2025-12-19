# Make K10 severity cuts

make_k10_severity_cuts() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make k10 severity
cuts. The function returns Severity cuts (a list).

## Usage

``` r
make_k10_severity_cuts(
  mild_int = c(10, 15),
  moderate_int = c(16, 21),
  high_int = c(22, 29),
  very_high_int = c(30, 50)
)
```

## Arguments

- mild_int:

  Mild (an integer vector), Default: c(10, 15)

- moderate_int:

  Moderate (an integer vector), Default: c(16, 21)

- high_int:

  High (an integer vector), Default: c(22, 29)

- very_high_int:

  Very high (an integer vector), Default: c(30, 50)

## Value

Severity cuts (a list)
