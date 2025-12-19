# Predict from pool

predict_from_pool() is a Predict function that applies a model to make
predictions. Specifically, this function implements an algorithm to
predict from pool. The function returns Predictions (an output object of
multiple potential types).

## Usage

``` r
predict_from_pool(
  pooled_xx,
  as_1L_chr = c("vector", "histogram", "summary"),
  adjustment_1L_dbl = numeric(0),
  distributions_chr = "best",
  maximum_1L_dbl = Inf,
  minimum_1L_dbl = -Inf,
  n_1L_int = 100L,
  quantiles_dbl = c(0.05, 0.25, 0.5, 0.75, 0.95),
  resample_1L_lgl = F,
  seed_1L_int = 2001L,
  title_1L_chr = "Pooled prediction distribution",
  type_1L_chr = c("r", "q", "p"),
  x_label_1L_chr = "Predictions",
  values_dbl = numeric(0),
  weights_dbl = 1,
  what_1L_chr = character(0),
  ...
)
```

## Arguments

- pooled_xx:

  Pooled (an output object of multiple potential types)

- as_1L_chr:

  As (a character vector of length one), Default: c("vector",
  "histogram", "summary")

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: numeric(0)

- distributions_chr:

  Distributions (a character vector), Default: 'best'

- maximum_1L_dbl:

  Maximum (a double vector of length one), Default: Inf

- minimum_1L_dbl:

  Minimum (a double vector of length one), Default: -Inf

- n_1L_int:

  N (an integer vector of length one), Default: 100

- quantiles_dbl:

  Quantiles (a double vector), Default: c(0.05, 0.25, 0.5, 0.75, 0.95)

- resample_1L_lgl:

  Resample (a logical vector of length one), Default: F

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- title_1L_chr:

  Title (a character vector of length one), Default: 'Pooled prediction
  distribution'

- type_1L_chr:

  Type (a character vector of length one), Default: c("r", "q", "p")

- x_label_1L_chr:

  X label (a character vector of length one), Default: 'Predictions'

- values_dbl:

  Values (a double vector), Default: numeric(0)

- weights_dbl:

  Weights (a double vector), Default: 1

- what_1L_chr:

  What (a character vector of length one), Default: character(0)

- ...:

  Additional arguments

## Value

Predictions (an output object of multiple potential types)
