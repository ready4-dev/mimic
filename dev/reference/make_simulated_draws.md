# Make simulated draws

make_simulated_draws() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make simulated
draws. The function returns Simulations (a data.frame).

## Usage

``` r
make_simulated_draws(
  model_mdl,
  new_data_tb,
  sample_fn = rnorm,
  iterations_int = 1:100
)
```

## Arguments

- model_mdl:

  Model (a model)

- new_data_tb:

  New data (a tibble)

- sample_fn:

  Sample (a function), Default: rnorm

- iterations_int:

  Iterations (an integer vector), Default: 1:100

## Value

Simulations (a data.frame)
