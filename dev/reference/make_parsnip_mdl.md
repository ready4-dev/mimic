# Make parsnip model

make_parsnip_mdl() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make parsnip
model. The function returns Model (a model).

## Usage

``` r
make_parsnip_mdl(
  data_tb,
  model_fn = parsnip::multinom_reg,
  model_args_ls = NULL,
  x_chr,
  y_1L_chr,
  ...
)
```

## Arguments

- data_tb:

  Data (a tibble)

- model_fn:

  Model (a function), Default: parsnip::multinom_reg

- model_args_ls:

  Model arguments (a list), Default: NULL

- x_chr:

  X (a character vector)

- y_1L_chr:

  Y (a character vector of length one)

- ...:

  Additional arguments

## Value

Model (a model)
