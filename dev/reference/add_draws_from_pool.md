# Add draws from pool

add_draws_from_pool() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add draws from pool. The function returns
Draws (a tibble).

## Usage

``` r
add_draws_from_pool(
  draws_tb,
  inputs_ls,
  iterations_1L_int,
  scale_1L_int = 10L,
  seed_1L_int = 1000L
)
```

## Arguments

- draws_tb:

  Draws (a tibble)

- inputs_ls:

  Inputs (a list)

- iterations_1L_int:

  Iterations (an integer vector of length one)

- scale_1L_int:

  Scale (an integer vector of length one), Default: 10

- seed_1L_int:

  Seed (an integer vector of length one), Default: 1000

## Value

Draws (a tibble)
