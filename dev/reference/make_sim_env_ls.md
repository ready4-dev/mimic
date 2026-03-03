# Make sim environment list

make_sim_env_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make sim
environment list. The function returns Sim (a list of environments).

## Usage

``` r
make_sim_env_ls(
  sim_env_ls,
  append_ls = list(),
  discard_chr = "X_MimicConfiguration"
)
```

## Arguments

- sim_env_ls:

  Sim (a list of environments)

- append_ls:

  Append (a list), Default: list()

- discard_chr:

  Discard (a character vector), Default: 'X_MimicConfiguration'

## Value

Sim (a list of environments)
