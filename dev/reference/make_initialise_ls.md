# Make initialise list

make_initialise_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make initialise
list. The function returns Initialise (a list).

## Usage

``` r
make_initialise_ls(
  default_fn = identity,
  derive_ls = list(),
  update_fn = identity
)
```

## Arguments

- default_fn:

  Default (a function), Default: identity

- derive_ls:

  Derive (a list), Default: list()

- update_fn:

  Update (a function), Default: identity

## Value

Initialise (a list)
