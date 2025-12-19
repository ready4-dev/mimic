# Get private keys list

get_private_keys_ls() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get
private keys list. The function returns Private keys (an output object
of multiple potential types).

## Usage

``` r
get_private_keys_ls(
  path_to_keys_1L_chr,
  divider_1L_chr = "//",
  what_1L_chr = c("data", "names")
)
```

## Arguments

- path_to_keys_1L_chr:

  Path to keys (a character vector of length one)

- divider_1L_chr:

  Divider (a character vector of length one), Default: '//'

- what_1L_chr:

  What (a character vector of length one), Default: c("data", "names")

## Value

Private keys (an output object of multiple potential types)
