# Get pooled

get_pooled() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get pooled. The
function returns Predictions (an output object of multiple potential
types).

## Usage

``` r
get_pooled(
  pooled_ls,
  what_1L_chr,
  as_1L_chr = c("vector", "histogram", "summary"),
  n_1L_int = 5000,
  seed_1L_int = 2001L,
  resample_1L_lgl = TRUE
)
```

## Arguments

- pooled_ls:

  Pooled (a list)

- what_1L_chr:

  What (a character vector of length one)

- as_1L_chr:

  As (a character vector of length one), Default: c("vector",
  "histogram", "summary")

- n_1L_int:

  N (an integer vector of length one), Default: 5000

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- resample_1L_lgl:

  Resample (a logical vector of length one), Default: TRUE

## Value

Predictions (an output object of multiple potential types)
