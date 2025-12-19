# Make pooled fit

make_pooled_fit() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make pooled fit.
The function returns Fit (an output object of multiple potential types).

## Usage

``` r
make_pooled_fit(experts_tb, question_1L_chr = character(0))
```

## Arguments

- experts_tb:

  Experts (a tibble)

- question_1L_chr:

  Question (a character vector of length one), Default: character(0)

## Value

Fit (an output object of multiple potential types)
