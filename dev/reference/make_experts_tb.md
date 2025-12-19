# Make experts tibble

make_experts_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make experts
tibble. The function returns Experts (a tibble).

## Usage

``` r
make_experts_tb(experts_ls, anonymise_1L_lgl = T, prefix_1L_chr = "Expert ")
```

## Arguments

- experts_ls:

  Experts (a list)

- anonymise_1L_lgl:

  Anonymise (a logical vector of length one), Default: T

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: 'Expert '

## Value

Experts (a tibble)
