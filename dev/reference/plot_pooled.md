# Plot pooled

plot_pooled() is a Plot function that plots data. Specifically, this
function implements an algorithm to plot pooled. The function returns
Plot (a plot).

## Usage

``` r
plot_pooled(
  pooled_fits_ls,
  what_1L_chr,
  distributions_chr = "best",
  pool_1L_lgl = TRUE,
  ...
)
```

## Arguments

- pooled_fits_ls:

  Pooled fits (a list)

- what_1L_chr:

  What (a character vector of length one)

- distributions_chr:

  Distributions (a character vector), Default: 'best'

- pool_1L_lgl:

  Pool (a logical vector of length one), Default: TRUE

- ...:

  Additional arguments

## Value

Plot (a plot)
