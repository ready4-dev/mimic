# Plot economic results

plot_economic_results() is a Plot function that plots data.
Specifically, this function implements an algorithm to plot economic
results. The function returns Plot (a plot).

## Usage

``` r
plot_economic_results(
  economic_results_ls,
  what_1L_chr,
  alpha_1L_dbl = 0.8,
  colour_1L_chr = ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr =
    "monash_2"),
  currency_1L_chr = "$",
  plot_tfmn_fn = identity,
  size_1L_dbl = 3,
  threshold_1L_dbl = 96000,
  title_1L_chr = " ",
  type_1L_chr = c("cep", "ceac", "evi"),
  x_limits_dbl = numeric(0),
  ...
)
```

## Arguments

- economic_results_ls:

  Economic results (a list)

- what_1L_chr:

  What (a character vector of length one)

- alpha_1L_dbl:

  Alpha (a double vector of length one), Default: 0.8

- colour_1L_chr:

  Colour (a character vector of length one), Default:
  ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr =
  "monash_2")

- currency_1L_chr:

  Currency (a character vector of length one), Default: '\$'

- plot_tfmn_fn:

  Plot transformation (a function), Default: identity

- size_1L_dbl:

  Size (a double vector of length one), Default: 3

- threshold_1L_dbl:

  Threshold (a double vector of length one), Default: 96000

- title_1L_chr:

  Title (a character vector of length one), Default: ' '

- type_1L_chr:

  Type (a character vector of length one), Default: c("cep", "ceac",
  "evi")

- x_limits_dbl:

  X limits (a double vector), Default: numeric(0)

- ...:

  Additional arguments

## Value

Plot (a plot)
