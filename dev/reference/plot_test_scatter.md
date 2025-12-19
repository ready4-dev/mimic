# Plot test scatter

plot_test_scatter() is a Plot function that plots data. Specifically,
this function implements an algorithm to plot test scatter. The function
returns Scatter (a plot).

## Usage

``` r
plot_test_scatter(
  X_Ready4useDyad,
  var_1L_chr,
  collapse_1L_lgl = F,
  colour_1L_chr = ready4use::get_colour_codes(style_1L_chr = "monash_2", type_1L_chr =
    "unicol"),
  grouping_1L_chr = character(0),
  new_1L_lgl = F,
  plot_tfmn_fn = identity,
  old_1L_chr = "Observed",
  type_1L_chr = c("Simulated", "Predicted")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- var_1L_chr:

  Variable (a character vector of length one)

- collapse_1L_lgl:

  Collapse (a logical vector of length one), Default: F

- colour_1L_chr:

  Colour (a character vector of length one), Default:
  ready4use::get_colour_codes(style_1L_chr = "monash_2", type_1L_chr =
  "unicol")

- grouping_1L_chr:

  Grouping (a character vector of length one), Default: character(0)

- new_1L_lgl:

  New (a logical vector of length one), Default: F

- plot_tfmn_fn:

  Plot transformation (a function), Default: identity

- old_1L_chr:

  Old (a character vector of length one), Default: 'Observed'

- type_1L_chr:

  Type (a character vector of length one), Default: c("Simulated",
  "Predicted")

## Value

Scatter (a plot)
