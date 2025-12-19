# Plot regression

plot_regression() is a Plot function that plots data. Specifically, this
function implements an algorithm to plot regression. The function
returns Plot (a plot).

## Usage

``` r
plot_regression(
  regressions_ls,
  what_1L_chr,
  close_1L_lgl = TRUE,
  colors_chr = ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr =
    "monash_2"),
  constrained_1L_lgl = logical(0),
  model_1L_int = integer(0),
  named_1L_lgl = FALSE,
  part_1L_int = integer(0),
  plot_tfmn_fn = identity,
  report_1L_chr = c("all", "check", "compare", "confusion", "density", "estimates",
    "histogram", "scatter", "test"),
  type_1L_chr = c("candidates", "assessments", "models", "tests"),
  ...
)
```

## Arguments

- regressions_ls:

  Regressions (a list)

- what_1L_chr:

  What (a character vector of length one)

- close_1L_lgl:

  Close (a logical vector of length one), Default: TRUE

- colors_chr:

  Colors (a character vector), Default:
  ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr =
  "monash_2")

- constrained_1L_lgl:

  Constrained (a logical vector of length one), Default: logical(0)

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- named_1L_lgl:

  Named (a logical vector of length one), Default: FALSE

- part_1L_int:

  Part (an integer vector of length one), Default: integer(0)

- plot_tfmn_fn:

  Plot transformation (a function), Default: identity

- report_1L_chr:

  Report (a character vector of length one), Default: c("all", "check",
  "compare", "confusion", "density", "estimates", "histogram",
  "scatter", "test")

- type_1L_chr:

  Type (a character vector of length one), Default: c("candidates",
  "assessments", "models", "tests")

- ...:

  Additional arguments

## Value

Plot (a plot)
