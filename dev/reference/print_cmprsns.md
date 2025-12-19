# Print comparisons

print_cmprsns() is a Print function that prints output to console.
Specifically, this function implements an algorithm to print
comparisons. The function returns Table (an output object of multiple
potential types).

## Usage

``` r
print_cmprsns(
  data_xx,
  output_type_1L_chr = c("HTML", "PDF", "Word"),
  what_1L_chr = c("gtsummary", "df", "null"),
  html_table_fn = NULL,
  pdf_table_fn = NULL,
  word_table_fn = NULL
)
```

## Arguments

- data_xx:

  Data (an output object of multiple potential types)

- output_type_1L_chr:

  Output type (a character vector of length one), Default: c("HTML",
  "PDF", "Word")

- what_1L_chr:

  What (a character vector of length one), Default: c("gtsummary", "df",
  "null")

- html_table_fn:

  Html table (a function), Default: NULL

- pdf_table_fn:

  Pdf table (a function), Default: NULL

- word_table_fn:

  Word table (a function), Default: NULL

## Value

Table (an output object of multiple potential types)
