# Add box conditionally

add_box_conditionally() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add box conditionally. The function returns
Table (an output object of multiple potential types).

## Usage

``` r
add_box_conditionally(
  table_xx,
  html_table_fn = identity,
  output_type_1L_chr = c("HTML", "PDF", "Word"),
  pdf_table_fn = identity,
  word_table_fn = flextable::theme_box
)
```

## Arguments

- table_xx:

  Table (an output object of multiple potential types)

- html_table_fn:

  Html table (a function), Default: identity

- output_type_1L_chr:

  Output type (a character vector of length one), Default: c("HTML",
  "PDF", "Word")

- pdf_table_fn:

  Pdf table (a function), Default: identity

- word_table_fn:

  Word table (a function), Default: flextable::theme_box

## Value

Table (an output object of multiple potential types)
