# Make disciplines

make_disciplines() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make disciplines.
The function returns Disciplines (a character vector).

## Usage

``` r
make_disciplines(
  arrange_1L_chr = c("default", "semi", "ordered"),
  exclude_chr = character(0)
)
```

## Arguments

- arrange_1L_chr:

  Arrange (a character vector of length one), Default: c("default",
  "semi", "ordered")

- exclude_chr:

  Exclude (a character vector), Default: character(0)

## Value

Disciplines (a character vector)
