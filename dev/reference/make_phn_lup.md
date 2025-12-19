# Make Primary Health Network lookup table

make_phn_lup() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make primary
health network lookup table. The function returns a Primary Health
Network lookup table (an output object of multiple potential types).

## Usage

``` r
make_phn_lup(
  code_1L_chr = character(0),
  name_1L_chr = character(0),
  jurisdiction_1L_chr = character(0)
)
```

## Arguments

- code_1L_chr:

  Code (a character vector of length one), Default: character(0)

- name_1L_chr:

  Name (a character vector of length one), Default: character(0)

- jurisdiction_1L_chr:

  Jurisdiction (a character vector of length one), Default: character(0)

## Value

a Primary Health Network lookup table (an output object of multiple
potential types)
