# Make Initial Assessment andeferral parameters

make_iar_params() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make initial
assessment andeferral parameters. The function returns Parameters (a
double vector).

## Usage

``` r
make_iar_params(
  processed_ls,
  raw_mds_data_ls,
  test_1L_chr,
  comparator_1L_chr = "Comparator",
  comparator_int = integer(0),
  intervention_1L_chr = "Intervention",
  type_1L_chr = c(intervention_1L_chr, comparator_1L_chr),
  what_1L_chr = "InHouseIAR"
)
```

## Arguments

- processed_ls:

  Processed (a list)

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- test_1L_chr:

  Test (a character vector of length one)

- comparator_1L_chr:

  Comparator (a character vector of length one), Default: 'Comparator'

- comparator_int:

  Comparator (an integer vector), Default: integer(0)

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- type_1L_chr:

  Type (a character vector of length one), Default:
  c(intervention_1L_chr, comparator_1L_chr)

- what_1L_chr:

  What (a character vector of length one), Default: 'InHouseIAR'

## Value

Parameters (a double vector)
