# Calculate Estimatedesident Population for areas

calculate_erp_for_areas() is a Calculate function that performs a
numeric calculation. Specifically, this function implements an algorithm
to calculate estimatedesident population for areas. The function returns
Estimatedesident Population for areas (a double vector).

## Usage

``` r
calculate_erp_for_areas(
  raw_erp_tb = NULL,
  path_1L_chr = character(0),
  areas_chr,
  count_1L_chr = character(0),
  name_1L_chr = character(0),
  summarise_1L_lgl = TRUE,
  var_1L_chr = "LGA"
)
```

## Arguments

- raw_erp_tb:

  Raw Estimatedesident Population (a tibble), Default: NULL

- path_1L_chr:

  Path (a character vector of length one), Default: character(0)

- areas_chr:

  Areas (a character vector)

- count_1L_chr:

  Count (a character vector of length one), Default: character(0)

- name_1L_chr:

  Name (a character vector of length one), Default: character(0)

- summarise_1L_lgl:

  Summarise (a logical vector of length one), Default: TRUE

- var_1L_chr:

  Variable (a character vector of length one), Default: 'LGA'

## Value

Estimatedesident Population for areas (a double vector)
