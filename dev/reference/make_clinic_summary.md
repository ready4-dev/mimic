# Make clinic summary

make_clinic_summary() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make clinic
summary. The function returns Clinics (a tibble).

## Usage

``` r
make_clinic_summary(processed_ls, type_1L_chr = serious::make_temporal_vars())
```

## Arguments

- processed_ls:

  Processed (a list)

- type_1L_chr:

  Type (a character vector of length one), Default:
  serious::make_temporal_vars()

## Value

Clinics (a tibble)
