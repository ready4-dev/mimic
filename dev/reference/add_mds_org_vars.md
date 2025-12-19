# Add Minimum Dataset organisation variables

add_mds_org_vars() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add minimum dataset organisation variables.
The function returns Data (a tibble).

## Usage

``` r
add_mds_org_vars(
  data_tb,
  provider_lup_tb,
  phn_code_1L_chr = "PHN_code",
  phn_name_1L_chr = "PHN_area_name"
)
```

## Arguments

- data_tb:

  Data (a tibble)

- provider_lup_tb:

  Provider lookup table (a tibble)

- phn_code_1L_chr:

  Primary Health Network code (a character vector of length one),
  Default: 'PHN_code'

- phn_name_1L_chr:

  Primary Health Network name (a character vector of length one),
  Default: 'PHN_area_name'

## Value

Data (a tibble)
