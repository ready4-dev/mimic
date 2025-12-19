# Make Minimum Dataset expenditure tibble

make_mds_expenditure_tb() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
minimum dataset expenditure tibble. The function returns Expenditure (a
tibble).

## Usage

``` r
make_mds_expenditure_tb(
  raw_expenditure_tb = NULL,
  path_to_param_data_1L_chr = character(0),
  file_nm_1L_chr = character(0),
  program_1L_chr,
  sheet_1L_chr = character(0),
  skip_1L_int = 0,
  additional_tb = NULL,
  price_indices_dbl = make_aus_price_years("2021-22"),
  program_services_lup = NULL,
  price_ref_1L_int = 3,
  year_1L_chr = "2023-24"
)
```

## Arguments

- raw_expenditure_tb:

  Raw expenditure (a tibble), Default: NULL

- path_to_param_data_1L_chr:

  Path to parameter data (a character vector of length one), Default:
  character(0)

- file_nm_1L_chr:

  File name (a character vector of length one), Default: character(0)

- program_1L_chr:

  Program (a character vector of length one)

- sheet_1L_chr:

  Sheet (a character vector of length one), Default: character(0)

- skip_1L_int:

  Skip (an integer vector of length one), Default: 0

- additional_tb:

  Additional (a tibble), Default: NULL

- price_indices_dbl:

  Price indices (a double vector), Default:
  make_aus_price_years("2021-22")

- program_services_lup:

  Program services (a lookup table), Default: NULL

- price_ref_1L_int:

  Price reference (an integer vector of length one), Default: 3

- year_1L_chr:

  Year (a character vector of length one), Default: '2023-24'

## Value

Expenditure (a tibble)
