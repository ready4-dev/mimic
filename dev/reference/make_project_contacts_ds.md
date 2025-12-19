# Make project contacts dataset

make_project_contacts_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project contacts dataset. The function returns Contacts (a tibble).

## Usage

``` r
make_project_contacts_ds(
  raw_data_ls,
  demographics_tb,
  recode_lup_r3 = make_project_recode_lup()
)
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- demographics_tb:

  Demographics (a tibble)

- recode_lup_r3:

  Recode (a ready4 submodule extension of lookup table), Default:
  make_project_recode_lup()

## Value

Contacts (a tibble)
