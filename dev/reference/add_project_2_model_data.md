# Add project 2 model data

add_project_2_model_data() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project 2 model data. The function
returns Model data (a list).

## Usage

``` r
add_project_2_model_data(
  model_data_ls,
  sample_ls,
  filter_true_1L_chr = "FlexPsych",
  intervention_1L_chr = "Intervention",
  cut_off_date_1L_chr = "2025-01-01"
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- sample_ls:

  Sample (a list)

- filter_true_1L_chr:

  Filter true (a character vector of length one), Default: 'FlexPsych'

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- cut_off_date_1L_chr:

  Cut off date (a character vector of length one), Default: '2025-01-01'

## Value

Model data (a list)
