# Add project model data

add_project_model_data() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project model data. The function returns
Model data (a list).

## Usage

``` r
add_project_model_data(
  model_data_ls = NULL,
  mdls_lup = NULL,
  processed_ls = NULL,
  timestamp_1L_chr = get_timestamp(),
  type_1L_chr = c("unimputed", "imputed"),
  what_1L_chr = c("CostWide", "Joiners", "JulyJoiners", "MicroLong", "MicroWide",
    "MicroWide1Year", "MinutesLong", "Outcomes", "OutcomesJoiners", "Series")
)
```

## Arguments

- model_data_ls:

  Model data (a list), Default: NULL

- mdls_lup:

  Models (a lookup table), Default: NULL

- processed_ls:

  Processed (a list), Default: NULL

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- type_1L_chr:

  Type (a character vector of length one), Default: c("unimputed",
  "imputed")

- what_1L_chr:

  What (a character vector of length one), Default: c("CostWide",
  "Joiners", "JulyJoiners", "MicroLong", "MicroWide", "MicroWide1Year",
  "MinutesLong", "Outcomes", "OutcomesJoiners", "Series")

## Value

Model data (a list)
