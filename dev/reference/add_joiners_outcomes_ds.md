# Add joiners outcomes dataset

add_joiners_outcomes_ds() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add joiners outcomes dataset. The function
returns Model data (a list).

## Usage

``` r
add_joiners_outcomes_ds(
  model_data_ls,
  keys_chr = c("platform", "clinic_type", "Age", "gender", "employment_status",
    "clinic_state", "treatment_stage"),
  outcomes_chr = c("treatment_status", "gad7", "k10", "phq9", "AQoL6D", "CHU9D")
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- keys_chr:

  Keys (a character vector), Default: c("platform", "clinic_type",
  "Age", "gender", "employment_status", "clinic_state",
  "treatment_stage")

- outcomes_chr:

  Outcomes (a character vector), Default: c("treatment_status", "gad7",
  "k10", "phq9", "AQoL6D", "CHU9D")

## Value

Model data (a list)
