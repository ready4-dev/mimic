# Make project sim summary

make_project_sim_summary() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project sim summary. The function returns Summary (a tibble).

## Usage

``` r
make_project_sim_summary(
  sim_results_ls,
  element_1L_chr = "Z",
  groupings_chr = c("clinic_type", "treatment_status_start", "Distress"),
  order_1L_lgl = TRUE,
  convert_1L_lgl = TRUE,
  platform_1L_chr = "Intervention",
  select_1L_chr = c("both", "AQoL-6D", "CHU-9D"),
  timestamp_1L_chr = get_timestamp(),
  type_1L_chr = c("outcomes", "economic"),
  what_1L_chr = c("total", "clinic", "clinic_stage", "distress", "full_combos", "stage")
)
```

## Arguments

- sim_results_ls:

  Sim results (a list)

- element_1L_chr:

  Element (a character vector of length one), Default: 'Z'

- groupings_chr:

  Groupings (a character vector), Default: c("clinic_type",
  "treatment_status_start", "Distress")

- order_1L_lgl:

  Order (a logical vector of length one), Default: TRUE

- convert_1L_lgl:

  Convert (a logical vector of length one), Default: TRUE

- platform_1L_chr:

  Platform (a character vector of length one), Default: 'Intervention'

- select_1L_chr:

  Select (a character vector of length one), Default: c("both",
  "AQoL-6D", "CHU-9D")

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- type_1L_chr:

  Type (a character vector of length one), Default: c("outcomes",
  "economic")

- what_1L_chr:

  What (a character vector of length one), Default: c("total", "clinic",
  "clinic_stage", "distress", "full_combos", "stage")

## Value

Summary (a tibble)
