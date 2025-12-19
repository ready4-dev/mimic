# Add treatment status

add_treatment_status() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add treatment status. The function returns
Data (an output object of multiple potential types).

## Usage

``` r
add_treatment_status(
  data_xx,
  arrange_by_1L_chr = c("category", "name"),
  ctg_1L_chr = "Service",
  group_by_1L_chr = character(0),
  source_vars_chr = c("treatment_stage", "stage_of_treatment"),
  three_levels_1L_lgl = FALSE,
  type_1L_int = 1L:2L,
  update_dict_1L_lgl = TRUE,
  var_1L_chr = "treatment_status"
)
```

## Arguments

- data_xx:

  Data (an output object of multiple potential types)

- arrange_by_1L_chr:

  Arrange by (a character vector of length one), Default: c("category",
  "name")

- ctg_1L_chr:

  Category (a character vector of length one), Default: 'Service'

- group_by_1L_chr:

  Group by (a character vector of length one), Default: character(0)

- source_vars_chr:

  Source variables (a character vector), Default: c("treatment_stage",
  "stage_of_treatment")

- three_levels_1L_lgl:

  Three levels (a logical vector of length one), Default: FALSE

- type_1L_int:

  Type (an integer vector of length one), Default: 1L:2L

- update_dict_1L_lgl:

  Update dictionary (a logical vector of length one), Default: TRUE

- var_1L_chr:

  Variable (a character vector of length one), Default:
  'treatment_status'

## Value

Data (an output object of multiple potential types)
