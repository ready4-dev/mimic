# Transform project 2 model dataset

transform_project_2_model_ds() is a Transform function that edits an
object in such a way that core object attributes - e.g. shape,
dimensions, elements, type - are altered. Specifically, this function
implements an algorithm to transform project 2 model dataset. The
function returns Data (a tibble).

## Usage

``` r
transform_project_2_model_ds(
  data_tb,
  cut_off_date_1L_chr,
  intervention_1L_chr = "Intervention",
  type_1L_chr = c("has_iar", "representation")
)
```

## Arguments

- data_tb:

  Data (a tibble)

- cut_off_date_1L_chr:

  Cut off date (a character vector of length one)

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- type_1L_chr:

  Type (a character vector of length one), Default: c("has_iar",
  "representation")

## Value

Data (a tibble)
