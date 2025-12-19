# Add age to project datasets

add_age_to_project_dss() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add age to project datasets. The function
returns Project datasets (a list).

## Usage

``` r
add_age_to_project_dss(
  project_dss_ls,
  age_1L_chr = "Age",
  drop_1L_lgl = FALSE,
  date_of_birth_1L_chr = "date_of_birth",
  index_date_1L_chr = "onboarding_date",
  what_chr = c("contacts", "outcomes", "overview")
)
```

## Arguments

- project_dss_ls:

  Project datasets (a list)

- age_1L_chr:

  Age (a character vector of length one), Default: 'Age'

- drop_1L_lgl:

  Drop (a logical vector of length one), Default: FALSE

- date_of_birth_1L_chr:

  Date of birth (a character vector of length one), Default:
  'date_of_birth'

- index_date_1L_chr:

  Index date (a character vector of length one), Default:
  'onboarding_date'

- what_chr:

  What (a character vector), Default: c("contacts", "outcomes",
  "overview")

## Value

Project datasets (a list)
