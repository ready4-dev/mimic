# Transform project outcomes dataset

transform_project_outcomes_ds() is a Transform function that edits an
object in such a way that core object attributes - e.g. shape,
dimensions, elements, type - are altered. Specifically, this function
implements an algorithm to transform project outcomes dataset. The
function returns Outcomes (an output object of multiple potential
types).

## Usage

``` r
transform_project_outcomes_ds(
  outcomes_xx,
  transform_gender_1L_lgl = F,
  follow_up_1L_int = 12,
  minutes_chr = c("direct_mins", "indirect_mins", "Minutes")
)
```

## Arguments

- outcomes_xx:

  Outcomes (an output object of multiple potential types)

- transform_gender_1L_lgl:

  Transform gender (a logical vector of length one), Default: F

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: 12

- minutes_chr:

  Minutes (a character vector), Default: c("direct_mins",
  "indirect_mins", "Minutes")

## Value

Outcomes (an output object of multiple potential types)
