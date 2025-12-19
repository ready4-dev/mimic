# Transform to remove duplicates

transform_to_remove_duplicates() is a Transform function that edits an
object in such a way that core object attributes - e.g. shape,
dimensions, elements, type - are altered. Specifically, this function
implements an algorithm to transform to remove duplicates. The function
returns Data (a tibble).

## Usage

``` r
transform_to_remove_duplicates(
  data_tb,
  group_by_chr = c("episode_key", "collection_occasion_date", "reason_for_collection"),
  uid_1L_chr = "episode_key"
)
```

## Arguments

- data_tb:

  Data (a tibble)

- group_by_chr:

  Group by (a character vector), Default: c("episode_key",
  "collection_occasion_date", "reason_for_collection")

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default:
  'episode_key'

## Value

Data (a tibble)
