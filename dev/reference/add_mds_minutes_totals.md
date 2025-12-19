# Add Minimum Dataset minutes totals

add_mds_minutes_totals() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add minimum dataset minutes totals. The
function returns Services (a tibble).

## Usage

``` r
add_mds_minutes_totals(
  services_tb,
  add_chr = c("Contacts", "Use"),
  type_1L_chr = c("both", "total", "prop")
)
```

## Arguments

- services_tb:

  Services (a tibble)

- add_chr:

  Add (a character vector), Default: c("Contacts", "Use")

- type_1L_chr:

  Type (a character vector of length one), Default: c("both", "total",
  "prop")

## Value

Services (a tibble)
