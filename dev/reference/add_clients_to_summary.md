# Add clients to summary

add_clients_to_summary() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add clients to summary. The function returns
Summaries (a list).

## Usage

``` r
add_clients_to_summary(
  summaries_ls,
  onboarded_tb,
  arrange_1L_chr = character(0),
  reference_1L_chr = "Status quo"
)
```

## Arguments

- summaries_ls:

  Summaries (a list)

- onboarded_tb:

  Onboarded (a tibble)

- arrange_1L_chr:

  Arrange (a character vector of length one), Default: character(0)

- reference_1L_chr:

  Reference (a character vector of length one), Default: 'Status quo'

## Value

Summaries (a list)
