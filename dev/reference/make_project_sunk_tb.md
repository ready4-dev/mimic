# Make project sunk tibble

make_project_sunk_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project sunk
tibble. The function returns Sunk (a tibble).

## Usage

``` r
make_project_sunk_tb(
  global_1L_dbl = numeric(0),
  content_dbl = 0.9,
  development_dbl = 0.75,
  implementation_dbl = 0.25,
  infrastructure_dbl = 0,
  management_dbl = 0,
  marketing_dbl = 0,
  onboarding_dbl = 0.1,
  reporting_dbl = 0.9
)
```

## Arguments

- global_1L_dbl:

  Global (a double vector of length one), Default: numeric(0)

- content_dbl:

  Content (a double vector), Default: 0.9

- development_dbl:

  Development (a double vector), Default: 0.75

- implementation_dbl:

  Implementation (a double vector), Default: 0.25

- infrastructure_dbl:

  Infrastructure (a double vector), Default: 0

- management_dbl:

  Management (a double vector), Default: 0

- marketing_dbl:

  Marketing (a double vector), Default: 0

- onboarding_dbl:

  Onboarding (a double vector), Default: 0.1

- reporting_dbl:

  Reporting (a double vector), Default: 0.9

## Value

Sunk (a tibble)
