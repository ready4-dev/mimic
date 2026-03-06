# Make project 2 wrap up sequence

make_project_2_wrap_up_sequence() is a Make function that creates a new
R object. Specifically, this function implements an algorithm to make
project 2 wrap up sequence. The function is called for its side effects
and does not return a value.

## Usage

``` r
make_project_2_wrap_up_sequence(
  event_nm_1L_chr = "WrapUpSequence",
  outcome_var_1L_chr = "K10",
  ineligible_1L_chr = character(0),
  functions_ls = make_ineligibility_fns_ls(),
  type_schedule_1L_chr = "End",
  use_schedule_1L_chr = "Y",
  use_trigger_1L_chr = "Z",
  validate_schedule_1L_chr = character(0)
)
```

## Arguments

- event_nm_1L_chr:

  Event name (a character vector of length one), Default:
  'WrapUpSequence'

- outcome_var_1L_chr:

  Outcome variable (a character vector of length one), Default: 'K10'

- ineligible_1L_chr:

  Ineligible (a character vector of length one), Default: character(0)

- functions_ls:

  Functions (a list), Default: make_ineligibility_fns_ls()

- type_schedule_1L_chr:

  Type schedule (a character vector of length one), Default: 'End'

- use_schedule_1L_chr:

  Use schedule (a character vector of length one), Default: 'Y'

- use_trigger_1L_chr:

  Use trigger (a character vector of length one), Default: 'Z'

- validate_schedule_1L_chr:

  Validate schedule (a character vector of length one), Default:
  character(0)

## Value

X (Model event scheduling and event logic data.)
