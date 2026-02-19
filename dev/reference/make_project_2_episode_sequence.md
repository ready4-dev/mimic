# Make project 2 episode sequence

make_project_2_episode_sequence() is a Make function that creates a new
R object. Specifically, this function implements an algorithm to make
project 2 episode sequence. The function is called for its side effects
and does not return a value.

## Usage

``` r
make_project_2_episode_sequence(
  event_nm_1L_chr = "EpisodeofCareSequence",
  outcome_var_1L_chr = "K10",
  start_mdl_1L_chr = "EpisodeStart_mdl",
  use_trigger_1L_chr = "Z",
  validate_schedule_1L_chr = "WaitInDays"
)
```

## Arguments

- event_nm_1L_chr:

  Event name (a character vector of length one), Default:
  'EpisodeofCareSequence'

- outcome_var_1L_chr:

  Outcome variable (a character vector of length one), Default: 'K10'

- start_mdl_1L_chr:

  Start model (a character vector of length one), Default:
  'EpisodeStart_mdl'

- use_trigger_1L_chr:

  Use trigger (a character vector of length one), Default: 'Z'

- validate_schedule_1L_chr:

  Validate schedule (a character vector of length one), Default:
  'WaitInDays'

## Value

X (Model event scheduling and event logic data.)
