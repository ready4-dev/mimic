# Make project 2 episode sequence

make_project_2_episode_sequence() is a Make function that creates a new
R object. Specifically, this function implements an algorithm to make
project 2 episode sequence. The function is called for its side effects
and does not return a value.

## Usage

``` r
make_project_2_episode_sequence(
  event_nm_1L_chr = "EpisodeOfCareSequence",
  outcome_var_1L_chr = "K10",
  change_first_mdl = "K10_mdl",
  change_relapse_1L_chr = "K10Relapse_mdl",
  ineligible_1L_chr = character(0),
  end_mdl_1L_chr = "EpisodeEnd_mdl",
  functions_ls = make_ineligibility_fns_ls(),
  start_mdl_1L_chr = "EpisodeStart_mdl",
  type_schedule_1L_chr = c("first", "repeat"),
  use_schedule_1L_chr = "Y",
  use_trigger_1L_chr = "Z",
  validate_schedule_1L_chr = "WaitInDays",
  vars_chr = c("WaitInDays", "DaysToYearOneRepresentation"),
  workers_chr = make_worker_types(),
  workers_medical_chr = make_worker_types("medical")
)
```

## Arguments

- event_nm_1L_chr:

  Event name (a character vector of length one), Default:
  'EpisodeOfCareSequence'

- outcome_var_1L_chr:

  Outcome variable (a character vector of length one), Default: 'K10'

- change_first_mdl:

  Change first (a model), Default: 'K10_mdl'

- change_relapse_1L_chr:

  Change relapse (a character vector of length one), Default:
  'K10Relapse_mdl'

- ineligible_1L_chr:

  Ineligible (a character vector of length one), Default: character(0)

- end_mdl_1L_chr:

  End model (a character vector of length one), Default:
  'EpisodeEnd_mdl'

- functions_ls:

  Functions (a list), Default: make_ineligibility_fns_ls()

- start_mdl_1L_chr:

  Start model (a character vector of length one), Default:
  'EpisodeStart_mdl'

- type_schedule_1L_chr:

  Type schedule (a character vector of length one), Default: c("first",
  "repeat")

- use_schedule_1L_chr:

  Use schedule (a character vector of length one), Default: 'Y'

- use_trigger_1L_chr:

  Use trigger (a character vector of length one), Default: 'Z'

- validate_schedule_1L_chr:

  Validate schedule (a character vector of length one), Default:
  'WaitInDays'

- vars_chr:

  Variables (a character vector), Default: c("WaitInDays",
  "DaysToYearOneRepresentation")

- workers_chr:

  Workers (a character vector), Default: make_worker_types()

- workers_medical_chr:

  Workers medical (a character vector), Default:
  make_worker_types("medical")

## Value

X (Model event scheduling and event logic data.)
