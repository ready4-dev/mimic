renew_MimicConfiguration <- function(x,
                                     arm_1L_chr = character(0),
                                     batch_1L_int = integer(0),
                                     draws_tb = NULL,
                                     # iterations_int = integer(0), # Necessary? draws_tb$Iteration
                                     tx_prefix_1L_chr = character(0),
                                     type_1L_chr = c("default", "form"),
                                     what_1L_chr = c("population"),
                                     ...){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "population"){
    if(type_1L_chr == "form"){
      population_ls <- add_enter_model_event(X_Ready4useDyad = x@x_MimicInputs@y_Ready4useDyad, 
                                             default_fn = x@x_MimicAlgorithms@processing_ls$initialise_ls$default_fn,
                                             derive_fn_ls = x@x_MimicAlgorithms@processing_ls$initialise_ls$derive_ls,
                                             horizon_dtm = x@horizon_dtm,
                                             modifiable_chr = x@x_MimicAlgorithms@processing_ls$initialise_ls$update_fn(x@modifiable_chr),
                                             start_dtm = x@start_dtm,  
                                             tfmn_ls = x@x_MimicAlgorithms@transformations_ls, 
                                             tx_duration_dtm = procure(x, match_value_xx = arm_1L_chr, empty_xx = NULL, target_1L_chr = "Treatment duration"),
                                             arm_1L_chr = arm_1L_chr, 
                                             default_args_ls = list(sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls),
                                             draws_tb = draws_tb,
                                             iterations_int = manufacture(x, batch_1L_int = batch_1L_int, what_1L_chr = "iterations"), 
                                             tidy_cols_1L_lgl = T,
                                             tx_prefix_1L_chr = tx_prefix_1L_chr) %>%
        update_population_ls(population_ls = NULL,  type_1L_chr = type_1L_chr)
      x <- renewSlot(x,"x_MimicPopulation",
                     renew(x@x_MimicPopulation, population_ls = population_ls, type_1L_chr = "transform"))
    }
  }
  return(x)
}
renew_MimicPopulation <- function(x,
                                  invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf | x <0),
                                  schedule_args_ls = list(),
                                  schedule_fn = NULL,
                                  population_ls = NULL,
                                  step_dtm = lubridate::days(0),
                                  type_1L_chr = c("default", "customise", "schedule", "transform"),
                                  use_1L_chr = c("Y", "Z"),
                                  validate_chr = character(0),
                                  what_1L_chr = character(0),
                                  X_MimicConfiguration = MimicConfiguration(),
                                  ...){
  type_1L_chr <- match.arg(type_1L_chr)
  use_1L_chr <- match.arg(use_1L_chr)
  if(type_1L_chr == "customise"){
    population_ls <- manufacture(x, what_1L_chr = "population_ls")
    population_ls$X_Ready4useDyad <- add_non_helpseekers(population_ls$X_Ready4useDyad,
                                                         arms_for_non_helpseeking_chr = procure(X_MimicConfiguration, empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "Helpseeking adjustment")) 
    population_ls$X_Ready4useDyad <- add_non_iar(population_ls$X_Ready4useDyad,
                                                 arms_for_iar_adjustment_chr = procure(X_MimicConfiguration, empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "IAR adjustment"))
    population_ls <- update_population_ls(population_ls)
    x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
  }
  if(type_1L_chr == "schedule"){
    population_ls <- manufacture(x, what_1L_chr = "population_ls")
    if(nrow(population_ls$X_Ready4useDyad@ds_tb)>0){
      population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, # This could become a method of a new MimicScheduler class - which could then obviate the need for some of the arguments above. MimicScheduler could be part of MimicEvents which includes MimicActions. A list of MimicEvents could be part of MimicConfiguration
                                                         event_1L_chr = what_1L_chr, 
                                                         schedule_fn = schedule_fn,
                                                         schedule_args_ls = schedule_args_ls,
                                                         step_dtm = step_dtm)
      print_errors(population_ls$X_Ready4useDyad,
                   vars_chr = validate_chr,
                   assert_1L_lgl = FALSE,
                   invalid_fn = invalid_fn)
      population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
      population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
      population_ls <- update_population_ls(population_ls, use_1L_chr = use_1L_chr)
      x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
    }
  }
  if(type_1L_chr == "transform"){
    x <- renewSlot(x,"x_Ready4useDyad", population_ls$X_Ready4useDyad) %>%
      renewSlot("y_Ready4useDyad", population_ls$Y_Ready4useDyad) %>%
      renewSlot("z_Ready4useDyad", population_ls$Z_Ready4useDyad)
  }
  return(x)
}