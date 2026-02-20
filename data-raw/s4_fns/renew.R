renew_MimicActive <- function(x,
                              batch_1L_int = integer(0),
                              env_ls = list(),
                              type_1L_chr = c("trigger", "customise", "schedule"),
                              X_MimicConfiguration = MimicConfiguration(),
                              X_MimicEvent = MimicEvent(),
                              ...){
  
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "trigger"){
    args_ls <- manufacture(X_MimicEvent@y_MimicTrigger@x_MimicArguments, batch_1L_int = batch_1L_int, env_ls = env_ls,
                           what_1L_chr = c("args_ls"), X_MimicConfiguration = X_MimicConfiguration)
    x <- renewSlot(x, "x_Ready4useDyad", rlang::exec(X_MimicEvent@y_MimicTrigger@functions_ls$action_fn,
                                                     X_MimicConfiguration@x_MimicPopulation@x_MimicActive@x_Ready4useDyad,
                                                     !!!args_ls))
    x <- renewSlot(x,"x_Ready4useDyad@ds_tb",
                   x@x_Ready4useDyad@ds_tb %>% dplyr::mutate(NextEvent = NA_character_, ScheduledFor = lubridate::NA_Date_))
  }
  if(type_1L_chr == "customise"){
    # The below customise method should be defined for an inheriting class of MimicActive and this method replaced with x <- rlang::exec(tfmn_fn, x, !!!args_ls) or better still x <- tfmn_fn(x, X_MimicConfiguration)
  x <- renewSlot(x, "x_Ready4useDyad",
                 add_non_helpseekers(x@x_Ready4useDyad,
                                     arms_for_non_helpseeking_chr = procure(X_MimicConfiguration, empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "Helpseeking adjustment"))) 
  x <- renewSlot(x, "x_Ready4useDyad",
                 add_non_iar(x@x_Ready4useDyad,
                             arms_for_iar_adjustment_chr = procure(X_MimicConfiguration, empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "IAR adjustment")))
  }
  if(type_1L_chr == "schedule"){
    x <- renewSlot(x, "x_Ready4useDyad", add_time_to_event(x@x_Ready4useDyad, 
                                                           event_1L_chr = X_MimicEvent@x_MimicSchedule@event_1L_chr, 
                                                           schedule_fn = X_MimicEvent@x_MimicSchedule@functions_ls$schedule_fn,
                                                           schedule_args_ls = manufacture(X_MimicEvent@x_MimicSchedule@x_MimicArguments, 
                                                                                          batch_1L_int = batch_1L_int, env_ls = env_ls,
                                                                                          what_1L_chr = "args_ls", X_MimicConfiguration = X_MimicConfiguration),
                                                           step_dtm = X_MimicEvent@x_MimicSchedule@step_dtm))
    print_errors(x@x_Ready4useDyad,
                 vars_chr = X_MimicEvent@x_MimicSchedule@validate_chr,
                 assert_1L_lgl = X_MimicEvent@x_MimicSchedule@assert_1L_lgl,
                 invalid_fn = X_MimicEvent@x_MimicSchedule@functions_ls$invalid_fn)
    x@x_Ready4useDyad <- update_current_date(x@x_Ready4useDyad)
    x@x_Ready4useDyad <- update_current_event(x@x_Ready4useDyad)
  }
  return(x)
}
renew_MimicConfiguration <- function(x,
                                     arm_1L_chr = character(0),
                                     batch_1L_int = integer(0),
                                     draws_tb = NULL,
                                     env_ls = list(),
                                     # iterations_int = integer(0), # Necessary? draws_tb$Iteration
                                     tx_prefix_1L_chr = character(0),
                                     type_1L_chr = c("event","form", "schedule", "trigger"),
                                     what_1L_chr = c("population"),
                                     X_MimicEvent = MimicEvent(), # remove argument once incorporated into MimicConfiguration
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
    if(type_1L_chr %in% c("schedule", "trigger")){
      x <- renewSlot(x,"x_MimicPopulation", renew(x@x_MimicPopulation,
                                                  batch_1L_int = batch_1L_int, env_ls = env_ls, 
                                                  type_1L_chr = type_1L_chr, 
                                                  X_MimicConfiguration = x, 
                                                  X_MimicEvent = X_MimicEvent))
    }
    if(type_1L_chr == "event"){
      x <- renew(x,batch_1L_int = batch_1L_int, env_ls = env_ls, 
                 type_1L_chr = "schedule", 
                 X_MimicConfiguration = x, 
                 X_MimicEvent = X_MimicEvent)
      x <- renew(x,batch_1L_int = batch_1L_int, env_ls = env_ls, 
                 type_1L_chr = "trigger", 
                 X_MimicConfiguration = x, 
                 X_MimicEvent = X_MimicEvent)

    }
  }
  return(x)
}
renew_MimicPopulation <- function(x,
                                  batch_1L_int = integer(0),
                                  env_ls = list(),
                                  population_ls = NULL,
                                  type_1L_chr = c("trigger", "customise", "schedule", "switch", "transform"),
                                  what_1L_chr = character(0),
                                  X_MimicConfiguration = MimicConfiguration(),
                                  X_MimicEvent = MimicEvent(),
                                  ...){
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "customise"){
    population_ls <- manufacture(x, what_1L_chr = "population_ls")
    population_ls$X_Ready4useDyad <- renew(x@x_MimicActive, type_1L_chr = type_1L_chr, X_MimicConfiguration = X_MimicConfiguration) %>%
      procureSlot("x_Ready4useDyad")
    population_ls <- update_population_ls(population_ls)
    x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
  }
  if(type_1L_chr %in% c("schedule", "trigger")){
    if(nrow(x@x_MimicActive@x_Ready4useDyad@ds_tb)>0){
      population_ls <- manufacture(x, what_1L_chr = "population_ls")
      X_MimicActive <- renew(x@x_MimicActive, batch_1L_int = batch_1L_int, env_ls = env_ls, type_1L_chr = type_1L_chr, X_MimicConfiguration = X_MimicConfiguration, X_MimicEvent = X_MimicEvent)
      population_ls$X_Ready4useDyad <- X_MimicActive@x_Ready4useDyad
      use_1L_chr <- ifelse(type_1L_chr=="schedule", X_MimicEvent@x_MimicSchedule@use_1L_chr, X_MimicEvent@y_MimicTrigger@use_1L_chr)
      if(!is.na(use_1L_chr)){
        population_ls <- update_population_ls(population_ls, 
                                              type_1L_chr = ifelse(type_1L_chr=="schedule", "split", "join"),
                                              use_1L_chr = use_1L_chr)
      }
      x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
    }
  }
  if(type_1L_chr == "switch"){
    population_ls <- manufacture(x, what_1L_chr = "population_ls") %>%
      update_population_ls(type_1L_chr = type_1L_chr, use_1L_chr = what_1L_chr)
  }
  if(type_1L_chr == "transform"){
    x <- renewSlot(x,"x_MimicActive@x_Ready4useDyad", population_ls$X_Ready4useDyad) %>%
      renewSlot("y_Ready4useDyad", population_ls$Y_Ready4useDyad) %>%
      renewSlot("z_Ready4useDyad", population_ls$Z_Ready4useDyad)
  }
  return(x)
}