#' 
#' Renew (update) values
#' @name renew-MimicActive
#' @description renew method applied to MimicActive
#' @param x An object of class MimicActive
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param env_ls Environment (a list), Default: list()
#' @param type_1L_chr Type (a character vector of length one), Default: c("default", "customise", "schedule")
#' @param X_MimicConfiguration X_MimicConfiguration, Default: MimicConfiguration()
#' @param X_MimicSchedule X_MimicSchedule, Default: X_MimicSchedule()
#' @param ... Additional arguments
#' @return x (An object of class MimicActive)
#' @rdname renew-methods
#' @aliases renew,MimicActive-method
#' @export 
methods::setMethod("renew", "MimicActive", function(x,
                                                    batch_1L_int = integer(0),
                                                    env_ls = list(),
                                                    type_1L_chr = c("default", "customise", "schedule"),
                                                    X_MimicConfiguration = MimicConfiguration(),
                                                    X_MimicSchedule = MimicSchedule(),
                                                    ...){
  
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "customise"){
    x <- renewSlot(x, "x_Ready4useDyad",
                   add_non_helpseekers(x@x_Ready4useDyad,
                                       arms_for_non_helpseeking_chr = procure(X_MimicConfiguration, empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "Helpseeking adjustment"))) 
    x <- renewSlot(x, "x_Ready4useDyad",
                   add_non_iar(x@x_Ready4useDyad,
                               arms_for_iar_adjustment_chr = procure(X_MimicConfiguration, empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "IAR adjustment")))
  }
  if(type_1L_chr == "schedule"){
    x@x_Ready4useDyad <- add_time_to_event(x@x_Ready4useDyad, 
                                           event_1L_chr = X_MimicSchedule@event_1L_chr, 
                                           schedule_fn = X_MimicSchedule@functions_ls$schedule_fn,
                                           schedule_args_ls = manufacture(X_MimicSchedule@x_MimicArguments, batch_1L_int = batch_1L_int, env_ls = env_ls, 
                                                                          what_1L_chr = "args_ls", X_MimicConfiguration = X_MimicConfiguration),
                                           step_dtm = X_MimicSchedule@step_dtm)
    print_errors(x@x_Ready4useDyad,
                 vars_chr = X_MimicSchedule@validate_chr,
                 assert_1L_lgl = X_MimicSchedule@assert_1L_lgl,
                 invalid_fn = X_MimicSchedule@functions_ls$invalid_fn)
    x@x_Ready4useDyad <- update_current_date(x@x_Ready4useDyad)
    x@x_Ready4useDyad <- update_current_event(x@x_Ready4useDyad)
    
  }
  return(x)
})

#' 
#' Renew (update) values
#' @name renew-MimicPopulation
#' @description renew method applied to MimicPopulation
#' @param x An object of class MimicPopulation
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param env_ls Environment (a list), Default: list()
#' @param population_ls Population (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("default", "customise", "schedule", "transform")
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @param X_MimicConfiguration X_MimicConfiguration, Default: MimicConfiguration()
#' @param X_MimicSchedule X_MimicSchedule, Default: MimicSchedule()
#' @param ... Additional arguments
#' @return x (An object of class MimicPopulation)
#' @rdname renew-methods
#' @aliases renew,MimicPopulation-method
#' @export 
#' @importFrom lubridate days
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicPopulation", function(x,
                                                        batch_1L_int = integer(0),
                                                        env_ls = list(),
                                                        population_ls = NULL,
                                                        type_1L_chr = c("default", "customise", "schedule", "transform"),
                                                        what_1L_chr = character(0),
                                                        X_MimicConfiguration = MimicConfiguration(),
                                                        X_MimicSchedule = MimicSchedule(),
                                                        ...){
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "customise"){
    population_ls <- manufacture(x, what_1L_chr = "population_ls")
    population_ls$X_Ready4useDyad <- renew(x@x_MimicActive, type_1L_chr = type_1L_chr, X_MimicConfiguration = X_MimicConfiguration) %>%
      procureSlot("x_Ready4useDyad")
    population_ls <- update_population_ls(population_ls)
    x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
  }
  if(type_1L_chr == "schedule"){
    if(nrow(x@x_MimicActive@x_Ready4useDyad@ds_tb)>0){
      population_ls <- manufacture(x, what_1L_chr = "population_ls")
      X_MimicActive <- renew(x@x_MimicActive, batch_1L_int = batch_1L_int, env_ls = env_ls, type_1L_chr = "schedule", 
                             X_MimicConfiguration = X_MimicConfiguration, X_MimicSchedule = X_MimicSchedule)
      population_ls$X_Ready4useDyad <- X_MimicActive@x_Ready4useDyad
      population_ls <- update_population_ls(population_ls, use_1L_chr = X_MimicSchedule@use_1L_chr)
      x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
    }
  }
  if(type_1L_chr == "transform"){
    x <- renewSlot(x,"x_MimicActive@x_Ready4useDyad", population_ls$X_Ready4useDyad) %>%
      renewSlot("y_Ready4useDyad", population_ls$Y_Ready4useDyad) %>%
      renewSlot("z_Ready4useDyad", population_ls$Z_Ready4useDyad)
  }
  return(x)
})
#' 
#' Renew (update) values
#' @name renew-MimicConfiguration
#' @description renew method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param arm_1L_chr Arm (a character vector of length one), Default: character(0)
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("default", "form")
#' @param what_1L_chr What (a character vector of length one), Default: c("population")
#' @param ... Additional arguments
#' @return x (An object of class MimicConfiguration)
#' @rdname renew-methods
#' @aliases renew,MimicConfiguration-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicConfiguration", function (x, arm_1L_chr = character(0), batch_1L_int = integer(0), 
    draws_tb = NULL, tx_prefix_1L_chr = character(0), type_1L_chr = c("default", 
        "form"), what_1L_chr = c("population"), ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "population") {
        if (type_1L_chr == "form") {
            population_ls <- add_enter_model_event(X_Ready4useDyad = x@x_MimicInputs@y_Ready4useDyad, 
                default_fn = x@x_MimicAlgorithms@processing_ls$initialise_ls$default_fn, 
                derive_fn_ls = x@x_MimicAlgorithms@processing_ls$initialise_ls$derive_ls, 
                horizon_dtm = x@horizon_dtm, modifiable_chr = x@x_MimicAlgorithms@processing_ls$initialise_ls$update_fn(x@modifiable_chr), 
                start_dtm = x@start_dtm, tfmn_ls = x@x_MimicAlgorithms@transformations_ls, 
                tx_duration_dtm = procure(x, match_value_xx = arm_1L_chr, 
                  empty_xx = NULL, target_1L_chr = "Treatment duration"), 
                arm_1L_chr = arm_1L_chr, default_args_ls = list(sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls), 
                draws_tb = draws_tb, iterations_int = manufacture(x, 
                  batch_1L_int = batch_1L_int, what_1L_chr = "iterations"), 
                tidy_cols_1L_lgl = T, tx_prefix_1L_chr = tx_prefix_1L_chr) %>% 
                update_population_ls(population_ls = NULL, type_1L_chr = type_1L_chr)
            x <- renewSlot(x, "x_MimicPopulation", renew(x@x_MimicPopulation, 
                population_ls = population_ls, type_1L_chr = "transform"))
        }
    }
    return(x)
})
