predict_comparator_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, arm_1L_chr = "Comparator", 
                                        arms_chr,
                                        base_for_rates_int = c(1000L, 
                                                                                                                                              1, 1), draws_tb = NULL, 
                                        extra_draws_fn = add_draws_from_pool,
                                        iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
                                        modifiable_chr = c("treatment_status", "Minutes", "k10", 
                                                           "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
                                        start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
                                        utilities_chr = c("CHU9D", "AQoL6D"), 
                                        utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
                                        variable_unit_1L_chr = "Minutes") 
{
  if (is.null(draws_tb)) {
    draws_tb <- make_draws_tb(inputs_ls, 
                              extra_draws_fn = extra_draws_fn,
                              iterations_int = iterations_int, 
                              seed_1L_int = seed_1L_int)
  }
  X_Ready4useDyad <- add_enter_model_event(inputs_ls$Synthetic_r4, 
                                           arm_1L_chr = arm_1L_chr, draws_tb = draws_tb, horizon_dtm = horizon_dtm, 
                                           iterations_int = iterations_int, modifiable_chr = setdiff(modifiable_chr, 
                                                                                                     "Minutes"), start_dtm = start_dtm, tfmn_ls = tfmn_ls, 
                                           tx_duration_dtm = tx_duration_dtm)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", 
                                       step_dtm = lubridate::weeks(14))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, 
                                         bl_week_1L_dbl = 0, iterations_int = iterations_int, 
                                         tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
                                       step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
                                                 adjustment_1L_dbl = -2, iterations_int = iterations_int, 
                                                 inputs_ls = inputs_ls, k10_method_1L_chr = "Table", suffix_1L_chr = "_12_Weeks", 
                                                 tfmn_ls = make_class_tfmns(T), utilities_chr = utilities_chr, 
                                                 utility_fns_ls = utility_fns_ls,
                                                 type_1L_chr = "Model")
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", 
                                       step_dtm = lubridate::weeks(12))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, 
                                         bl_week_1L_dbl = 0, iterations_int = iterations_int, 
                                         measurement_1L_int = 24, tx_duration_dtm = tx_duration_dtm, 
                                         tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
                                       step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
                                                 adjustment_1L_dbl = -2, iterations_int = iterations_int, 
                                                 inputs_ls = inputs_ls, k10_method_1L_chr = "Table", suffix_1L_chr = "_24_Weeks", 
                                                 tfmn_ls = make_class_tfmns(T), utilities_chr = utilities_chr, 
                                                 utility_fns_ls = utility_fns_ls,
                                                 type_1L_chr = "Model")
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
                                       schedule_fn = update_scheduled_date)
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
                                                 add_sensitivity_1L_lgl = T, adjustment_1L_dbl = -2, iterations_int = iterations_int, 
                                                 inputs_ls = inputs_ls, sensitivities_ls = sensitivities_ls, 
                                                 tfmn_ls = make_class_tfmns(T), 
                                                 utilities_chr = c("AQoL6D", "CHU9D"), ### WHY Not utilities_chr? - Is this an ordering issue
                                                 utility_fns_ls = utility_fns_ls,
                                                 type_1L_chr = "Project")
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateCosts", 
                                       step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_costs_event(X_Ready4useDyad, add_logic_fn = add_logic_fn, add_offsets_1L_lgl = F, 
                                     base_for_rates_int = base_for_rates_int, inputs_ls = inputs_ls, 
                                     offsets_chr = names(inputs_ls$pooled_ls), type_1L_chr = "zero", 
                                     variable_unit_1L_chr = variable_unit_1L_chr)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", 
                                       step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_leave_model_event(X_Ready4useDyad)
  return(X_Ready4useDyad)
}
predict_digital_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, arm_1L_chr = "Intervention", 
                                     arms_chr,
                                     base_for_rates_int = c(1000L, 1L, 1L), 
                                     draws_tb = NULL, 
                                     extra_draws_fn = add_draws_from_pool,
                                     iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
                                     modifiable_chr = c("treatment_status", "Minutes", "k10", 
                                                        "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
                                     start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
                                     utilities_chr = c("CHU9D", "AQoL6D"), 
                                     utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
                                     variable_unit_1L_chr = "Minutes") 
{
  if (is.null(draws_tb)) {
    draws_tb <- make_draws_tb(inputs_ls, 
                              extra_draws_fn = extra_draws_fn,
                              iterations_int = iterations_int, 
                              seed_1L_int = seed_1L_int)
  }
  X_Ready4useDyad <- add_enter_model_event(inputs_ls$Synthetic_r4, 
                                           arm_1L_chr = arm_1L_chr, draws_tb = draws_tb, horizon_dtm = horizon_dtm, 
                                           iterations_int = iterations_int, modifiable_chr = setdiff(modifiable_chr, 
                                                                                                     "Minutes"), start_dtm = start_dtm, tfmn_ls = tfmn_ls, 
                                           tx_duration_dtm = tx_duration_dtm)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", 
                                       step_dtm = lubridate::weeks(14))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, 
                                         bl_week_1L_dbl = 0, iterations_int = iterations_int, 
                                         tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateMinutes", 
                                       step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_minutes_event(X_Ready4useDyad, iterations_int = iterations_int, 
                                       minutes_mdl = inputs_ls$models_ls$Minutes_mdl)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
                                       step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
                                                 adjustment_1L_dbl = -2, iterations_int = iterations_int, 
                                                 inputs_ls = inputs_ls, k10_method_1L_chr = "Model", tfmn_ls = make_class_tfmns(T), 
                                                 utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls, type_1L_chr = "Model")
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", 
                                       step_dtm = lubridate::weeks(12))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, 
                                         bl_week_1L_dbl = 0, iterations_int = iterations_int, 
                                         tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateMinutes", 
                                       schedule_fn = update_scheduled_date,
                                       schedule_args_ls = list(target_1L_int = 336, type_1L_chr = "Day"))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_minutes_event(X_Ready4useDyad, iterations_int = iterations_int, 
                                       minutes_mdl = inputs_ls$models_ls$Minutes_mdl)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateMinutes", 
                                       schedule_fn = update_scheduled_date)
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_minutes_event(X_Ready4useDyad, iterations_int = iterations_int, 
                                       fraction_1L_dbl = 4/38)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
                                       step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
                                                 add_sensitivity_1L_lgl = T, adjustment_1L_dbl = -2, iterations_int = iterations_int, 
                                                 inputs_ls = inputs_ls, k10_method_1L_chr = "Model", sensitivities_ls = sensitivities_ls, 
                                                 tfmn_ls = make_class_tfmns(T), utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls,
                                                 type_1L_chr = "Project")
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateCosts", 
                                       step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_costs_event(X_Ready4useDyad, add_offsets_1L_lgl = T, add_logic_fn = add_logic_fn,
                                     base_for_rates_int = base_for_rates_int, inputs_ls = inputs_ls, 
                                     offsets_chr = names(inputs_ls$pooled_ls), type_1L_chr = "both", 
                                     variable_unit_1L_chr = variable_unit_1L_chr)
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", 
                                       step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_leave_model_event(X_Ready4useDyad)
  return(X_Ready4useDyad)
}
predict_from_pool <- function(pooled_xx,
                              as_1L_chr = c("vector", "histogram", "summary"),
                              adjustment_1L_dbl = numeric(0),
                              distributions_chr = "best",
                              maximum_1L_dbl = Inf,
                              minimum_1L_dbl = -Inf,
                              n_1L_int = 100L,
                              quantiles_dbl = c(.05,0.25,.5,.75, 0.95),
                              resample_1L_lgl = F,
                              seed_1L_int = 2001L,
                              title_1L_chr = "Pooled prediction distribution",
                              type_1L_chr = c("r","q","p"),
                              x_label_1L_chr = "Predictions",
                              values_dbl = numeric(0),
                              weights_dbl = 1,
                              what_1L_chr = character(0),
                              ...){
  as_1L_chr <- match.arg(as_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if(inherits(pooled_xx,"elicitation")){
    pooled_mdl <- pooled_xx
  }else{
    assertthat::assert_that(!identical(what_1L_chr, character(0)))
    pooled_mdl <- pooled_xx %>% purrr::pluck(what_1L_chr)
  }

  if(!identical(seed_1L_int,integer(0))){
    set.seed(seed_1L_int)
  }
  if(type_1L_chr == "p"){
    assertthat::assert_that(!identical(values_dbl, numeric(0)))
    if(!identical(adjustment_1L_dbl, numeric(0))){
      values_dbl <- values_dbl + adjustment_1L_dbl
    }
    values_dbl <- values_dbl %>% purrr::map_dbl(~min(max(.x, minimum_1L_dbl),maximum_1L_dbl))

    predictions_dbl <- SHELF::plinearpool(pooled_mdl, x = values_dbl, d = distributions_chr, w = weights_dbl)
  }
  if(type_1L_chr == "q"){
    predictions_dbl <- SHELF::qlinearpool(pooled_mdl, q = c(.05,0.25,.5,.75,0.95), d = distributions_chr, w = weights_dbl)
  }
  if(type_1L_chr == "r"){
    predictions_dbl <- SHELF::rlinearpool(pooled_mdl, n = n_1L_int, d = distributions_chr, w = weights_dbl)
  }
  if(!identical(adjustment_1L_dbl, numeric(0)) & type_1L_chr %in% c("r", "q")){
    predictions_dbl <- predictions_dbl + adjustment_1L_dbl
  }
  predictions_dbl <- predictions_dbl %>% purrr::map_dbl(~min(max(.x, minimum_1L_dbl),maximum_1L_dbl))
  if(resample_1L_lgl){
    predictions_dbl <- replace(predictions_dbl, is.na(predictions_dbl), sample(predictions_dbl[!is.na(predictions_dbl)], replace = T))
  }
  if(as_1L_chr == "vector"){
    predictions_xx <- predictions_dbl
  }
  if(as_1L_chr == "summary"){
    predictions_xx <- summary(predictions_dbl, ...)
  }
  if(as_1L_chr == "histogram"){
    predictions_xx <- hist(predictions_dbl, main = title_1L_chr, xlab = x_label_1L_chr, ...)
  }
  return(predictions_xx)
}
predict_project_2_pathway <- function (inputs_ls = NULL, 
                                       add_logic_fn = identity, 
                                       arm_1L_chr, 
                                       arms_chr,
                                       arms_for_intervention_costs_chr = character(0),
                                       arms_for_offsets_chr = character(0), 
                                       arms_for_non_helpseeking_chr = character(0), 
                                       arms_for_iar_adjustment_chr = character(0), 
                                       batch_1L_int = integer(0), ###
                                       # derive_extras_ls = list(),
                                       draws_tb = NULL,  ###
                                       extra_draws_fn = NULL,
                                       horizon_dtm = lubridate::years(1), 
                                       iterations_int = 1:100L, 
                                       modifiable_chr = make_project_2_vars("modify"),
                                       seed_1L_int = 2001L, 
                                       sensitivities_ls = make_project_2_sensitivities_ls(), 
                                       start_dtm = Sys.Date(), 
                                       tfmn_ls = make_class_tfmns(), 
                                       tx_duration_dtm = lubridate::weeks(12), 
                                       treatment_ls = NULL,
                                       utilities_chr = c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2"),
                                       utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
                                       X_MimicConfiguration = MimicConfiguration()
) 
{
  old_algorithm_1L_lgl <- identical(X_MimicConfiguration, MimicConfiguration())
  X_MimicConfiguration <- update_project_2_configuration(X_MimicConfiguration = X_MimicConfiguration,
                                                         batch_1L_int = batch_1L_int,
                                                         arms_chr = arms_chr,
                                                         arms_for_intervention_costs_chr = arms_for_intervention_costs_chr,
                                                         arms_for_offsets_chr = arms_for_offsets_chr, 
                                                         arms_for_non_helpseeking_chr = arms_for_non_helpseeking_chr, 
                                                         arms_for_iar_adjustment_chr = arms_for_iar_adjustment_chr, 
                                                         extra_draws_fn = extra_draws_fn,
                                                         horizon_dtm = horizon_dtm, 
                                                         inputs_ls = inputs_ls, 
                                                         iterations_int = iterations_int, 
                                                         modifiable_chr = modifiable_chr,
                                                         seed_1L_int = seed_1L_int, 
                                                         sensitivities_ls = sensitivities_ls, 
                                                         start_dtm = start_dtm, 
                                                         tfmn_ls = tfmn_ls, 
                                                         tx_duration_dtm = tx_duration_dtm, 
                                                         treatment_ls = treatment_ls,
                                                         utilities_chr = utilities_chr,
                                                         utility_fns_ls = utility_fns_ls)
  
  # Temporary
  if(!old_algorithm_1L_lgl){
    drop_missing_1L_lgl <- X_MimicConfiguration@drop_missing_1L_lgl
    drop_suffix_1L_chr <- X_MimicConfiguration@drop_suffix_1L_chr
    extra_draws_fn <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls$extra_draws_fn
    horizon_dtm <- X_MimicConfiguration@horizon_dtm
    initialise_ls <- make_project_2_initialise_ls(derive_ls = X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls) # Note modification -> update X_MimicConfiguration
    inputs_ls <- manufacture(X_MimicConfiguration@x_MimicInputs, what_1L_chr = "inputs_ls")
    iterations_ls <- X_MimicConfiguration@iterations_ls
    modifiable_chr <- X_MimicConfiguration@modifiable_chr
    seed_1L_int <- X_MimicConfiguration@seed_1L_int
    sensitivities_ls <- X_MimicConfiguration@x_MimicAlgorithms@sensitivities_ls
    start_dtm <- X_MimicConfiguration@start_dtm
    synthesis_fn <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls$synthesis_fn
    tfmn_ls <- X_MimicConfiguration@x_MimicAlgorithms@transformations_ls
    tx_duration_dtm <- procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, target_1L_chr = "Treatment duration")
    utilities_chr <- X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@names_chr 
    utility_fns_ls <- X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls 
  }
  ## Preliminary
  if (is.null(draws_tb)) {
    draws_tb <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, what_1L_chr = "draws_tb") 
    # draws_tb <- make_draws_tb(inputs_ls, 
    #                           drop_missing_1L_lgl = T, drop_suffix_1L_chr = "_mean",
    #                           extra_draws_fn = extra_draws_fn,
    #                           iterations_int = iterations_int, 
    #                           seed_1L_int = seed_1L_int)
  }
  treatment_1L_chr <- procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, empty_xx = character(0), target_1L_chr = "Treatment")
  tx_prefix_1L_chr <- "Treatment"
  # Add below to MimicConfiguration@x_MimicAlgorithms
  # utility_fns_ls <- make_utility_fns_ls(utilities_chr = utilities_chr)
  ###
  # Update classes then start with methodising the following.
  ###
  ## Enter model
  population_ls <- add_enter_model_event(X_Ready4useDyad = X_MimicConfiguration@x_MimicInputs@y_Ready4useDyad, #inputs_ls$Synthetic_r4,
                                         default_fn = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls$default_fn,
                                         derive_fn_ls = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls$derive_ls,
                                         horizon_dtm = X_MimicConfiguration@horizon_dtm,
                                         modifiable_chr = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls$update_fn(X_MimicConfiguration@modifiable_chr),
                                         start_dtm = X_MimicConfiguration@start_dtm,  
                                         tfmn_ls = X_MimicConfiguration@x_MimicAlgorithms@transformations_ls, 
                                         tx_duration_dtm = procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, empty_xx = character(0), target_1L_chr = "Treatment duration"),
                                         arm_1L_chr = arm_1L_chr, 
                                         default_args_ls = list(sensitivities_ls = sensitivities_ls),
                                         draws_tb = draws_tb,
                                         iterations_int = iterations_int, 
                                         tidy_cols_1L_lgl = T,
                                         tx_prefix_1L_chr = tx_prefix_1L_chr) %>%
    update_population_ls(population_ls = NULL,  type_1L_chr = "form")
  ## Update population (if comparator)
  population_ls$X_Ready4useDyad <- add_non_helpseekers(population_ls$X_Ready4useDyad,
                                                       arms_for_non_helpseeking_chr = procure(X_MimicConfiguration, match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "Helpseeking adjustment")) 
  population_ls$X_Ready4useDyad <- add_non_iar(population_ls$X_Ready4useDyad,
                                               arms_for_iar_adjustment_chr = procure(X_MimicConfiguration, match_value_xx = T, target_1L_chr = "Arm", type_1L_chr = "IAR adjustment"))
  ### Remove those who are non helpseekers (for comparator only)
  population_ls <- update_population_ls(population_ls)
  ##
  ## Schedule start of episode of care 
  if(nrow(population_ls$X_Ready4useDyad@ds_tb)>0){
    population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, event_1L_chr = "StartEpisode", 
                                                       schedule_fn = add_episode_wait_time,
                                                       schedule_args_ls = list(episode_start_mdl = procureSlot(X_MimicConfiguration@x_MimicInputs, "models_ls")$EpisodeStart_mdl, 
                                                                               iterations_int = iterations_int, 
                                                                               treatment_1L_chr = procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, empty_xx = character(0), target_1L_chr = "Treatment")))
    print_errors(population_ls$X_Ready4useDyad,
                 vars_chr = c("WaitInDays"),
                 assert_1L_lgl = FALSE,
                 invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf | x <0))
    population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
    population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
    ### Remove those who won't receive any episodes
    population_ls <- update_population_ls(population_ls)
  }
  
  ##
  ## Add episode of care
  if(nrow(population_ls$X_Ready4useDyad@ds_tb)>0){
    population_ls$X_Ready4useDyad <- add_episode(population_ls$X_Ready4useDyad,
                                                 assert_1L_lgl = FALSE,
                                                 episode_1L_int = 1,
                                                 inputs_ls = manufacture(X_MimicConfiguration@x_MimicInputs, what_1L_chr = "inputs_ls"),
                                                 iterations_int = iterations_int,
                                                 k10_var_1L_chr = "K10",
                                                 sensitivities_ls = X_MimicConfiguration@x_MimicAlgorithms@sensitivities_ls,
                                                 tfmn_ls =  X_MimicConfiguration@x_MimicAlgorithms@transformations_ls,
                                                 treatment_1L_chr = procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, empty_xx = character(0), target_1L_chr = "Treatment"),
                                                 tx_prefix_1L_chr = tx_prefix_1L_chr,
                                                 utilities_chr = X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@names_chr,
                                                 utility_fns_ls = X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls)
    ## Schedule representation (new episode of care)
    population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, event_1L_chr = "Represent", 
                                                       schedule_fn = add_episode_wait_time,
                                                       schedule_args_ls = list(episode_start_mdl = procureSlot(X_MimicConfiguration@x_MimicInputs, "models_ls")$Representation_mdl, iterations_int = iterations_int, 
                                                                               type_1L_chr = "repeat", treatment_1L_chr = treatment_1L_chr))
    print_errors(population_ls$X_Ready4useDyad,
                 vars_chr = c("DaysToYearOneRepresentation"),
                 assert_1L_lgl = FALSE,
                 invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf | x <0))
    population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
    population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
    ### Remove those who do not have a repeat presentation
    population_ls <- update_population_ls(population_ls, use_1L_chr = "Z")
  }
  ##
  ## Add episode of care for representers
  if(nrow(population_ls$X_Ready4useDyad@ds_tb)>0){
    population_ls$X_Ready4useDyad <- add_episode(population_ls$X_Ready4useDyad,
                                                 assert_1L_lgl = FALSE,
                                                 episode_1L_int = 2,
                                                 inputs_ls = inputs_ls,
                                                 iterations_int = iterations_int,
                                                 k10_var_1L_chr = "K10",
                                                 sensitivities_ls = sensitivities_ls,
                                                 tfmn_ls = tfmn_ls,
                                                 treatment_1L_chr = treatment_1L_chr,
                                                 tx_prefix_1L_chr = tx_prefix_1L_chr,
                                                 utilities_chr = utilities_chr,
                                                 utility_fns_ls = utility_fns_ls)
  }
  ### Return group who did not represent
  if(nrow(population_ls$Z_Ready4useDyad@ds_tb)>0){
    population_ls <- update_population_ls(population_ls, type_1L_chr = "join", use_1L_chr = "Z") 
  }
  ##
  ## Apply regression to mean logic to group that did not receive any episode of care
  if(nrow(population_ls$Y_Ready4useDyad@ds_tb)>0){
    population_ls$Y_Ready4useDyad <- renewSlot(population_ls$Y_Ready4useDyad, "ds_tb",
                                               population_ls$Y_Ready4useDyad@ds_tb  %>%
                                                 dplyr::mutate(CurrentDate = EndDate)) %>%
      add_regression_to_mean(sensitivities_ls = sensitivities_ls,
                             k10_draws_fn = add_project_2_k10_draws,
                             tfmn_ls = tfmn_ls,
                             tx_prefix_1L_chr = tx_prefix_1L_chr,
                             utilities_chr = utilities_chr,
                             utility_fns_ls = utility_fns_ls)
    ## Return group who did not receive an episode of care
    population_ls <- update_population_ls(population_ls, type_1L_chr = "join")
  }
  ##
  ## Model exit and wrap-up
  population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, event_1L_chr = "WrapUp", 
                                                     schedule_fn = update_scheduled_date)
  population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
  population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
  population_ls$X_Ready4useDyad <- add_project_2_model_wrap_up(population_ls$X_Ready4useDyad,
                                                               arms_for_intervention_costs_chr = arms_for_intervention_costs_chr,
                                                               arms_for_offsets_chr = arms_for_offsets_chr,
                                                               disciplines_chr = make_disciplines(),  
                                                               inputs_ls = inputs_ls,
                                                               iterations_int = iterations_int,
                                                               sensitivities_ls = sensitivities_ls,
                                                               tfmn_ls = tfmn_ls,
                                                               tx_prefix_1L_chr = tx_prefix_1L_chr,
                                                               utilities_chr = utilities_chr,
                                                               utility_fns_ls = utility_fns_ls)  ##
  
  return(population_ls$X_Ready4useDyad)
}
predict_with_sim <- function (inputs_ls = NULL, 
                              arms_chr = c("Intervention", "Comparator"), 
                              comparator_fn = predict_comparator_pathway, # make NULL
                              draws_tb = NULL,
                              drop_missing_1L_lgl = FALSE, 
                              drop_suffix_1L_chr = character(0), 
                              extra_draws_fn = NULL,
                              horizon_dtm = lubridate::years(1), 
                              intervention_fn = predict_digital_pathway, # make NULL
                              iterations_ls = make_batches(5, of_1L_int = 20), 
                              modifiable_chr = c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D"), # Remove default
                              prior_batches_1L_int = 0, 
                              purge_1L_lgl = TRUE, 
                              seed_1L_int = 2001L, 
                              sensitivities_ls = make_sensitivities_ls(), 
                              start_dtm = Sys.Date(), 
                              synthesis_fn = make_project_results_synthesis,
                              tfmn_ls = make_class_tfmns(),
                              type_1L_chr = c("D", "AB", "C", "NULL"), 
                              unlink_1L_lgl = FALSE, 
                              utilities_chr = c("AQoL6D", "CHU9D"), # Remove default
                              utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
                              write_to_1L_chr = character(0),
                              X_MimicConfiguration = MimicConfiguration(),
                              Y_MimicRepos = MimicRepos(),
                              # NEED TO MAKE EXPLICT CALLS TO THE BELOW FOR PROJECT ONE
                              # add_logic_fn = add_project_offset_logic, # Make part of ...
                              # base_for_rates_int = c(1000L, 1L, 1L), 
                              # extra_draws_fn = add_draws_from_pool, # instead of: scale_1L_int = 10L, 
                              # tx_duration_dtm = lubridate::weeks(12), 
                              # variable_unit_1L_chr = "Minutes",
                              ...) 
{
  type_1L_chr <- match.arg(type_1L_chr)
  if (!identical(seed_1L_int, integer(0))) {
    set.seed(seed_1L_int)
  }
  if (identical(write_to_1L_chr, character(0))) {
    write_to_1L_chr <- tempdir()
  }
  predict_safely_fn <- purrr::safely(.f = write_batch, quiet = FALSE)
  if (unlink_1L_lgl) {
    list.files(write_to_1L_chr)[endsWith(list.files(write_to_1L_chr), 
                                         ".RDS")] %>% purrr::walk(~unlink(paste0(write_to_1L_chr, 
                                                                                 "/", .x)))
  }
  extras_ls <- list(...)
  output_xx <- 1:length(iterations_ls) %>% purrr::map(~{
    if(!is.null(draws_tb)){
      filtered_draws_tb <- draws_tb %>% dplyr::filter(Iteration %in% iterations_ls[[.x]]) 
    }else{
      filtered_draws_tb <- NULL
    }
    args_ls <- list(batch_1L_int = .x, 
                    # add_logic_fn = add_logic_fn, 
                    arms_chr = arms_chr, # set to character(0) if arms_tb is not empty
                    # base_for_rates_int = base_for_rates_int, 
                    comparator_fn = comparator_fn, 
                    draws_tb = filtered_draws_tb,
                    drop_missing_1L_lgl = drop_missing_1L_lgl, 
                    drop_suffix_1L_chr = drop_suffix_1L_chr, 
                    extra_draws_fn = extra_draws_fn,
                    horizon_dtm = horizon_dtm, 
                    inputs_ls = inputs_ls, 
                    intervention_fn = intervention_fn, 
                    iterations_ls = iterations_ls, 
                    modifiable_chr = modifiable_chr, 
                    prior_batches_1L_int = prior_batches_1L_int, 
                    # scale_1L_int = scale_1L_int, 
                    seed_1L_int = seed_1L_int, 
                    sensitivities_ls = sensitivities_ls, 
                    start_dtm = start_dtm, 
                    tfmn_ls = tfmn_ls, 
                    # tx_duration_dtm  = tx_duration_dtm, 
                    utilities_chr = utilities_chr, 
                    utility_fns_ls = utility_fns_ls,
                    # variable_unit_1L_chr = variable_unit_1L_chr, 
                    write_to_1L_chr = write_to_1L_chr,
                    X_MimicConfiguration = X_MimicConfiguration,
                    Y_MimicRepos = Y_MimicRepos
                    ) %>%
      append(extras_ls)
    rlang::exec(predict_safely_fn, !!!args_ls)
  })
  if (type_1L_chr != "NULL") {
    output_xx <- import_results_batches(dir_1L_chr = write_to_1L_chr)
  }
  if (purge_1L_lgl) {
    1:length(iterations_ls) %>% purrr::walk(~unlink(paste0(write_to_1L_chr, 
                                                           "/SimBatch", .x + prior_batches_1L_int, ".RDS")))
  }
  if (type_1L_chr != "NULL") {
    if(!identical(X_MimicConfiguration,MimicConfiguration())){
      synthesis_fn <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls$synthesis_fn
      modifiable_chr <- X_MimicConfiguration@modifiable_chr
    }
    output_xx <- synthesis_fn(inputs_ls, results_ls = output_xx, modifiable_chr = modifiable_chr, type_1L_chr = type_1L_chr)
  }
  return(output_xx)
}

