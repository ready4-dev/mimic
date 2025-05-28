predict_comparator_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, arm_1L_chr = "Comparator", base_for_rates_int = c(1000L, 
                                                                                                                                              1, 1), draws_tb = NULL, iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
                                        modifiable_chr = c("treatment_status", "Minutes", "k10", 
                                                           "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
                                        start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
                                        utilities_chr = c("AQoL6D", "CHU9D"), variable_unit_1L_chr = "Minutes") 
{
  if (is.null(draws_tb)) {
    draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, 
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
                                                 type_1L_chr = "Model")
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
                                       schedule_fn = update_scheduled_date)
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
                                                 add_sensitivity_1L_lgl = T, adjustment_1L_dbl = -2, iterations_int = iterations_int, 
                                                 inputs_ls = inputs_ls, sensitivities_ls = sensitivities_ls, 
                                                 tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", 
                                                                                                  "CHU9D"), type_1L_chr = "Project")
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
predict_digital_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, arm_1L_chr = "Intervention", base_for_rates_int = c(1000L, 
                                                                                                                                             1L, 1L), draws_tb = NULL, iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
                                     modifiable_chr = c("treatment_status", "Minutes", "k10", 
                                                        "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
                                     start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
                                     utilities_chr = c("AQoL6D", "CHU9D"), variable_unit_1L_chr = "Minutes") 
{
  if (is.null(draws_tb)) {
    draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, 
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
                                                 utilities_chr = utilities_chr, type_1L_chr = "Model")
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
                                                 tfmn_ls = make_class_tfmns(T), utilities_chr = utilities_chr, 
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
predict_with_sim <- function (inputs_ls, add_logic_fn = add_project_offset_logic, base_for_rates_int = c(1000L, 1L, 1L), 
                              comparator_fn = predict_comparator_pathway,
                              intervention_fn = predict_digital_pathway,
                              iterations_ls = list(1:100L), 
                              horizon_dtm = lubridate::years(1), modifiable_chr = c("treatment_status", 
                                                                                    "Minutes", "k10", "AQoL6D", "CHU9D"), purge_1L_lgl = TRUE, 
                              scale_1L_int = 10L, seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
                              start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
                              type_1L_chr = c("D", "AB", "C","NULL"), utilities_chr = c("AQoL6D", 
                                                                                        "CHU9D"), write_to_1L_chr = character(0)) 
{
  type_1L_chr <- match.arg(type_1L_chr)
  if (!identical(seed_1L_int, integer(0))) {
    set.seed(seed_1L_int)
  }
  if (identical(write_to_1L_chr, character(0))) {
    write_to_1L_chr <- tempdir()
  }
  1:length(iterations_ls) %>% purrr::walk(~{
    iterations_int <- iterations_ls[[.x]]
    draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, 
                              scale_1L_int = scale_1L_int, seed_1L_int = seed_1L_int + 
                                .x)
    if(!is.null(intervention_fn)){
      Y_Ready4useDyad <- intervention_fn(inputs_ls, add_logic_fn = add_logic_fn, 
                                         arm_1L_chr = "Intervention", base_for_rates_int = base_for_rates_int, 
                                         draws_tb = draws_tb, iterations_int = iterations_int, 
                                         horizon_dtm = horizon_dtm, modifiable_chr = modifiable_chr, 
                                         sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
                                         tx_duration_dtm = tx_duration_dtm, seed_1L_int = seed_1L_int + .x, 
                                         start_dtm = start_dtm, utilities_chr = utilities_chr, 
                                         variable_unit_1L_chr = "Minutes")
    }else{
      Y_Ready4useDyad <- ready4use::Ready4useDyad()
    }
    if(!is.null(comparator_fn)){
      Z_Ready4useDyad <- comparator_fn(inputs_ls, 
                                       arm_1L_chr = "Comparator", add_logic_fn = add_logic_fn, base_for_rates_int = base_for_rates_int, 
                                       draws_tb = draws_tb, iterations_int = iterations_int, 
                                       horizon_dtm = horizon_dtm, modifiable_chr = modifiable_chr, 
                                       sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
                                       tx_duration_dtm = tx_duration_dtm, seed_1L_int = seed_1L_int + .x, 
                                       start_dtm = start_dtm, utilities_chr = utilities_chr, 
                                       variable_unit_1L_chr = "Minutes")
    }else{
      Z_Ready4useDyad <- ready4use::Ready4useDyad()
    }
    
    
    saveRDS(list(Y_Ready4useDyad = Y_Ready4useDyad, Z_Ready4useDyad = Z_Ready4useDyad), 
            paste0(write_to_1L_chr, "/SimBatch", .x, ".RDS"))
  })
  if(type_1L_chr != "NULL"){
    results_ls <- import_results_batches(length(iterations_ls), 
                                         dir_1L_chr = write_to_1L_chr)
  }
  if (purge_1L_lgl) {
    1:length(iterations_ls) %>% purrr::walk(~unlink(paste0(write_to_1L_chr, 
                                                           "/SimBatch", .x, ".RDS")))
  }
  if(type_1L_chr != "NULL"){
    X_Ready4useDyad <- make_project_results_synthesis(inputs_ls, results_ls, modifiable_chr = modifiable_chr, type_1L_chr = type_1L_chr)
  }else{
    X_Ready4useDyad <- NULL
  }
  return(X_Ready4useDyad)
}

