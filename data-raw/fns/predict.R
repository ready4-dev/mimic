predict_comparator_pathway <- function(inputs_ls,
                                       base_for_rates_int = c(1000L,1,1),
                                       draws_tb = NULL,
                                       iterations_int = 1:100L,
                                       # iterations_1L_int = 100L,
                                       horizon_dtm = lubridate::years(1),
                                       modifiable_chr = c("treatment_status", "Minutes","k10", "AQoL6D", "CHU9D"), #character(0)
                                       tfmn_ls = make_class_tfmns(),
                                       tx_duration_dtm = lubridate::weeks(12),
                                       seed_1L_int = 2001L,
                                       start_dtm = Sys.Date()){
  if(is.null(draws_tb)){
    draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, seed_1L_int = seed_1L_int)
  }
  ## Enter model
  X_Ready4useDyad <- add_enter_model_event(inputs_ls$Synthetic_r4, arm_1L_chr = "Comparator", draws_tb = draws_tb, horizon_dtm = horizon_dtm, iterations_int = iterations_int, modifiable_chr = setdiff(modifiable_chr, "Minutes"), start_dtm = start_dtm, tfmn_ls = tfmn_ls, tx_duration_dtm = tx_duration_dtm)
  ## Update Treatment Status
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", step_dtm = lubridate::weeks(14))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, bl_week_1L_dbl = 0, iterations_int = iterations_int, tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  ## Update Outcomes
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, adjustment_1L_dbl = -2, iterations_int = iterations_int, inputs_ls = inputs_ls, k10_method_1L_chr = "Table", suffix_1L_chr = "_12_Weeks", tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", "CHU9D"))
  ## Update Treatment Status
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", step_dtm = lubridate::weeks(12))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, bl_week_1L_dbl = 0, iterations_int = iterations_int, measurement_1L_int = 24, tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  ## Update Outcomes
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, adjustment_1L_dbl = -2, iterations_int = iterations_int, inputs_ls = inputs_ls, k10_method_1L_chr = "Table", suffix_1L_chr = "_24_Weeks", tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", "CHU9D"))
  ## Update health utility and QALYs
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateUtility", schedule_fn = update_scheduled_date)
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, add_sensitivity_1L_lgl = T, adjustment_1L_dbl = 0, models_ls = NULL, iterations_int = iterations_int, simulate_1L_lgl = F, tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", "CHU9D"), what_1L_chr = "new")
  ## Update accrued costs
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateCosts", step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_costs_event(X_Ready4useDyad, add_offsets_1L_lgl = F, base_for_rates_int = base_for_rates_int, inputs_ls = inputs_ls, offsets_chr = names(inputs_ls$pooled_ls), type_1L_chr = "zero")
  ## Leave model
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_leave_model_event(X_Ready4useDyad)
  return(X_Ready4useDyad)
}
predict_digital_pathway <- function(inputs_ls,
                                    base_for_rates_int = c(1000L, 1L, 1L),
                                    draws_tb = NULL,
                                    iterations_int = 1:100L, # iterations_1L_int = 100L,
                                    horizon_dtm = lubridate::years(1),
                                    modifiable_chr = c("treatment_status", "Minutes","k10", "AQoL6D", "CHU9D"), #character(0)
                                    tfmn_ls = make_class_tfmns(),
                                    tx_duration_dtm = lubridate::weeks(12),
                                    seed_1L_int = 2001L,
                                    start_dtm = Sys.Date()){
  if(is.null(draws_tb)){
    draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, seed_1L_int = seed_1L_int)
  }
  ## Enter model
  X_Ready4useDyad <- add_enter_model_event(inputs_ls$Synthetic_r4, arm_1L_chr = "Intervention", draws_tb = draws_tb, horizon_dtm = horizon_dtm, iterations_int = iterations_int, modifiable_chr = setdiff(modifiable_chr, "Minutes"), start_dtm = start_dtm, tfmn_ls = tfmn_ls, tx_duration_dtm = tx_duration_dtm)
  ## Update Treatment Status
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", step_dtm = lubridate::weeks(14))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, bl_week_1L_dbl = 0, iterations_int = iterations_int, tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  ## Update Minutes
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateMinutes", step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_minutes_event(X_Ready4useDyad, iterations_int = iterations_int, minutes_mdl = inputs_ls$models_ls$Minutes_mdl)
  ## Update Outcomes
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", step_dtm = lubridate::weeks(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, adjustment_1L_dbl = -2, iterations_int = iterations_int, inputs_ls = inputs_ls, k10_method_1L_chr = "Model", tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", "CHU9D"))
  ## Update Treatment Status
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateTxStatus", step_dtm = lubridate::weeks(12))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_treatment_event(X_Ready4useDyad, adjustment_1L_dbl = -2, bl_week_1L_dbl = 0, iterations_int = iterations_int, tx_duration_dtm = tx_duration_dtm, tx_models_ls = inputs_ls$models_ls$Treatments_ls)
  ## Update Minutes
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateMinutes", schedule_fn = update_scheduled_date)
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_minutes_event(X_Ready4useDyad, iterations_int = iterations_int, minutes_mdl = inputs_ls$models_ls$Minutes_mdl)
  ## Update health utility and QALYs
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateUtility", step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, add_sensitivity_1L_lgl = T, adjustment_1L_dbl = 0, models_ls = inputs_ls$models_ls, iterations_int = iterations_int, simulate_1L_lgl = F, tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", "CHU9D"), what_1L_chr = "new")
  ## Update accrued costs
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateCosts", step_dtm = lubridate::days(0))
  X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
  X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
  X_Ready4useDyad <- add_costs_event(X_Ready4useDyad, add_offsets_1L_lgl = T, base_for_rates_int = base_for_rates_int, inputs_ls = inputs_ls, offsets_chr = names(inputs_ls$pooled_ls), type_1L_chr = "both")
  ## Leave model
  X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", step_dtm = lubridate::days(0))
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
predict_with_sim <- function(inputs_ls,
                             
                             base_for_rates_int = c(1000L, 1L, 1L),
                             iterations_ls = list(1:100L),
                             # iterations_int = 1:100L, # iterations_1L_int = 100L,
                             horizon_dtm = lubridate::years(1),
                             modifiable_chr = c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D"), #character(0)
                             purge_1L_lgl = TRUE,
                             scale_1L_int = 10L,
                             seed_1L_int = 2001L,
                             start_dtm = Sys.Date(),
                             tfmn_ls = make_class_tfmns(),
                             tx_duration_dtm = lubridate::weeks(12),
                             type_1L_chr = c("D","AB", "C"),
                             write_to_1L_chr = character(0)
                             ){
  type_1L_chr <- match.arg(type_1L_chr)
  if(!identical(seed_1L_int,integer(0))){
    set.seed(seed_1L_int)
  }
  if(identical(write_to_1L_chr, character(0))){
    write_to_1L_chr <- tempdir()
  }
  1:length(iterations_ls) %>%
    purrr::walk(~ {
      iterations_int <- iterations_ls[[.x]]
      # Parameter draws
      draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, scale_1L_int = scale_1L_int, seed_1L_int = seed_1L_int + .x)
      # Intervention
      Y_Ready4useDyad <- predict_digital_pathway(inputs_ls, base_for_rates_int = base_for_rates_int, draws_tb = draws_tb, iterations_int = iterations_int, horizon_dtm = horizon_dtm, modifiable_chr = modifiable_chr, tfmn_ls = tfmn_ls, tx_duration_dtm = tx_duration_dtm, seed_1L_int = seed_1L_int + .x, start_dtm = start_dtm)
      # Comparator
      Z_Ready4useDyad <- predict_comparator_pathway(inputs_ls, base_for_rates_int = base_for_rates_int, draws_tb = draws_tb, iterations_int = iterations_int, horizon_dtm = horizon_dtm, modifiable_chr = modifiable_chr, tfmn_ls = tfmn_ls, tx_duration_dtm = tx_duration_dtm, seed_1L_int = seed_1L_int + .x, start_dtm = start_dtm)
      saveRDS(list(Y_Ready4useDyad = Y_Ready4useDyad, Z_Ready4useDyad = Z_Ready4useDyad),
              paste0(write_to_1L_chr,"/SimBatch",.x,".RDS"))
    })
  results_ls <- 1:length(iterations_ls) %>% purrr::reduce(.init = list(),
                                            ~ {
                                              additions_ls <- readRDS(paste0(write_to_1L_chr,"/SimBatch",.y,".RDS"))
                                              if(identical(.x, list())){
                                                additions_ls
                                                }else{
                                                  list(Y_Ready4useDyad = renewSlot(.x$Y_Ready4useDyad, "ds_tb",
                                                                               dplyr::bind_rows(.x$Y_Ready4useDyad@ds_tb,
                                                                                                additions_ls$Y_Ready4useDyad@ds_tb)), 
                                                       Z_Ready4useDyad = renewSlot(.x$Z_Ready4useDyad, "ds_tb",
                                                                               dplyr::bind_rows(.x$Z_Ready4useDyad@ds_tb,
                                                                                                additions_ls$Z_Ready4useDyad@ds_tb)))
                                                  }
                                              }
                                            )
  if(purge_1L_lgl){
    1:length(iterations_ls) %>%
      purrr::walk(~ unlink(paste0(write_to_1L_chr,"/SimBatch",.x,".RDS")))
  }
   ## Synthesis
  X_Ready4useDyad <- make_results_synthesis(inputs_ls$Synthetic_r4, Y_Ready4useDyad = results_ls$Y_Ready4useDyad, Z_Ready4useDyad = results_ls$Z_Ready4useDyad, 
                                            add_severity_1L_lgl = T,     
                                            exclude_chr =c("Adult", "Period", "MeasurementWeek", "treatment_fraction", "treatment_measurement", "treatment_start"),
                                            exclude_suffixes_chr = c( "_change","_date", "_previous","52_Weeks"),
                                            keep_chr = c("platform", "clinic_state", "clinic_type", "Age", "gender", "employment_status"),
                                            modifiable_chr = modifiable_chr,
                                            type_1L_chr = type_1L_chr)
  return(X_Ready4useDyad)
}

