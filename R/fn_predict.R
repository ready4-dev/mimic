#' Predict comparator pathway
#' @description predict_comparator_pathway() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict comparator pathway. The function is called for its side effects and does not return a value.
#' @param inputs_ls Inputs (a list)
#' @param add_logic_fn Add logic (a function), Default: add_project_offset_logic
#' @param arm_1L_chr Arm (a character vector of length one), Default: 'Comparator'
#' @param base_for_rates_int Base for rates (an integer vector), Default: c(1000L, 1, 1)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param extra_draws_fn Extra draws (a function), Default: add_draws_from_pool
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param modifiable_chr Modifiable (a character vector), Default: c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D")
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @param utilities_chr Utilities (a character vector), Default: c("CHU9D", "AQoL6D")
#' @param variable_unit_1L_chr Variable unit (a character vector of length one), Default: 'Minutes'
#' @return X (A dataset and data dictionary pair.)
#' @rdname predict_comparator_pathway
#' @export 
#' @importFrom lubridate years weeks days
predict_comparator_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, 
    arm_1L_chr = "Comparator", base_for_rates_int = c(1000L, 
        1, 1), draws_tb = NULL, extra_draws_fn = add_draws_from_pool, 
    iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
    modifiable_chr = c("treatment_status", "Minutes", "k10", 
        "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
    start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
    utilities_chr = c("CHU9D", "AQoL6D"), variable_unit_1L_chr = "Minutes") 
{
    if (is.null(draws_tb)) {
        draws_tb <- make_draws_tb(inputs_ls, extra_draws_fn = extra_draws_fn, 
            iterations_int = iterations_int, seed_1L_int = seed_1L_int)
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
    X_Ready4useDyad <- add_costs_event(X_Ready4useDyad, add_logic_fn = add_logic_fn, 
        add_offsets_1L_lgl = F, base_for_rates_int = base_for_rates_int, 
        inputs_ls = inputs_ls, offsets_chr = names(inputs_ls$pooled_ls), 
        type_1L_chr = "zero", variable_unit_1L_chr = variable_unit_1L_chr)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_leave_model_event(X_Ready4useDyad)
    return(X_Ready4useDyad)
}
#' Predict digital pathway
#' @description predict_digital_pathway() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict digital pathway. The function is called for its side effects and does not return a value.
#' @param inputs_ls Inputs (a list)
#' @param add_logic_fn Add logic (a function), Default: add_project_offset_logic
#' @param arm_1L_chr Arm (a character vector of length one), Default: 'Intervention'
#' @param base_for_rates_int Base for rates (an integer vector), Default: c(1000L, 1L, 1L)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param extra_draws_fn Extra draws (a function), Default: add_draws_from_pool
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param modifiable_chr Modifiable (a character vector), Default: c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D")
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @param utilities_chr Utilities (a character vector), Default: c("CHU9D", "AQoL6D")
#' @param variable_unit_1L_chr Variable unit (a character vector of length one), Default: 'Minutes'
#' @return X (A dataset and data dictionary pair.)
#' @rdname predict_digital_pathway
#' @export 
#' @importFrom lubridate years weeks days
predict_digital_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, 
    arm_1L_chr = "Intervention", base_for_rates_int = c(1000L, 
        1L, 1L), draws_tb = NULL, extra_draws_fn = add_draws_from_pool, 
    iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
    modifiable_chr = c("treatment_status", "Minutes", "k10", 
        "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
    start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
    utilities_chr = c("CHU9D", "AQoL6D"), variable_unit_1L_chr = "Minutes") 
{
    if (is.null(draws_tb)) {
        draws_tb <- make_draws_tb(inputs_ls, extra_draws_fn = extra_draws_fn, 
            iterations_int = iterations_int, seed_1L_int = seed_1L_int)
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
        schedule_fn = update_scheduled_date, schedule_args_ls = list(target_1L_int = 336, 
            type_1L_chr = "Day"))
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
    X_Ready4useDyad <- add_costs_event(X_Ready4useDyad, add_offsets_1L_lgl = T, 
        add_logic_fn = add_logic_fn, base_for_rates_int = base_for_rates_int, 
        inputs_ls = inputs_ls, offsets_chr = names(inputs_ls$pooled_ls), 
        type_1L_chr = "both", variable_unit_1L_chr = variable_unit_1L_chr)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_leave_model_event(X_Ready4useDyad)
    return(X_Ready4useDyad)
}
#' Predict from pool
#' @description predict_from_pool() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict from pool. The function returns Predictions (an output object of multiple potential types).
#' @param pooled_xx Pooled (an output object of multiple potential types)
#' @param as_1L_chr As (a character vector of length one), Default: c("vector", "histogram", "summary")
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: numeric(0)
#' @param distributions_chr Distributions (a character vector), Default: 'best'
#' @param maximum_1L_dbl Maximum (a double vector of length one), Default: Inf
#' @param minimum_1L_dbl Minimum (a double vector of length one), Default: -Inf
#' @param n_1L_int N (an integer vector of length one), Default: 100
#' @param quantiles_dbl Quantiles (a double vector), Default: c(0.05, 0.25, 0.5, 0.75, 0.95)
#' @param resample_1L_lgl Resample (a logical vector of length one), Default: F
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param title_1L_chr Title (a character vector of length one), Default: 'Pooled prediction distribution'
#' @param type_1L_chr Type (a character vector of length one), Default: c("r", "q", "p")
#' @param x_label_1L_chr X label (a character vector of length one), Default: 'Predictions'
#' @param values_dbl Values (a double vector), Default: numeric(0)
#' @param weights_dbl Weights (a double vector), Default: 1
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @param ... Additional arguments
#' @return Predictions (an output object of multiple potential types)
#' @rdname predict_from_pool
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom purrr pluck map_dbl
#' @importFrom SHELF plinearpool qlinearpool rlinearpool
predict_from_pool <- function (pooled_xx, as_1L_chr = c("vector", "histogram", "summary"), 
    adjustment_1L_dbl = numeric(0), distributions_chr = "best", 
    maximum_1L_dbl = Inf, minimum_1L_dbl = -Inf, n_1L_int = 100L, 
    quantiles_dbl = c(0.05, 0.25, 0.5, 0.75, 0.95), resample_1L_lgl = F, 
    seed_1L_int = 2001L, title_1L_chr = "Pooled prediction distribution", 
    type_1L_chr = c("r", "q", "p"), x_label_1L_chr = "Predictions", 
    values_dbl = numeric(0), weights_dbl = 1, what_1L_chr = character(0), 
    ...) 
{
    as_1L_chr <- match.arg(as_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (inherits(pooled_xx, "elicitation")) {
        pooled_mdl <- pooled_xx
    }
    else {
        assertthat::assert_that(!identical(what_1L_chr, character(0)))
        pooled_mdl <- pooled_xx %>% purrr::pluck(what_1L_chr)
    }
    if (!identical(seed_1L_int, integer(0))) {
        set.seed(seed_1L_int)
    }
    if (type_1L_chr == "p") {
        assertthat::assert_that(!identical(values_dbl, numeric(0)))
        if (!identical(adjustment_1L_dbl, numeric(0))) {
            values_dbl <- values_dbl + adjustment_1L_dbl
        }
        values_dbl <- values_dbl %>% purrr::map_dbl(~min(max(.x, 
            minimum_1L_dbl), maximum_1L_dbl))
        predictions_dbl <- SHELF::plinearpool(pooled_mdl, x = values_dbl, 
            d = distributions_chr, w = weights_dbl)
    }
    if (type_1L_chr == "q") {
        predictions_dbl <- SHELF::qlinearpool(pooled_mdl, q = c(0.05, 
            0.25, 0.5, 0.75, 0.95), d = distributions_chr, w = weights_dbl)
    }
    if (type_1L_chr == "r") {
        predictions_dbl <- SHELF::rlinearpool(pooled_mdl, n = n_1L_int, 
            d = distributions_chr, w = weights_dbl)
    }
    if (!identical(adjustment_1L_dbl, numeric(0)) & type_1L_chr %in% 
        c("r", "q")) {
        predictions_dbl <- predictions_dbl + adjustment_1L_dbl
    }
    predictions_dbl <- predictions_dbl %>% purrr::map_dbl(~min(max(.x, 
        minimum_1L_dbl), maximum_1L_dbl))
    if (resample_1L_lgl) {
        predictions_dbl <- replace(predictions_dbl, is.na(predictions_dbl), 
            sample(predictions_dbl[!is.na(predictions_dbl)], 
                replace = T))
    }
    if (as_1L_chr == "vector") {
        predictions_xx <- predictions_dbl
    }
    if (as_1L_chr == "summary") {
        predictions_xx <- summary(predictions_dbl, ...)
    }
    if (as_1L_chr == "histogram") {
        predictions_xx <- hist(predictions_dbl, main = title_1L_chr, 
            xlab = x_label_1L_chr, ...)
    }
    return(predictions_xx)
}
#' Predict project 2 pathway
#' @description predict_project_2_pathway() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict project 2 pathway. The function is called for its side effects and does not return a value.
#' @param inputs_ls Inputs (a list)
#' @param arm_1L_chr Arm (a character vector of length one)
#' @param add_logic_fn Add logic (a function), Default: identity
#' @param arms_for_intervention_costs_chr Arms for intervention costs (a character vector)
#' @param arms_for_offsets_chr Arms for offsets (a character vector), Default: character(0)
#' @param arms_for_non_helpseeking_chr Arms for non helpseeking (a character vector), Default: character(0)
#' @param arms_for_iar_adjustment_chr Arms for Initial Assessment andeferral adjustment (a character vector), Default: character(0)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param extra_draws_fn Extra draws (a function), Default: NULL
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param modifiable_chr Modifiable (a character vector), Default: make_project_2_vars("modify")
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: make_project_2_sensitivities_ls()
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @param treatment_ls Treatment (a list), Default: NULL
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2")
#' @return Population ls$X (A dataset and data dictionary pair.)
#' @rdname predict_project_2_pathway
#' @export 
#' @importFrom lubridate years weeks
#' @importFrom purrr pluck reduce
#' @importFrom dplyr mutate
#' @importFrom rlang sym
predict_project_2_pathway <- function (inputs_ls, arm_1L_chr, add_logic_fn = identity, arms_for_intervention_costs_chr, 
    arms_for_offsets_chr = character(0), arms_for_non_helpseeking_chr = character(0), 
    arms_for_iar_adjustment_chr = character(0), draws_tb = NULL, 
    extra_draws_fn = NULL, horizon_dtm = lubridate::years(1), 
    iterations_int = 1:100L, modifiable_chr = make_project_2_vars("modify"), 
    seed_1L_int = 2001L, sensitivities_ls = make_project_2_sensitivities_ls(), 
    start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
    treatment_ls = NULL, utilities_chr = c("AQoL8D", "EQ5D", 
        "EQ5DM2", "SF6D", "SF6DM2")) 
{
    if (is.null(draws_tb)) {
        draws_tb <- make_draws_tb(inputs_ls, drop_missing_1L_lgl = T, 
            drop_suffix_1L_chr = "_mean", extra_draws_fn = extra_draws_fn, 
            iterations_int = iterations_int, seed_1L_int = seed_1L_int)
    }
    if (is.null(treatment_ls)) {
        treatment_1L_chr <- character(0)
    }
    else {
        treatment_1L_chr <- treatment_ls %>% purrr::pluck(arm_1L_chr)
    }
    tx_prefix_1L_chr <- "Treatment"
    utility_fns_ls <- make_utility_fns_ls(utilities_chr = utilities_chr)
    population_ls <- add_enter_model_event(X_Ready4useDyad = inputs_ls$Synthetic_r4, 
        arm_1L_chr = arm_1L_chr, default_fn = function(X) renewSlot(X, 
            "ds_tb", c("Episode", "ClinicalPsychologistUseMins", 
                "GPUseMins", "PsychiatristUseMins", "OtherMedicalUseMins", 
                "NurseUseMins", "OtherUseMins", "TotalUseMins", 
                "Cost", "Cost_S1", "Cost_S2") %>% purrr::reduce(.init = X@ds_tb, 
                ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                  0)))), derive_fn_ls = utility_fns_ls, draws_tb = draws_tb, 
        horizon_dtm = lubridate::years(1), iterations_int = iterations_int, 
        modifiable_chr = setdiff(modifiable_chr, c("ClinicalPsychologistUseMins", 
            "GPUseMins", "PsychiatristUseMins", "OtherMedicalUseMins", 
            "NurseUseMins", "OtherUseMins", "TotalUseMins")), 
        start_dtm = start_dtm, tidy_cols_1L_lgl = T, tfmn_ls = tfmn_ls, 
        tx_duration_dtm = lubridate::weeks(12), tx_prefix_1L_chr = tx_prefix_1L_chr) %>% 
        update_population_ls(population_ls = NULL, type_1L_chr = "form")
    population_ls$X_Ready4useDyad <- add_non_helpseekers(population_ls$X_Ready4useDyad, 
        arms_for_non_helpseeking_chr = arms_for_non_helpseeking_chr)
    population_ls$X_Ready4useDyad <- add_non_iar(population_ls$X_Ready4useDyad, 
        arms_for_iar_adjustment_chr = arms_for_iar_adjustment_chr)
    population_ls <- update_population_ls(population_ls)
    if (nrow(population_ls$X_Ready4useDyad@ds_tb) > 0) {
        population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, 
            event_1L_chr = "StartEpisode", schedule_fn = add_episode_wait_time, 
            schedule_args_ls = list(episode_start_mdl = inputs_ls$models_ls$EpisodeStart_mdl, 
                iterations_int = iterations_int, treatment_1L_chr = treatment_1L_chr))
        print_errors(population_ls$X_Ready4useDyad, vars_chr = c("WaitInDays"), 
            assert_1L_lgl = FALSE, invalid_fn = function(x) (is.na(x) | 
                is.nan(x) | is.null(x) | x == -Inf | x == Inf | 
                x < 0))
        population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
        population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
        population_ls <- update_population_ls(population_ls)
    }
    if (nrow(population_ls$X_Ready4useDyad@ds_tb) > 0) {
        population_ls$X_Ready4useDyad <- add_episode(population_ls$X_Ready4useDyad, 
            assert_1L_lgl = FALSE, episode_1L_int = 1, inputs_ls = inputs_ls, 
            iterations_int = iterations_int, k10_var_1L_chr = "K10", 
            sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
            treatment_1L_chr = treatment_1L_chr, tx_prefix_1L_chr = tx_prefix_1L_chr, 
            utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls)
        population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, 
            event_1L_chr = "Represent", schedule_fn = add_episode_wait_time, 
            schedule_args_ls = list(episode_start_mdl = inputs_ls$models_ls$Representation_mdl, 
                iterations_int = iterations_int, type_1L_chr = "repeat", 
                treatment_1L_chr = treatment_1L_chr))
        print_errors(population_ls$X_Ready4useDyad, vars_chr = c("DaysToYearOneRepresentation"), 
            assert_1L_lgl = FALSE, invalid_fn = function(x) (is.na(x) | 
                is.nan(x) | is.null(x) | x == -Inf | x == Inf | 
                x < 0))
        population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
        population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
        population_ls <- update_population_ls(population_ls, 
            use_1L_chr = "Z")
    }
    if (nrow(population_ls$X_Ready4useDyad@ds_tb) > 0) {
        population_ls$X_Ready4useDyad <- add_episode(population_ls$X_Ready4useDyad, 
            assert_1L_lgl = FALSE, episode_1L_int = 2, inputs_ls = inputs_ls, 
            iterations_int = iterations_int, k10_var_1L_chr = "K10", 
            sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
            treatment_1L_chr = treatment_1L_chr, tx_prefix_1L_chr = tx_prefix_1L_chr, 
            utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls)
    }
    if (nrow(population_ls$Z_Ready4useDyad@ds_tb) > 0) {
        population_ls <- update_population_ls(population_ls, 
            type_1L_chr = "join", use_1L_chr = "Z")
    }
    if (nrow(population_ls$Y_Ready4useDyad@ds_tb) > 0) {
        population_ls$Y_Ready4useDyad <- renewSlot(population_ls$Y_Ready4useDyad, 
            "ds_tb", population_ls$Y_Ready4useDyad@ds_tb %>% 
                dplyr::mutate(CurrentDate = EndDate)) %>% add_regression_to_mean(sensitivities_ls = sensitivities_ls, 
            k10_draws_fn = add_project_2_k10_draws, tfmn_ls = tfmn_ls, 
            tx_prefix_1L_chr = tx_prefix_1L_chr, utilities_chr = utilities_chr, 
            utility_fns_ls = utility_fns_ls)
        population_ls <- update_population_ls(population_ls, 
            type_1L_chr = "join")
    }
    population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, 
        event_1L_chr = "WrapUp", schedule_fn = update_scheduled_date)
    population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
    population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
    population_ls$X_Ready4useDyad <- add_project_2_model_wrap_up(population_ls$X_Ready4useDyad, 
        arms_for_intervention_costs_chr = arms_for_intervention_costs_chr, 
        arms_for_offsets_chr = arms_for_offsets_chr, disciplines_chr = make_disciplines(), 
        inputs_ls = inputs_ls, iterations_int = iterations_int, 
        sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
        tx_prefix_1L_chr = tx_prefix_1L_chr, utilities_chr = utilities_chr)
    return(population_ls$X_Ready4useDyad)
}
#' Predict with sim
#' @description predict_with_sim() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict with sim. The function returns Output (an output object of multiple potential types).
#' @param inputs_ls Inputs (a list)
#' @param arms_chr Arms (a character vector), Default: c("Intervention", "Comparator")
#' @param comparator_fn Comparator (a function), Default: predict_comparator_pathway
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_suffix_1L_chr Drop suffix (a character vector of length one), Default: character(0)
#' @param extra_draws_fn Extra draws (a function), Default: NULL
#' @param intervention_fn Intervention (a function), Default: predict_digital_pathway
#' @param iterations_ls Iterations (a list), Default: make_batches(5, of_1L_int = 20)
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param modifiable_chr Modifiable (a character vector), Default: c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D")
#' @param prior_batches_1L_int Prior batches (an integer vector of length one), Default: 0
#' @param purge_1L_lgl Purge (a logical vector of length one), Default: TRUE
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param synthesis_fn Synthesis (a function), Default: make_project_results_synthesis
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param type_1L_chr Type (a character vector of length one), Default: c("D", "AB", "C", "NULL")
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: FALSE
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL6D", "CHU9D")
#' @param write_to_1L_chr Write to (a character vector of length one), Default: character(0)
#' @param Y_MimicRepos PARAM_DESCRIPTION, Default: MimicRepos()
#' @param ... Additional arguments
#' @return Output (an output object of multiple potential types)
#' @rdname predict_with_sim
#' @export 
#' @importFrom lubridate years
#' @importFrom purrr safely walk map
#' @importFrom dplyr filter
#' @importFrom rlang exec
predict_with_sim <- function (inputs_ls, arms_chr = c("Intervention", "Comparator"), 
    comparator_fn = predict_comparator_pathway, draws_tb = NULL, 
    drop_missing_1L_lgl = FALSE, drop_suffix_1L_chr = character(0), 
    extra_draws_fn = NULL, intervention_fn = predict_digital_pathway, 
    iterations_ls = make_batches(5, of_1L_int = 20), horizon_dtm = lubridate::years(1), 
    modifiable_chr = c("treatment_status", "Minutes", "k10", 
        "AQoL6D", "CHU9D"), prior_batches_1L_int = 0, purge_1L_lgl = TRUE, 
    seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
    synthesis_fn = make_project_results_synthesis, start_dtm = Sys.Date(), 
    tfmn_ls = make_class_tfmns(), type_1L_chr = c("D", "AB", 
        "C", "NULL"), unlink_1L_lgl = FALSE, utilities_chr = c("AQoL6D", 
        "CHU9D"), write_to_1L_chr = character(0), Y_MimicRepos = MimicRepos(), 
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
        if (!is.null(draws_tb)) {
            filtered_draws_tb <- draws_tb %>% dplyr::filter(Iteration %in% 
                iterations_ls[[.x]])
        }
        else {
            filtered_draws_tb <- NULL
        }
        args_ls <- list(batch_1L_int = .x, arms_chr = arms_chr, 
            comparator_fn = comparator_fn, draws_tb = filtered_draws_tb, 
            drop_missing_1L_lgl = drop_missing_1L_lgl, drop_suffix_1L_chr = drop_suffix_1L_chr, 
            extra_draws_fn = extra_draws_fn, horizon_dtm = horizon_dtm, 
            inputs_ls = inputs_ls, intervention_fn = intervention_fn, 
            iterations_ls = iterations_ls, modifiable_chr = modifiable_chr, 
            prior_batches_1L_int = prior_batches_1L_int, seed_1L_int = seed_1L_int, 
            sensitivities_ls = sensitivities_ls, start_dtm = start_dtm, 
            tfmn_ls = tfmn_ls, utilities_chr = utilities_chr, 
            write_to_1L_chr = write_to_1L_chr, Y_MimicRepos = Y_MimicRepos) %>% 
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
        output_xx <- synthesis_fn(inputs_ls, results_ls = output_xx, 
            modifiable_chr = modifiable_chr, type_1L_chr = type_1L_chr)
    }
    return(output_xx)
}
