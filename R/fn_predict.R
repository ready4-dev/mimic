#' Predict comparator pathway
#' @description predict_comparator_pathway() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict comparator pathway. The function is called for its side effects and does not return a value.
#' @param inputs_ls Inputs (a list)
#' @param add_logic_fn Add logic (a function), Default: add_project_offset_logic
#' @param arm_1L_chr Arm (a character vector of length one), Default: 'Comparator'
#' @param arms_chr Arms (a character vector)
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
#' @param utility_fns_ls Utility functions (a list), Default: make_utility_fns_ls(utilities_chr = utilities_chr)
#' @param variable_unit_1L_chr Variable unit (a character vector of length one), Default: 'Minutes'
#' @return X (A dataset and data dictionary pair.)
#' @rdname predict_comparator_pathway
#' @export 
#' @importFrom lubridate years weeks days
predict_comparator_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, 
    arm_1L_chr = "Comparator", arms_chr, base_for_rates_int = c(1000L, 
        1, 1), draws_tb = NULL, extra_draws_fn = add_draws_from_pool, 
    iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
    modifiable_chr = c("treatment_status", "Minutes", "k10", 
        "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
    start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
    utilities_chr = c("CHU9D", "AQoL6D"), utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr), 
    variable_unit_1L_chr = "Minutes") 
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
        utility_fns_ls = utility_fns_ls, type_1L_chr = "Model")
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
        utility_fns_ls = utility_fns_ls, type_1L_chr = "Model")
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateOutcomes", 
        schedule_fn = update_scheduled_date)
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_outcomes_event_sequence(X_Ready4useDyad, 
        add_sensitivity_1L_lgl = T, adjustment_1L_dbl = -2, iterations_int = iterations_int, 
        inputs_ls = inputs_ls, sensitivities_ls = sensitivities_ls, 
        tfmn_ls = make_class_tfmns(T), utilities_chr = c("AQoL6D", 
            "CHU9D"), utility_fns_ls = utility_fns_ls, type_1L_chr = "Project")
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
#' @param arms_chr Arms (a character vector)
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
#' @param utility_fns_ls Utility functions (a list), Default: make_utility_fns_ls(utilities_chr = utilities_chr)
#' @param variable_unit_1L_chr Variable unit (a character vector of length one), Default: 'Minutes'
#' @return X (A dataset and data dictionary pair.)
#' @rdname predict_digital_pathway
#' @export 
#' @importFrom lubridate years weeks days
predict_digital_pathway <- function (inputs_ls, add_logic_fn = add_project_offset_logic, 
    arm_1L_chr = "Intervention", arms_chr, base_for_rates_int = c(1000L, 
        1L, 1L), draws_tb = NULL, extra_draws_fn = add_draws_from_pool, 
    iterations_int = 1:100L, horizon_dtm = lubridate::years(1), 
    modifiable_chr = c("treatment_status", "Minutes", "k10", 
        "AQoL6D", "CHU9D"), seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
    start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
    utilities_chr = c("CHU9D", "AQoL6D"), utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr), 
    variable_unit_1L_chr = "Minutes") 
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
        utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls, 
        type_1L_chr = "Model")
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
        utility_fns_ls = utility_fns_ls, type_1L_chr = "Project")
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
#' @param inputs_ls Inputs (a list), Default: NULL
#' @param add_logic_fn Add logic (a function), Default: identity
#' @param arm_1L_chr Arm (a character vector of length one)
#' @param arms_chr Arms (a character vector), Default: character(0)
#' @param arms_for_intervention_costs_chr Arms for intervention costs (a character vector), Default: character(0)
#' @param arms_for_offsets_chr Arms for offsets (a character vector), Default: character(0)
#' @param arms_for_non_helpseeking_chr Arms for non helpseeking (a character vector), Default: character(0)
#' @param arms_for_iar_adjustment_chr Arms for Initial Assessment andeferral adjustment (a character vector), Default: character(0)
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param extra_draws_fn Extra draws (a function), Default: NULL
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param iterations_int Iterations (an integer vector), Default: integer(0)
#' @param modifiable_chr Modifiable (a character vector), Default: make_project_2_vars("modify")
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: make_project_2_sensitivities_ls()
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @param treatment_ls Treatment (a list), Default: NULL
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2")
#' @param utility_fns_ls Utility functions (a list), Default: make_utility_fns_ls(utilities_chr = utilities_chr)
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @return Population ls$X (A dataset and data dictionary pair.)
#' @rdname predict_project_2_pathway
#' @export 
#' @importFrom lubridate years weeks days
predict_project_2_pathway <- function (inputs_ls = NULL, add_logic_fn = identity, arm_1L_chr, 
    arms_chr = character(0), arms_for_intervention_costs_chr = character(0), 
    arms_for_offsets_chr = character(0), arms_for_non_helpseeking_chr = character(0), 
    arms_for_iar_adjustment_chr = character(0), batch_1L_int = integer(0), 
    draws_tb = NULL, extra_draws_fn = NULL, horizon_dtm = lubridate::years(1), 
    iterations_int = integer(0), modifiable_chr = make_project_2_vars("modify"), 
    seed_1L_int = 2001L, sensitivities_ls = make_project_2_sensitivities_ls(), 
    start_dtm = Sys.Date(), tfmn_ls = make_class_tfmns(), tx_duration_dtm = lubridate::weeks(12), 
    treatment_ls = NULL, utilities_chr = c("AQoL8D", "EQ5D", 
        "EQ5DM2", "SF6D", "SF6DM2"), utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr), 
    X_MimicConfiguration = MimicConfiguration()) 
{
    X_MimicConfiguration <- renew(X_MimicConfiguration, env_ls = list(main_ls = list(`Project 2` = predict_project_2_pathway), 
        initialise_ls = make_project_2_initialise_ls(derive_ls = X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls)), 
        what_1L_chr = c("legacy"))
    if (identical(names(X_MimicConfiguration@arms_tb), "Arm")) {
        X_MimicConfiguration <- renewSlot(X_MimicConfiguration, 
            "arms_tb", make_arms_tb(X_MimicConfiguration@arms_tb, 
                settings_ls = X_MimicConfiguration@arms_tb$Arm %>% 
                  make_project_2_arms_extras_ls(arms_for_iar_adjustment_chr = arms_for_iar_adjustment_chr, 
                    arms_for_intervention_costs_chr = arms_for_intervention_costs_chr, 
                    arms_for_non_helpseeking_chr = arms_for_non_helpseeking_chr, 
                    arms_for_offsets_chr = arms_for_offsets_chr, 
                    treatment_ls = treatment_ls, tx_duration_dtm = tx_duration_dtm)))
    }
    if (is.null(draws_tb)) {
        draws_tb <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, 
            what_1L_chr = "draws_tb")
    }
    tx_prefix_1L_chr <- "Treatment"
    events_ls <- list(EpisodeOfCareSequence = make_project_2_episode_sequence(event_nm_1L_chr = "EpisodeOfCareSequence", 
        change_first_mdl = "K10_mdl", change_relapse_1L_chr = "K10Relapse_mdl", 
        end_mdl_1L_chr = "EpisodeEnd_mdl", ineligible_1L_chr = "Episode >0 | NonHelpSeeking", 
        outcome_var_1L_chr = "K10", start_mdl_1L_chr = "EpisodeStart_mdl", 
        type_schedule_1L_chr = c("first"), use_schedule_1L_chr = "Y", 
        use_trigger_1L_chr = NA_character_, validate_schedule_1L_chr = "WaitInDays", 
        vars_chr = c("WaitInDays", "DaysToYearOneRepresentation"), 
        workers_chr = make_worker_types(), workers_medical_chr = make_worker_types("medical")), 
        RepeatEpisodeOfCareSequence = make_project_2_episode_sequence(event_nm_1L_chr = "RepeatEpisodeOfCareSequence", 
            change_first_mdl = "K10_mdl", change_relapse_1L_chr = "K10Relapse_mdl", 
            end_mdl_1L_chr = "EpisodeEnd_mdl", ineligible_1L_chr = "Episode ==0 | NonHelpSeeking", 
            outcome_var_1L_chr = "K10", start_mdl_1L_chr = "Representation_mdl", 
            type_schedule_1L_chr = c("repeat"), use_schedule_1L_chr = "Z", 
            use_trigger_1L_chr = "Z", validate_schedule_1L_chr = "DaysToYearOneRepresentation", 
            vars_chr = c("WaitInDays", "DaysToYearOneRepresentation"), 
            workers_chr = make_worker_types(), workers_medical_chr = make_worker_types("medical")), 
        UpdateUntreatedOutcomes = make_project_2_untreated_sequence(event_nm_1L_chr = "UpdateUntreatedOutcomes", 
            action_fn = add_regression_to_mean, draws_fn = add_project_2_k10_draws, 
            ineligible_1L_chr = "!NonHelpSeeking", use_schedule_1L_chr = "Y", 
            use_trigger_1L_chr = "Z"), RegressionToMean = make_project_2_regression_to_mean(event_nm_1L_chr = "RegressionToMean", 
            ineligible_1L_chr = "!NonHelpSeeking", outcome_var_1L_chr = "K10", 
            use_schedule_1L_chr = "Y", use_trigger_1L_chr = "Z"))
    X_MimicPopulation <- metamorphose(X_MimicConfiguration, arm_1L_chr = arm_1L_chr, 
        batch_1L_int = batch_1L_int, draws_tb = draws_tb, env_ls = list(arms_for_non_helpseeking_chr = procure(X_MimicConfiguration, 
            empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", 
            type_1L_chr = "Helpseeking adjustment"), arms_for_iar_adjustment_chr = procure(X_MimicConfiguration, 
            empty_xx = character(0), match_value_xx = T, target_1L_chr = "Arm", 
            type_1L_chr = "IAR adjustment"), reset_date_1L_lgl = FALSE), 
        tx_prefix_1L_chr = tx_prefix_1L_chr, Y_Ready4Module = MimicPopulation())
    X_MimicPopulation <- renew(X_MimicPopulation, batch_1L_int = batch_1L_int, 
        env_ls = list(arm_1L_chr = arm_1L_chr, episode_1L_int = 1, 
            never_1L_int = ceiling(X_MimicConfiguration@horizon_dtm/lubridate::days(1)), 
            tx_prefix_1L_chr = tx_prefix_1L_chr), tx_prefix_1L_chr = tx_prefix_1L_chr, 
        type_1L_chr = "event", X_MimicConfiguration = X_MimicConfiguration, 
        X_MimicEvent = events_ls$EpisodeOfCareSequence)
    X_MimicPopulation <- renew(X_MimicPopulation, batch_1L_int = batch_1L_int, 
        env_ls = list(arm_1L_chr = arm_1L_chr, episode_1L_int = 2, 
            never_1L_int = ceiling(X_MimicConfiguration@horizon_dtm/lubridate::days(1)), 
            tx_prefix_1L_chr = tx_prefix_1L_chr), tx_prefix_1L_chr = tx_prefix_1L_chr, 
        type_1L_chr = "event", X_MimicConfiguration = X_MimicConfiguration, 
        X_MimicEvent = events_ls$RepeatEpisodeOfCareSequence)
    X_MimicPopulation <- renew(X_MimicPopulation, batch_1L_int = batch_1L_int, 
        env_ls = list(arm_1L_chr = arm_1L_chr, tx_prefix_1L_chr = tx_prefix_1L_chr), 
        tx_prefix_1L_chr = tx_prefix_1L_chr, type_1L_chr = "event", 
        X_MimicConfiguration = X_MimicConfiguration, X_MimicEvent = events_ls$RegressionToMean)
    population_ls <- manufacture(X_MimicPopulation, what_1L_chr = "population_ls")
    population_ls$X_Ready4useDyad <- add_time_to_event(population_ls$X_Ready4useDyad, 
        event_1L_chr = "WrapUp", schedule_fn = update_scheduled_date)
    population_ls$X_Ready4useDyad <- update_current_date(population_ls$X_Ready4useDyad)
    population_ls$X_Ready4useDyad <- update_current_event(population_ls$X_Ready4useDyad)
    drop_missing_1L_lgl <- X_MimicConfiguration@drop_missing_1L_lgl
    drop_suffix_1L_chr <- X_MimicConfiguration@drop_suffix_1L_chr
    extra_draws_fn <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls$extra_draws_fn
    horizon_dtm <- X_MimicConfiguration@horizon_dtm
    initialise_ls <- make_project_2_initialise_ls(derive_ls = X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls)
    inputs_ls <- manufacture(X_MimicConfiguration@x_MimicInputs, 
        what_1L_chr = "inputs_ls")
    iterations_int <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, 
        what_1L_chr = "iterations")
    iterations_ls <- X_MimicConfiguration@iterations_ls
    modifiable_chr <- X_MimicConfiguration@modifiable_chr
    seed_1L_int <- X_MimicConfiguration@seed_1L_int
    sensitivities_ls <- X_MimicConfiguration@x_MimicAlgorithms@sensitivities_ls
    start_dtm <- X_MimicConfiguration@start_dtm
    synthesis_fn <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls$synthesis_fn
    tfmn_ls <- X_MimicConfiguration@x_MimicAlgorithms@transformations_ls
    tx_duration_dtm <- procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, 
        target_1L_chr = "Treatment duration")
    utilities_chr <- X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@names_chr
    utility_fns_ls <- X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls
    population_ls$X_Ready4useDyad <- add_project_2_model_wrap_up(population_ls$X_Ready4useDyad, 
        arms_for_intervention_costs_chr = arms_for_intervention_costs_chr, 
        arms_for_offsets_chr = arms_for_offsets_chr, disciplines_chr = make_disciplines(), 
        inputs_ls = inputs_ls, iterations_int = iterations_int, 
        sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
        tx_prefix_1L_chr = tx_prefix_1L_chr, utilities_chr = utilities_chr, 
        utility_fns_ls = utility_fns_ls)
    return(population_ls$X_Ready4useDyad)
}
#' Predict with sim
#' @description predict_with_sim() is a Predict function that applies a model to make predictions. Specifically, this function implements an algorithm to predict with sim. The function returns Output (an output object of multiple potential types).
#' @param inputs_ls Inputs (a list), Default: NULL
#' @param arms_chr Arms (a character vector), Default: c("Intervention", "Comparator")
#' @param comparator_fn Comparator (a function), Default: predict_comparator_pathway
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_suffix_1L_chr Drop suffix (a character vector of length one), Default: character(0)
#' @param extra_draws_fn Extra draws (a function), Default: NULL
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param intervention_fn Intervention (a function), Default: predict_digital_pathway
#' @param iterations_ls Iterations (a list), Default: make_batches(5, of_1L_int = 20)
#' @param modifiable_chr Modifiable (a character vector), Default: c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D")
#' @param prior_batches_1L_int Prior batches (an integer vector of length one), Default: 0
#' @param purge_1L_lgl Purge (a logical vector of length one), Default: TRUE
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param synthesis_fn Synthesis (a function), Default: make_project_results_synthesis
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param type_1L_chr Type (a character vector of length one), Default: c("D", "AB", "C", "NULL")
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: FALSE
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL6D", "CHU9D")
#' @param utility_fns_ls Utility functions (a list), Default: make_utility_fns_ls(utilities_chr = utilities_chr)
#' @param write_to_1L_chr Write to (a character vector of length one), Default: character(0)
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param Y_MimicRepos PARAM_DESCRIPTION, Default: MimicRepos()
#' @param ... Additional arguments
#' @return Output (an output object of multiple potential types)
#' @rdname predict_with_sim
#' @export 
#' @importFrom lubridate years
#' @importFrom purrr safely walk map
#' @importFrom dplyr filter
#' @importFrom rlang exec
predict_with_sim <- function (inputs_ls = NULL, arms_chr = c("Intervention", "Comparator"), 
    comparator_fn = predict_comparator_pathway, draws_tb = NULL, 
    drop_missing_1L_lgl = FALSE, drop_suffix_1L_chr = character(0), 
    extra_draws_fn = NULL, horizon_dtm = lubridate::years(1), 
    intervention_fn = predict_digital_pathway, iterations_ls = make_batches(5, 
        of_1L_int = 20), modifiable_chr = c("treatment_status", 
        "Minutes", "k10", "AQoL6D", "CHU9D"), prior_batches_1L_int = 0, 
    purge_1L_lgl = TRUE, seed_1L_int = 2001L, sensitivities_ls = make_sensitivities_ls(), 
    start_dtm = Sys.Date(), synthesis_fn = make_project_results_synthesis, 
    tfmn_ls = make_class_tfmns(), type_1L_chr = c("D", "AB", 
        "C", "NULL"), unlink_1L_lgl = FALSE, utilities_chr = c("AQoL6D", 
        "CHU9D"), utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr), 
    write_to_1L_chr = character(0), X_MimicConfiguration = MimicConfiguration(), 
    Y_MimicRepos = MimicRepos(), ...) 
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
    if (!identical(X_MimicConfiguration, MimicConfiguration())) {
        batches_int <- 1:length(X_MimicConfiguration@iterations_ls)
    }
    else {
        batches_int <- 1:length(iterations_ls)
    }
    output_xx <- batches_int %>% purrr::map(~{
        if (!is.null(draws_tb)) {
            if (!identical(X_MimicConfiguration, MimicConfiguration())) {
                iterations_int <- manufacture(X_MimicConfiguration, 
                  batch_1L_int = .x, what_1L_chr = "iterations")
            }
            else {
                iterations_int <- iterations_ls[[.x]]
            }
            filtered_draws_tb <- draws_tb %>% dplyr::filter(Iteration %in% 
                iterations_int)
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
            utility_fns_ls = utility_fns_ls, write_to_1L_chr = write_to_1L_chr, 
            X_MimicConfiguration = X_MimicConfiguration, Y_MimicRepos = Y_MimicRepos) %>% 
            append(extras_ls)
        rlang::exec(predict_safely_fn, !!!args_ls)
    })
    if (type_1L_chr != "NULL") {
        output_xx <- import_results_batches(dir_1L_chr = write_to_1L_chr)
    }
    if (purge_1L_lgl) {
        batches_int %>% purrr::walk(~unlink(paste0(write_to_1L_chr, 
            "/SimBatch", .x + prior_batches_1L_int, ".RDS")))
    }
    if (type_1L_chr != "NULL") {
        if (!identical(X_MimicConfiguration, MimicConfiguration())) {
            synthesis_fn <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls$synthesis_fn
            modifiable_chr <- X_MimicConfiguration@modifiable_chr
        }
        output_xx <- synthesis_fn(inputs_ls, results_ls = output_xx, 
            modifiable_chr = modifiable_chr, type_1L_chr = type_1L_chr)
    }
    return(output_xx)
}
