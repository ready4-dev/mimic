#' Add activity
#' @description add_activity() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add activity. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param neither_1L_chr Neither (a character vector of length one), Default: 'ERROR'
#' @return Data (a tibble)
#' @rdname add_activity
#' @export 
#' @importFrom dplyr mutate case_when
#' @keywords internal
add_activity <- function (data_tb, neither_1L_chr = "ERROR") 
{
    data_tb <- data_tb %>% dplyr::mutate(Activity = dplyr::case_when(SignedUp == 
        1 & Onboarded == 0 ~ "SignUp", SignedUp == 0 & Onboarded == 
        1 ~ "Onboard", SignedUp == 1 & Onboarded == 1 ~ "SignUpOnboard", 
        T ~ neither_1L_chr))
    return(data_tb)
}
#' Add age to project datasets
#' @description add_age_to_project_dss() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add age to project datasets. The function returns Project datasets (a list).
#' @param project_dss_ls Project datasets (a list)
#' @param age_1L_chr Age (a character vector of length one), Default: 'Age'
#' @param drop_1L_lgl Drop (a logical vector of length one), Default: FALSE
#' @param date_of_birth_1L_chr Date of birth (a character vector of length one), Default: 'date_of_birth'
#' @param index_date_1L_chr Index date (a character vector of length one), Default: 'onboarding_date'
#' @param what_chr What (a character vector), Default: c("contacts", "outcomes", "overview")
#' @return Project datasets (a list)
#' @rdname add_age_to_project_dss
#' @export 
#' @importFrom purrr map2
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @importFrom lubridate interval years
#' @importFrom ready4use add_dictionary ready4use_dictionary renew.ready4use_dictionary
#' @importFrom tidyselect all_of
#' @keywords internal
add_age_to_project_dss <- function (project_dss_ls, age_1L_chr = "Age", drop_1L_lgl = FALSE, 
    date_of_birth_1L_chr = "date_of_birth", index_date_1L_chr = "onboarding_date", 
    what_chr = c("contacts", "outcomes", "overview")) 
{
    project_dss_ls <- project_dss_ls %>% purrr::map2(names(project_dss_ls), 
        ~{
            if (.y %in% what_chr) {
                if (inherits(.x, "Ready4useDyad")) {
                  ds_tb <- .x@ds_tb
                }
                else {
                  ds_tb <- .x
                }
                ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(age_1L_chr), 
                  (lubridate::interval(!!rlang::sym(date_of_birth_1L_chr), 
                    !!rlang::sym(index_date_1L_chr))/lubridate::years(1)) %>% 
                    round(0) %>% as.integer()))
                if (inherits(.x, "Ready4useDyad")) {
                  X <- .x
                  X@ds_tb <- ds_tb
                  X <- ready4use::add_dictionary(X, new_cases_r3 = ready4use::ready4use_dictionary() %>% 
                    ready4use::renew.ready4use_dictionary(var_nm_chr = age_1L_chr, 
                      var_ctg_chr = "Demographic", var_desc_chr = "Age at onboarding", 
                      var_type_chr = "ingeger"))
                  if (drop_1L_lgl) {
                    X <- X %>% renew(type_1L_chr = "drop", names_chr = "date_of_birth")
                  }
                  X
                }
                else {
                  if (drop_1L_lgl) {
                    ds_tb <- ds_tb %>% dplyr::select(-tidyselect::all_of(date_of_birth_1L_chr))
                  }
                  ds_tb
                }
            }
            else {
                .x
            }
        })
    return(project_dss_ls)
}
#' Add clients to summary
#' @description add_clients_to_summary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add clients to summary. The function returns Summaries (a list).
#' @param summaries_ls Summaries (a list)
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: character(0)
#' @param reference_1L_chr Reference (a character vector of length one), Default: 'Status quo'
#' @param weeks_1L_int Weeks (an integer vector of length one), Default: 26
#' @return Summaries (a list)
#' @rdname add_clients_to_summary
#' @export 
#' @importFrom dplyr select where bind_rows filter mutate across pull arrange
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @keywords internal
add_clients_to_summary <- function (summaries_ls, arrange_1L_chr = character(0), reference_1L_chr = "Status quo", 
    weeks_1L_int = 26) 
{
    summaries_ls$scenarios_ls$clients_tb <- summaries_ls$scenarios_ls$retained_tb %>% 
        dplyr::select(dplyr::where(is.numeric)) %>% names() %>% 
        purrr::reduce(.init = dplyr::bind_rows(summaries_ls$scenarios_ls$onboarders_tb, 
            summaries_ls$scenarios_ls$onboarders_tb %>% dplyr::filter(Scenario != 
                reference_1L_chr) %>% dplyr::mutate(Scenario = paste0(Scenario, 
                ", 100% of onboarder growth"))) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~.x/weeks_1L_int)), ~{
            name_1L_chr <- .y
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(name_1L_chr), 
                !!rlang::sym(name_1L_chr) + summaries_ls$scenarios_ls$retained_tb %>% 
                  dplyr::pull(!!rlang::sym(name_1L_chr))))
        })
    if (!identical(arrange_1L_chr, character(0))) {
        summaries_ls$scenarios_ls$clients_tb <- summaries_ls$scenarios_ls$clients_tb %>% 
            dplyr::arrange(!!rlang::sym(arrange_1L_chr))
    }
    return(summaries_ls)
}
#' Add cost calculations
#' @description add_cost_calculations() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cost calculations. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param inputs_ls Inputs (a list)
#' @param add_fixed_1L_lgl Add fixed (a logical vector of length one), Default: FALSE
#' @param add_offsets_1L_lgl Add offsets (a logical vector of length one), Default: FALSE
#' @param add_variable_1L_lgl Add variable (a logical vector of length one), Default: TRUE
#' @param base_for_rates_int Base for rates (an integer vector), Default: 1
#' @param offsets_chr Offsets (a character vector), Default: character(0)
#' @return Data (a tibble)
#' @rdname add_cost_calculations
#' @export 
#' @importFrom purrr reduce map_int map_dfr
#' @importFrom dplyr mutate group_by summarise n first left_join select
#' @importFrom rlang sym
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' @keywords internal
add_cost_calculations <- function (data_tb, inputs_ls, add_fixed_1L_lgl = FALSE, add_offsets_1L_lgl = FALSE, 
    add_variable_1L_lgl = TRUE, base_for_rates_int = 1L, offsets_chr = character(0)) 
{
    scenarios_chr <- get_unit_cost_detail(inputs_ls$unit_costs_tb, 
        what_1L_chr = "scenarios")
    variable_costs_dbl <- get_unit_cost_detail(inputs_ls$unit_costs_tb, 
        what_1L_chr = "variable")
    cost_names_chr <- get_unit_cost_detail(inputs_ls$unit_costs_tb, 
        what_1L_chr = "names")
    if (!identical(offsets_chr, character(0))) {
        offset_counts_chr <- (paste0("OffsetCount", offsets_chr))
        offset_costs_chr <- (paste0("OffsetCosts", offsets_chr))
    }
    else {
        offset_counts_chr <- offset_costs_chr <- character(0)
    }
    need_to_set_chr <- setdiff(c(cost_names_chr, offset_counts_chr, 
        offset_costs_chr), names(data_tb))
    if (!identical(need_to_set_chr, character(0))) {
        data_tb <- need_to_set_chr %>% purrr::reduce(.init = data_tb, 
            ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 0)))
    }
    if (add_variable_1L_lgl) {
        data_tb <- 1:length(scenarios_chr) %>% purrr::reduce(.init = data_tb, 
            ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(cost_names_chr[.y]), 
                !!rlang::sym(cost_names_chr[.y]) + Minutes * 
                  variable_costs_dbl[.y])))
    }
    if (add_fixed_1L_lgl) {
        fixed_costs_dbl <- get_unit_cost_detail(inputs_ls$unit_costs_tb, 
            what_1L_chr = "fixed")
        data_tb <- 1:length(scenarios_chr) %>% purrr::reduce(.init = data_tb, 
            ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(cost_names_chr[.y]), 
                !!rlang::sym(cost_names_chr[.y]) + fixed_costs_dbl[.y])))
    }
    if (add_offsets_1L_lgl) {
        if (length(base_for_rates_int) == 1 & length(base_for_rates_int) < 
            length(offsets_chr)) {
            base_for_rates_int <- rep(base_for_rates_int, length(offset_costs_chr))
        }
        data_tb <- 1:length(offsets_chr) %>% purrr::reduce(.init = data_tb, 
            ~{
                rate_1L_chr <- paste0("ParamPool", offsets_chr[.y])
                mean_1L_chr <- paste0("Param", offsets_chr[.y], 
                  "OOSCost_mean")
                sd_1L_chr <- paste0("Param", offsets_chr[.y], 
                  "OOSCost_sd")
                unit_1L_chr <- paste0(stringr::str_remove(mean_1L_chr, 
                  "_mean"), "Unit")
                rates_tb <- .x %>% dplyr::group_by(Iteration) %>% 
                  dplyr::summarise(NumberOfAgents = dplyr::n(), 
                    `:=`(!!rlang::sym(rate_1L_chr), dplyr::first(!!rlang::sym(rate_1L_chr))), 
                    `:=`(!!rlang::sym(mean_1L_chr), dplyr::first(!!rlang::sym(mean_1L_chr))), 
                    `:=`(!!rlang::sym(sd_1L_chr), dplyr::first(!!rlang::sym(sd_1L_chr)))) %>% 
                  dplyr::mutate(`:=`(!!rlang::sym(paste0(rate_1L_chr, 
                    "Rate")), abs(!!rlang::sym(rate_1L_chr))/base_for_rates_int[.y]), 
                    `:=`(!!rlang::sym(paste0(rate_1L_chr, "Multiplier")), 
                      !!rlang::sym(rate_1L_chr) %>% purrr::map_int(~ifelse(.x < 
                        0, -1, 1))), `:=`(!!rlang::sym(unit_1L_chr), 
                      !!rlang::sym(mean_1L_chr)))
                count_1L_chr <- offset_counts_chr[.y]
                cost_1L_chr <- offset_costs_chr[.y]
                iterations_int <- .x$Iteration %>% unique()
                join_tb <- purrr::map_dfr(1:nrow(rates_tb), ~tibble::tibble(Iteration = iterations_int[.x], 
                  UID = data_tb$UID %>% unique(), `:=`(!!rlang::sym(paste0(count_1L_chr, 
                    "_new")), rpois(n = rates_tb$NumberOfAgents[.x], 
                    lambda = rates_tb[[.x, paste0(rate_1L_chr, 
                      "Rate")]]) * rates_tb[[.x, paste0(rate_1L_chr, 
                    "Multiplier")]])) %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(cost_1L_chr, 
                  "_new")), !!rlang::sym(paste0(count_1L_chr, 
                  "_new")) * rates_tb[[.x, unit_1L_chr]])))
                joined_tb <- .x %>% dplyr::left_join(join_tb) %>% 
                  dplyr::mutate(`:=`(!!rlang::sym(count_1L_chr), 
                    !!rlang::sym(count_1L_chr) + !!rlang::sym(paste0(count_1L_chr, 
                      "_new"))), `:=`(!!rlang::sym(cost_1L_chr), 
                    !!rlang::sym(cost_1L_chr) + !!rlang::sym(paste0(cost_1L_chr, 
                      "_new")))) %>% dplyr::select(-tidyselect::all_of(c(paste0(count_1L_chr, 
                  "_new"), paste0(cost_1L_chr, "_new"))))
                1:length(scenarios_chr) %>% purrr::reduce(.init = joined_tb, 
                  ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(cost_names_chr[.y]), 
                    !!rlang::sym(cost_names_chr[.y]) + !!rlang::sym(cost_1L_chr))))
            })
    }
    return(data_tb)
}
#' Add cost effectiveness statistics
#' @description add_cost_effectiveness_stats() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cost effectiveness statistics. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @return Data (a tibble)
#' @rdname add_cost_effectiveness_stats
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr map2_dbl
#' @keywords internal
add_cost_effectiveness_stats <- function (data_tb, threshold_1L_dbl = 96000) 
{
    data_tb <- data_tb %>% dplyr::mutate(Dominance_AQoL6D = Cost %>% 
        purrr::map2_chr(AQoL6D_QALYs_YR1, ~ifelse(.x > 0 & .y < 
            0, "Dominated", ifelse(.x < 0 & .y > 0, "Dominant", 
            "Ratio"))), Dominance_AQoL6D_S10 = Cost_S1 %>% purrr::map2_chr(AQoL6D_QALYs_YR1, 
        ~ifelse(.x > 0 & .y < 0, "Dominated", ifelse(.x < 0 & 
            .y > 0, "Dominant", "Ratio"))), Dominance_AQoL6D_S01 = Cost %>% 
        purrr::map2_chr(AQoL6D_QALYs_YR1_S1, ~ifelse(.x > 0 & 
            .y < 0, "Dominated", ifelse(.x < 0 & .y > 0, "Dominant", 
            "Ratio"))), Dominance_AQoL6D_S02 = Cost %>% purrr::map2_chr(AQoL6D_QALYs_YR1_S2, 
        ~ifelse(.x > 0 & .y < 0, "Dominated", ifelse(.x < 0 & 
            .y > 0, "Dominant", "Ratio"))), Dominance_AQoL6D_S11 = Cost_S1 %>% 
        purrr::map2_chr(AQoL6D_QALYs_YR1_S1, ~ifelse(.x > 0 & 
            .y < 0, "Dominated", ifelse(.x < 0 & .y > 0, "Dominant", 
            "Ratio"))), Dominance_AQoL6D_S12 = Cost_S1 %>% purrr::map2_chr(AQoL6D_QALYs_YR1_S2, 
        ~ifelse(.x > 0 & .y < 0, "Dominated", ifelse(.x < 0 & 
            .y > 0, "Dominant", "Ratio"))), ICER_AQoL6D = Cost/AQoL6D_QALYs_YR1, 
        ICER_AQoL6D_S10 = Cost_S1/AQoL6D_QALYs_YR1, ICER_AQoL6D_S01 = Cost/AQoL6D_QALYs_YR1_S1, 
        ICER_AQoL6D_S02 = Cost/AQoL6D_QALYs_YR1_S2, ICER_AQoL6D_S11 = Cost_S1/AQoL6D_QALYs_YR1_S1, 
        ICER_AQoL6D_S12 = Cost_S1/AQoL6D_QALYs_YR1_S2, CE_AQoL6D = ICER_AQoL6D %>% 
            purrr::map2_dbl(Dominance_AQoL6D, ~ifelse(.y == "Ratio", 
                ifelse(.x <= threshold_1L_dbl, 1, 0), ifelse(.y == 
                  "Dominant", 1, 0))), CE_AQoL6D_S10 = ICER_AQoL6D_S10 %>% 
            purrr::map2_dbl(Dominance_AQoL6D_S10, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_AQoL6D_S01 = ICER_AQoL6D_S01 %>% 
            purrr::map2_dbl(Dominance_AQoL6D_S01, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_AQoL6D_S02 = ICER_AQoL6D_S02 %>% 
            purrr::map2_dbl(Dominance_AQoL6D_S02, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_AQoL6D_S11 = ICER_AQoL6D_S11 %>% 
            purrr::map2_dbl(Dominance_AQoL6D_S11, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_AQoL6D_S12 = ICER_AQoL6D_S12 %>% 
            purrr::map2_dbl(Dominance_AQoL6D_S12, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), Dominance_CHU9D = Cost %>% 
            purrr::map2_chr(CHU9D_QALYs_YR1, ~ifelse(.x > 0 & 
                .y < 0, "Dominated", ifelse(.x < 0 & .y > 0, 
                "Dominant", "Ratio"))), Dominance_CHU9D_S10 = Cost_S1 %>% 
            purrr::map2_chr(CHU9D_QALYs_YR1, ~ifelse(.x > 0 & 
                .y < 0, "Dominated", ifelse(.x < 0 & .y > 0, 
                "Dominant", "Ratio"))), Dominance_CHU9D_S01 = Cost %>% 
            purrr::map2_chr(CHU9D_QALYs_YR1_S1, ~ifelse(.x > 
                0 & .y < 0, "Dominated", ifelse(.x < 0 & .y > 
                0, "Dominant", "Ratio"))), Dominance_CHU9D_S02 = Cost %>% 
            purrr::map2_chr(CHU9D_QALYs_YR1_S2, ~ifelse(.x > 
                0 & .y < 0, "Dominated", ifelse(.x < 0 & .y > 
                0, "Dominant", "Ratio"))), Dominance_CHU9D_S11 = Cost_S1 %>% 
            purrr::map2_chr(CHU9D_QALYs_YR1_S1, ~ifelse(.x > 
                0 & .y < 0, "Dominated", ifelse(.x < 0 & .y > 
                0, "Dominant", "Ratio"))), Dominance_CHU9D_S12 = Cost_S1 %>% 
            purrr::map2_chr(CHU9D_QALYs_YR1_S2, ~ifelse(.x > 
                0 & .y < 0, "Dominated", ifelse(.x < 0 & .y > 
                0, "Dominant", "Ratio"))), ICER_CHU9D = Cost/CHU9D_QALYs_YR1, 
        ICER_CHU9D_S10 = Cost_S1/CHU9D_QALYs_YR1, ICER_CHU9D_S01 = Cost/CHU9D_QALYs_YR1_S1, 
        ICER_CHU9D_S02 = Cost/CHU9D_QALYs_YR1_S2, ICER_CHU9D_S11 = Cost_S1/CHU9D_QALYs_YR1_S1, 
        ICER_CHU9D_S12 = Cost_S1/CHU9D_QALYs_YR1_S2, CE_CHU9D = ICER_CHU9D %>% 
            purrr::map2_dbl(Dominance_CHU9D, ~ifelse(.y == "Ratio", 
                ifelse(.x <= threshold_1L_dbl, 1, 0), ifelse(.y == 
                  "Dominant", 1, 0))), CE_CHU9D_S10 = ICER_CHU9D_S10 %>% 
            purrr::map2_dbl(Dominance_CHU9D_S10, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_CHU9D_S01 = ICER_CHU9D_S01 %>% 
            purrr::map2_dbl(Dominance_CHU9D_S01, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_CHU9D_S02 = ICER_CHU9D_S02 %>% 
            purrr::map2_dbl(Dominance_CHU9D_S02, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_CHU9D_S11 = ICER_CHU9D_S11 %>% 
            purrr::map2_dbl(Dominance_CHU9D_S11, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))), CE_CHU9D_S12 = ICER_CHU9D_S12 %>% 
            purrr::map2_dbl(Dominance_CHU9D_S12, ~ifelse(.y == 
                "Ratio", ifelse(.x <= threshold_1L_dbl, 1, 0), 
                ifelse(.y == "Dominant", 1, 0))))
    return(data_tb)
}
#' Add costs event
#' @description add_costs_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add costs event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param inputs_ls Inputs (a list)
#' @param add_offsets_1L_lgl Add offsets (a logical vector of length one), Default: FALSE
#' @param base_for_rates_int Base for rates (an integer vector), Default: 1
#' @param offsets_chr Offsets (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("variable", "fixed", "both", "zero")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_costs_event
#' @export 
#' @keywords internal
add_costs_event <- function (X_Ready4useDyad, inputs_ls, add_offsets_1L_lgl = FALSE, 
    base_for_rates_int = 1L, offsets_chr = character(0), type_1L_chr = c("variable", 
        "fixed", "both", "zero")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "zero") {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% add_cost_calculations(inputs_ls = inputs_ls, 
                add_fixed_1L_lgl = F, add_offsets_1L_lgl = add_offsets_1L_lgl, 
                add_variable_1L_lgl = F, base_for_rates_int = base_for_rates_int, 
                offsets_chr = offsets_chr))
    }
    else {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% add_cost_calculations(inputs_ls = inputs_ls, 
                add_fixed_1L_lgl = (type_1L_chr %in% c("fixed", 
                  "both")), add_variable_1L_lgl = (type_1L_chr %in% 
                  c("variable", "both")), add_offsets_1L_lgl = add_offsets_1L_lgl, 
                base_for_rates_int = base_for_rates_int, offsets_chr = offsets_chr))
    }
    return(X_Ready4useDyad)
}
#' Add costs to summary
#' @description add_costs_to_summary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add costs to summary. The function returns Summaries (a list).
#' @param summaries_ls Summaries (a list)
#' @param processed_ls Processed (a list)
#' @param periods_1L_int Periods (an integer vector of length one), Default: 26
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: character(0)
#' @param reference_1L_chr Reference (a character vector of length one), Default: 'Status quo'
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param type_1L_chr Type (a character vector of length one), Default: c("change_scenario", "change_sq", "scenario", "sq")
#' @return Summaries (a list)
#' @rdname add_costs_to_summary
#' @export 
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter mutate across where arrange pull
#' @importFrom ready4 get_from_lup_obj
#' @importFrom rlang sym
#' @keywords internal
add_costs_to_summary <- function (summaries_ls, processed_ls, periods_1L_int = 26, arrange_1L_chr = character(0), 
    reference_1L_chr = "Status quo", tfmn_fn = identity, type_1L_chr = c("change_scenario", 
        "change_sq", "scenario", "sq")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "sq") {
        summaries_ls$scenarios_ls$costs_sq_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            make_forecast_growth(summaries_ls$scenarios_ls$clients_tb %>% 
                dplyr::filter(Scenario == reference_1L_chr), 
                reference_1L_dbl = summaries_ls$empirical_ls$onboarded_1L_dbl/periods_1L_int + 
                  summaries_ls$empirical_ls$retained_1L_dbl, 
                tfmn_fn = identity) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~processed_ls$costs_unit@ds_tb %>% dplyr::filter(Scenario == 
                  cost_scenario_1L_chr) %>% ready4::get_from_lup_obj(match_var_nm_1L_chr = "Type", 
                  match_value_xx = "Variable", target_var_nm_1L_chr = "TotalCost") * 
                  .x)) %>% dplyr::mutate(Scenario = paste0(Scenario, 
                ", ", cost_scenario_1L_chr))
        })
        if (!identical(arrange_1L_chr, character(0))) {
            summaries_ls$scenarios_ls$costs_sq_tb <- summaries_ls$scenarios_ls$costs_sq_tb %>% 
                dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        summaries_ls$scenarios_ls$costs_sq_tb <- summaries_ls$scenarios_ls$costs_sq_tb %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~tfmn_fn(.x)))
    }
    if (type_1L_chr == "change_sq") {
        unformatted_ls <- add_costs_to_summary(summaries_ls, 
            processed_ls = processed_ls, periods_1L_int = periods_1L_int, 
            reference_1L_chr = reference_1L_chr, type_1L_chr = "sq")
        summaries_ls$scenarios_ls$costs_change_sq_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            total_cost_1L_dbl <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == cost_scenario_1L_chr) %>% 
                dplyr::pull(TotalCost) %>% sum()
            unformatted_ls$scenarios_ls$costs_sq_tb %>% dplyr::filter(endsWith(Scenario, 
                paste0(", ", cost_scenario_1L_chr))) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~.x/total_cost_1L_dbl))
        })
        if (!identical(arrange_1L_chr, character(0))) {
            summaries_ls$scenarios_ls$costs_change_sq_tb <- summaries_ls$scenarios_ls$costs_change_sq_tb %>% 
                dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        summaries_ls$scenarios_ls$costs_change_sq_tb <- summaries_ls$scenarios_ls$costs_change_sq_tb %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~tfmn_fn(.x)))
    }
    if (type_1L_chr == "scenario") {
        summaries_ls$scenarios_ls$costs_scenarios_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            make_forecast_growth(summaries_ls$scenarios_ls$clients_tb, 
                reference_1L_chr = reference_1L_chr, tfmn_fn = identity) %>% 
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                  ~processed_ls$costs_unit@ds_tb %>% dplyr::filter(Scenario == 
                    cost_scenario_1L_chr) %>% ready4::get_from_lup_obj(match_var_nm_1L_chr = "Type", 
                    match_value_xx = "Variable", target_var_nm_1L_chr = "TotalCost") * 
                    .x)) %>% dplyr::mutate(Scenario = paste0(Scenario, 
                ", ", cost_scenario_1L_chr))
        })
        if (!identical(arrange_1L_chr, character(0))) {
            summaries_ls$scenarios_ls$costs_scenarios_tb <- summaries_ls$scenarios_ls$costs_scenarios_tb %>% 
                dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        summaries_ls$scenarios_ls$costs_scenarios_tb <- summaries_ls$scenarios_ls$costs_scenarios_tb %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~tfmn_fn(.x)))
    }
    if (type_1L_chr == "change_scenario") {
        unformatted_ls <- add_costs_to_summary(summaries_ls, 
            processed_ls = processed_ls, periods_1L_int = periods_1L_int, 
            reference_1L_chr = reference_1L_chr, type_1L_chr = "scenario")
        summaries_ls$scenarios_ls$costs_change_scenarios_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            total_cost_1L_dbl <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == cost_scenario_1L_chr) %>% 
                dplyr::pull(TotalCost) %>% sum()
            unformatted_ls$scenarios_ls$costs_scenarios_tb %>% 
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                  ~.x/total_cost_1L_dbl))
        })
        if (!identical(arrange_1L_chr, character(0))) {
            summaries_ls$scenarios_ls$costs_change_scenarios_tb <- summaries_ls$scenarios_ls$costs_change_scenarios_tb %>% 
                dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        summaries_ls$scenarios_ls$costs_change_scenarios_tb <- summaries_ls$scenarios_ls$costs_change_scenarios_tb %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~tfmn_fn(.x)))
    }
    return(summaries_ls)
}
#' Add enter model event
#' @description add_enter_model_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add enter model event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arm_1L_chr Arm (a character vector of length one)
#' @param draws_tb Draws (a tibble)
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: NULL
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_enter_model_event
#' @export 
#' @importFrom lubridate years weeks NA_Date_
#' @importFrom dplyr mutate select everything inner_join
#' @importFrom purrr map_dfr
#' @keywords internal
add_enter_model_event <- function (X_Ready4useDyad, arm_1L_chr, draws_tb, horizon_dtm = lubridate::years(1), 
    iterations_int = 1:100L, modifiable_chr = character(0), start_dtm = Sys.Date(), 
    tfmn_ls = NULL, tx_duration_dtm = lubridate::weeks(12)) 
{
    X_Ready4useDyad <- X_Ready4useDyad %>% update_population_classes(tfmn_ls = tfmn_ls)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(Arm = arm_1L_chr, InModel = T, , Iteration = NA_integer_, 
            StartDate = start_dtm, EndDate = start_dtm + horizon_dtm, 
            CurrentDate = start_dtm, CurrentEvent = "EnterModel", 
            NextEvent = NA_character_, ScheduledFor = lubridate::NA_Date_) %>% 
        dplyr::select(Iteration, UID, InModel, Arm, StartDate, 
            CurrentDate, EndDate, CurrentEvent, NextEvent, ScheduledFor, 
            dplyr::everything()))
    X_Ready4useDyad <- update_tx_start_end(X_Ready4useDyad, tx_duration_dtm = tx_duration_dtm)
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = modifiable_chr, 
        pattern_1L_chr = "{col}_start")
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = modifiable_chr)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", iterations_int %>% 
        purrr::map_dfr(~X_Ready4useDyad@ds_tb %>% dplyr::mutate(Iteration = .x)))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::inner_join(draws_tb))
    return(X_Ready4useDyad)
}
#' Add imputed data
#' @description add_imputed_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add imputed data. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param add_cumulatives_1L_lgl Add cumulatives (a logical vector of length one), Default: FALSE
#' @param characteristics_chr Characteristics (a character vector), Default: c("platform", "clinic_type", "gender", "employment_status", "clinic_state", 
#'    "clinic_postcode", "role_type", "treatment_stage")
#' @param extras_chr Extras (a character vector), Default: character(0)
#' @param ignore_x_chr Ignore x (a character vector), Default: character(0)
#' @param ignore_y_chr Ignore y (a character vector), Default: character(0)
#' @param impute_age_1L_lgl Impute age (a logical vector of length one), Default: FALSE
#' @param method_1L_chr Method (a character vector of length one), Default: 'rf'
#' @param treatment_status_1L_int Treatment status (an integer vector of length one), Default: 0
#' @param x_is_z_1L_lgl X is z (a logical vector of length one), Default: FALSE
#' @return Z (A dataset and data dictionary pair.)
#' @rdname add_imputed_data
#' @export 
#' @importFrom ready4use Ready4useDyad add_from_lup_prototype
#' @importFrom dplyr select mutate across arrange case_when filter pull left_join
#' @importFrom tidyselect all_of
#' @importFrom mice mice complete
#' @importFrom tidyr any_of
#' @importFrom tibble as_tibble
#' @importFrom serious add_cumulatives
#' @importFrom stringr str_remove
#' @keywords internal
add_imputed_data <- function (X_Ready4useDyad, Y_Ready4useDyad = ready4use::Ready4useDyad(), 
    add_cumulatives_1L_lgl = FALSE, characteristics_chr = c("platform", 
        "clinic_type", "gender", "employment_status", "clinic_state", 
        "clinic_postcode", "role_type", "treatment_stage"), extras_chr = character(0), 
    ignore_x_chr = character(0), ignore_y_chr = character(0), 
    impute_age_1L_lgl = FALSE, method_1L_chr = "rf", treatment_status_1L_int = 0L, 
    x_is_z_1L_lgl = FALSE) 
{
    if (!identical(ignore_x_chr, character(0))) {
        A_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::select(tidyselect::all_of(c("UID", 
                "Date", ignore_x_chr))))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::select(-tidyselect::all_of(ignore_x_chr)))
    }
    if (x_is_z_1L_lgl) {
        Z_Ready4useDyad <- X_Ready4useDyad
    }
    else {
        imputed_1_xx <- mice::mice(X_Ready4useDyad@ds_tb %>% 
            dplyr::mutate(dplyr::across(tidyr::any_of(c(characteristics_chr, 
                extras_chr)), ~as.factor(.x))), method = method_1L_chr, 
            m = 1, maxit = 1)
        Z_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            mice::complete(imputed_1_xx) %>% tibble::as_tibble() %>% 
                dplyr::mutate(dplyr::across(tidyr::any_of(c(characteristics_chr, 
                  extras_chr)), ~as.character(.x))))
    }
    if (!identical(Y_Ready4useDyad, ready4use::Ready4useDyad())) {
        if (!identical(ignore_y_chr, character(0))) {
            B_Ready4useDyad <- Y_Ready4useDyad
            Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                Y_Ready4useDyad@ds_tb %>% dplyr::select(-tidyselect::all_of(ignore_y_chr)))
        }
        Z_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            ready4use::add_from_lup_prototype(Y_Ready4useDyad@ds_tb, 
                lup_prototype_tb = Z_Ready4useDyad@ds_tb, match_var_nm_1L_chr = "UID", 
                type_1L_chr = c("self"), vars_chr = intersect(characteristics_chr, 
                  names(X_Ready4useDyad@ds_tb))) %>% dplyr::arrange(UID, 
                Date))
    }
    if (!identical(extras_chr, character(0))) {
        imputed_2_xx <- mice::mice(Z_Ready4useDyad@ds_tb %>% 
            dplyr::mutate(dplyr::across(tidyr::any_of(extras_chr), 
                ~as.factor(.x))), method = method_1L_chr, m = 1, 
            maxit = 1)
        Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
            mice::complete(imputed_2_xx) %>% tibble::as_tibble() %>% 
                dplyr::mutate(dplyr::across(tidyr::any_of(extras_chr), 
                  ~as.character(.x))))
    }
    if (impute_age_1L_lgl) {
        Z_Ready4useDyad@ds_tb <- Z_Ready4useDyad@ds_tb %>% dplyr::mutate(Age = dplyr::case_when(is.na(Age) & 
            platform == "over_15" ~ round(mean(Z_Ready4useDyad@ds_tb %>% 
            dplyr::filter(platform == "over_15") %>% dplyr::pull(Age), 
            na.rm = T), 0), is.na(Age) & platform == "under_15" ~ 
            round(mean(Z_Ready4useDyad@ds_tb %>% dplyr::filter(platform == 
                "under_15") %>% dplyr::pull(Age), na.rm = T), 
                0), TRUE ~ Age))
    }
    if (treatment_status_1L_int %in% 1:2) {
        Z_Ready4useDyad <- Z_Ready4useDyad %>% add_treatment_status(type_1L_int = treatment_status_1L_int)
    }
    if (!identical(ignore_x_chr, character(0))) {
        Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
            Z_Ready4useDyad@ds_tb %>% dplyr::left_join(A_Ready4useDyad@ds_tb))
    }
    if (!identical(ignore_y_chr, character(0))) {
        Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
            Z_Ready4useDyad@ds_tb %>% dplyr::left_join(B_Ready4useDyad@ds_tb))
    }
    if (add_cumulatives_1L_lgl) {
        cumulatives_chr <- setdiff(names(Z_Ready4useDyad@ds_tb)[startsWith(names(Z_Ready4useDyad@ds_tb), 
            "Cumulative")], c(ignore_x_chr, ignore_y_chr))
        Z_Ready4useDyad <- serious::add_cumulatives(Z_Ready4useDyad, 
            metrics_chr = cumulatives_chr %>% stringr::str_remove("Cumulative"), 
            group_by_1L_chr = "UID")
    }
    return(Z_Ready4useDyad)
}
#' Add joiners outcomes dataset
#' @description add_joiners_outcomes_ds() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add joiners outcomes dataset. The function returns Model data (a list).
#' @param model_data_ls Model data (a list)
#' @param keys_chr Keys (a character vector), Default: c("platform", "clinic_type", "Age", "gender", "employment_status", 
#'    "clinic_state", "treatment_stage")
#' @param outcomes_chr Outcomes (a character vector), Default: c("treatment_status", "gad7", "k10", "phq9", "AQoL6D", "CHU9D")
#' @return Model data (a list)
#' @rdname add_joiners_outcomes_ds
#' @export 
#' @importFrom dplyr filter select left_join
#' @importFrom tidyselect any_of
#' @keywords internal
add_joiners_outcomes_ds <- function (model_data_ls, keys_chr = c("platform", "clinic_type", 
    "Age", "gender", "employment_status", "clinic_state", "treatment_stage"), 
    outcomes_chr = c("treatment_status", "gad7", "k10", "phq9", 
        "AQoL6D", "CHU9D")) 
{
    X_Ready4useDyad <- model_data_ls$imputed_ls$Joiners_r4
    Y_Ready4useDyad <- model_data_ls$unimputed_ls$Outcomes0To12Wide_r4
    model_data_ls$imputed_ls$OutcomesJoiners_r4 <- renewSlot(X_Ready4useDyad, 
        "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::filter(Date >= 
            min(Y_Ready4useDyad@ds_tb$Date) & Date <= max(Y_Ready4useDyad@ds_tb$Date)) %>% 
            dplyr::select(tidyselect::any_of(c("UID", keys_chr))) %>% 
            dplyr::left_join(Y_Ready4useDyad@ds_tb %>% dplyr::select(tidyselect::any_of(c("UID", 
                outcomes_chr)))) %>% dplyr::select(-"UID")) %>% 
        add_treatment_status(three_levels_1L_lgl = T, update_dict_1L_lgl = F, 
            type_1L_int = 1L)
    return(model_data_ls)
}
#' Add K10 event
#' @description add_k10_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add k10 event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: 0
#' @param k10_mdl K10 (a model), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param params_tb Parameters (a tibble), Default: make_project_params_tb()
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project", "Table")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_k10_event
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_k10_event <- function (X_Ready4useDyad, adjustment_1L_dbl = 0, k10_mdl = NULL, 
    iterations_int = 1:100L, params_tb = make_project_params_tb(), 
    sensitivities_ls = make_sensitivities_ls(), suffix_1L_chr = character(0), 
    tfmn_ls = make_class_tfmns(T), type_1L_chr = c("Model", "Project", 
        "Table")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (identical(suffix_1L_chr, character(0)) & type_1L_chr != 
        "Project") {
        suffix_1L_chr <- make_weeks_suffix(X_Ready4useDyad, adjustment_1L_dbl = adjustment_1L_dbl)
    }
    if (!"Minutes" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(Minutes = 0))
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0("Minutes", suffix_1L_chr)), 
            Minutes)))
    if (!"k10_change" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(k10_change = 0))
    }
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = c("k10", 
        "k10_change"))
    var_1L_chr <- paste0("k10", suffix_1L_chr)
    X_Ready4useDyad <- add_k10_scores(X_Ready4useDyad, k10_mdl = k10_mdl, 
        join_with_chr = c("Iteration"), k10_var_1L_chr = "k10", 
        iterations_int = iterations_int, params_tb = params_tb, 
        sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
        type_1L_chr = type_1L_chr, var_1L_chr = var_1L_chr, what_1L_chr = "new")
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(k10 = !!rlang::sym(var_1L_chr)))
    return(X_Ready4useDyad)
}
#' Add K10 scores
#' @description add_k10_scores() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add k10 scores. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param k10_mdl K10 (a model), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'k10'
#' @param params_tb Parameters (a tibble), Default: make_project_params_tb()
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project", "Table")
#' @param var_1L_chr Variable (a character vector of length one), Default: 'k10_12_Weeks'
#' @param what_1L_chr What (a character vector of length one), Default: c("old", "new")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_k10_scores
#' @export 
#' @importFrom dplyr select where mutate filter across case_when arrange
#' @importFrom purrr map map2_dfr reduce map_int
#' @importFrom tidyselect all_of any_of
#' @importFrom stats setNames
#' @importFrom rlang sym
#' @importFrom youthvars youthvars_k10_aus
#' @keywords internal
add_k10_scores <- function (X_Ready4useDyad, k10_mdl = NULL, iterations_int = 1:100L, 
    join_with_chr = character(0), k10_var_1L_chr = "k10", params_tb = make_project_params_tb(), 
    sensitivities_ls = make_sensitivities_ls(), tfmn_ls = make_class_tfmns(T), 
    type_1L_chr = c("Model", "Project", "Table"), var_1L_chr = "k10_12_Weeks", 
    what_1L_chr = c("old", "new")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    k10_vars_chr <- X_Ready4useDyad@ds_tb %>% dplyr::select(dplyr::where(~inherits(.x, 
        "youthvars_k10_aus"))) %>% names()
    if (type_1L_chr == "Model") {
        X_Ready4useDyad <- add_simulated_data(k10_mdl, var_1L_chr = var_1L_chr, 
            Y_Ready4useDyad = X_Ready4useDyad, iterations_int = iterations_int, 
            join_with_chr = join_with_chr, type_1L_chr = "third", 
            what_1L_chr = what_1L_chr)
    }
    if (type_1L_chr == "Project") {
        X_Ready4useDyad <- add_outcome_sensitivity(X_Ready4useDyad, 
            outcome_1L_chr = k10_var_1L_chr, sensitivities_ls = sensitivities_ls, 
            tfmn_fn = tfmn_ls$K10)
    }
    if (type_1L_chr == "Table") {
        if (!"k10_part" %in% names(X_Ready4useDyad@ds_tb)) {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(k10_part = 0))
        }
        severity_ls <- make_k10_severity_cuts()
        k10_ls <- iterations_int %>% purrr::map(~{
            data_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(Iteration == 
                .x) %>% dplyr::mutate(dplyr::across(tidyselect::all_of(k10_vars_chr), 
                ~as.integer(.x)))
            quantiles_dbl <- round(quantile(data_tb$k10, probs = c(0.2, 
                0.4, 0.6, 0.8)), 0) %>% unname()
            rtm_ls <- list(c(10, quantiles_dbl[1]), c(quantiles_dbl[1] + 
                1, quantiles_dbl[2]), c(quantiles_dbl[2] + 1, 
                quantiles_dbl[3]), c(quantiles_dbl[3] + 1, quantiles_dbl[4]), 
                c(quantiles_dbl[4] + 1, 50)) %>% stats::setNames(paste0("ParamRTM_Q", 
                1:5))
            data_tb <- severity_ls %>% purrr::map2_dfr(names(severity_ls), 
                ~{
                  cut_tb <- data_tb %>% dplyr::filter(as.integer(k10) >= 
                    .x[1] & as.integer(k10) <= .x[2])
                  if (nrow(cut_tb) > 0) {
                    cut_tb <- dplyr::mutate(cut_tb, Improvement = dplyr::case_when(as.character(clinic_type) == 
                      "headspace" ~ rnorm(nrow(cut_tb), mean = cut_tb[[1, 
                      paste0("ParamK10ChangeHeadspace", .y, "_mean")]], 
                      sd = cut_tb[[1, paste0("ParamK10ChangeHeadspace", 
                        .y, "_sd")]]), as.character(clinic_type) == 
                      "Specialist Services" & as.character(gender) == 
                      "Female" ~ rnorm(nrow(cut_tb), mean = cut_tb[[1, 
                      "ParamK10ChangeSpecialistFemale_mean"]], 
                      sd = cut_tb[[1, "ParamK10ChangeSpecialistFemale_sd"]]), 
                      as.character(clinic_type) == "Specialist Services" & 
                        as.character(gender) == "Male" ~ rnorm(nrow(cut_tb), 
                        mean = cut_tb[[1, "ParamK10ChangeSpecialistMale_mean"]], 
                        sd = cut_tb[[1, "ParamK10ChangeSpecialistMale_sd"]]), 
                      T ~ rnorm(nrow(cut_tb), mean = cut_tb[[1, 
                        "ParamK10ChangeSpecialistAll_mean"]], 
                        sd = cut_tb[[1, "ParamK10ChangeSpecialistAll_sd"]])))
                    cut_tb <- cut_tb %>% dplyr::mutate(k10_part = dplyr::case_when(!is.na(treatment_start) & 
                      (treatment_measurement > CurrentDate) ~ 
                      k10_part + 1, !is.na(treatment_start) & 
                      (treatment_measurement <= CurrentDate) & 
                      floor(k10_part) == 1 ~ k10_part + 1, !is.na(treatment_start) & 
                      (treatment_measurement <= CurrentDate) & 
                      floor(k10_part) != 1 ~ k10_part + 2, T ~ 
                      k10_part + 0.001)) %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                      dplyr::case_when((!is.na(treatment_start) & 
                        floor(k10_part) == 2) ~ as.integer(round((as.double(k10) - 
                        Improvement * treatment_fraction), 0)), 
                        T ~ as.integer(k10)))) %>% dplyr::select(-Improvement)
                  }
                  cut_tb
                })
            data_tb <- rtm_ls %>% purrr::map2_dfr(names(rtm_ls), 
                ~{
                  cut_tb <- data_tb %>% dplyr::filter(k10 >= 
                    .x[1] & k10 <= .x[2])
                  if (nrow(cut_tb) > 0) {
                    cut_tb <- dplyr::mutate(cut_tb, RTM = dplyr::case_when((as.character(clinic_type) == 
                      "headspace" & floor(k10_part) %in% 1:2) | 
                      ((k10_part - floor(k10_part)) <= 0.001) ~ 
                      rnorm(nrow(cut_tb), mean = cut_tb[[1, paste0(.y, 
                        "_mean")]], sd = cut_tb[[1, paste0(.y, 
                        "_sd")]]), T ~ 0))
                    cut_tb <- cut_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                      as.integer(round(!!rlang::sym(var_1L_chr) + 
                        RTM, 0)))) %>% dplyr::select(-RTM)
                  }
                  cut_tb
                })
            data_tb %>% dplyr::arrange(UID)
        })
        X_Ready4useDyad@ds_tb <- k10_ls %>% purrr::reduce(.init = k10_ls[[1]] %>% 
            dplyr::filter(F), ~rbind(.x, .y)) %>% dplyr::arrange(Iteration, 
            UID)
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(dplyr::across(tidyselect::any_of(c(k10_vars_chr, 
            var_1L_chr)), ~round(.x, 0) %>% as.integer() %>% 
            purrr::map_int(~min(max(.x, 10), 50)) %>% youthvars::youthvars_k10_aus()), 
            k10_change = as.integer(!!rlang::sym(var_1L_chr) - 
                as.integer(!!rlang::sym(k10_var_1L_chr)))))
    return(X_Ready4useDyad)
}
#' Add leave model event
#' @description add_leave_model_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add leave model event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_leave_model_event
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_leave_model_event <- function (X_Ready4useDyad) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(InModel = F))
    return(X_Ready4useDyad)
}
#' Add minutes
#' @description add_minutes() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add minutes. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param end_dtm End (a date vector), Default: NULL
#' @param period_dtm Period (a date vector), Default: lubridate::years(1)
#' @param start_at_1L_int Start at (an integer vector of length one), Default: -2L
#' @param weeks_dbl Weeks (a double vector), Default: c(14, 53)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_minutes
#' @export 
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom lubridate years weeks
#' @importFrom dplyr group_by mutate filter summarise first select everything arrange across case_when lag left_join right_join distinct
#' @importFrom purrr map_dfr reduce
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @importFrom tidyr pivot_wider all_of
#' @importFrom stringr str_remove_all str_remove
#' @importFrom assertthat assert_that
#' @keywords internal
add_minutes <- function (X_Ready4useDyad = ready4use::Ready4useDyad(), Y_Ready4useDyad, 
    end_dtm = NULL, period_dtm = lubridate::years(1), start_at_1L_int = -2L, 
    weeks_dbl = c(14, 53)) 
{
    if (identical(X_Ready4useDyad, ready4use::Ready4useDyad())) {
        X_Ready4useDyad <- Y_Ready4useDyad
        cutoffs_tb <- X_Ready4useDyad@ds_tb
        if (is.null(end_dtm)) {
            end_dtm <- max(cutoffs_tb$Date)
        }
        start_by_dtm <- end_dtm - period_dtm
        cutoffs_tb <- cutoffs_tb %>% dplyr::group_by(UID) %>% 
            dplyr::mutate(Start = min(Date)) %>% dplyr::filter(Start <= 
            start_by_dtm, Date <= Start + period_dtm)
        weeks_dbl <- sort(weeks_dbl)
        minutes_tb <- weeks_dbl %>% purrr::map_dfr(~{
            cutoff_dtm <- lubridate::weeks(.x)
            cutoffs_tb %>% dplyr::filter(Date <= Start + cutoff_dtm) %>% 
                dplyr::summarise(UID = dplyr::first(UID), LastObservation = max(Date), 
                  Start = dplyr::first(Start), MeasurementWeek = paste0("Week", 
                    .x), direct_mins = sum(direct_mins, na.rm = T), 
                  indirect_mins = sum(indirect_mins, na.rm = T), 
                  Minutes = sum(Minutes, na.rm = T), .groups = "drop") %>% 
                dplyr::mutate(End = Start + cutoff_dtm) %>% dplyr::select(UID, 
                Start, End, LastObservation, MeasurementWeek, 
                dplyr::everything())
        }) %>% dplyr::arrange(UID, MeasurementWeek) %>% dplyr::mutate(MeasurementWeek = as.factor(MeasurementWeek))
        minutes_tb <- minutes_tb %>% dplyr::mutate(dplyr::across(c(direct_mins, 
            indirect_mins, Minutes), .names = "{.col}_change", 
            ~dplyr::case_when(MeasurementWeek != paste0("Week", 
                weeks_dbl[1]) ~ .x - dplyr::lag(.x), T ~ .x)))
        minutes_tb <- c("direct_mins", "indirect_mins", "Minutes") %>% 
            purrr::reduce(.init = minutes_tb, ~{
                .x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, 
                  "_start")), !!rlang::sym(.y) - !!rlang::sym(paste0(.y, 
                  "_change"))))
            })
        X_Ready4useDyad@ds_tb <- minutes_tb %>% dplyr::mutate(Date = LastObservation) %>% 
            dplyr::left_join(X_Ready4useDyad@ds_tb %>% dplyr::group_by(UID) %>% 
                dplyr::summarise(dplyr::across(tidyselect::any_of(c("treatment_stage", 
                  "treatment_status", "platform", "clinic_type", 
                  "Age", "gender", "employment_status", "clinic_state", 
                  "clinic_postcode")), ~dplyr::first(.x))))
    }
    else {
        cutoffs_tb <- X_Ready4useDyad@ds_tb
        cutoffs_tb <- cutoffs_tb %>% dplyr::select(UID, Date, 
            MeasurementWeek) %>% tidyr::pivot_wider(values_from = Date, 
            names_from = MeasurementWeek)
        date_cols_chr <- setdiff(names(cutoffs_tb), "UID")
        date_cols_chr <- date_cols_chr[date_cols_chr %>% stringr::str_remove_all("Week") %>% 
            as.numeric() %>% order()]
        cutoffs_tb <- cutoffs_tb %>% dplyr::select(c("UID", date_cols_chr))
        assertthat::assert_that(length(date_cols_chr) > 1)
        bl_week_1L_int <- date_cols_chr[1] %>% stringr::str_remove("Week") %>% 
            as.numeric() %>% as.integer()
        difference_1L_int <- bl_week_1L_int - start_at_1L_int
        if (difference_1L_int > 0) {
            cutoffs_tb <- cutoffs_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("Week", 
                start_at_1L_int)), !!rlang::sym(date_cols_chr[1]) - 
                lubridate::weeks(difference_1L_int)))
            date_cols_chr <- c(paste0("Week", start_at_1L_int), 
                date_cols_chr)
        }
        else {
            date_cols_chr <- c(date_cols_chr[1], date_cols_chr)
        }
        intersected_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::right_join(cutoffs_tb) %>% 
            dplyr::select(tidyr::all_of(c("UID", "Date", "direct_mins", 
                "indirect_mins", "Minutes", unique(date_cols_chr))))
        minutes_tb <- 2:length(date_cols_chr) %>% purrr::map_dfr(~{
            start_1L_chr <- date_cols_chr[.x - 1]
            end_1L_chr <- date_cols_chr[.x]
            if (identical(start_1L_chr, end_1L_chr)) {
                wip_tb <- intersected_tb %>% dplyr::group_by(UID) %>% 
                  dplyr::summarise(direct_mins = 0, indirect_mins = 0, 
                    Minutes = 0)
            }
            else {
                wip_tb <- intersected_tb %>% dplyr::filter(Date >= 
                  !!rlang::sym(start_1L_chr) & Date <= !!rlang::sym(end_1L_chr)) %>% 
                  dplyr::group_by(UID) %>% dplyr::summarise(dplyr::across(c("direct_mins", 
                  "indirect_mins", "Minutes"), ~sum(.x)))
            }
            wip_tb %>% dplyr::mutate(MeasurementWeek = end_1L_chr)
        })
        minutes_tb <- 2:length(date_cols_chr) %>% purrr::map_dfr(~{
            first_tb <- minutes_tb %>% dplyr::filter(MeasurementWeek == 
                date_cols_chr[.x])
            second_tb <- minutes_tb %>% dplyr::filter(MeasurementWeek != 
                date_cols_chr[.x])
            rbind(first_tb, second_tb %>% dplyr::filter(!UID %in% 
                first_tb$UID) %>% dplyr::mutate(direct_mins = 0, 
                indirect_mins = 0, Minutes = 0, MeasurementWeek = date_cols_chr[.x]))
        })
        minutes_tb <- rbind(minutes_tb, date_cols_chr[2:length(date_cols_chr)] %>% 
            purrr::map_dfr(~intersected_tb %>% dplyr::filter(!UID %in% 
                minutes_tb$UID) %>% dplyr::mutate(dplyr::across(c("direct_mins", 
                "indirect_mins", "Minutes"), ~0)) %>% dplyr::select(c("UID", 
                "direct_mins", "indirect_mins", "Minutes")) %>% 
                dplyr::distinct() %>% dplyr::mutate(MeasurementWeek = .x))) %>% 
            dplyr::arrange(UID, MeasurementWeek)
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::left_join(minutes_tb)
    }
    X_Ready4useDyad@dictionary_r3 <- ready4use::renew.ready4use_dictionary(X_Ready4useDyad@dictionary_r3 %>% 
        dplyr::filter(!var_nm_chr %in% c("direct_mins", "indirect_mins", 
            "Minutes"), var_nm_chr %in% names(X_Ready4useDyad@ds_tb)), 
        new_cases_r3 = Y_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% 
            c("direct_mins", "indirect_mins", "Minutes")))
    return(X_Ready4useDyad)
}
#' Add minutes event
#' @description add_minutes_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add minutes event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param minutes_mdl Minutes (a model)
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_minutes_event
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map_int
#' @keywords internal
add_minutes_event <- function (X_Ready4useDyad, minutes_mdl, iterations_int = 1:100L) 
{
    if (!"Minutes" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(Minutes = 0))
        X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = "Minutes", 
            pattern_1L_chr = "{col}_start")
        X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = "Minutes")
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(MeasurementWeek = paste0("Week", round(as.numeric((CurrentDate - 
            StartDate))/7, 0) %>% purrr::map_int(~ifelse(.x == 
            52, 53, .x)))))
    X_Ready4useDyad <- add_simulated_data(minutes_mdl, var_1L_chr = "Minutes_change", 
        Y_Ready4useDyad = X_Ready4useDyad, iterations_int = iterations_int, 
        join_with_chr = c("Iteration"), type_1L_chr = "third", 
        what_1L_chr = "new")
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = c("Minutes", 
        "Minutes_change"))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(Minutes = Minutes + Minutes_change))
    return(X_Ready4useDyad)
}
#' Add model tests
#' @description add_model_tests() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add model tests. The function returns Regressions (a list).
#' @param model_data_ls Model data (a list)
#' @param regressions_ls Regressions (a list)
#' @param imputed_1L_lgl Imputed (a logical vector of length one), Default: T
#' @param iterations_1L_int Iterations (an integer vector of length one), Default: 100
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param max_1L_dbl Maximum (a double vector of length one), Default: numeric(0)
#' @param min_1L_dbl Minimum (a double vector of length one), Default: numeric(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("models", "candidates")
#' @param use_1L_chr Use (a character vector of length one), Default: character(0)
#' @param var_1L_chr Variable (a character vector of length one), Default: character(0)
#' @param x_label_1L_chr X label (a character vector of length one), Default: 'NA'
#' @param what_1L_chr What (a character vector of length one), Default: c("AQoL6D", "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
#'    "Tx_Treatment", "Tx_Discharged")
#' @return Regressions (a list)
#' @rdname add_model_tests
#' @export 
#' @importFrom purrr map_chr map assign_in
#' @importFrom stats setNames
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @keywords internal
add_model_tests <- function (model_data_ls, regressions_ls, imputed_1L_lgl = T, 
    iterations_1L_int = 100, join_with_chr = character(0), max_1L_dbl = numeric(0), 
    min_1L_dbl = numeric(0), model_1L_int = integer(0), type_1L_chr = c("models", 
        "candidates"), use_1L_chr = character(0), var_1L_chr = character(0), 
    x_label_1L_chr = NA_character_, what_1L_chr = c("AQoL6D", 
        "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
        "Tx_Treatment", "Tx_Discharged")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    constraints_dbl <- list(min_1L_dbl, max_1L_dbl) %>% purrr::map_chr(~ifelse(identical(.x, 
        numeric(0)), "", as.character(.x))) %>% as.numeric() %>% 
        stats::setNames((c("min", "max")))
    constraints_dbl <- constraints_dbl[!is.na(constraints_dbl)]
    constraints_chr <- constraints_dbl %>% names()
    if (!identical(constraints_chr, character(0))) {
        constraints_chr <- constraints_chr %>% paste0(collapse = "_")
    }
    constraints_dbl <- c(ifelse(identical(min_1L_dbl, numeric(0)), 
        -Inf, min_1L_dbl), ifelse(identical(max_1L_dbl, numeric(0)), 
        Inf, max_1L_dbl))
    if (what_1L_chr %in% c("Minutes") && identical(join_with_chr, 
        character(0))) {
        join_with_chr <- "MeasurementWeek"
    }
    model_mdl <- get_regression(regressions_ls, model_1L_int = model_1L_int, 
        type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)
    if (identical(use_1L_chr, character(0))) {
        if (what_1L_chr == "Minutes") {
            use_1L_chr <- "MinutesLong"
        }
        if (what_1L_chr) {
            use_1L_chr <- "Outcomes0To12Wide"
        }
    }
    Y_Ready4useDyad <- get_project_model_data(model_data_ls, 
        type_1L_chr = ifelse(imputed_1L_lgl, "imputed", "unimputed"), 
        what_1L_chr = use_1L_chr)
    test_ls <- list(Simulated_r4 = add_simulated_data(model_mdl = model_mdl, 
        join_with_chr = join_with_chr, var_1L_chr = var_1L_chr, 
        Y_Ready4useDyad = Y_Ready4useDyad, iterations_int = 1:iterations_1L_int) %>% 
        update_predictions_ds(var_1L_chr = var_1L_chr, do_int = 1))
    test_ls$Comparison_r4 <- make_predd_observed_ds(Y_Ready4useDyad, 
        Y_Ready4useDyad = test_ls$Simulated_r4, consolidate_1L_chr = var_1L_chr, 
        join_with_chr = join_with_chr, select_chr = paste0(var_1L_chr, 
            "_sim_mean"), slim_1L_lgl = T)
    plots_ls <- c("density", "histogram", "scatter") %>% purrr::map(~{
        plot_1L_chr <- .x
        c("all_plt", constraints_chr) %>% purrr::map(~{
            X_Ready4useDyad <- test_ls$Comparison_r4
            if (.x != "all_plt") {
                new_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(var_1L_chr) >= 
                  constraints_dbl[1] & !!rlang::sym(var_1L_chr) <= 
                  constraints_dbl[2])
                X_Ready4useDyad <- renewSlot(X_Ready4useDyad, 
                  "ds_tb", new_tb)
            }
            if (plot_1L_chr == "scatter") {
                grouping_1L_chr <- character(0)
                if (!identical(join_with_chr, character(0))) {
                  grouping_1L_chr <- join_with_chr[1]
                }
                plot_test_scatter(X_Ready4useDyad, grouping_1L_chr = grouping_1L_chr, 
                  var_1L_chr = var_1L_chr, collapse_1L_lgl = T)
            }
            else {
                depict(X_Ready4useDyad, x_vars_chr = var_1L_chr, 
                  x_labels_chr = x_label_1L_chr, y_labels_chr = "", 
                  z_vars_chr = "Data", z_labels_chr = "", as_percent_1L_lgl = T, 
                  drop_missing_1L_lgl = T, what_1L_chr = plot_1L_chr)
            }
        }) %>% stats::setNames(c("all_plt", "constrained_plt")[1:(ifelse(length(constraints_chr) == 
            0, 1, 2))])
    }) %>% stats::setNames(paste0(c("density", "histogram", "scatter"), 
        "_ls"))
    test_ls <- append(test_ls, plots_ls)
    if (what_1L_chr == "Minutes") {
        test_ls$comparison_tb <- make_project_minutes_cmprsn(test_ls$Simulated_r4, 
            Y_Ready4useDyad = test_ls$Comparison_r4, type_1L_chr = "prediction")
    }
    regressions_ls$tests_ls <- purrr::assign_in(regressions_ls$tests_ls, 
        where = paste0(what_1L_chr, "_ls"), value = test_ls)
    return(regressions_ls)
}
#' Add outcome change schedule
#' @description add_outcome_change_schedule() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add outcome change schedule. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param step_dtm Step (a date vector), Default: lubridate::weeks(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_outcome_change_schedule
#' @export 
#' @importFrom lubridate weeks
#' @importFrom dplyr mutate case_when
#' @keywords internal
add_outcome_change_schedule <- function (X_Ready4useDyad, step_dtm = lubridate::weeks(0)) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(ScheduledFor = dplyr::case_when(k10_part == 
            1 ~ treatment_measurement, T ~ CurrentDate + step_dtm)))
    return(X_Ready4useDyad)
}
#' Add outcome sensitivity
#' @description add_outcome_sensitivity() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add outcome sensitivity. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param outcome_1L_chr Outcome (a character vector of length one)
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param tfmn_fn Transformation (a function), Default: NULL
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_outcome_sensitivity
#' @export 
#' @importFrom purrr pluck reduce
#' @importFrom rlang exec
#' @keywords internal
add_outcome_sensitivity <- function (X_Ready4useDyad, outcome_1L_chr, sensitivities_ls = make_sensitivities_ls(), 
    tfmn_fn = NULL, tfmn_ls = make_class_tfmns(T)) 
{
    if (is.null(tfmn_fn)) {
        tfmn_fn <- tfmn_ls %>% purrr::pluck(outcome_1L_chr)
    }
    X_Ready4useDyad <- purrr::reduce(1:length(sensitivities_ls$outcomes_ls), 
        .init = X_Ready4useDyad, ~{
            sensitivity_fn <- sensitivities_ls$outcomes_ls[[.y]]
            args_ls <- list(outcome_1L_chr = outcome_1L_chr, 
                suffix_1L_chr = paste0("_", names(sensitivities_ls$outcomes_ls)[.y]), 
                tfmn_fn = tfmn_fn)
            rlang::exec(sensitivity_fn, .x, !!!args_ls)
        })
    return(X_Ready4useDyad)
}
#' Add outcome time variables
#' @description add_outcome_time_vars() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add outcome time variables. The function is called for its side effects and does not return a value.
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param outcome_1L_chr Outcome (a character vector of length one)
#' @param add_adjustments_1L_lgl Add adjustments (a logical vector of length one), Default: FALSE
#' @param fup_var_1L_chr Follow-up variable (a character vector of length one), Default: character(0)
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: integer(0)
#' @param maintain_for_1L_int Maintain for (an integer vector of length one), Default: 0
#' @return Y (A dataset and data dictionary pair.)
#' @rdname add_outcome_time_vars
#' @export 
#' @importFrom lubridate weeks years days
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_outcome_time_vars <- function (Y_Ready4useDyad, outcome_1L_chr, add_adjustments_1L_lgl = FALSE, 
    fup_var_1L_chr = character(0), follow_up_1L_int = integer(0), 
    maintain_for_1L_int = 0L) 
{
    maintain_yrs_1L_dbl <- lubridate::weeks(maintain_for_1L_int)/lubridate::years(1)
    end_var_1L_chr <- make_conditional_vars(outcome_1L_chr, follow_up_1L_int = follow_up_1L_int, 
        fup_var_1L_chr = fup_var_1L_chr, type_1L_chr = "end")
    start_var_1L_chr <- make_conditional_vars(outcome_1L_chr, 
        follow_up_1L_int = follow_up_1L_int, fup_var_1L_chr = fup_var_1L_chr, 
        type_1L_chr = "start")
    if (!identical(follow_up_1L_int, integer(0))) {
        yrs_1L_dbl <- lubridate::weeks(follow_up_1L_int)/lubridate::years(1)
        multiplier_1L_dbl <- 1 + maintain_yrs_1L_dbl/yrs_1L_dbl
        adjusted_yrs_1L_dbl <- yrs_1L_dbl + maintain_yrs_1L_dbl
        Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                "_years")), yrs_1L_dbl), `:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                "_multiplier")), multiplier_1L_dbl), `:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                "_adjusted")), adjusted_yrs_1L_dbl)))
    }
    else {
        Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                "_days")), lubridate::days(!!rlang::sym(paste0(outcome_1L_chr, 
                "_date")) - !!rlang::sym(paste0(outcome_1L_chr, 
                "_date_previous")))), `:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                "_years")), (!!rlang::sym(paste0(outcome_1L_chr, 
                "_days"))/lubridate::years(1)))))
        if (add_adjustments_1L_lgl) {
            Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                  "_multiplier")), 1 + maintain_yrs_1L_dbl/(!!rlang::sym(paste0(outcome_1L_chr, 
                  "_years")))), `:=`(!!rlang::sym(paste0(outcome_1L_chr, 
                  "_adjusted")), !!rlang::sym(paste0(outcome_1L_chr, 
                  "_years")) + maintain_yrs_1L_dbl)))
        }
    }
    return(Y_Ready4useDyad)
}
#' Add outcomes event sequence
#' @description add_outcomes_event_sequence() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add outcomes event sequence. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param inputs_ls Inputs (a list)
#' @param add_sensitivity_1L_lgl Add sensitivity (a logical vector of length one), Default: FALSE
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: -2
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param k10_method_1L_chr K10 method (a character vector of length one), Default: c("Model", "Table")
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL6D", "CHU9D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_outcomes_event_sequence
#' @export 
#' @importFrom lubridate weeks
#' @importFrom dplyr filter mutate bind_rows arrange
#' @importFrom purrr reduce map_lgl pluck
#' @importFrom rlang sym
#' @keywords internal
add_outcomes_event_sequence <- function (X_Ready4useDyad, inputs_ls, add_sensitivity_1L_lgl = FALSE, 
    adjustment_1L_dbl = -2, iterations_int = 1:100L, k10_method_1L_chr = c("Model", 
        "Table"), sensitivities_ls = make_sensitivities_ls(), 
    suffix_1L_chr = character(0), tfmn_ls = make_class_tfmns(T), 
    utilities_chr = c("AQoL6D", "CHU9D"), type_1L_chr = c("Model", 
        "Project")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    k10_method_1L_chr <- match.arg(k10_method_1L_chr)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateK10", 
        step_dtm = lubridate::weeks(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_k10_event(X_Ready4useDyad, adjustment_1L_dbl = adjustment_1L_dbl, 
        k10_mdl = inputs_ls$models_ls$K10_mdl, iterations_int = iterations_int, 
        params_tb = inputs_ls$params_tb, sensitivities_ls = sensitivities_ls, 
        suffix_1L_chr = suffix_1L_chr, tfmn_ls = tfmn_ls, type_1L_chr = ifelse(type_1L_chr == 
            "Project", "Project", k10_method_1L_chr))
    X_Ready4useDyad <- update_k10_event_schedule(X_Ready4useDyad, 
        type_1L_chr = k10_method_1L_chr)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateUtility", 
        step_dtm = lubridate::weeks(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, 
        add_sensitivity_1L_lgl = add_sensitivity_1L_lgl, adjustment_1L_dbl = adjustment_1L_dbl, 
        models_ls = inputs_ls$models_ls, iterations_int = iterations_int, 
        sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
        utilities_chr = utilities_chr, type_1L_chr = type_1L_chr, 
        what_1L_chr = "new")
    if (k10_method_1L_chr == "Table" & type_1L_chr == "Model") {
        Y_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(floor(k10_part) != 
                1))
        Z_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(floor(k10_part) == 
                1))
        if (nrow(Z_Ready4useDyad@ds_tb) > 0) {
            Z_Ready4useDyad <- add_time_to_event(Z_Ready4useDyad, 
                event_1L_chr = "UpdateK10", schedule_fn = add_outcome_change_schedule)
            Z_Ready4useDyad <- update_current_date(Z_Ready4useDyad)
            Z_Ready4useDyad <- update_current_event(Z_Ready4useDyad)
            Z_Ready4useDyad <- add_k10_event(Z_Ready4useDyad, 
                adjustment_1L_dbl = adjustment_1L_dbl, params_tb = inputs_ls$params_tb, 
                iterations_int = iterations_int, suffix_1L_chr = suffix_1L_chr, 
                tfmn_ls = tfmn_ls, type_1L_chr = k10_method_1L_chr)
            Z_Ready4useDyad <- add_time_to_event(Z_Ready4useDyad, 
                event_1L_chr = "UpdateUtility", step_dtm = lubridate::weeks(0))
            Z_Ready4useDyad <- update_current_date(Z_Ready4useDyad)
            Z_Ready4useDyad <- update_current_event(Z_Ready4useDyad)
            Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
                c("k10", utilities_chr) %>% purrr::reduce(.init = Z_Ready4useDyad@ds_tb, 
                  ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                    !!rlang::sym(paste0(.y, "_previous"))), `:=`(!!rlang::sym(paste0(.y, 
                    "_previous")), !!rlang::sym(paste0(.y, "_start"))))))
            Z_Ready4useDyad <- add_utility_event(Z_Ready4useDyad, 
                add_qalys_1L_lgl = T, add_sensitivity_1L_lgl = add_sensitivity_1L_lgl, 
                adjustment_1L_dbl = adjustment_1L_dbl, models_ls = inputs_ls$models_ls, 
                iterations_int = iterations_int, tfmn_ls = tfmn_ls, 
                utilities_chr = utilities_chr, what_1L_chr = "new")
            new_chr <- setdiff(names(Z_Ready4useDyad@ds_tb), 
                names(Y_Ready4useDyad@ds_tb))
            if (!identical(new_chr, character(0))) {
                Y_Ready4useDyad <- new_chr %>% purrr::reduce(.init = Y_Ready4useDyad, 
                  ~{
                    variable_1L_chr <- .y
                    utility_1L_chr <- utilities_chr[utilities_chr %>% 
                      purrr::map_lgl(~startsWith(variable_1L_chr, 
                        .x))]
                    tfmn_fn <- tfmn_ls %>% purrr::pluck(utility_1L_chr)
                    renewSlot(.x, "ds_tb", .x@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(variable_1L_chr), 
                      NA_real_ %>% tfmn_fn())))
                  })
            }
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                dplyr::bind_rows(Y_Ready4useDyad@ds_tb, Z_Ready4useDyad@ds_tb) %>% 
                  dplyr::arrange(Iteration, UID))
        }
    }
    X_Ready4useDyad <- update_order(X_Ready4useDyad)
    return(X_Ready4useDyad)
}
#' Add project assessments
#' @description add_project_assessments() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project assessments. The function returns Regressions (a list).
#' @param regressions_ls Regressions (a list)
#' @param confusion_1L_lgl Confusion (a logical vector of length one), Default: F
#' @param exclude_int Exclude (an integer vector), Default: integer(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param rank_1L_lgl Rank (a logical vector of length one), Default: TRUE
#' @param residual_1L_chr Residual (a character vector of length one), Default: 'normal'
#' @param two_part_1L_lgl Two part (a logical vector of length one), Default: FALSE
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "tests", "models")
#' @param what_1L_chr What (a character vector of length one), Default: c("AQoL6D", "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
#'    "Tx_Treatment", "Tx_Discharged")
#' @param var_1L_chr Variable (a character vector of length one), Default: character(0)
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @return Regressions (a list)
#' @rdname add_project_assessments
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom purrr map assign_in
#' @importFrom stats setNames
#' @importFrom stringr str_remove
#' @keywords internal
add_project_assessments <- function (regressions_ls, confusion_1L_lgl = F, exclude_int = integer(0), 
    model_1L_int = integer(0), rank_1L_lgl = TRUE, residual_1L_chr = "normal", 
    two_part_1L_lgl = FALSE, type_1L_chr = c("candidates", "tests", 
        "models"), what_1L_chr = c("AQoL6D", "CHU9D", "K10", 
        "Minutes", "Treatments", "Tx_Waitlist", "Tx_Treatment", 
        "Tx_Discharged"), var_1L_chr = character(0), X_Ready4useDyad = ready4use::Ready4useDyad()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "Treatments") {
        updated_ls <- c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged") %>% 
            purrr::map(~make_regression_report(regressions_ls, 
                X_Ready4useDyad = X_Ready4useDyad, exclude_int = exclude_int, 
                model_1L_int = model_1L_int, report_1L_chr = ifelse(confusion_1L_lgl, 
                  "all", "main"), rank_1L_lgl = rank_1L_lgl, 
                residual_1L_chr = residual_1L_chr, type_1L_chr = type_1L_chr, 
                what_1L_chr = .x, var_1L_chr = var_1L_chr))
    }
    else {
        if (two_part_1L_lgl) {
            updated_ls <- 1:2 %>% purrr::map(~make_regression_report(regressions_ls, 
                X_Ready4useDyad = X_Ready4useDyad, exclude_int = exclude_int, 
                model_1L_int = model_1L_int, report_1L_chr = ifelse(confusion_1L_lgl, 
                  "all", "main"), part_1L_int = .x, rank_1L_lgl = rank_1L_lgl, 
                residual_1L_chr = residual_1L_chr, type_1L_chr = type_1L_chr, 
                what_1L_chr = what_1L_chr, var_1L_chr = var_1L_chr)) %>% 
                stats::setNames(paste0("part_", 1:2, "_ls"))
        }
        else {
            updated_ls <- make_regression_report(regressions_ls, 
                X_Ready4useDyad = X_Ready4useDyad, exclude_int = exclude_int, 
                model_1L_int = model_1L_int, report_1L_chr = ifelse(confusion_1L_lgl, 
                  "all", "main"), rank_1L_lgl = rank_1L_lgl, 
                residual_1L_chr = residual_1L_chr, type_1L_chr = type_1L_chr, 
                what_1L_chr = what_1L_chr, var_1L_chr = var_1L_chr)
        }
    }
    if (what_1L_chr %in% c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged")) {
        regressions_ls$assessments_ls$Treatments_ls <- purrr::assign_in(regressions_ls$assessments_ls$Treatments_ls, 
            where = paste0(stringr::str_remove(what_1L_chr, "Tx_"), 
                "_ls"), value = updated_ls)
    }
    else {
        regressions_ls$assessments_ls <- purrr::assign_in(regressions_ls$assessments_ls, 
            where = paste0(what_1L_chr, "_ls"), value = updated_ls)
    }
    return(regressions_ls)
}
#' Add project model data
#' @description add_project_model_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project model data. The function returns Model data (a list).
#' @param model_data_ls Model data (a list), Default: NULL
#' @param mdls_lup Models (a lookup table), Default: NULL
#' @param processed_ls Processed (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("unimputed", "imputed")
#' @param what_1L_chr What (a character vector of length one), Default: c("CostWide", "Joiners", "MicroLong", "MicroWide", "MicroWide1Year", 
#'    "MinutesLong", "Outcomes", "OutcomesJoiners", "Series")
#' @return Model data (a list)
#' @rdname add_project_model_data
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom youthu get_mdls_lup get_ttu_dv_dss
#' @importFrom dplyr filter
#' @importFrom lubridate years days
#' @importFrom purrr assign_in
#' @keywords internal
add_project_model_data <- function (model_data_ls = NULL, mdls_lup = NULL, processed_ls = NULL, 
    type_1L_chr = c("unimputed", "imputed"), what_1L_chr = c("CostWide", 
        "Joiners", "MicroLong", "MicroWide", "MicroWide1Year", 
        "MinutesLong", "Outcomes", "OutcomesJoiners", "Series")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (is.null(model_data_ls)) {
        model_data_ls <- list(unimputed_ls = list(), imputed_ls = list())
    }
    if (what_1L_chr %in% c("Outcomes", "OutcomesJoiners")) {
        if (what_1L_chr == "Outcomes") {
            assertthat::assert_that(type_1L_chr == "unimputed")
            if (is.null(mdls_lup)) {
                mdls_lup <- youthu::get_mdls_lup(youthu::get_ttu_dv_dss("TTU"), 
                  utility_type_chr = "AQoL-6D", mdl_predrs_in_ds_chr = c("PHQ9 total score", 
                    "GAD7 total score"))
            }
            model_data_ls <- add_project_outcomes_data(model_data_ls, 
                mdls_lup = mdls_lup, processed_ls = processed_ls)
        }
        if (what_1L_chr == "OutcomesJoiners") {
            assertthat::assert_that(type_1L_chr == "imputed")
            model_data_ls <- add_joiners_outcomes_ds(model_data_ls)
        }
    }
    else {
        if (what_1L_chr == "CostWide") {
            assertthat::assert_that(type_1L_chr == "imputed")
            if (is.null(model_data_ls$unimputed_ls$Joiners_r4)) {
                model_data_ls <- add_project_model_data(model_data_ls, 
                  processed_ls = processed_ls, what_1L_chr = "Joiners")
            }
            if (is.null(model_data_ls$imputed_ls$MicroWide_r4)) {
                model_data_ls <- add_project_model_data(model_data_ls, 
                  processed_ls = processed_ls, type_1L_chr = "imputed", 
                  what_1L_chr = "MicroWide")
            }
            X_Ready4useDyad <- make_project_cost_mdlng_ds(model_data_ls$unimputed_ls$Joiners_r4, 
                X_Ready4useDyad = model_data_ls$imputed_ls$MicroWide_r4)
        }
        if (what_1L_chr == "Joiners") {
            if (type_1L_chr == "unimputed") {
                X_Ready4useDyad <- make_project_joiners_ds(processed_ls)
            }
            else {
                if (is.null(model_data_ls$unimputed_ls$Joiners_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, what_1L_chr = "Joiners")
                }
                X_Ready4useDyad <- add_imputed_data(renewSlot(model_data_ls$unimputed_ls$Joiners_r4, 
                  "ds_tb", model_data_ls$unimputed_ls$Joiners_r4@ds_tb %>% 
                    dplyr::filter(!duplicated(UID))), Y_Ready4useDyad = model_data_ls$unimputed_ls$Joiners_r4, 
                  impute_age_1L_lgl = T, treatment_status_1L_int = 1)
            }
        }
        if (what_1L_chr == "MicroLong") {
            if (type_1L_chr == "unimputed") {
                X_Ready4useDyad <- make_project_service_use_ds(processed_ls)
            }
            else {
                if (is.null(model_data_ls$unimputed_ls$Joiners_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, what_1L_chr = "Joiners")
                }
                if (is.null(model_data_ls$unimputed_ls$MicroLong_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, what_1L_chr = "MicroLong")
                }
                X_Ready4useDyad <- add_imputed_data(renewSlot(model_data_ls$imputed_ls$Joiners_r4, 
                  "ds_tb", model_data_ls$imputed_ls$Joiners_r4@ds_tb %>% 
                    dplyr::filter(!duplicated(UID))), Y_Ready4useDyad = model_data_ls$unimputed_ls$MicroLong_r4, 
                  add_cumulatives_1L_lgl = TRUE, ignore_y_chr = c("Episodes", 
                    "Separations", "Active", "CumulativeEpisodes", 
                    "CumulativeSeparations"), extras_chr = c("primary_mode", 
                    "primary_participant", "primary_purpose"), 
                  treatment_status_1L_int = 1)
            }
        }
        if (what_1L_chr == "MicroWide") {
            if (type_1L_chr == "unimputed") {
                if (is.null(model_data_ls$unimputed_ls$MicroLong_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, what_1L_chr = "MicroLong")
                }
                X_Ready4useDyad <- transform_ds_to_wide(model_data_ls$unimputed_ls$MicroLong_r4, 
                  processed_ls = processed_ls)
            }
            else {
                if (is.null(model_data_ls$imputed_ls$MicroLong_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, type_1L_chr = "imputed", 
                    what_1L_chr = "MicroLong")
                }
                X_Ready4useDyad <- transform_ds_to_wide(model_data_ls$imputed_ls$MicroLong_r4, 
                  processed_ls = processed_ls)
            }
        }
        if (what_1L_chr == "MicroWide1Year") {
            if (type_1L_chr == "unimputed") {
                if (is.null(model_data_ls$unimputed_ls$MicroLong_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, what_1L_chr = "MicroLong")
                }
                X_Ready4useDyad <- transform_ds_to_wide(model_data_ls$unimputed_ls$MicroLong_r4, 
                  processed_ls = processed_ls, join_before_dtm = (model_data_ls$unimputed_ls$MicroLong_r4@ds_tb$Date %>% 
                    max()) - lubridate::years(1) + lubridate::days(1), 
                  max_periods_1L_int = 1, max_tenure_1L_dbl = 1)
            }
            else {
                if (is.null(model_data_ls$imputed_ls$MicroLong_r4)) {
                  model_data_ls <- add_project_model_data(model_data_ls, 
                    processed_ls = processed_ls, type_1L_chr = "imputed", 
                    what_1L_chr = "MicroLong")
                }
                X_Ready4useDyad <- transform_ds_to_wide(model_data_ls$imputed_ls$MicroLong_r4, 
                  processed_ls = processed_ls, join_before_dtm = (model_data_ls$unimputed_ls$MicroLong_r4@ds_tb$Date %>% 
                    max()) - lubridate::years(1) + lubridate::days(1), 
                  max_periods_1L_int = 1, max_tenure_1L_dbl = 1)
            }
        }
        if (what_1L_chr == "MinutesLong") {
            assertthat::assert_that(type_1L_chr == "imputed")
            if (is.null(model_data_ls$imputed_ls$MicroLong_r4)) {
                model_data_ls <- add_project_model_data(model_data_ls, 
                  processed_ls = processed_ls, type_1L_chr = "imputed", 
                  what_1L_chr = "MicroLong")
            }
            X_Ready4useDyad <- make_project_minutes_ds(processed_ls, 
                model_data_ls = model_data_ls, cut_weeks_int = c(14, 
                  53), period_dtm = lubridate::years(1) - lubridate::days(1), 
                type_1L_chr = "imputed", what_1L_chr = "long")
        }
        if (what_1L_chr == "Series") {
            if (is.null(model_data_ls$imputed_ls$MicroLong_r4)) {
                model_data_ls <- add_project_model_data(model_data_ls, 
                  processed_ls = processed_ls, type_1L_chr = "imputed", 
                  what_1L_chr = "MicroLong")
            }
            X_Ready4useDyad <- make_project_ts_ds(model_data_ls$imputed_ls$MicroLong_r4, 
                processed_ls = processed_ls)
        }
        if (type_1L_chr == "imputed") {
            model_data_ls$imputed_ls <- purrr::assign_in(model_data_ls$imputed_ls, 
                where = paste0(what_1L_chr, "_r4"), value = X_Ready4useDyad)
        }
        else {
            model_data_ls$unimputed_ls <- purrr::assign_in(model_data_ls$unimputed_ls, 
                where = paste0(what_1L_chr, "_r4"), value = X_Ready4useDyad)
        }
    }
    return(model_data_ls)
}
#' Add project outcomes data
#' @description add_project_outcomes_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project outcomes data. The function returns Model data (a list).
#' @param model_data_ls Model data (a list)
#' @param processed_ls Processed (a list)
#' @param mdls_lup Models (a lookup table), Default: NULL
#' @return Model data (a list)
#' @rdname add_project_outcomes_data
#' @export 
#' @importFrom youthu get_mdls_lup get_ttu_dv_dss
#' @keywords internal
add_project_outcomes_data <- function (model_data_ls, processed_ls, mdls_lup = NULL) 
{
    if (is.null(mdls_lup)) {
        mdls_lup <- youthu::get_mdls_lup(youthu::get_ttu_dv_dss("TTU"), 
            utility_type_chr = "AQoL-6D", mdl_predrs_in_ds_chr = c("PHQ9 total score", 
                "GAD7 total score"))
    }
    model_data_ls$unimputed_ls$OutcomesTo0Long_r4 <- make_project_outcomes_ds(processed_ls, 
        mdls_lup = mdls_lup, start_at_1L_int = -2L, weeks_dbl = c(-2, 
            0), complete_1L_lgl = T, Y_Ready4useDyad = model_data_ls$unimputed_ls$MicroLong_r4)
    model_data_ls$unimputed_ls$OutcomesTo0Wide_r4 <- transform_project_outcomes_ds(model_data_ls$unimputed_ls$OutcomesTo0Long_r4, 
        follow_up_1L_int = 0, transform_gender_1L_lgl = T)
    model_data_ls$unimputed_ls$OutcomesTo0WideTfmd_r4 <- transform_to_min_and_max(model_data_ls$unimputed_ls$OutcomesTo0Wide_r4, 
        vars_chr = c("AQoL6D_0_Weeks", "CHU9D_0_Weeks"))
    model_data_ls$unimputed_ls$Outcomes0To12Long_r4 <- make_project_outcomes_ds(processed_ls, 
        mdls_lup = mdls_lup, start_at_1L_int = -2L, weeks_dbl = c(0, 
            12), complete_1L_lgl = T, Y_Ready4useDyad = model_data_ls$unimputed_ls$MicroLong_r4)
    model_data_ls$unimputed_ls$Outcomes0To12Wide_r4 <- transform_project_outcomes_ds(model_data_ls$unimputed_ls$Outcomes0To12Long_r4, 
        follow_up_1L_int = 12, transform_gender_1L_lgl = T)
    model_data_ls$unimputed_ls$Outcomes0To12WideTfmd_r4 <- transform_to_min_and_max(model_data_ls$unimputed_ls$Outcomes0To12Wide_r4, 
        vars_chr = c("AQoL6D_12_Weeks", "CHU9D_12_Weeks"))
    model_data_ls$unimputed_ls$Outcomes12To24Long_r4 <- make_project_outcomes_ds(processed_ls, 
        mdls_lup = mdls_lup, start_at_1L_int = -2L, weeks_dbl = c(12, 
            24), complete_1L_lgl = T, Y_Ready4useDyad = model_data_ls$unimputed_ls$MicroLong_r4)
    model_data_ls$unimputed_ls$Outcomes12To24Wide_r4 <- transform_project_outcomes_ds(model_data_ls$unimputed_ls$Outcomes12To24Long_r4, 
        follow_up_1L_int = 24, transform_gender_1L_lgl = T)
    model_data_ls$unimputed_ls$Outcomes12To24WideTfmd_r4 <- transform_to_min_and_max(model_data_ls$unimputed_ls$Outcomes12To24Wide_r4, 
        vars_chr = c("AQoL6D_24_Weeks", "CHU9D_24_Weeks"))
    model_data_ls$unimputed_ls$OutcomesAllLong_r4 <- make_project_consolidated_ds(model_data_ls$unimputed_ls$Outcomes0To12Long_r4, 
        model_data_ls$unimputed_ls$Outcomes12To24Long_r4)
    model_data_ls$unimputed_ls$OutcomesAllWide_r4 <- make_project_consolidated_ds(model_data_ls$unimputed_ls$OutcomesTo0Wide_r4, 
        model_data_ls$unimputed_ls$Outcomes0To12Wide_r4, model_data_ls$unimputed_ls$Outcomes12To24Wide_r4, 
        type_1L_chr = "tx_status")
    return(model_data_ls)
}
#' Add project treatment change
#' @description add_project_treatment_change() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project treatment change. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param arrange_by_id_lgl Arrange by identity (a logical vector), Default: T
#' @param change_var_nm_1L_chr Change variable name (a character vector of length one), Default: 'treatment_change'
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: '_previous'
#' @param timepoint_1L_chr Timepoint (a character vector of length one), Default: 'MeasurementWeek'
#' @param timepoint_bl_1L_chr Timepoint baseline (a character vector of length one), Default: 'Week0'
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param var_nm_1L_chr Variable name (a character vector of length one), Default: 'treatment_status'
#' @param wide_1L_lgl Wide (a logical vector of length one), Default: F
#' @return Dataset (a tibble)
#' @rdname add_project_treatment_change
#' @export 
#' @importFrom dplyr mutate case_when group_by lag ungroup arrange
#' @importFrom rlang sym
#' @keywords internal
add_project_treatment_change <- function (ds_tb, arrange_by_id_lgl = T, change_var_nm_1L_chr = "treatment_change", 
    suffix_1L_chr = "_previous", timepoint_1L_chr = "MeasurementWeek", 
    timepoint_bl_1L_chr = "Week0", uid_1L_chr = "UID", var_nm_1L_chr = "treatment_status", 
    wide_1L_lgl = F) 
{
    if (wide_1L_lgl) {
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(change_var_nm_1L_chr), 
            dplyr::case_when(!!rlang::sym(timepoint_1L_chr) == 
                timepoint_bl_1L_chr ~ "Identical", !!rlang::sym(var_nm_1L_chr) == 
                "Waitlist" & !!rlang::sym(paste0(var_nm_1L_chr, 
                suffix_1L_chr)) == "Waitlist" ~ "Wait", !!rlang::sym(var_nm_1L_chr) == 
                "Treatment" & !!rlang::sym(paste0(var_nm_1L_chr, 
                suffix_1L_chr)) == "Waitlist" ~ "Start", !!rlang::sym(var_nm_1L_chr) == 
                "Discharged" & !!rlang::sym(paste0(var_nm_1L_chr, 
                suffix_1L_chr)) == "Waitlist" ~ "End", !!rlang::sym(var_nm_1L_chr) == 
                "Waitlist" & !!rlang::sym(paste0(var_nm_1L_chr, 
                suffix_1L_chr)) == "Treatment" ~ "StartWait", 
                !!rlang::sym(var_nm_1L_chr) == "Treatment" & 
                  !!rlang::sym(paste0(var_nm_1L_chr, suffix_1L_chr)) == 
                    "Treatment" ~ "Continue", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & !!rlang::sym(paste0(var_nm_1L_chr, 
                  suffix_1L_chr)) == "Treatment" ~ "End", !!rlang::sym(var_nm_1L_chr) == 
                  "Waitlist" & !!rlang::sym(paste0(var_nm_1L_chr, 
                  suffix_1L_chr)) == "Discharged" ~ "StartWait", 
                !!rlang::sym(var_nm_1L_chr) == "Treatment" & 
                  !!rlang::sym(paste0(var_nm_1L_chr, suffix_1L_chr)) == 
                    "Discharged" ~ "Start", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & !!rlang::sym(paste0(var_nm_1L_chr, 
                  suffix_1L_chr)) == "Discharged" ~ "StayAway", 
                !!rlang::sym(var_nm_1L_chr) == "Waitlist" & !!rlang::sym(paste0(var_nm_1L_chr, 
                  suffix_1L_chr)) == "Other" ~ "StartWait", !!rlang::sym(var_nm_1L_chr) == 
                  "Treatment" & !!rlang::sym(paste0(var_nm_1L_chr, 
                  suffix_1L_chr)) == "Other" ~ "Start", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & !!rlang::sym(paste0(var_nm_1L_chr, 
                  suffix_1L_chr)) == "Other" ~ "End", T ~ "Other") %>% 
                as.factor()))
    }
    else {
        ds_tb <- ds_tb %>% dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% 
            dplyr::mutate(`:=`(!!rlang::sym(change_var_nm_1L_chr), 
                dplyr::case_when(!!rlang::sym(timepoint_1L_chr) == 
                  timepoint_bl_1L_chr ~ "Identical", !!rlang::sym(var_nm_1L_chr) == 
                  "Waitlist" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Waitlist" ~ "Wait", !!rlang::sym(var_nm_1L_chr) == 
                  "Treatment" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Waitlist" ~ "Start", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Waitlist" ~ "End", !!rlang::sym(var_nm_1L_chr) == 
                  "Waitlist" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Treatment" ~ "StartWait", !!rlang::sym(var_nm_1L_chr) == 
                  "Treatment" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Treatment" ~ "Continue", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Treatment" ~ "End", !!rlang::sym(var_nm_1L_chr) == 
                  "Waitlist" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Discharged" ~ "StartWait", !!rlang::sym(var_nm_1L_chr) == 
                  "Treatment" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Discharged" ~ "Start", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Discharged" ~ "StayAway", !!rlang::sym(var_nm_1L_chr) == 
                  "Waitlist" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Other" ~ "StartWait", !!rlang::sym(var_nm_1L_chr) == 
                  "Treatment" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Other" ~ "Start", !!rlang::sym(var_nm_1L_chr) == 
                  "Discharged" & dplyr::lag(!!rlang::sym(var_nm_1L_chr)) == 
                  "Other" ~ "End", T ~ "Other") %>% as.factor())) %>% 
            dplyr::ungroup()
    }
    if (arrange_by_id_lgl) 
        ds_tb <- ds_tb %>% dplyr::arrange(!!rlang::sym(uid_1L_chr))
    return(ds_tb)
}
#' Add projected decay
#' @description add_projected_decay() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add projected decay. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param outcome_1L_chr Outcome (a character vector of length one)
#' @param suffix_1L_chr Suffix (a character vector of length one)
#' @param proportion_1L_dbl Proportion (a double vector of length one), Default: 1
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_projected_decay
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_projected_decay <- function (X_Ready4useDyad, outcome_1L_chr, suffix_1L_chr, proportion_1L_dbl = 1, 
    tfmn_fn = identity, ...) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(outcome_1L_chr, 
            suffix_1L_chr)), tfmn_fn(!!rlang::sym(paste0(outcome_1L_chr, 
            "_previous")) + (!!rlang::sym(paste0(outcome_1L_chr, 
            "_start")) - !!rlang::sym(paste0(outcome_1L_chr, 
            "_previous"))) * proportion_1L_dbl))))
    return(X_Ready4useDyad)
}
#' Add projected growth
#' @description add_projected_growth() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add projected growth. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param outcome_1L_chr Outcome (a character vector of length one)
#' @param suffix_1L_chr Suffix (a character vector of length one)
#' @param proportion_1L_dbl Proportion (a double vector of length one), Default: 0.2
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_projected_growth
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_projected_growth <- function (X_Ready4useDyad, outcome_1L_chr, suffix_1L_chr, proportion_1L_dbl = 0.2, 
    tfmn_fn = identity, ...) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(outcome_1L_chr, 
            suffix_1L_chr)), tfmn_fn(!!rlang::sym(paste0(outcome_1L_chr, 
            "_previous")) + (!!rlang::sym(paste0(outcome_1L_chr, 
            "_previous")) - !!rlang::sym(paste0(outcome_1L_chr, 
            "_start"))) * proportion_1L_dbl))))
    return(X_Ready4useDyad)
}
#' Add projected maintenance
#' @description add_projected_maintenance() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add projected maintenance. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param outcome_1L_chr Outcome (a character vector of length one)
#' @param suffix_1L_chr Suffix (a character vector of length one)
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_projected_maintenance
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_projected_maintenance <- function (X_Ready4useDyad, outcome_1L_chr, suffix_1L_chr, tfmn_fn = identity, 
    ...) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(outcome_1L_chr, 
            suffix_1L_chr)), tfmn_fn(!!rlang::sym(paste0(outcome_1L_chr, 
            "_previous"))))))
    return(X_Ready4useDyad)
}
#' Add Quality Adjusted Life Years sensitivities
#' @description add_qalys_sensitivities() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add quality adjusted life years sensitivities. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param end_var_1L_chr End variable (a character vector of length one), Default: character(0)
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param start_var_1L_chr Start variable (a character vector of length one), Default: character(0)
#' @param utility_1L_chr Utility (a character vector of length one), Default: c("AQoL6D", "CHU9D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "legacy")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_qalys_sensitivities
#' @export 
#' @importFrom purrr pluck map_dbl reduce
#' @importFrom dplyr mutate arrange
#' @importFrom rlang sym
#' @keywords internal
add_qalys_sensitivities <- function (X_Ready4useDyad, end_var_1L_chr = character(0), sensitivities_ls = make_sensitivities_ls(), 
    start_var_1L_chr = character(0), utility_1L_chr = c("AQoL6D", 
        "CHU9D"), type_1L_chr = c("main", "legacy")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    utility_1L_chr <- match.arg(utility_1L_chr)
    if (type_1L_chr == "legacy") {
        suffixes_chr <- paste0("_", names(sensitivities_ls$outcomes_ls))
        qaly_vars_chr <- paste0(paste0(utility_1L_chr, "_QALYs"), 
            c("", suffixes_chr))
        tfmn_ls <- make_class_tfmns()
        class_fn <- tfmn_ls %>% purrr::pluck(utility_1L_chr)
        min_1L_dbl <- ifelse(utility_1L_chr == "CHU9D", -0.2118, 
            0.03)
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(end_var_1L_chr), 
                !!rlang::sym(end_var_1L_chr) %>% purrr::map_dbl(~max(min(.x, 
                  1), min_1L_dbl)) %>% class_fn())) %>% dplyr::mutate(`:=`(!!rlang::sym(qaly_vars_chr[2]), 
                !!rlang::sym(qaly_vars_chr[1]) * !!rlang::sym(paste0(utility_1L_chr, 
                  "_multiplier")) + (((!!rlang::sym(end_var_1L_chr) + 
                  !!rlang::sym(start_var_1L_chr))/2) %>% as.double()) * 
                  (1 - !!rlang::sym(paste0(start_var_1L_chr, 
                    "_adjusted"))))) %>% dplyr::mutate(`:=`(!!rlang::sym(qaly_vars_chr[3]), 
                !!rlang::sym(qaly_vars_chr[1]) * !!rlang::sym(paste0(utility_1L_chr, 
                  "_multiplier")) + (((!!rlang::sym(start_var_1L_chr))) %>% 
                  as.double()) * (1 - !!rlang::sym(paste0(start_var_1L_chr, 
                  "_adjusted"))))) %>% dplyr::mutate(`:=`(!!rlang::sym(qaly_vars_chr[4]), 
                !!rlang::sym(qaly_vars_chr[1]) * !!rlang::sym(paste0(utility_1L_chr, 
                  "_multiplier")) + (((!!rlang::sym(end_var_1L_chr))) %>% 
                  as.double()) * (1 - !!rlang::sym(paste0(start_var_1L_chr, 
                  "_adjusted"))))) %>% dplyr::arrange(UID))
    }
    else {
        X_Ready4useDyad <- 1:length(sensitivities_ls$outcomes_ls) %>% 
            purrr::reduce(.init = X_Ready4useDyad, ~{
                renewSlot(.x, "ds_tb", .x@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(utility_1L_chr, 
                  "_QALYs_", names(sensitivities_ls$outcomes_ls)[.y])), 
                  !!rlang::sym(paste0(utility_1L_chr, "_QALYs", 
                    "_previous")) + (((!!rlang::sym(paste0(utility_1L_chr, 
                    "_previous")) + !!rlang::sym(paste0(utility_1L_chr, 
                    "_", names(sensitivities_ls$outcomes_ls)[.y])))/2) %>% 
                    as.double()) * !!rlang::sym(paste0(utility_1L_chr, 
                    "_years")))))
            })
    }
    return(X_Ready4useDyad)
}
#' Add regressions
#' @description add_regressions() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add regressions. The function returns Regressions (a list).
#' @param regressions_ls Regressions (a list)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param fn_args_ls Function arguments (a list), Default: list()
#' @param model_fn Model (a function), Default: NULL
#' @param named_1L_lgl Named (a logical vector of length one), Default: FALSE
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "tests", "models")
#' @param what_1L_chr What (a character vector of length one), Default: c("AQoL6D", "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
#'    "Tx_Treatment", "Tx_Discharged")
#' @return Regressions (a list)
#' @rdname add_regressions
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom rlang exec
#' @importFrom purrr assign_in map
#' @importFrom stringr str_remove
#' @keywords internal
add_regressions <- function (regressions_ls, model_1L_int = integer(0), fn_args_ls = list(), 
    model_fn = NULL, named_1L_lgl = FALSE, X_Ready4useDyad = ready4use::Ready4useDyad(), 
    type_1L_chr = c("candidates", "tests", "models"), what_1L_chr = c("AQoL6D", 
        "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
        "Tx_Treatment", "Tx_Discharged")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (type_1L_chr == "candidates") {
        if (is.null(model_fn)) {
            if (what_1L_chr == "AQoL6D") {
                model_fn <- make_project_aqol6d_mdls
            }
            if (what_1L_chr == "K10") {
                model_fn <- make_project_k10_mdls
            }
            if (what_1L_chr == "Treatments") {
                model_fn <- make_project_tx_mdls
            }
        }
        updated_xx <- rlang::exec(model_fn, X_Ready4useDyad, 
            !!!fn_args_ls)
        if (what_1L_chr %in% c("Tx_Waitlist", "Tx_Treatment", 
            "Tx_Discharged")) {
            regressions_ls$candidates_ls$Treatments_ls <- purrr::assign_in(regressions_ls$candidates_ls$Treatments_ls, 
                where = paste0(stringr::str_remove(what_1L_chr, 
                  "Tx_"), "_ls"), value = updated_xx)
        }
        else {
            regressions_ls$candidates_ls <- purrr::assign_in(regressions_ls$candidates_ls, 
                where = paste0(what_1L_chr, "_ls"), value = updated_xx)
        }
    }
    if (type_1L_chr == "models") {
        if (what_1L_chr == "Treatments") {
            updated_xx <- c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged") %>% 
                purrr::map(~get_regression(regressions_ls, model_1L_int = model_1L_int, 
                  named_1L_lgl = named_1L_lgl, type_1L_chr = "candidates", 
                  what_1L_chr = .x))
            suffix_1L_chr <- "_ls"
        }
        else {
            updated_xx <- get_regression(regressions_ls, model_1L_int = model_1L_int, 
                named_1L_lgl = named_1L_lgl, type_1L_chr = "candidates", 
                what_1L_chr = what_1L_chr)
            suffix_1L_chr <- "_mdl"
        }
        if (what_1L_chr %in% c("Tx_Waitlist", "Tx_Treatment", 
            "Tx_Discharged")) {
            regressions_ls$models_ls$Treatments_ls <- purrr::assign_in(regressions_ls$models_ls$Treatments_ls, 
                where = paste0(stringr::str_remove(what_1L_chr, 
                  "Tx_"), "_mdl"), value = updated_xx)
        }
        else {
            regressions_ls$models_ls <- purrr::assign_in(regressions_ls$models_ls, 
                where = paste0(what_1L_chr, suffix_1L_chr), value = updated_xx)
        }
    }
    return(regressions_ls)
}
#' Add simulated data
#' @description add_simulated_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add simulated data. The function is called for its side effects and does not return a value.
#' @param model_mdl Model (a model)
#' @param var_1L_chr Variable (a character vector of length one)
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param tfmn_1L_chr Transformation (a character vector of length one), Default: 'NTF'
#' @param type_1L_chr Type (a character vector of length one), Default: c("first", "second", "third", "fourth")
#' @param what_1L_chr What (a character vector of length one), Default: c("old", "new")
#' @return Y (A dataset and data dictionary pair.)
#' @rdname add_simulated_data
#' @export 
#' @importFrom dplyr mutate select across everything rename_with inner_join filter starts_with
#' @importFrom tidyr all_of any_of
#' @importFrom rlang sym
#' @importFrom purrr map_dfc map_dfr reduce
#' @importFrom didgformula sim
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom specific calculate_depnt_var_tfmn
#' @importFrom tidyselect any_of
#' @keywords internal
add_simulated_data <- function (model_mdl, var_1L_chr, Y_Ready4useDyad, iterations_int = 1:100L, 
    join_with_chr = character(0), tfmn_1L_chr = "NTF", type_1L_chr = c("first", 
        "second", "third", "fourth"), what_1L_chr = c("old", 
        "new")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    iterations_1L_int <- length(unique(iterations_int))
    if (type_1L_chr == "first") {
        new_data_tb <- Y_Ready4useDyad@ds_tb
        if (inherits(model_mdl, "twopartm")) {
            new_part_1_tb <- new_data_tb %>% dplyr::mutate(nonzero = NA_real_) %>% 
                dplyr::select(tidyr::all_of(names(model_mdl@model_part1$data)))
            new_part_2_tb <- new_data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                NA_real_))
            zeros_mat <- purrr::map_dfc(iterations_int, ~didgformula::sim(model_mdl@model_part1, 
                newdata = new_part_1_tb)) %>% as.matrix()
            zeros_dbl <- rbinom(nrow(new_part_1_tb), size = 1, 
                prob = predict(model_mdl@model_part1, newdata = new_part_1_tb, 
                  type = "response"))
            positive_mat <- purrr::map_dfc(iterations_int, ~didgformula::sim(model_mdl@model_part2, 
                newdata = new_part_2_tb)) %>% stats::setNames(paste0("sim_", 
                iterations_int)) %>% as.matrix()
            positive_dbl <- predict(model_mdl@model_part2, newdata = new_part_2_tb, 
                type = "response")
            sims_df <- (zeros_mat * positive_mat) %>% as.data.frame() %>% 
                stats::setNames(paste0("sim_", iterations_int))
            means_dbl <- zeros_dbl * positive_dbl
        }
        else {
            if (inherits(model_mdl, "lm") & !inherits(model_mdl, 
                "glm")) {
                if (what_1L_chr == "old") {
                  model_ls <- predict(model_mdl, se.fit = TRUE)
                }
                else {
                  model_ls <- predict(model_mdl, newdata = Y_Ready4useDyad@ds_tb %>% 
                    dplyr::select(tidyr::any_of(names(model_mdl$model))), 
                    se.fit = TRUE)
                }
                sims_df <- 1:length(model_ls$se.fit) %>% purrr::map_dfr(~rnorm(iterations_1L_int, 
                  mean = model_ls$fit[.x], sd = model_ls$se.fit[.x]) %>% 
                  stats::setNames(paste0("sim_", iterations_int)) %>% 
                  as.data.frame() %>% t() %>% as.data.frame())
            }
            else {
                if (what_1L_chr == "new") {
                  new_data_tb <- new_data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                    NA_real_)) %>% dplyr::select(tidyr::any_of(names(model_mdl$model)))
                  positive_mat <- purrr::map_dfc(iterations_int, 
                    ~didgformula::sim(model_mdl, newdata = new_data_tb)) %>% 
                    stats::setNames(paste0("sim_", iterations_int)) %>% 
                    as.matrix()
                  sims_df <- positive_mat %>% as.data.frame() %>% 
                    stats::setNames(paste0("sim_", iterations_int))
                }
                else {
                  sims_df <- simulate(model_mdl, nsim = iterations_1L_int)
                }
            }
            if (type_1L_chr == "old") {
                means_dbl <- predict(model_mdl, type = "response")
            }
            else {
                means_dbl <- predict(model_mdl, newdata = new_data_tb, 
                  type = "response")
            }
        }
        sims_tb <- tibble::as_tibble(sims_df) %>% dplyr::mutate(dplyr::across(dplyr::everything(), 
            ~.x %>% specific::calculate_depnt_var_tfmn(tfmn_1L_chr = tfmn_1L_chr, 
                tfmn_is_outp_1L_lgl = T))) %>% dplyr::rename_with(~paste0(var_1L_chr, 
            "_", .x))
        sims_tb <- sims_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(var_1L_chr, 
            "_sim_mean")), means_dbl)) %>% dplyr::mutate(UID = Y_Ready4useDyad@ds_tb$UID)
        if (!identical(join_with_chr, character(0))) {
            sims_tb <- sims_tb %>% cbind(Y_Ready4useDyad@ds_tb %>% 
                dplyr::select(tidyselect::any_of(join_with_chr)))
        }
        Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::inner_join(sims_tb)
    }
    if (type_1L_chr == "second") {
        Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            Y_Ready4useDyad@ds_tb$Iteration %>% unique() %>% 
                sort() %>% purrr::reduce(.init = Y_Ready4useDyad@ds_tb %>% 
                dplyr::filter(F) %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                numeric(0))), ~{
                rbind(.x, add_simulated_data(model_mdl = model_mdl, 
                  var_1L_chr = var_1L_chr, Y_Ready4useDyad = renewSlot(Y_Ready4useDyad, 
                    "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::filter(Iteration == 
                      .y)), iterations_int = 1L, what_1L_chr = what_1L_chr) %>% 
                  procureSlot("ds_tb") %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                  !!rlang::sym(paste0(var_1L_chr, "_sim_", ifelse(.y == 
                    0, "mean", "1"))))) %>% dplyr::select(-tidyr::all_of(paste0(var_1L_chr, 
                  "_sim_", c("1", "mean")))))
            }))
    }
    if (type_1L_chr == "third") {
        Y_Ready4useDyad <- add_simulated_data(model_mdl = model_mdl, 
            var_1L_chr = var_1L_chr, Y_Ready4useDyad = Y_Ready4useDyad, 
            iterations_int = iterations_int, join_with_chr = join_with_chr, 
            what_1L_chr = what_1L_chr)
        Y_Ready4useDyad <- add_simulated_data(model_mdl = model_mdl, 
            var_1L_chr = var_1L_chr, Y_Ready4useDyad = Y_Ready4useDyad, 
            iterations_int = iterations_int, type_1L_chr = "fourth", 
            what_1L_chr = what_1L_chr)
    }
    if (type_1L_chr == "fourth") {
        Y_Ready4useDyad <- Y_Ready4useDyad %>% update_predictions_ds(var_1L_chr = var_1L_chr, 
            do_int = 1)
        Y_Ready4useDyad <- make_predd_observed_ds(Y_Ready4useDyad, 
            Y_Ready4useDyad = Y_Ready4useDyad, consolidate_1L_chr = var_1L_chr, 
            join_with_chr = join_with_chr)
        Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            dplyr::select(Y_Ready4useDyad@ds_tb, -dplyr::starts_with(paste0(var_1L_chr, 
                "_sim"))) %>% dplyr::filter(Data == "Simulated") %>% 
                dplyr::select(-Data))
    }
    Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    return(Y_Ready4useDyad)
}
#' Add simulated treatments
#' @description add_simulated_treatments() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add simulated treatments. The function is called for its side effects and does not return a value.
#' @param treatment_mdls_ls Treatment models (a list)
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param bl_week_1L_dbl Baseline week (a double vector of length one), Default: 0
#' @param change_var_1L_chr Change variable (a character vector of length one), Default: 'treatment_change'
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param status_var_1L_chr Status variable (a character vector of length one), Default: 'treatment_status'
#' @param tidy_1L_lgl Tidy (a logical vector of length one), Default: FALSE
#' @return Y (A dataset and data dictionary pair.)
#' @rdname add_simulated_treatments
#' @export 
#' @importFrom purrr map2_dfr map_dfr
#' @importFrom stringr str_remove_all str_sub
#' @importFrom dplyr filter mutate select bind_cols across starts_with case_when
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @importFrom stats predict
#' @importFrom tibble as_tibble
#' @keywords internal
add_simulated_treatments <- function (treatment_mdls_ls, Y_Ready4useDyad, bl_week_1L_dbl = 0, 
    change_var_1L_chr = "treatment_change", iterations_int = 1:100L, 
    status_var_1L_chr = "treatment_status", tidy_1L_lgl = FALSE) 
{
    iterations_1L_int <- length(unique(iterations_int))
    data_tb <- treatment_mdls_ls %>% purrr::map2_dfr(names(treatment_mdls_ls) %>% 
        stringr::str_remove_all("_mdl"), ~{
        new_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(status_var_1L_chr) == 
            .y) %>% dplyr::mutate(`:=`(!!rlang::sym(status_var_1L_chr), 
            as.factor(as.character(!!rlang::sym(status_var_1L_chr))))) %>% 
            dplyr::select(-tidyselect::any_of(change_var_1L_chr))
        probs_tb <- dplyr::bind_cols(stats::predict(.x, new_data = new_tb), 
            stats::predict(.x, new_data = new_tb, type = "prob"))
        draws_tb <- 1:nrow(probs_tb) %>% purrr::map_dfr(~{
            two_1L_int <- round(probs_tb[[.x, 2]] * iterations_1L_int, 
                0)
            three_1L_int <- round(probs_tb[[.x, 3]] * iterations_1L_int, 
                0)
            four_1L_int <- iterations_1L_int - two_1L_int - three_1L_int
            sims_df <- c(probs_tb[[.x, 1]], rep(names(probs_tb)[2] %>% 
                stringr::str_sub(start = 7), two_1L_int), rep(names(probs_tb)[3] %>% 
                stringr::str_sub(start = 7), three_1L_int), rep(names(probs_tb)[4] %>% 
                stringr::str_sub(start = 7), four_1L_int)) %>% 
                sample() %>% as.data.frame() %>% t()
            colnames(sims_df) <- paste0(status_var_1L_chr, "_sim_", 
                c("mean", iterations_int))
            sims_df %>% tibble::as_tibble()
        })
        dplyr::bind_cols(new_tb, draws_tb)
    }) %>% dplyr::mutate(dplyr::across(dplyr::starts_with("treatment_status_sim_"), 
        ~dplyr::case_when(.x == 1 ~ "Discharged", .x == 2 ~ "Treatment", 
            .x == 3 ~ "Waitlist", T ~ .x)))
    Y_Ready4useDyad@ds_tb <- data_tb
    Y_Ready4useDyad <- add_simulated_data(model_mdl = NULL, var_1L_chr = "treatment_status", 
        Y_Ready4useDyad = Y_Ready4useDyad, iterations_int = iterations_int, 
        type_1L_chr = "fourth", what_1L_chr = "new")
    if (tidy_1L_lgl) {
        Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% add_project_treatment_change(arrange_by_id_lgl = F, 
            timepoint_bl_1L_chr = paste0("Week", bl_week_1L_dbl), 
            wide_1L_lgl = T) %>% dplyr::mutate(`:=`(!!rlang::sym(status_var_1L_chr), 
            as.factor(as.character(!!rlang::sym(status_var_1L_chr)))))
    }
    return(Y_Ready4useDyad)
}
#' Add time to event
#' @description add_time_to_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add time to event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param event_1L_chr Event (a character vector of length one)
#' @param schedule_args_ls Schedule arguments (a list), Default: list()
#' @param schedule_fn Schedule (a function), Default: NULL
#' @param step_dtm Step (a date vector), Default: lubridate::days(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_time_to_event
#' @export 
#' @importFrom lubridate days
#' @importFrom dplyr mutate
#' @importFrom rlang exec
#' @keywords internal
add_time_to_event <- function (X_Ready4useDyad, event_1L_chr, schedule_args_ls = list(), 
    schedule_fn = NULL, step_dtm = lubridate::days(0)) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(NextEvent = event_1L_chr))
    if (is.null(schedule_fn)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(ScheduledFor = CurrentDate + 
                step_dtm))
    }
    else {
        X_Ready4useDyad <- rlang::exec(schedule_fn, X_Ready4useDyad, 
            !!!schedule_args_ls)
    }
    return(X_Ready4useDyad)
}
#' Add treatment event
#' @description add_treatment_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add treatment event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param tx_models_ls Treatment models (a list)
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: -2
#' @param bl_week_1L_dbl Baseline week (a double vector of length one), Default: 0
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param measurement_1L_int Measurement (an integer vector of length one), Default: integer(0)
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_treatment_event
#' @export 
#' @importFrom lubridate weeks time_length
#' @importFrom dplyr mutate case_when
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
#' @keywords internal
add_treatment_event <- function (X_Ready4useDyad, tx_models_ls, adjustment_1L_dbl = -2, 
    bl_week_1L_dbl = 0, iterations_int = 1:100L, measurement_1L_int = integer(0), 
    tx_duration_dtm = lubridate::weeks(12)) 
{
    if (!"Period" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(Period = "0 to 0 Weeks"))
    }
    if (!"treatment_change" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_change = NA_character_))
    }
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = c("treatment_status", 
        "treatment_change"))
    if (identical(measurement_1L_int, integer(0))) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(MeasurementWeek = paste0("Week", 
                adjustment_1L_dbl + (lubridate::time_length((CurrentDate - 
                  StartDate), unit = "day")/7))))
    }
    else {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(MeasurementWeek = paste0("Week", 
                measurement_1L_int)))
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(Adult = dplyr::case_when(Age < 18 ~ FALSE, 
            Age >= 1 ~ TRUE, T ~ NA_real_), Period = paste0(Period %>% 
            strsplit(" ") %>% purrr::map_chr(~.x[3]), " to ", 
            MeasurementWeek %>% stringr::str_replace_all("Week", 
                ""), " Weeks")))
    X_Ready4useDyad <- add_simulated_treatments(tx_models_ls, 
        Y_Ready4useDyad = X_Ready4useDyad, bl_week_1L_dbl = bl_week_1L_dbl, 
        iterations_int = iterations_int, tidy_1L_lgl = T)
    X_Ready4useDyad <- update_tx_start_end(X_Ready4useDyad, tx_duration_dtm = tx_duration_dtm) %>% 
        update_order()
    return(X_Ready4useDyad)
}
#' Add treatment status
#' @description add_treatment_status() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add treatment status. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: c("category", "name")
#' @param ctg_1L_chr Category (a character vector of length one), Default: 'Service'
#' @param group_by_1L_chr Group by (a character vector of length one), Default: character(0)
#' @param source_vars_chr Source variables (a character vector), Default: c("treatment_stage", "stage_of_treatment")
#' @param three_levels_1L_lgl Three levels (a logical vector of length one), Default: FALSE
#' @param type_1L_int Type (an integer vector of length one), Default: 1L:2L
#' @param update_dict_1L_lgl Update dictionary (a logical vector of length one), Default: TRUE
#' @param var_1L_chr Variable (a character vector of length one), Default: 'treatment_status'
#' @return Data (an output object of multiple potential types)
#' @rdname add_treatment_status
#' @export 
#' @importFrom dplyr mutate case_when group_by n ungroup
#' @importFrom rlang sym
#' @importFrom purrr map_chr
#' @importFrom ready4use add_dictionary renew.ready4use_dictionary
#' @keywords internal
add_treatment_status <- function (data_xx, arrange_by_1L_chr = c("category", "name"), 
    ctg_1L_chr = "Service", group_by_1L_chr = character(0), source_vars_chr = c("treatment_stage", 
        "stage_of_treatment"), three_levels_1L_lgl = FALSE, type_1L_int = 1L:2L, 
    update_dict_1L_lgl = TRUE, var_1L_chr = "treatment_status") 
{
    X_Ready4useDyad <- transform_data_fmt(data_xx, type_1L_chr = "input")
    if (1L %in% type_1L_int) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                dplyr::case_when(!!rlang::sym(source_vars_chr[1]) %in% 
                  "WAITING_F2F" ~ "Waitlist", !!rlang::sym(source_vars_chr[1]) %in% 
                  c("APPROACHING_DISCHARGE", "RECEIVING_F2F") ~ 
                  "Treatment", !!rlang::sym(source_vars_chr[1]) %in% 
                  "DISCHARGED" ~ "Discharged", !!rlang::sym(source_vars_chr[1]) %in% 
                  "SUBTHRESHOLD_DISCHARGE" ~ "Other", T ~ !!rlang::sym(source_vars_chr[1])) %>% 
                  as.factor())))
    }
    if (2L %in% type_1L_int) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                !!rlang::sym(source_vars_chr[2]) %>% purrr::map_chr(~ifelse(is.na(.x), 
                  NA_character_, switch(as.integer(.x), "Waitlist", 
                    "Treatment", "Discharged", "Other"))) %>% 
                  as.factor())))
    }
    if (three_levels_1L_lgl) {
        ds_tb <- X_Ready4useDyad@ds_tb
        if (!identical(group_by_1L_chr, character(0))) {
            ds_tb <- dplyr::group_by(ds_tb, !!rlang::sym(group_by_1L_chr))
        }
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            as.character(!!rlang::sym(var_1L_chr))))
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            dplyr::case_when(!!rlang::sym(var_1L_chr) == "Other" ~ 
                !!rlang::sym(var_1L_chr) %>% setdiff("Other") %>% 
                  sample(size = dplyr::n(), replace = T), T ~ 
                !!rlang::sym(var_1L_chr))))
        if (!identical(group_by_1L_chr, character(0))) {
            ds_tb <- dplyr::ungroup(ds_tb)
        }
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            as.factor(!!rlang::sym(var_1L_chr))))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            ds_tb)
    }
    if (update_dict_1L_lgl) {
        X_Ready4useDyad <- X_Ready4useDyad %>% ready4use::add_dictionary(new_cases_r3 = ready4use_dictionary() %>% 
            ready4use::renew.ready4use_dictionary(var_nm_chr = var_1L_chr, 
                var_ctg_chr = ctg_1L_chr, var_desc_chr = "Treatment status", 
                var_type_chr = "factor"), arrange_by_1L_chr = arrange_by_1L_chr)
    }
    data_xx <- transform_data_fmt(data_xx, X_Ready4useDyad = X_Ready4useDyad)
    return(data_xx)
}
#' Add utility event
#' @description add_utility_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add utility event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param add_qalys_1L_lgl Add Quality Adjusted Life Years (a logical vector of length one), Default: FALSE
#' @param add_sensitivity_1L_lgl Add sensitivity (a logical vector of length one), Default: FALSE
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: 0
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: integer(0)
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param maintain_for_1L_int Maintain for (an integer vector of length one), Default: 0
#' @param models_ls Models (a list), Default: NULL
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param simulate_1L_lgl Simulate (a logical vector of length one), Default: TRUE
#' @param tfmn_ls Transformation (a list), Default: NULL
#' @param tidy_1L_lgl Tidy (a logical vector of length one), Default: TRUE
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL6D", "CHU9D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project")
#' @param what_1L_chr What (a character vector of length one), Default: c("old", "new")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_utility_event
#' @export 
#' @importFrom purrr reduce pluck
#' @importFrom dplyr mutate across
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @keywords internal
add_utility_event <- function (X_Ready4useDyad, add_qalys_1L_lgl = FALSE, add_sensitivity_1L_lgl = FALSE, 
    adjustment_1L_dbl = 0, follow_up_1L_int = integer(0), iterations_int = 1:100L, 
    maintain_for_1L_int = 0L, models_ls = NULL, sensitivities_ls = make_sensitivities_ls(), 
    simulate_1L_lgl = TRUE, tfmn_ls = NULL, tidy_1L_lgl = TRUE, 
    utilities_chr = c("AQoL6D", "CHU9D"), type_1L_chr = c("Model", 
        "Project"), what_1L_chr = c("old", "new")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (type_1L_chr == "Project") {
        suffix_1L_chr <- paste0("_", names(sensitivities_ls$outcomes_ls)[1])
    }
    else {
        suffix_1L_chr <- make_weeks_suffix(X_Ready4useDyad, adjustment_1L_dbl = adjustment_1L_dbl, 
            follow_up_1L_int = follow_up_1L_int)
    }
    X_Ready4useDyad <- utilities_chr %>% purrr::reduce(.init = X_Ready4useDyad, 
        ~{
            Y_Ready4useDyad <- .x
            if (!paste0(.y, "_change") %in% names(Y_Ready4useDyad@ds_tb)) {
                Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, 
                  "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, 
                    "_change")), 0)))
            }
            if (!paste0(.y, "_date") %in% names(Y_Ready4useDyad@ds_tb)) {
                Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, 
                  "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, 
                    "_date")), StartDate)))
            }
            update_previous(Y_Ready4useDyad, modifiable_chr = c(.y, 
                paste0(.y, "_change"), paste0(.y, "_date")))
        })
    X_Ready4useDyad <- utilities_chr %>% purrr::reduce(.init = X_Ready4useDyad, 
        ~{
            if (type_1L_chr == "Model" & simulate_1L_lgl) {
                Y_Ready4useDyad <- add_simulated_data(models_ls %>% 
                  purrr::pluck(paste0(.y, "_mdl")), var_1L_chr = paste0(.y, 
                  suffix_1L_chr), Y_Ready4useDyad = .x, iterations_int = iterations_int, 
                  join_with_chr = "Iteration", type_1L_chr = "second", 
                  what_1L_chr = what_1L_chr)
                Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, 
                  "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                    !!rlang::sym(paste0(.y, suffix_1L_chr)))))
            }
            else {
                if (type_1L_chr == "Project") {
                  Y_Ready4useDyad <- add_outcome_sensitivity(.x, 
                    outcome_1L_chr = .y, sensitivities_ls = sensitivities_ls, 
                    tfmn_ls = tfmn_ls)
                  Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, 
                    "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                      !!rlang::sym(paste0(.y, suffix_1L_chr)))))
                }
                else {
                  Y_Ready4useDyad <- renewSlot(.x, "ds_tb", .x@ds_tb %>% 
                    dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, 
                      suffix_1L_chr)), !!rlang::sym(.y))))
                }
            }
            tfmn_fn <- tfmn_ls %>% purrr::pluck(.y)
            renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% 
                dplyr::mutate(dplyr::across(tidyselect::any_of(paste0(.y, 
                  c("", suffix_1L_chr))), ~tfmn_fn(.x))) %>% 
                dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, "_change")), 
                  as.numeric((!!rlang::sym(.y) - !!rlang::sym(paste0(.y, 
                    "_previous"))))), `:=`(!!rlang::sym(paste0(.y, 
                  "_date")), CurrentDate)) %>% dplyr::mutate(dplyr::across(tidyselect::any_of(paste0(.y, 
                c("", "_start", "_previous"))), ~tfmn_fn(.x))))
        })
    if (add_qalys_1L_lgl) {
        X_Ready4useDyad <- update_qalys(X_Ready4useDyad, add_sensitivity_1L_lgl = add_sensitivity_1L_lgl, 
            adjustment_1L_dbl = adjustment_1L_dbl, follow_up_1L_int = follow_up_1L_int, 
            maintain_for_1L_int = maintain_for_1L_int, tidy_1L_lgl = tidy_1L_lgl, 
            utilities_chr = utilities_chr)
    }
    return(X_Ready4useDyad)
}
