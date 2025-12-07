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
#' Add Assessment of Quality of Life Eight Dimension from K10
#' @description add_aqol8d_from_k10() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add assessment of quality of life eight dimension from k10. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param norway_1L_lgl Norway (a logical vector of length one), Default: FALSE
#' @param source_1L_chr Source (a character vector of length one), Default: c("10.1192/bjp.bp.113.136036")
#' @param tidy_cols_1L_lgl Tidy columns (a logical vector of length one), Default: FALSE
#' @param var_1L_chr Variable (a character vector of length one), Default: 'AQoL8D'
#' @return Data (an output object of multiple potential types)
#' @rdname add_aqol8d_from_k10
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom serious transform_data_fmt
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @importFrom purrr map_dbl
#' @keywords internal
add_aqol8d_from_k10 <- function (data_xx, correspondences_r3 = ready4show::ready4show_correspondences(), 
    norway_1L_lgl = FALSE, source_1L_chr = c("10.1192/bjp.bp.113.136036"), 
    tidy_cols_1L_lgl = FALSE, var_1L_chr = "AQoL8D") 
{
    X_Ready4useDyad <- serious::transform_data_fmt(data_xx, type_1L_chr = "input")
    data_tb <- X_Ready4useDyad@ds_tb
    if (identical(correspondences_r3, ready4show::ready4show_correspondences())) {
        correspondences_r3 <- correspondences_r3 %>% ready4show::renew.ready4show_correspondences(old_nms_chr = c("K10"), 
            new_nms_chr = c("K10"))
    }
    test_1L_lgl <- assertthat::assert_that(length(intersect(correspondences_r3$old_nms_chr, 
        c("K10"))) == 1)
    correspondences_r3 <- correspondences_r3 %>% dplyr::filter(old_nms_chr %in% 
        c("K10"))
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
        !!rlang::sym(correspondences_r3$new_nms_chr[1]) %>% purrr::map_dbl(~{
            k10_1L_dbl <- .x
            calculate_aqol8d_from_k10(k10_1L_dbl = k10_1L_dbl, 
                norway_1L_lgl = norway_1L_lgl, source_1L_chr = source_1L_chr)
        })))
    X_Ready4useDyad@ds_tb <- data_tb
    if (tidy_cols_1L_lgl) {
        X_Ready4useDyad <- update_order(X_Ready4useDyad, type_1L_chr = "columns")
    }
    data_xx <- serious::transform_data_fmt(data_xx, X_Ready4useDyad = X_Ready4useDyad)
    return(data_xx)
}
#' Add box conditionally
#' @description add_box_conditionally() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add box conditionally. The function returns Table (an output object of multiple potential types).
#' @param table_xx Table (an output object of multiple potential types)
#' @param html_table_fn Html table (a function), Default: identity
#' @param output_type_1L_chr Output type (a character vector of length one), Default: c("HTML", "PDF", "Word")
#' @param pdf_table_fn Pdf table (a function), Default: identity
#' @param word_table_fn Word table (a function), Default: flextable::theme_box
#' @return Table (an output object of multiple potential types)
#' @rdname add_box_conditionally
#' @export 
#' @importFrom flextable theme_box
#' @importFrom ready4show add_tfmn_for_fmt make_table_fns_ls
#' @keywords internal
add_box_conditionally <- function (table_xx, html_table_fn = identity, output_type_1L_chr = c("HTML", 
    "PDF", "Word"), pdf_table_fn = identity, word_table_fn = flextable::theme_box) 
{
    output_type_1L_chr <- match.arg(output_type_1L_chr)
    table_xx <- table_xx %>% ready4show::add_tfmn_for_fmt(output_type_1L_chr = output_type_1L_chr, 
        tfmns_fn_ls = ready4show::make_table_fns_ls(html_table_fn = html_table_fn, 
            pdf_table_fn = pdf_table_fn, word_table_fn = word_table_fn))
    return(table_xx)
}
#' Add clients to summary
#' @description add_clients_to_summary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add clients to summary. The function returns Summaries (a list).
#' @param summaries_ls Summaries (a list)
#' @param onboarded_tb Onboarded (a tibble)
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: character(0)
#' @param reference_1L_chr Reference (a character vector of length one), Default: 'Status quo'
#' @return Summaries (a list)
#' @rdname add_clients_to_summary
#' @export 
#' @importFrom dplyr select where mutate left_join arrange
#' @importFrom purrr reduce map_chr map2_chr
#' @importFrom stringr str_extract str_remove
#' @importFrom rlang sym
#' @keywords internal
add_clients_to_summary <- function (summaries_ls, onboarded_tb, arrange_1L_chr = character(0), 
    reference_1L_chr = "Status quo") 
{
    summaries_ls$scenarios_ls$clients_tb <- summaries_ls$scenarios_ls$retained_tb %>% 
        dplyr::select(dplyr::where(is.numeric)) %>% names() %>% 
        purrr::reduce(.init = summaries_ls$scenarios_ls$retained_tb %>% 
            dplyr::mutate(Group = stringr::str_extract(Scenario, 
                ",.*") %>% purrr::map_chr(~ifelse(is.na(.x), 
                "", .x))) %>% dplyr::mutate(Scenario = Scenario %>% 
            purrr::map2_chr(Group, ~ifelse(.y != "", stringr::str_remove(.x, 
                .y), .x))) %>% dplyr::left_join(onboarded_tb) %>% 
            dplyr::mutate(Scenario = paste0(Scenario, Group)) %>% 
            dplyr::select(-Group), ~{
            name_1L_chr <- .y
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(name_1L_chr), 
                !!rlang::sym(name_1L_chr) + Onboarded))
        }) %>% dplyr::select(-Onboarded)
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
#' @param add_logic_fn Add logic (a function), Default: identity
#' @param add_offsets_1L_lgl Add offsets (a logical vector of length one), Default: FALSE
#' @param add_variable_1L_lgl Add variable (a logical vector of length one), Default: TRUE
#' @param base_for_rates_int Base for rates (an integer vector), Default: 1
#' @param offsets_chr Offsets (a character vector), Default: character(0)
#' @param variable_unit_1L_chr Variable unit (a character vector of length one), Default: 'Minutes'
#' @return Data (a tibble)
#' @rdname add_cost_calculations
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_cost_calculations <- function (data_tb, inputs_ls, add_fixed_1L_lgl = FALSE, add_logic_fn = identity, 
    add_offsets_1L_lgl = FALSE, add_variable_1L_lgl = TRUE, base_for_rates_int = 1L, 
    offsets_chr = character(0), variable_unit_1L_chr = "Minutes") 
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
    data_tb <- add_unset_vars(data_tb, var_names_chr = c(cost_names_chr, 
        offset_counts_chr, offset_costs_chr), value_xx = 0)
    if (add_variable_1L_lgl) {
        data_tb <- 1:length(scenarios_chr) %>% purrr::reduce(.init = data_tb, 
            ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(cost_names_chr[.y]), 
                !!rlang::sym(cost_names_chr[.y]) + !!rlang::sym(variable_unit_1L_chr) * 
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
        data_tb <- add_cost_offsets(data_tb, add_logic_fn = add_logic_fn, 
            offsets_chr = offsets_chr, base_for_rates_int = base_for_rates_int, 
            inputs_ls = inputs_ls)
    }
    return(data_tb)
}
#' Add cost effectiveness
#' @description add_cost_effectiveness() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cost effectiveness. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param cost_1L_chr Cost (a character vector of length one), Default: 'Cost'
#' @param dominance_1L_chr Dominance (a character vector of length one), Default: character(0)
#' @param effect_1L_chr Effect (a character vector of length one), Default: 'QALYs'
#' @param icer_1L_chr Icer (a character vector of length one), Default: character(0)
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @return Data (a tibble)
#' @rdname add_cost_effectiveness
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @importFrom purrr map2_dbl pmap_dbl
#' @keywords internal
add_cost_effectiveness <- function (data_tb, cost_1L_chr = "Cost", dominance_1L_chr = character(0), 
    effect_1L_chr = "QALYs", icer_1L_chr = character(0), suffix_1L_chr = "", 
    threshold_1L_dbl = 96000) 
{
    if (identical(icer_1L_chr, character(0))) {
        icer_1L_chr <- paste0("ICER", suffix_1L_chr)
    }
    if (identical(dominance_1L_chr, character(0))) {
        dominance_1L_chr <- paste0("Dominance", suffix_1L_chr)
    }
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("CE", 
        suffix_1L_chr)), !!rlang::sym(icer_1L_chr) %>% purrr::map2_dbl(!!rlang::sym(dominance_1L_chr), 
        ~ifelse(.y == "Ratio", ifelse(.x <= threshold_1L_dbl, 
            1, 0), ifelse(.y == "Dominant", 1, 0)))))
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("CE", 
        suffix_1L_chr)), data_tb %>% dplyr::select(!!rlang::sym(paste0("CE", 
        suffix_1L_chr)), !!rlang::sym(cost_1L_chr), !!rlang::sym(effect_1L_chr), 
        !!rlang::sym(dominance_1L_chr)) %>% purrr::pmap_dbl(~ifelse(..2 < 
        0 & ..3 < 0, (!(..1 == 1)) %>% as.numeric(), ..1))))
    return(data_tb)
}
#' Add cost effectiveness statistics
#' @description add_cost_effectiveness_stats() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cost effectiveness statistics. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL6D", "CHU9D")
#' @return Data (a tibble)
#' @rdname add_cost_effectiveness_stats
#' @export 
#' @importFrom purrr reduce
#' @keywords internal
add_cost_effectiveness_stats <- function (data_tb, threshold_1L_dbl = 96000, utilities_chr = c("AQoL6D", 
    "CHU9D")) 
{
    data_tb <- purrr::reduce(utilities_chr, .init = data_tb, 
        ~{
            utility_1L_chr <- .y
            .x %>% add_dominated(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                ""), suffix_1L_chr = paste0("_", utility_1L_chr, 
                "")) %>% add_dominated(cost_1L_chr = paste0("Cost", 
                "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                "_QALYs_YR1", ""), suffix_1L_chr = paste0("_", 
                utility_1L_chr, "_S10")) %>% add_dominated(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                "_S1"), suffix_1L_chr = paste0("_", utility_1L_chr, 
                "_S01")) %>% add_dominated(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                "_S2"), suffix_1L_chr = paste0("_", utility_1L_chr, 
                "_S02")) %>% add_dominated(cost_1L_chr = paste0("Cost", 
                "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                "_QALYs_YR1", "_S1"), suffix_1L_chr = paste0("_", 
                utility_1L_chr, "_S11")) %>% add_dominated(cost_1L_chr = paste0("Cost", 
                "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                "_QALYs_YR1", "_S2"), suffix_1L_chr = paste0("_", 
                utility_1L_chr, "_S12")) %>% add_icer(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                ""), suffix_1L_chr = paste0("_", utility_1L_chr, 
                "")) %>% add_icer(cost_1L_chr = paste0("Cost", 
                "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                "_QALYs_YR1", ""), suffix_1L_chr = paste0("_", 
                utility_1L_chr, "_S10")) %>% add_icer(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                "_S1"), suffix_1L_chr = paste0("_", utility_1L_chr, 
                "_S01")) %>% add_icer(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                "_S2"), suffix_1L_chr = paste0("_", utility_1L_chr, 
                "_S02")) %>% add_icer(cost_1L_chr = paste0("Cost", 
                "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                "_QALYs_YR1", "_S1"), suffix_1L_chr = paste0("_", 
                utility_1L_chr, "_S11")) %>% add_icer(cost_1L_chr = paste0("Cost", 
                "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                "_QALYs_YR1", "_S2"), suffix_1L_chr = paste0("_", 
                utility_1L_chr, "_S12")) %>% add_cost_effectiveness(cost_1L_chr = paste0("Cost", 
                ""), effect_1L_chr = paste0(utility_1L_chr, "_QALYs_YR1", 
                ""), suffix_1L_chr = paste0("_", utility_1L_chr, 
                ""), threshold_1L_dbl = threshold_1L_dbl) %>% 
                add_cost_effectiveness(cost_1L_chr = paste0("Cost", 
                  "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                  "_QALYs_YR1", ""), suffix_1L_chr = paste0("_", 
                  utility_1L_chr, "_S10"), threshold_1L_dbl = threshold_1L_dbl) %>% 
                add_cost_effectiveness(cost_1L_chr = paste0("Cost", 
                  ""), effect_1L_chr = paste0(utility_1L_chr, 
                  "_QALYs_YR1", "_S1"), suffix_1L_chr = paste0("_", 
                  utility_1L_chr, "_S01"), threshold_1L_dbl = threshold_1L_dbl) %>% 
                add_cost_effectiveness(cost_1L_chr = paste0("Cost", 
                  ""), effect_1L_chr = paste0(utility_1L_chr, 
                  "_QALYs_YR1", "_S2"), suffix_1L_chr = paste0("_", 
                  utility_1L_chr, "_S02"), threshold_1L_dbl = threshold_1L_dbl) %>% 
                add_cost_effectiveness(cost_1L_chr = paste0("Cost", 
                  "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                  "_QALYs_YR1", "_S1"), suffix_1L_chr = paste0("_", 
                  utility_1L_chr, "_S11"), threshold_1L_dbl = threshold_1L_dbl) %>% 
                add_cost_effectiveness(cost_1L_chr = paste0("Cost", 
                  "_S1"), effect_1L_chr = paste0(utility_1L_chr, 
                  "_QALYs_YR1", "_S2"), suffix_1L_chr = paste0("_", 
                  utility_1L_chr, "_S12"), threshold_1L_dbl = threshold_1L_dbl)
        })
    return(data_tb)
}
#' Add cost offsets
#' @description add_cost_offsets() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cost offsets. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param inputs_ls Inputs (a list)
#' @param offsets_chr Offsets (a character vector)
#' @param add_logic_fn Add logic (a function), Default: identity
#' @param base_for_rates_int Base for rates (an integer vector), Default: 1
#' @return Data (a tibble)
#' @rdname add_cost_offsets
#' @export 
#' @importFrom purrr reduce map_int map_dfr
#' @importFrom stringr str_remove
#' @importFrom dplyr group_by summarise n first mutate left_join select
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' @keywords internal
add_cost_offsets <- function (data_tb, inputs_ls, offsets_chr, add_logic_fn = identity, 
    base_for_rates_int = 1L) 
{
    cost_names_chr <- get_unit_cost_detail(inputs_ls$unit_costs_tb, 
        what_1L_chr = "names")
    scenarios_chr <- get_unit_cost_detail(inputs_ls$unit_costs_tb, 
        what_1L_chr = "scenarios")
    offset_counts_chr <- paste0("OffsetCount", offsets_chr)
    offset_costs_chr <- paste0("OffsetCosts", offsets_chr)
    offset_logic_chr <- paste0("OffsetLogic", offsets_chr)
    data_tb <- add_unset_vars(data_tb, var_names_chr = c(offset_counts_chr, 
        offset_costs_chr), value_xx = 0)
    data_tb <- add_unset_vars(data_tb, var_names_chr = offset_logic_chr, 
        value_xx = 1)
    data_tb <- add_logic_fn(data_tb)
    if (length(base_for_rates_int) == 1 & length(base_for_rates_int) < 
        length(offsets_chr)) {
        base_for_rates_int <- rep(base_for_rates_int, length(offset_costs_chr))
    }
    data_tb <- 1:length(offsets_chr) %>% purrr::reduce(.init = data_tb, 
        ~{
            rate_1L_chr <- paste0("ParamPool", offsets_chr[.y])
            mean_1L_chr <- paste0("Param", offsets_chr[.y], "OOSCost_mean")
            sd_1L_chr <- paste0("Param", offsets_chr[.y], "OOSCost_sd")
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
                "_new")), !!rlang::sym(paste0(count_1L_chr, "_new")) * 
                rates_tb[[.x, unit_1L_chr]])))
            joined_tb <- .x %>% dplyr::left_join(join_tb) %>% 
                dplyr::mutate(`:=`(!!rlang::sym(count_1L_chr), 
                  !!rlang::sym(count_1L_chr) + (!!rlang::sym(paste0(count_1L_chr, 
                    "_new")) * !!rlang::sym(offset_logic_chr[.y]))), 
                  `:=`(!!rlang::sym(cost_1L_chr), !!rlang::sym(cost_1L_chr) + 
                    (!!rlang::sym(paste0(cost_1L_chr, "_new")) * 
                      !!rlang::sym(offset_logic_chr[.y])))) %>% 
                dplyr::select(-tidyselect::all_of(c(paste0(count_1L_chr, 
                  "_new"), paste0(cost_1L_chr, "_new"))))
            1:length(scenarios_chr) %>% purrr::reduce(.init = joined_tb, 
                ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(cost_names_chr[.y]), 
                  !!rlang::sym(cost_names_chr[.y]) + !!rlang::sym(cost_1L_chr))))
        })
    return(data_tb)
}
#' Add costs event
#' @description add_costs_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add costs event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param inputs_ls Inputs (a list)
#' @param add_logic_fn Add logic (a function), Default: identity
#' @param add_offsets_1L_lgl Add offsets (a logical vector of length one), Default: FALSE
#' @param base_for_rates_int Base for rates (an integer vector), Default: 1
#' @param offsets_chr Offsets (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("variable", "fixed", "both", "zero")
#' @param variable_unit_1L_chr Variable unit (a character vector of length one), Default: 'Minutes'
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_costs_event
#' @export 
#' @keywords internal
add_costs_event <- function (X_Ready4useDyad, inputs_ls, add_logic_fn = identity, 
    add_offsets_1L_lgl = FALSE, base_for_rates_int = 1L, offsets_chr = character(0), 
    type_1L_chr = c("variable", "fixed", "both", "zero"), variable_unit_1L_chr = "Minutes") 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "zero") {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% add_cost_calculations(inputs_ls = inputs_ls, 
                add_fixed_1L_lgl = F, add_logic_fn = add_logic_fn, 
                add_offsets_1L_lgl = add_offsets_1L_lgl, add_variable_1L_lgl = F, 
                base_for_rates_int = base_for_rates_int, offsets_chr = offsets_chr, 
                variable_unit_1L_chr = variable_unit_1L_chr))
    }
    else {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% add_cost_calculations(inputs_ls = inputs_ls, 
                add_fixed_1L_lgl = (type_1L_chr %in% c("fixed", 
                  "both")), add_variable_1L_lgl = (type_1L_chr %in% 
                  c("variable", "both")), add_logic_fn = add_logic_fn, 
                add_offsets_1L_lgl = add_offsets_1L_lgl, base_for_rates_int = base_for_rates_int, 
                offsets_chr = offsets_chr, variable_unit_1L_chr = variable_unit_1L_chr))
    }
    return(X_Ready4useDyad)
}
#' Add costs to summary
#' @description add_costs_to_summary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add costs to summary. The function returns Summaries (a list).
#' @param summaries_ls Summaries (a list)
#' @param processed_ls Processed (a list)
#' @param periods_1L_int Periods (an integer vector of length one), Default: 26
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: character(0)
#' @param bind_to_tb Bind to (a tibble), Default: NULL
#' @param cost_tfmn_fn Cost transformation (a function), Default: identity
#' @param reference_1L_chr Reference (a character vector of length one), Default: 'Status quo'
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param type_1L_chr Type (a character vector of length one), Default: c("change_scenario", "change_scen_sq", "change_sq", "scenario", 
#'    "scen_sq", "sq", "summary_scenario")
#' @param unit_1L_chr Unit (a character vector of length one), Default: c("minutes", "clients")
#' @return Summaries (a list)
#' @rdname add_costs_to_summary
#' @export 
#' @importFrom purrr map_dfr pluck map reduce map2_dbl
#' @importFrom dplyr filter mutate across where arrange bind_rows pull summarise n left_join select
#' @importFrom ready4 get_from_lup_obj
#' @importFrom rlang sym
#' @importFrom lubridate weeks
#' @importFrom tibble tibble add_case
#' @keywords internal
add_costs_to_summary <- function (summaries_ls, processed_ls, periods_1L_int = 26, arrange_1L_chr = character(0), 
    bind_to_tb = NULL, cost_tfmn_fn = identity, reference_1L_chr = "Status quo", 
    tfmn_fn = identity, type_1L_chr = c("change_scenario", "change_scen_sq", 
        "change_sq", "scenario", "scen_sq", "sq", "summary_scenario"), 
    unit_1L_chr = c("minutes", "clients")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    unit_1L_chr <- match.arg(unit_1L_chr)
    if (type_1L_chr %in% c("scen_sq", "sq")) {
        costs_sq_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            make_forecast_growth(summaries_ls$scenarios_ls %>% 
                purrr::pluck(paste0(unit_1L_chr, "_tb")) %>% 
                dplyr::filter(Scenario == reference_1L_chr), 
                reference_1L_dbl = ifelse(unit_1L_chr == "clients", 
                  summaries_ls$empirical_ls$onboarded_1L_dbl/periods_1L_int + 
                    summaries_ls$empirical_ls$retained_1L_dbl, 
                  summaries_ls$empirical_ls$minutes_1L_dbl), 
                tfmn_fn = identity) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~processed_ls$costs_unit@ds_tb %>% dplyr::filter(Scenario == 
                  cost_scenario_1L_chr) %>% ready4::get_from_lup_obj(match_var_nm_1L_chr = "Type", 
                  match_value_xx = "Variable", target_var_nm_1L_chr = "TotalCost") * 
                  .x)) %>% dplyr::mutate(Scenario = paste0(Scenario, 
                ", ", cost_scenario_1L_chr))
        })
        if (!identical(arrange_1L_chr, character(0))) {
            costs_sq_tb <- costs_sq_tb %>% dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        costs_sq_tb <- costs_sq_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~tfmn_fn(.x)))
        if (!is.null(bind_to_tb)) {
            costs_sq_tb <- dplyr::bind_rows(bind_to_tb, costs_sq_tb)
        }
        if (type_1L_chr == "scen_sq") {
            summaries_ls$scenarios_ls$costs_scen_sq_tb <- costs_sq_tb
        }
        else {
            summaries_ls$scenarios_ls$costs_sq_tb <- costs_sq_tb
        }
    }
    if (type_1L_chr %in% c("change_sq", "change_scen_sq")) {
        unformatted_ls <- add_costs_to_summary(summaries_ls, 
            processed_ls = processed_ls, periods_1L_int = periods_1L_int, 
            reference_1L_chr = reference_1L_chr, type_1L_chr = "sq", 
            unit_1L_chr = unit_1L_chr)
        costs_change_sq_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
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
            costs_change_sq_tb <- costs_change_sq_tb %>% dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        costs_change_sq_tb <- costs_change_sq_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~tfmn_fn(.x)))
        if (!is.null(bind_to_tb)) {
            costs_change_sq_tb <- dplyr::bind_rows(bind_to_tb, 
                costs_change_sq_tb)
        }
        if (type_1L_chr == "change_scen_sq") {
            summaries_ls$scenarios_ls$costs_change_scen_sq_tb <- costs_change_sq_tb
        }
        else {
            summaries_ls$scenarios_ls$costs_change_sq_tb <- costs_change_sq_tb
        }
    }
    if (type_1L_chr == "scenario") {
        summaries_ls$scenarios_ls$costs_scenarios_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            make_forecast_growth(summaries_ls$scenarios_ls %>% 
                purrr::pluck(paste0(unit_1L_chr, "_tb")), reference_1L_chr = reference_1L_chr, 
                tfmn_fn = identity) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
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
        if (!is.null(bind_to_tb)) {
            summaries_ls$scenarios_ls$costs_scenarios_tb <- dplyr::bind_rows(bind_to_tb, 
                summaries_ls$scenarios_ls$costs_scenarios_tb)
        }
    }
    if (type_1L_chr == "change_scenario") {
        unformatted_ls <- add_costs_to_summary(summaries_ls, 
            processed_ls = processed_ls, periods_1L_int = periods_1L_int, 
            reference_1L_chr = reference_1L_chr, type_1L_chr = "scenario", 
            unit_1L_chr = unit_1L_chr)
        summaries_ls$scenarios_ls$costs_change_scenarios_tb <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map_dfr(~{
            cost_scenario_1L_chr <- .x
            total_cost_1L_dbl <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == cost_scenario_1L_chr) %>% 
                dplyr::pull(TotalCost) %>% sum()
            unformatted_ls$scenarios_ls$costs_scenarios_tb %>% 
                dplyr::filter(Scenario %>% endsWith(paste0(", ", 
                  cost_scenario_1L_chr))) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~.x/total_cost_1L_dbl))
        })
        if (!identical(arrange_1L_chr, character(0))) {
            summaries_ls$scenarios_ls$costs_change_scenarios_tb <- summaries_ls$scenarios_ls$costs_change_scenarios_tb %>% 
                dplyr::arrange(!!rlang::sym(arrange_1L_chr))
        }
        summaries_ls$scenarios_ls$costs_change_scenarios_tb <- summaries_ls$scenarios_ls$costs_change_scenarios_tb %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~tfmn_fn(.x)))
        if (!is.null(bind_to_tb)) {
            summaries_ls$scenarios_ls$costs_change_scenarios_tb <- dplyr::bind_rows(bind_to_tb, 
                summaries_ls$scenarios_ls$costs_change_scenarios_tb)
        }
    }
    if (type_1L_chr %in% c("summary_scenario")) {
        X_Ready4useDyad <- make_report_data(processed_ls = processed_ls, 
            date_end_dtm = as.POSIXct("2023-07-31"), date_start_dtm = as.POSIXct("2023-07-01"), 
            period_dtm = lubridate::weeks(14), platform_1L_chr = "MOST", 
            what_1L_chr = "serviceusecost")
        some_1L_dbl <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(Some = Minutes > 
            0) %>% dplyr::summarise(CumulativeMinutes = sum(Minutes), 
            Minutes = mean(Minutes), Some = sum(Some), N = dplyr::n()) %>% 
            dplyr::mutate(`Minutes, if >0 Minutes` = CumulativeMinutes/Some, 
                `% with >0 Minutes` = Some/N) %>% dplyr::pull(`% with >0 Minutes`)
        costs_scenarios_ls <- processed_ls$costs_unit@ds_tb$Scenario %>% 
            unique() %>% purrr::map(~{
            scenario_1L_chr <- .x
            cost_1L_dbl <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == scenario_1L_chr) %>% 
                dplyr::pull(TotalCost) %>% sum() %>% cost_tfmn_fn()
            clients_1L_int <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == scenario_1L_chr, Unit == 
                  "Clients") %>% dplyr::pull(Quantity)
            variable_1L_dbl <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == scenario_1L_chr, Type == 
                  "Variable") %>% dplyr::pull(UnitCost)
            fixed_1L_dbl <- processed_ls$costs_unit@ds_tb %>% 
                dplyr::filter(Scenario == scenario_1L_chr, Type == 
                  "Fixed") %>% dplyr::pull(TotalCost)
            scenario_onboarders_tb <- tibble::tibble(Scale = c(0.5, 
                1, 1.5, 2)) %>% dplyr::mutate(Scenario = paste0(Scale * 
                100, "% of onboarders, ", scenario_1L_chr)) %>% 
                tibble::add_case(Scale = 1, Scenario = paste0(c("Status quo, ", 
                  "Unscaled empirical, "), scenario_1L_chr))
            scenario_minutes_tb <- summaries_ls$scenarios_ls$minutes_tb %>% 
                dplyr::filter(!endsWith(Scenario, "100% of onboarder growth")) %>% 
                dplyr::mutate(Scenario = paste0(Scenario, ", ", 
                  scenario_1L_chr))
            total_scenario_tb <- scenario_minutes_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~fixed_1L_dbl + (.x * variable_1L_dbl)/periods_1L_int/7 * 
                  365.25))
            scenario_clients_tb <- summaries_ls$scenarios_ls$clients_tb %>% 
                dplyr::filter(!endsWith(Scenario, "100% of onboarder growth")) %>% 
                dplyr::mutate(Scenario = paste0(Scenario, ", ", 
                  scenario_1L_chr)) %>% make_forecast_growth(reference_1L_chr = paste0(reference_1L_chr, 
                ", ", scenario_1L_chr), tfmn_fn = identity) %>% 
                dplyr::left_join(scenario_onboarders_tb) %>% 
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                  ~(1 + .x) * clients_1L_int * some_1L_dbl + 
                    clients_1L_int * (1 - some_1L_dbl) * Scale)) %>% 
                dplyr::select(-Scale)
            unit_scenario_tb <- total_scenario_tb %>% dplyr::select(dplyr::where(is.numeric)) %>% 
                names() %>% purrr::reduce(.init = total_scenario_tb, 
                ~{
                  metric_1L_chr <- .y
                  .x %>% dplyr::mutate(`:=`(!!rlang::sym(metric_1L_chr), 
                    !!rlang::sym(metric_1L_chr) %>% purrr::map2_dbl(Scenario, 
                      ~.x/ready4::get_from_lup_obj(scenario_clients_tb, 
                        match_var_nm_1L_chr = "Scenario", match_value_xx = .y, 
                        target_var_nm_1L_chr = metric_1L_chr))))
                })
            unit_change_scenario_tb <- unit_scenario_tb %>% make_forecast_growth(reference_1L_chr = paste0(reference_1L_chr, 
                ", ", scenario_1L_chr), tfmn_fn = identity)
            list(total_scenario_tb = total_scenario_tb, unit_scenario_tb = unit_scenario_tb, 
                unit_change_scenario_tb = unit_change_scenario_tb)
        })
        summaries_ls$scenarios_ls$costs_total_scenario_tb <- costs_scenarios_ls %>% 
            purrr::map_dfr(~.x$total_scenario_tb)
        summaries_ls$scenarios_ls$costs_unit_scenario_tb <- costs_scenarios_ls %>% 
            purrr::map_dfr(~.x$unit_scenario_tb)
        summaries_ls$scenarios_ls$unit_change_scenario_tb <- costs_scenarios_ls %>% 
            purrr::map_dfr(~.x$unit_change_scenario_tb)
    }
    return(summaries_ls)
}
#' Add dominated
#' @description add_dominated() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dominated. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param cost_1L_chr Cost (a character vector of length one), Default: 'Cost'
#' @param effect_1L_chr Effect (a character vector of length one), Default: 'QALYs'
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @return Data (a tibble)
#' @rdname add_dominated
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom purrr map2_chr
#' @keywords internal
add_dominated <- function (data_tb, cost_1L_chr = "Cost", effect_1L_chr = "QALYs", 
    suffix_1L_chr = "") 
{
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("Dominance", 
        suffix_1L_chr)), !!rlang::sym(cost_1L_chr) %>% purrr::map2_chr(!!rlang::sym(effect_1L_chr), 
        ~ifelse(.x > 0 & .y < 0, "Dominated", ifelse(.x < 0 & 
            .y > 0, "Dominant", "Ratio")))))
    return(data_tb)
}
#' Add enter model event
#' @description add_enter_model_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add enter model event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arm_1L_chr Arm (a character vector of length one)
#' @param draws_tb Draws (a tibble)
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param default_fn Default (a function), Default: NULL
#' @param derive_fn_ls Derive (a list of functions), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tidy_cols_1L_lgl Tidy columns (a logical vector of length one), Default: FALSE
#' @param tfmn_ls Transformation (a list), Default: NULL
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: 'treatment'
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_enter_model_event
#' @export 
#' @importFrom lubridate years weeks NA_Date_
#' @importFrom dplyr mutate select everything inner_join
#' @importFrom purrr map_dfr reduce pluck
#' @keywords internal
add_enter_model_event <- function (X_Ready4useDyad, arm_1L_chr, draws_tb, horizon_dtm = lubridate::years(1), 
    default_fn = NULL, derive_fn_ls = NULL, iterations_int = 1:100L, 
    modifiable_chr = character(0), start_dtm = Sys.Date(), tidy_cols_1L_lgl = FALSE, 
    tfmn_ls = NULL, tx_duration_dtm = lubridate::weeks(12), tx_prefix_1L_chr = "treatment") 
{
    X_Ready4useDyad <- X_Ready4useDyad %>% update_population_classes(tfmn_ls = tfmn_ls)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(Arm = arm_1L_chr, InModel = T, Iteration = NA_integer_, 
            StartDate = start_dtm, EndDate = start_dtm + horizon_dtm, 
            CurrentDate = start_dtm, CurrentEvent = "EnterModel", 
            NextEvent = NA_character_, ScheduledFor = lubridate::NA_Date_) %>% 
        dplyr::select(Iteration, UID, InModel, Arm, StartDate, 
            CurrentDate, EndDate, CurrentEvent, NextEvent, ScheduledFor, 
            dplyr::everything()))
    if (paste0(tx_prefix_1L_chr, "_status") %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- update_tx_start_end(X_Ready4useDyad, 
            prefix_1L_chr = tx_prefix_1L_chr, tx_duration_dtm = tx_duration_dtm)
    }
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = modifiable_chr, 
        pattern_1L_chr = "{col}_start")
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = modifiable_chr)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", iterations_int %>% 
        purrr::map_dfr(~X_Ready4useDyad@ds_tb %>% dplyr::mutate(Iteration = .x)))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::inner_join(draws_tb))
    if (!is.null(default_fn)) {
        X_Ready4useDyad <- default_fn(X_Ready4useDyad)
    }
    if (!is.null(derive_fn_ls)) {
        X_Ready4useDyad <- names(derive_fn_ls) %>% purrr::reduce(.init = X_Ready4useDyad, 
            ~{
                derive_fn <- derive_fn_ls %>% purrr::pluck(.y)
                .x %>% derive_fn(var_1L_chr = .y)
            })
        X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = names(derive_fn_ls), 
            pattern_1L_chr = "{col}_start")
        X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = names(derive_fn_ls))
    }
    if (tidy_cols_1L_lgl) {
        X_Ready4useDyad <- update_order(X_Ready4useDyad, type_1L_chr = "columns")
    }
    return(X_Ready4useDyad)
}
#' Add episode
#' @description add_episode() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add episode. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param assert_1L_lgl Assert (a logical vector of length one)
#' @param episode_1L_int Episode (an integer vector of length one)
#' @param inputs_ls Inputs (a list)
#' @param iterations_int Iterations (an integer vector)
#' @param sensitivities_ls Sensitivities (a list)
#' @param tfmn_ls Transformation (a list)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one)
#' @param utilities_chr Utilities (a character vector)
#' @param utility_fns_ls Utility functions (a list)
#' @param episode_end_1L_chr Episode end (a character vector of length one), Default: 'EpisodeEnd_mdl'
#' @param k10_1L_chr K10 (a character vector of length one), Default: 'K10_mdl'
#' @param k10_relapse_1L_chr K10 relapse (a character vector of length one), Default: 'K10Relapse_mdl'
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'K10'
#' @param medical_chr Medical (a character vector), Default: make_worker_types("medical")
#' @param treatment_1L_chr Treatment (a character vector of length one), Default: character(0)
#' @param workers_chr Workers (a character vector), Default: make_worker_types()
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_episode
#' @export 
#' @importFrom dplyr mutate select across
#' @importFrom purrr pluck reduce
#' @importFrom lubridate days
#' @importFrom tidyselect all_of
#' @keywords internal
add_episode <- function (X_Ready4useDyad, assert_1L_lgl, episode_1L_int, inputs_ls, 
    iterations_int, sensitivities_ls, tfmn_ls, tx_prefix_1L_chr, 
    utilities_chr, utility_fns_ls, episode_end_1L_chr = "EpisodeEnd_mdl", 
    k10_1L_chr = "K10_mdl", k10_relapse_1L_chr = "K10Relapse_mdl", 
    k10_var_1L_chr = "K10", medical_chr = make_worker_types("medical"), 
    treatment_1L_chr = character(0), workers_chr = make_worker_types()) 
{
    update_1L_int <- episode_1L_int
    X_Ready4useDyad <- add_episode_start(X_Ready4useDyad)
    if (episode_1L_int > 1) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(K10Discharge = K10, 
                K10ChangeDischarge = K10_change))
        X_Ready4useDyad <- add_outcomes_update(X_Ready4useDyad, 
            assert_1L_lgl = assert_1L_lgl, k10_mdl = inputs_ls$models_ls %>% 
                purrr::pluck(k10_relapse_1L_chr), k10_var_1L_chr = k10_var_1L_chr, 
            iterations_int = iterations_int, params_tb = inputs_ls$params_tb, 
            sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
            types_chr = c("Model", "Function"), tx_prefix_1L_chr = tx_prefix_1L_chr, 
            update_1L_int = update_1L_int, utilities_chr = utilities_chr, 
            utility_fns_ls = utility_fns_ls)
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::select(-c(K10Discharge, 
                K10ChangeDischarge)))
        update_1L_int <- update_1L_int + 1
    }
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "EndEpisode", 
        schedule_fn = add_episode_duration, schedule_args_ls = list(episode_end_mdl = inputs_ls$models_ls %>% 
            purrr::pluck(episode_end_1L_chr), iterations_int = iterations_int, 
            treatment_1L_chr = treatment_1L_chr))
    print_errors(X_Ready4useDyad, vars_chr = c("EpisodeDurationDays"), 
        assert_1L_lgl = assert_1L_lgl, invalid_fn = function(x) (is.na(x) | 
            is.nan(x) | is.null(x) | x == -Inf | x == Inf | x < 
            0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateMinutes", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- workers_chr %>% purrr::reduce(.init = X_Ready4useDyad, 
        ~add_minutes_event(.x, add_dependency_1L_lgl = F, iterations_int = iterations_int, 
            minutes_mdl = inputs_ls$models_ls %>% purrr::pluck(paste0(.y, 
                "Mins_mdl")), var_1L_chr = paste0(.y, "UseMins")))
    print_errors(X_Ready4useDyad, vars_chr = paste0(c(workers_chr, 
        "Total"), "UseMins"), assert_1L_lgl = assert_1L_lgl, 
        invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | 
            x == -Inf | x == Inf | x < 0))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(TotalUseMins = rowSums(dplyr::across(tidyselect::all_of(paste0(workers_chr, 
            "UseMins"))))))
    if (!identical(intersect(workers_chr, medical_chr), character(0))) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(MedicalUseMins = rowSums(dplyr::across(tidyselect::all_of(paste0(intersect(workers_chr, 
                medical_chr), "UseMins"))))))
    }
    X_Ready4useDyad <- add_outcomes_update(X_Ready4useDyad, assert_1L_lgl = assert_1L_lgl, 
        k10_mdl = inputs_ls$models_ls %>% purrr::pluck(k10_1L_chr), 
        k10_var_1L_chr = k10_var_1L_chr, iterations_int = iterations_int, 
        params_tb = inputs_ls$params_tb, sensitivities_ls = sensitivities_ls, 
        tfmn_ls = tfmn_ls, types_chr = c("Model", "Function"), 
        tx_prefix_1L_chr = tx_prefix_1L_chr, update_1L_int = update_1L_int, 
        utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls)
    return(X_Ready4useDyad)
}
#' Add episode duration
#' @description add_episode_duration() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add episode duration. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param episode_end_mdl Episode end (a model), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param treatment_1L_chr Treatment (a character vector of length one), Default: character(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_episode_duration
#' @export 
#' @importFrom dplyr mutate case_when select
#' @importFrom lubridate time_length
#' @importFrom purrr map2_int map2_dbl
#' @keywords internal
add_episode_duration <- function (X_Ready4useDyad, episode_end_mdl = NULL, iterations_int = 1:100L, 
    treatment_1L_chr = character(0)) 
{
    if (!"EpisodeDurationDays" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(EpisodeDurationDays = 0))
    }
    if (!"Intervention" %in% names(X_Ready4useDyad@ds_tb)) {
        if (identical(treatment_1L_chr, character(0))) {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(Intervention = Arm))
        }
        else {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(Intervention = treatment_1L_chr))
        }
    }
    if (!is.null(episode_end_mdl)) {
        X_Ready4useDyad <- add_simulated_data(episode_end_mdl, 
            var_1L_chr = "EpisodeDurationDays", Y_Ready4useDyad = X_Ready4useDyad, 
            iterations_int = iterations_int, join_with_chr = c("Iteration"), 
            type_1L_chr = "third", what_1L_chr = "new")
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(EpisodeDurationDays = dplyr::case_when(is.nan(EpisodeDurationDays) ~ 
                0, T ~ EpisodeDurationDays)))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(DaysRemaining = lubridate::time_length((EndDate - 
                CurrentDate), "days"), EpisodeDurationDaysUncapped = round(EpisodeDurationDays, 
                0), EpisodeDurationDays = purrr::map2_int(DaysRemaining, 
                EpisodeDurationDays, ~round(min(.x, .y), 0)), 
                EpisodeIncludedFraction = purrr::map2_dbl(EpisodeDurationDaysUncapped, 
                  EpisodeDurationDays, ~min(1, .x, .y)), EpisodeDurationCategory = dplyr::case_when(EpisodeDurationDays < 
                  100 ~ "Under 100 days", EpisodeDurationDays >= 
                  100 & EpisodeDurationDays < 200 ~ "100-200 days", 
                  EpisodeDurationDays >= 200 ~ "200 days or more", 
                  T ~ NA_character_) %>% as.factor()) %>% dplyr::select(-DaysRemaining))
    }
    X_Ready4useDyad <- update_scheduled_date(X_Ready4useDyad, 
        variable_1L_chr = "EpisodeDurationDays", type_1L_chr = "Day")
    return(X_Ready4useDyad)
}
#' Add episode start
#' @description add_episode_start() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add episode start. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_episode_start
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
add_episode_start <- function (X_Ready4useDyad) 
{
    if (!"Episode" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(Episode = 0))
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(Episode = Episode + 1))
    return(X_Ready4useDyad)
}
#' Add episode wait time
#' @description add_episode_wait_time() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add episode wait time. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param episode_start_mdl Episode start (a model), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param type_1L_chr Type (a character vector of length one), Default: c("first", "repeat")
#' @param treatment_1L_chr Treatment (a character vector of length one), Default: character(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_episode_wait_time
#' @export 
#' @importFrom dplyr mutate case_when select
#' @importFrom rlang sym
#' @keywords internal
add_episode_wait_time <- function (X_Ready4useDyad, episode_start_mdl = NULL, iterations_int = 1:100L, 
    type_1L_chr = c("first", "repeat"), treatment_1L_chr = character(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    var_1L_chr <- ifelse(type_1L_chr == "first", "WaitInDays", 
        "DaysToYearOneRepresentation")
    if (!"WaitInDays" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(WaitInDays = 0))
    }
    if (!"Intervention" %in% names(X_Ready4useDyad@ds_tb)) {
        if (identical(treatment_1L_chr, character(0))) {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(Intervention = Arm))
        }
        else {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(Intervention = treatment_1L_chr))
        }
    }
    if (type_1L_chr == "repeat") {
        if (!"DaysToYearOneRepresentation" %in% names(X_Ready4useDyad@ds_tb)) {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(DaysToYearOneRepresentation = 366))
        }
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(DaysSinceIndexService = as.numeric(CurrentDate - 
                StartDate)))
    }
    if (!is.null(episode_start_mdl)) {
        X_Ready4useDyad <- add_simulated_data(episode_start_mdl, 
            var_1L_chr = var_1L_chr, Y_Ready4useDyad = X_Ready4useDyad, 
            iterations_int = iterations_int, join_with_chr = c("Iteration"), 
            type_1L_chr = "third", what_1L_chr = "new")
        if (type_1L_chr == "repeat") {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                  dplyr::case_when(!!rlang::sym(var_1L_chr) == 
                    0 ~ 366, T ~ !!rlang::sym(var_1L_chr)))))
        }
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                round(!!rlang::sym(var_1L_chr), 0))))
    }
    X_Ready4useDyad <- update_scheduled_date(X_Ready4useDyad, 
        variable_1L_chr = var_1L_chr, type_1L_chr = "Day")
    if (type_1L_chr == "repeat") {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::select(-DaysSinceIndexService))
    }
    return(X_Ready4useDyad)
}
#' Add EQ5D from draws
#' @description add_eq5d_from_draws() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add eq5d from draws. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'ParamEQ5DBeta'
#' @param value_with_fn Value with (a function), Default: add_eq5d_from_k10
#' @param var_1L_chr Variable (a character vector of length one), Default: 'EQ5D'
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_eq5d_from_draws
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @keywords internal
add_eq5d_from_draws <- function (X_Ready4useDyad, correspondences_r3 = ready4show::ready4show_correspondences(), 
    prefix_1L_chr = "ParamEQ5DBeta", value_with_fn = add_eq5d_from_k10, 
    var_1L_chr = "EQ5D") 
{
    X_Ready4useDyad <- X_Ready4useDyad %>% add_iteration_values_set(value_with_fn = value_with_fn, 
        value_with_args_ls = list(correspondences_r3 = correspondences_r3, 
            prefix_1L_chr = prefix_1L_chr, var_1L_chr = var_1L_chr, 
            type_1L_chr = "internal"))
    return(X_Ready4useDyad)
}
#' Add EQ5D from K10
#' @description add_eq5d_from_k10() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add eq5d from k10. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param beta_age_1L_dbl Beta age (a double vector of length one), Default: -0.01382
#' @param beta_constant_1L_dbl Beta constant (a double vector of length one), Default: 3.5222
#' @param beta_k10_1L_dbl Beta K10 (a double vector of length one), Default: -0.06476
#' @param germany_1L_lgl Germany (a logical vector of length one), Default: FALSE
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'ParamEQ5DBeta'
#' @param var_1L_chr Variable (a character vector of length one), Default: 'EQ5D'
#' @param source_1L_chr Source (a character vector of length one), Default: c("10.1192/bjo.2018.21", "10.1192/bjp.bp.113.136036")
#' @param tidy_cols_1L_lgl Tidy columns (a logical vector of length one), Default: FALSE
#' @param type_1L_chr Type (a character vector of length one), Default: c("internal", "external")
#' @return Data (an output object of multiple potential types)
#' @rdname add_eq5d_from_k10
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom serious transform_data_fmt
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange mutate select
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @importFrom purrr pmap_dbl map2_dbl
#' @keywords internal
add_eq5d_from_k10 <- function (data_xx, correspondences_r3 = ready4show::ready4show_correspondences(), 
    beta_age_1L_dbl = -0.01382, beta_constant_1L_dbl = 3.5222, 
    beta_k10_1L_dbl = -0.06476, germany_1L_lgl = FALSE, prefix_1L_chr = "ParamEQ5DBeta", 
    var_1L_chr = "EQ5D", source_1L_chr = c("10.1192/bjo.2018.21", 
        "10.1192/bjp.bp.113.136036"), tidy_cols_1L_lgl = FALSE, 
    type_1L_chr = c("internal", "external")) 
{
    source_1L_chr <- match.arg(source_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    X_Ready4useDyad <- serious::transform_data_fmt(data_xx, type_1L_chr = "input")
    data_tb <- X_Ready4useDyad@ds_tb
    if (identical(correspondences_r3, ready4show::ready4show_correspondences())) {
        correspondences_r3 <- correspondences_r3 %>% ready4show::renew.ready4show_correspondences(old_nms_chr = c("Age", 
            "K10"), new_nms_chr = c("Age", "K10"))
    }
    test_1L_lgl <- assertthat::assert_that(length(intersect(correspondences_r3$old_nms_chr, 
        c("Age", "K10"))) == 2)
    correspondences_r3 <- correspondences_r3 %>% dplyr::filter(old_nms_chr %in% 
        c("Age", "K10")) %>% dplyr::arrange(old_nms_chr)
    if (type_1L_chr == "internal") {
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            data_tb %>% dplyr::select(tidyselect::all_of(c(correspondences_r3$new_nms_chr, 
                paste0(prefix_1L_chr, c("Constant", correspondences_r3$new_nms_chr))))) %>% 
                purrr::pmap_dbl(~calculate_eq5d_from_k10(age_1L_dbl = ..1, 
                  k10_1L_dbl = ..2, beta_age_1L_dbl = ..4, beta_constant_1L_dbl = ..3, 
                  beta_k10_1L_dbl = ..5))))
    }
    else {
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            !!rlang::sym(correspondences_r3$new_nms_chr[1]) %>% 
                purrr::map2_dbl(!!rlang::sym(correspondences_r3$new_nms_chr[2]), 
                  ~{
                    age_1L_dbl <- .x
                    k10_1L_dbl <- .y
                    calculate_eq5d_from_k10(age_1L_dbl = age_1L_dbl, 
                      k10_1L_dbl = k10_1L_dbl, beta_age_1L_dbl = beta_age_1L_dbl, 
                      beta_constant_1L_dbl = beta_constant_1L_dbl, 
                      beta_k10_1L_dbl = beta_k10_1L_dbl, germany_1L_lgl = germany_1L_lgl, 
                      source_1L_chr = source_1L_chr)
                  })))
    }
    X_Ready4useDyad@ds_tb <- data_tb
    if (tidy_cols_1L_lgl) {
        X_Ready4useDyad <- update_order(X_Ready4useDyad, type_1L_chr = "columns")
    }
    data_xx <- serious::transform_data_fmt(data_xx, X_Ready4useDyad = X_Ready4useDyad)
    return(data_xx)
}
#' Add Initial Assessment andeferral parameters
#' @description add_iar_params() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add initial assessment andeferral parameters. The function returns Parameters (a tibble).
#' @param params_tb Parameters (a tibble)
#' @param comparator_int Comparator (an integer vector)
#' @param model_data_ls Model data (a list)
#' @param processed_ls Processed (a list)
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param test_1L_chr Test (a character vector of length one)
#' @param comparator_1L_chr Comparator (a character vector of length one), Default: 'Comparator'
#' @param comparator_filter_fn Comparator filter (a function), Default: identity
#' @param cost_1L_dbl Cost (a double vector of length one), Default: 0
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param intervention_filter_fn Intervention filter (a function), Default: identity
#' @return Parameters (a tibble)
#' @rdname add_iar_params
#' @export 
#' @importFrom stats setNames
#' @importFrom purrr map reduce
#' @importFrom dplyr filter pull arrange
#' @importFrom tibble add_case
#' @keywords internal
add_iar_params <- function (params_tb, comparator_int, model_data_ls, processed_ls, 
    raw_mds_data_ls, test_1L_chr, comparator_1L_chr = "Comparator", 
    comparator_filter_fn = identity, cost_1L_dbl = 0, intervention_1L_chr = "Intervention", 
    intervention_filter_fn = identity) 
{
    types_chr <- c(intervention_1L_chr, comparator_1L_chr)
    filters_ls <- list(intervention_filter_fn, comparator_filter_fn) %>% 
        stats::setNames(c("Intervention", "Comparator"))
    iar_ls <- 1:length(types_chr) %>% purrr::map(~{
        filter_fn <- filters_ls[[.x]]
        model_data_ls$imputed_ls$Z_Ready4useDyad@ds_tb %>% dplyr::filter(InterventionGroup == 
            types_chr[.x]) %>% filter_fn() %>% dplyr::pull(HasIAR) %>% 
            as.numeric()
    }) %>% stats::setNames(names(filters_ls))
    params_tb <- types_chr %>% purrr::reduce(.init = params_tb, 
        ~{
            parameters_dbl <- make_iar_params(processed_ls = processed_ls, 
                raw_mds_data_ls = raw_mds_data_ls, test_1L_chr = test_1L_chr, 
                comparator_1L_chr = comparator_1L_chr, comparator_int = comparator_int, 
                intervention_1L_chr = intervention_1L_chr, type_1L_chr = .y, 
                what_1L_chr = "InHouseIAR")
            .x %>% tibble::add_case(Parameter = paste0("InHouseIAR", 
                c("Intervention", "Comparator")[types_chr == 
                  .y]), Mean = parameters_dbl[1], SE = parameters_dbl[2], 
                Source = "make_iar_params")
        }) %>% tibble::add_case(Parameter = "ExternalIARCost", 
        Mean = cost_1L_dbl, SE = 0, SD = NA_real_, Source = "add_iar_params")
    params_tb <- 1:length(iar_ls) %>% purrr::reduce(.init = params_tb, 
        ~{
            .x %>% tibble::add_case(Parameter = paste0("HasIAR", 
                names(iar_ls)[.y]), Mean = mean(iar_ls[[.y]]), 
                SE = sd(iar_ls[[.y]])/sqrt(length(iar_ls[[.y]])), 
                SD = NA_real_, Source = "add_iar_params")
        }) %>% dplyr::arrange(Parameter)
    return(params_tb)
}
#' Add icer
#' @description add_icer() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add icer. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param cost_1L_chr Cost (a character vector of length one), Default: 'Cost'
#' @param effect_1L_chr Effect (a character vector of length one), Default: 'QALYs'
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @return Data (a tibble)
#' @rdname add_icer
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_icer <- function (data_tb, cost_1L_chr = "Cost", effect_1L_chr = "QALYs", 
    suffix_1L_chr = "") 
{
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("ICER", 
        suffix_1L_chr)), !!rlang::sym(cost_1L_chr)/!!rlang::sym(effect_1L_chr)))
    return(data_tb)
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
#' Add iteration values set
#' @description add_iteration_values_set() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add iteration values set. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param value_with_fn Value with (a function)
#' @param value_with_args_ls Value with arguments (a list), Default: NULL
#' @param tidy_cols_1L_lgl Tidy columns (a logical vector of length one), Default: TRUE
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_iteration_values_set
#' @export 
#' @importFrom purrr map reduce
#' @importFrom dplyr filter arrange
#' @importFrom rlang exec
#' @keywords internal
add_iteration_values_set <- function (X_Ready4useDyad, value_with_fn, value_with_args_ls = NULL, 
    tidy_cols_1L_lgl = TRUE) 
{
    iterations_int <- unique(X_Ready4useDyad@ds_tb$Iteration)
    samples_ls <- iterations_int %>% purrr::map(~{
        data_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(Iteration == 
            .x)
        if (!is.null(value_with_args_ls)) {
            data_tb <- rlang::exec(value_with_fn, data_tb, !!!value_with_args_ls)
        }
        else {
            data_tb <- data_tb %>% value_with_fn()
        }
        data_tb <- data_tb %>% dplyr::arrange(UID)
    })
    X_Ready4useDyad@ds_tb <- samples_ls %>% purrr::reduce(.init = samples_ls[[1]] %>% 
        dplyr::filter(F), ~rbind(.x, .y)) %>% dplyr::arrange(Iteration, 
        UID)
    if (tidy_cols_1L_lgl) {
        X_Ready4useDyad <- update_order(X_Ready4useDyad, type_1L_chr = "columns")
    }
    return(X_Ready4useDyad)
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
#' @param defaults_ls Defaults (a list), Default: list(Minutes = 0)
#' @param k10_draws_fn K10 draws (a function), Default: add_project_1_k10_draws
#' @param k10_mdl K10 (a model), Default: NULL
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'k10'
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param params_tb Parameters (a tibble), Default: make_project_params_tb()
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project", "Table")
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: 'treatment'
#' @param update_1L_int Update (an integer vector of length one), Default: integer(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_k10_event
#' @export 
#' @importFrom purrr reduce pluck
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_k10_event <- function (X_Ready4useDyad, adjustment_1L_dbl = 0, defaults_ls = list(Minutes = 0), 
    k10_draws_fn = add_project_1_k10_draws, k10_mdl = NULL, k10_var_1L_chr = "k10", 
    iterations_int = 1:100L, params_tb = make_project_params_tb(), 
    sensitivities_ls = make_sensitivities_ls(), suffix_1L_chr = character(0), 
    tfmn_ls = make_class_tfmns(T), type_1L_chr = c("Model", "Project", 
        "Table"), tx_prefix_1L_chr = "treatment", update_1L_int = integer(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (identical(suffix_1L_chr, character(0)) & type_1L_chr != 
        "Project") {
        suffix_1L_chr <- make_suffix(X_Ready4useDyad, adjustment_1L_dbl = adjustment_1L_dbl, 
            sensitivities_ls = sensitivities_ls, type_1L_chr = type_1L_chr, 
            update_1L_int = update_1L_int)
    }
    if (!is.null(defaults_ls)) {
        X_Ready4useDyad <- 1:length(defaults_ls) %>% purrr::reduce(.init = X_Ready4useDyad, 
            ~{
                default_var_1L_chr <- names(defaults_ls)[.y]
                if (!default_var_1L_chr %in% names(.x@ds_tb)) {
                  default_value_1L_chr <- defaults_ls %>% purrr::pluck(.y)
                  renewSlot(.x, "ds_tb", .x@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(default_var_1L_chr), 
                    default_value_1L_chr)))
                }
                else {
                  .x
                }
            })
        X_Ready4useDyad <- 1:length(defaults_ls) %>% purrr::reduce(.init = X_Ready4useDyad, 
            ~{
                default_var_1L_chr <- names(defaults_ls)[.y]
                renewSlot(.x, "ds_tb", .x@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(default_var_1L_chr, 
                  suffix_1L_chr)), !!rlang::sym(default_var_1L_chr))))
            })
    }
    if (!paste0(k10_var_1L_chr, "_change") %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(k10_var_1L_chr, 
                "_change")), 0)))
    }
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = c(k10_var_1L_chr, 
        paste0(k10_var_1L_chr, "_change")))
    var_1L_chr <- paste0(k10_var_1L_chr, suffix_1L_chr)
    X_Ready4useDyad <- add_k10_scores(X_Ready4useDyad, k10_draws_fn = k10_draws_fn, 
        k10_mdl = k10_mdl, join_with_chr = c("Iteration"), k10_var_1L_chr = k10_var_1L_chr, 
        iterations_int = iterations_int, params_tb = params_tb, 
        sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
        type_1L_chr = type_1L_chr, tx_prefix_1L_chr = tx_prefix_1L_chr, 
        var_1L_chr = var_1L_chr, what_1L_chr = "new")
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(k10_var_1L_chr)), 
            !!rlang::sym(var_1L_chr))))
    return(X_Ready4useDyad)
}
#' Add K10 scores
#' @description add_k10_scores() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add k10 scores. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param k10_mdl K10 (a model), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param k10_draws_fn K10 draws (a function), Default: add_project_1_k10_draws
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'k10'
#' @param params_tb Parameters (a tibble), Default: make_project_params_tb()
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: 'treatment'
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project", "Table")
#' @param var_1L_chr Variable (a character vector of length one), Default: 'k10_12_Weeks'
#' @param what_1L_chr What (a character vector of length one), Default: c("old", "new")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_k10_scores
#' @export 
#' @importFrom dplyr select where mutate across
#' @importFrom tidyselect any_of
#' @importFrom purrr map_int
#' @importFrom youthvars youthvars_k10_aus
#' @importFrom rlang sym
#' @keywords internal
add_k10_scores <- function (X_Ready4useDyad, k10_mdl = NULL, iterations_int = 1:100L, 
    join_with_chr = character(0), k10_draws_fn = add_project_1_k10_draws, 
    k10_var_1L_chr = "k10", params_tb = make_project_params_tb(), 
    sensitivities_ls = make_sensitivities_ls(), tfmn_ls = make_class_tfmns(T), 
    tx_prefix_1L_chr = "treatment", type_1L_chr = c("Model", 
        "Project", "Table"), var_1L_chr = "k10_12_Weeks", what_1L_chr = c("old", 
        "new")) 
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
        X_Ready4useDyad <- k10_draws_fn(X_Ready4useDyad, iterations_int = iterations_int, 
            k10_var_1L_chr = k10_var_1L_chr, k10_vars_chr = k10_vars_chr, 
            prefix_1L_chr = tx_prefix_1L_chr, sensitivities_ls = sensitivities_ls, 
            var_1L_chr = var_1L_chr)
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(dplyr::across(tidyselect::any_of(c(k10_vars_chr, 
            var_1L_chr)), ~round(.x, 0) %>% as.integer() %>% 
            purrr::map_int(~min(max(.x, 10), 50)) %>% youthvars::youthvars_k10_aus()), 
            `:=`(!!rlang::sym(paste0(k10_var_1L_chr, "_change")), 
                as.integer(!!rlang::sym(var_1L_chr) - as.integer(!!rlang::sym(k10_var_1L_chr))))))
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
#' Add Minimum Dataset minutes totals
#' @description add_mds_minutes_totals() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add minimum dataset minutes totals. The function returns Services (a tibble).
#' @param services_tb Services (a tibble)
#' @param add_chr Add (a character vector), Default: c("Contacts", "Use")
#' @param type_1L_chr Type (a character vector of length one), Default: c("both", "total", "prop")
#' @return Services (a tibble)
#' @rdname add_mds_minutes_totals
#' @export 
#' @importFrom dplyr mutate across
#' @keywords internal
add_mds_minutes_totals <- function (services_tb, add_chr = c("Contacts", "Use"), type_1L_chr = c("both", 
    "total", "prop")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr %in% c("total", "both")) {
        if ("Contacts" %in% add_chr) {
            services_tb <- services_tb %>% dplyr::mutate(TotalContactMins = ClinicalPsychologistContactMins + 
                GPContactMins + PsychiatristContactMins + OtherMedicalContactMins + 
                NurseContactMins + OtherContactMins)
        }
        if ("Use" %in% add_chr) {
            services_tb <- services_tb %>% dplyr::mutate(TotalUseMins = ClinicalPsychologistUseMins + 
                GPUseMins + PsychiatristUseMins + OtherMedicalUseMins + 
                NurseUseMins + OtherUseMins)
        }
    }
    if (type_1L_chr %in% c("prop", "both")) {
        if ("Contacts" %in% add_chr) {
            services_tb <- services_tb %>% dplyr::mutate(dplyr::across(c(ClinicalPsychologistContactMins, 
                GPContactMins, PsychiatristContactMins, OtherMedicalContactMins, 
                NurseContactMins, OtherContactMins), ~.x/TotalContactMins, 
                .names = "{col}Prop"))
        }
        if ("Use" %in% add_chr) {
            services_tb <- services_tb %>% dplyr::mutate(dplyr::across(c(ClinicalPsychologistUseMins, 
                GPUseMins, PsychiatristUseMins, OtherMedicalUseMins, 
                NurseUseMins, OtherUseMins), ~.x/TotalUseMins, 
                .names = "{col}Prop"))
        }
    }
    return(services_tb)
}
#' Add Minimum Dataset organisation variables
#' @description add_mds_org_vars() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add minimum dataset organisation variables. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param phn_code_1L_chr Primary Health Network code (a character vector of length one), Default: 'PHN_code'
#' @param phn_name_1L_chr Primary Health Network name (a character vector of length one), Default: 'PHN_area_name'
#' @return Data (a tibble)
#' @rdname add_mds_org_vars
#' @export 
#' @importFrom dplyr mutate left_join
#' @importFrom stringr str_sub
#' @keywords internal
add_mds_org_vars <- function (data_tb, provider_lup_tb, phn_code_1L_chr = "PHN_code", 
    phn_name_1L_chr = "PHN_area_name") 
{
    data_tb <- data_tb %>% dplyr::mutate(organisation_key = stringr::str_sub(organisation_path, 
        start = 8), PHN_code = stringr::str_sub(organisation_path, 
        end = 6))
    data_tb <- data_tb %>% dplyr::left_join(make_phn_lup(code_1L_chr = phn_code_1L_chr, 
        name_1L_chr = phn_name_1L_chr))
    data_tb <- data_tb %>% dplyr::left_join(provider_lup_tb)
    return(data_tb)
}
#' Add Minimum Dataset program variables
#' @description add_mds_program_vars() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add minimum dataset program variables. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param processed_ls Processed (a list)
#' @param program_services_lup Program services (a lookup table)
#' @param program_true_chr Program true (a character vector)
#' @param program_type_ls Program type (a list)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param add_start_date_1L_lgl Add start date (a logical vector of length one), Default: TRUE
#' @param filter_fn Filter (a function), Default: identity
#' @param mature_after_dtm Mature after (a date vector), Default: lubridate::years(1)
#' @return Data (a tibble)
#' @rdname add_mds_program_vars
#' @export 
#' @importFrom lubridate years
#' @importFrom dplyr left_join mutate case_when select
#' @importFrom rlang sym
#' @importFrom purrr map_chr map_lgl
#' @keywords internal
add_mds_program_vars <- function (data_tb, processed_ls, program_services_lup, program_true_chr, 
    program_type_ls, provider_lup_tb, add_start_date_1L_lgl = TRUE, 
    filter_fn = identity, mature_after_dtm = lubridate::years(1)) 
{
    program_true_ls <- program_type_ls[names(program_type_ls) %in% 
        names(program_true_chr)]
    program_false_ls <- program_type_ls[!names(program_type_ls) %in% 
        names(program_true_chr)]
    join_tb <- make_episodes_lup(processed_ls, program_services_lup = program_services_lup, 
        filter_fn = filter_fn, program_true_int = program_true_ls[names(program_true_ls) == 
            names(program_true_chr)[1]][[1]], program_true_1L_chr = unname(program_true_chr[1]), 
        provider_lup_tb = provider_lup_tb)
    if (length(program_true_chr == 2)) {
        join_tb <- join_tb %>% dplyr::left_join(make_episodes_lup(processed_ls, 
            program_services_lup = program_services_lup, program_true_int = program_true_ls[names(program_true_ls) == 
                names(program_true_chr)[2]][[1]], filter_1L_lgl = F, 
            filter_fn = filter_fn, program_true_1L_chr = unname(program_true_chr[2]), 
            provider_lup_tb = provider_lup_tb))
    }
    data_tb <- data_tb %>% dplyr::left_join(join_tb)
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(unname(program_true_chr[1])), 
        dplyr::case_when(is.na(service_provider_key) ~ FALSE, 
            T ~ !!rlang::sym(unname(program_true_chr[1]))))) %>% 
        dplyr::left_join(program_services_lup %>% dplyr::select(c(names(program_services_lup)[1:3], 
            unname(program_true_chr)), "include"))
    data_tb <- data_tb %>% dplyr::mutate(InterventionGroup = dplyr::case_when(!!rlang::sym(program_true_chr[1]) ~ 
        names(program_true_chr)[1], T ~ NA_character_))
    if (length(program_true_chr == 2)) {
        data_tb <- data_tb %>% dplyr::mutate(InterventionGroup = dplyr::case_when(is.na(InterventionGroup) & 
            (!!rlang::sym(program_true_chr[2]) & !(!!rlang::sym(program_true_chr[1]))) ~ 
            names(program_true_chr)[2], T ~ InterventionGroup))
    }
    data_tb <- data_tb %>% dplyr::mutate(InterventionGroup = dplyr::case_when(is.na(InterventionGroup) ~ 
        program_type %>% purrr::map_chr(~{
            program_1L_int <- .x
            program_1L_chr <- names(program_false_ls)[purrr::map_lgl(program_false_ls, 
                ~program_1L_int %in% .x)]
            ifelse(identical(program_1L_chr, character(0)), NA_character_, 
                program_1L_chr)
        }), T ~ InterventionGroup))
    data_tb <- data_tb %>% dplyr::mutate(InScope = dplyr::case_when(InterventionGroup == 
        names(program_true_chr)[1] & include ~ T, T ~ F)) %>% 
        dplyr::select(-include)
    if (add_start_date_1L_lgl) {
        start_up_lup <- make_mds_program_starts(processed_ls, 
            add_start_date_1L_lgl = FALSE, filter_fn = filter_fn, 
            mature_after_dtm = mature_after_dtm, program_services_lup = program_services_lup, 
            program_true_chr = program_true_chr, program_type_ls = program_type_ls, 
            provider_lup_tb = provider_lup_tb)
        data_tb <- data_tb %>% dplyr::left_join(start_up_lup)
        data_tb <- data_tb %>% dplyr::mutate(Mature = (referral_date >= 
            start_date + mature_after_dtm))
    }
    return(data_tb)
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
#' @param add_dependency_1L_lgl Add dependency (a logical vector of length one), Default: T
#' @param minutes_mdl Minutes (a model), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param fraction_1L_dbl Fraction (a double vector of length one), Default: numeric(0)
#' @param var_1L_chr Variable (a character vector of length one), Default: 'Minutes'
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_minutes_event
#' @export 
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom purrr map_int
#' @importFrom assertthat assert_that
#' @keywords internal
add_minutes_event <- function (X_Ready4useDyad, add_dependency_1L_lgl = T, minutes_mdl = NULL, 
    iterations_int = 1:100L, fraction_1L_dbl = numeric(0), var_1L_chr = "Minutes") 
{
    if (!var_1L_chr %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                0)))
        X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = var_1L_chr, 
            pattern_1L_chr = "{col}_start")
        X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = var_1L_chr)
    }
    if (add_dependency_1L_lgl) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(MeasurementWeek = paste0("Week", 
                round(as.numeric((CurrentDate - StartDate))/7, 
                  0) %>% purrr::map_int(~ifelse(.x == 52, 53, 
                  .x)))))
    }
    if (!is.null(minutes_mdl)) {
        X_Ready4useDyad <- add_simulated_data(minutes_mdl, var_1L_chr = paste0(var_1L_chr, 
            "_change"), Y_Ready4useDyad = X_Ready4useDyad, iterations_int = iterations_int, 
            join_with_chr = c("Iteration"), type_1L_chr = "third", 
            what_1L_chr = "new")
    }
    else {
        assertthat::assert_that(!identical(fraction_1L_dbl, numeric(0)))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(var_1L_chr, 
                "_change")), fraction_1L_dbl * !!rlang::sym(paste0(var_1L_chr, 
                "_change_previous")))))
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(var_1L_chr, "_change")), 
            dplyr::case_when(is.nan(!!rlang::sym(paste0(var_1L_chr, 
                "_change"))) ~ 0, T ~ !!rlang::sym(paste0(var_1L_chr, 
                "_change"))))))
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = c(var_1L_chr, 
        paste0(var_1L_chr, "_change")))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), !!rlang::sym(var_1L_chr) + 
            !!rlang::sym(paste0(var_1L_chr, "_change")))))
    return(X_Ready4useDyad)
}
#' Add model tests
#' @description add_model_tests() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add model tests. The function returns Regressions (a list).
#' @param model_data_ls Model data (a list)
#' @param regressions_ls Regressions (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param colour_1L_chr Colour (a character vector of length one), Default: character(0)
#' @param colours_chr Colours (a character vector), Default: ready4use::get_colour_codes(9, style_1L_chr = "monash_2", type_1L_chr = "unicol")[c(1, 
#'    9)]
#' @param imputed_1L_lgl Imputed (a logical vector of length one), Default: T
#' @param iterations_1L_int Iterations (an integer vector of length one), Default: 100
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param max_1L_dbl Maximum (a double vector of length one), Default: numeric(0)
#' @param min_1L_dbl Minimum (a double vector of length one), Default: numeric(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param plot_tfmn_fn Plot transformation (a function), Default: identity
#' @param summary_1L_lgl Summary (a logical vector of length one), Default: FALSE
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param type_1L_chr Type (a character vector of length one), Default: c("models", "candidates")
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param use_1L_chr Use (a character vector of length one), Default: character(0)
#' @param var_1L_chr Variable (a character vector of length one), Default: character(0)
#' @param x_label_1L_chr X label (a character vector of length one), Default: 'NA'
#' @return Regressions (a list)
#' @rdname add_model_tests
#' @export 
#' @importFrom ready4use get_colour_codes
#' @importFrom purrr map_chr map assign_in
#' @importFrom stats setNames
#' @importFrom dplyr filter select rename left_join group_by across summarise mutate
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_abline geom_point theme_classic
#' @importFrom tune coord_obs_pred
#' @importFrom tidyr pivot_longer
#' @keywords internal
add_model_tests <- function (model_data_ls, regressions_ls, what_1L_chr, colour_1L_chr = character(0), 
    colours_chr = ready4use::get_colour_codes(9, style_1L_chr = "monash_2", 
        type_1L_chr = "unicol")[c(1, 9)], imputed_1L_lgl = T, 
    iterations_1L_int = 100, join_with_chr = character(0), max_1L_dbl = numeric(0), 
    min_1L_dbl = numeric(0), model_1L_int = integer(0), plot_tfmn_fn = identity, 
    summary_1L_lgl = FALSE, tfmn_fn = identity, type_1L_chr = c("models", 
        "candidates"), uid_1L_chr = "UID", use_1L_chr = character(0), 
    var_1L_chr = character(0), x_label_1L_chr = NA_character_) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (identical(colour_1L_chr, character(0))) {
        colour_1L_chr <- colours_chr[1]
    }
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
    if (summary_1L_lgl) {
        Z_Ready4useDyad <- test_ls$Comparison_r4
        test_ls$Comparison_r4 <- renewSlot(test_ls$Comparison_r4, 
            "ds_tb", test_ls$Comparison_r4@ds_tb %>% dplyr::filter(Iteration == 
                0) %>% dplyr::select(-tidyselect::all_of(c(paste0(var_1L_chr, 
                "_sim_mean"), "Iteration", "Data"))) %>% dplyr::rename(Observed = !!rlang::sym(var_1L_chr)) %>% 
                dplyr::left_join(test_ls$Comparison_r4@ds_tb %>% 
                  dplyr::filter(Data == "Simulated") %>% dplyr::group_by(dplyr::across(tidyselect::all_of(c(uid_1L_chr, 
                  join_with_chr)))) %>% dplyr::summarise(Simulated = mean(!!rlang::sym(var_1L_chr)))))
    }
    plots_ls <- c("density", "histogram", "scatter") %>% purrr::map(~{
        plot_1L_chr <- .x
        c("all_plt", constraints_chr) %>% purrr::map(~{
            X_Ready4useDyad <- test_ls$Comparison_r4
            if (.x != "all_plt") {
                new_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(ifelse(summary_1L_lgl, 
                  "Observed", var_1L_chr)), tfmn_fn(!!rlang::sym(ifelse(summary_1L_lgl, 
                  "Observed", var_1L_chr))))) %>% dplyr::filter(!!rlang::sym(ifelse(summary_1L_lgl, 
                  "Observed", var_1L_chr)) >= constraints_dbl[1] & 
                  !!rlang::sym(ifelse(summary_1L_lgl, "Observed", 
                    var_1L_chr)) <= constraints_dbl[2])
                X_Ready4useDyad <- renewSlot(X_Ready4useDyad, 
                  "ds_tb", new_tb)
                if (summary_1L_lgl) {
                  Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, 
                    "ds_tb", Z_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                      tfmn_fn(!!rlang::sym(var_1L_chr)))) %>% 
                      dplyr::filter(!!rlang::sym(var_1L_chr) >= 
                        constraints_dbl[1] & !!rlang::sym(var_1L_chr) <= 
                        constraints_dbl[2]))
                }
            }
            if (plot_1L_chr == "scatter") {
                grouping_1L_chr <- character(0)
                if (!identical(join_with_chr, character(0))) {
                  grouping_1L_chr <- join_with_chr[1]
                }
                if (summary_1L_lgl) {
                  X_Ready4useDyad@ds_tb %>% ggplot2::ggplot(ggplot2::aes(x = Observed, 
                    y = Simulated)) + ggplot2::geom_abline(lty = 2) + 
                    ggplot2::geom_point(alpha = 0.5, colour = colour_1L_chr) + 
                    ggplot2::theme_classic() %>% plot_tfmn_fn() + 
                    tune::coord_obs_pred()
                }
                else {
                  plot_test_scatter(X_Ready4useDyad, colour_1L_chr = colour_1L_chr, 
                    grouping_1L_chr = grouping_1L_chr, plot_tfmn_fn = plot_tfmn_fn, 
                    var_1L_chr = var_1L_chr, collapse_1L_lgl = T)
                }
            }
            else {
                if (summary_1L_lgl) {
                  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, 
                    "ds_tb", X_Ready4useDyad@ds_tb %>% tidyr::pivot_longer(cols = tidyselect::all_of(c("Observed", 
                      "Simulated")), names_to = "Data", values_to = var_1L_chr))
                }
                else {
                  Z_Ready4useDyad <- X_Ready4useDyad
                }
                depict(Z_Ready4useDyad, x_vars_chr = var_1L_chr, 
                  x_labels_chr = x_label_1L_chr, y_labels_chr = "", 
                  z_vars_chr = "Data", z_labels_chr = "", as_percent_1L_lgl = T, 
                  colours_chr = colours_chr, drop_missing_1L_lgl = T, 
                  type_1L_chr = "manual", what_1L_chr = plot_1L_chr) %>% 
                  plot_tfmn_fn()
            }
        }) %>% stats::setNames(c("all_plt", "constrained_plt")[1:(ifelse(length(constraints_chr) == 
            0, 1, 2))])
    }) %>% stats::setNames(paste0(c("density", "histogram", "scatter"), 
        "_ls"))
    test_ls <- append(test_ls, plots_ls)
    if (what_1L_chr == "Minutes") {
        test_ls$comparison_tb <- make_project_minutes_cmprsn(test_ls$Simulated_r4, 
            Y_Ready4useDyad = test_ls$Comparison_r4, type_1L_chr = "prediction", 
            weeks_chr = Y_Ready4useDyad@ds_tb$MeasurementWeek %>% 
                unique())
    }
    regressions_ls$tests_ls <- purrr::assign_in(regressions_ls$tests_ls, 
        where = paste0(what_1L_chr, "_ls"), value = test_ls)
    return(regressions_ls)
}
#' Add non helpseekers
#' @description add_non_helpseekers() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add non helpseekers. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_non_helpseeking_chr Arms for non helpseeking (a character vector), Default: character(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_non_helpseekers
#' @export 
#' @importFrom dplyr mutate case_when select
#' @importFrom lubridate NA_Date_
#' @keywords internal
add_non_helpseekers <- function (X_Ready4useDyad, arms_for_non_helpseeking_chr = character(0)) 
{
    if (identical(arms_for_non_helpseeking_chr, character(0))) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(NonHelpSeeking = FALSE))
    }
    else {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(DrawsForNonHelpseeking = runif(nrow(.))) %>% 
                dplyr::mutate(NonHelpSeeking = dplyr::case_when((Arm %in% 
                  arms_for_non_helpseeking_chr) ~ (DrawsForNonHelpseeking < 
                  ParamNonHelpSeekers), T ~ FALSE)) %>% dplyr::mutate(CurrentDate = dplyr::case_when(NonHelpSeeking ~ 
                lubridate::NA_Date_, T ~ CurrentDate)) %>% dplyr::select(-DrawsForNonHelpseeking))
    }
    return(X_Ready4useDyad)
}
#' Add non Initial Assessment andeferral
#' @description add_non_iar() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add non initial assessment andeferral. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_iar_adjustment_chr Arms for Initial Assessment andeferral adjustment (a character vector), Default: character(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_non_iar
#' @export 
#' @importFrom dplyr mutate case_when select
#' @keywords internal
add_non_iar <- function (X_Ready4useDyad, arms_for_iar_adjustment_chr = character(0)) 
{
    if (!identical(arms_for_iar_adjustment_chr, character(0))) {
        adjustment_1L_dbl <- sum(X_Ready4useDyad@ds_tb$HasIAR)/nrow(X_Ready4useDyad@ds_tb)
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(DrawsForNonIAR = runif(nrow(.))) %>% 
                dplyr::mutate(HasIAR = dplyr::case_when((Arm %in% 
                  arms_for_iar_adjustment_chr) & HasIAR ~ (DrawsForNonIAR < 
                  (ParamHasIARComparator/adjustment_1L_dbl)), 
                  T ~ HasIAR)) %>% dplyr::mutate(HasIAR = dplyr::case_when(NonHelpSeeking ~ 
                FALSE, T ~ HasIAR)) %>% dplyr::select(-DrawsForNonIAR))
    }
    return(X_Ready4useDyad)
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
#' @param k10_draws_fn K10 draws (a function), Default: add_project_1_k10_draws
#' @param k10_method_1L_chr K10 method (a character vector of length one), Default: c("Model", "Table")
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'k10'
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns(T)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: 'treatment'
#' @param utilities_chr Utilities (a character vector), Default: c("CHU9D", "AQoL6D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Project")
#' @param update_1L_int Update (an integer vector of length one), Default: integer(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_outcomes_event_sequence
#' @export 
#' @importFrom lubridate weeks
#' @importFrom dplyr filter mutate bind_rows arrange
#' @importFrom rlang sym
#' @importFrom purrr reduce map_lgl pluck
#' @keywords internal
add_outcomes_event_sequence <- function (X_Ready4useDyad, inputs_ls, add_sensitivity_1L_lgl = FALSE, 
    adjustment_1L_dbl = -2, iterations_int = 1:100L, k10_draws_fn = add_project_1_k10_draws, 
    k10_method_1L_chr = c("Model", "Table"), k10_var_1L_chr = "k10", 
    sensitivities_ls = make_sensitivities_ls(), suffix_1L_chr = character(0), 
    tfmn_ls = make_class_tfmns(T), tx_prefix_1L_chr = "treatment", 
    utilities_chr = c("CHU9D", "AQoL6D"), type_1L_chr = c("Model", 
        "Project"), update_1L_int = integer(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    k10_method_1L_chr <- match.arg(k10_method_1L_chr)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateK10", 
        step_dtm = lubridate::weeks(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_k10_event(X_Ready4useDyad, adjustment_1L_dbl = adjustment_1L_dbl, 
        k10_draws_fn = k10_draws_fn, k10_mdl = inputs_ls$models_ls$K10_mdl, 
        iterations_int = iterations_int, params_tb = inputs_ls$params_tb, 
        sensitivities_ls = sensitivities_ls, suffix_1L_chr = suffix_1L_chr, 
        tfmn_ls = tfmn_ls, type_1L_chr = ifelse(type_1L_chr == 
            "Project", "Project", k10_method_1L_chr), tx_prefix_1L_chr = tx_prefix_1L_chr, 
        update_1L_int = update_1L_int)
    X_Ready4useDyad <- update_k10_event_schedule(X_Ready4useDyad, 
        type_1L_chr = k10_method_1L_chr)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateUtility", 
        step_dtm = lubridate::weeks(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, 
        add_sensitivity_1L_lgl = add_sensitivity_1L_lgl, adjustment_1L_dbl = adjustment_1L_dbl, 
        models_ls = inputs_ls$models_ls, iterations_int = iterations_int, 
        rewind_chr = k10_var_1L_chr, sensitivities_ls = sensitivities_ls, 
        tfmn_ls = tfmn_ls, utilities_chr = utilities_chr, type_1L_chr = type_1L_chr, 
        what_1L_chr = "new")
    if (k10_method_1L_chr == "Table" & type_1L_chr == "Model") {
        Y_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                "_part"))) != 1))
        Z_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                "_part"))) == 1))
        if (nrow(Z_Ready4useDyad@ds_tb) > 0) {
            Z_Ready4useDyad <- add_time_to_event(Z_Ready4useDyad, 
                event_1L_chr = "UpdateK10", schedule_fn = add_outcome_change_schedule)
            Z_Ready4useDyad <- update_current_date(Z_Ready4useDyad)
            Z_Ready4useDyad <- update_current_event(Z_Ready4useDyad)
            Z_Ready4useDyad <- add_k10_event(Z_Ready4useDyad, 
                adjustment_1L_dbl = adjustment_1L_dbl, k10_draws_fn = k10_draws_fn, 
                params_tb = inputs_ls$params_tb, iterations_int = iterations_int, 
                suffix_1L_chr = suffix_1L_chr, tfmn_ls = tfmn_ls, 
                type_1L_chr = k10_method_1L_chr, tx_prefix_1L_chr = tx_prefix_1L_chr, 
                update_1L_int = update_1L_int)
            Z_Ready4useDyad <- add_time_to_event(Z_Ready4useDyad, 
                event_1L_chr = "UpdateUtility", step_dtm = lubridate::weeks(0))
            Z_Ready4useDyad <- update_current_date(Z_Ready4useDyad)
            Z_Ready4useDyad <- update_current_event(Z_Ready4useDyad)
            Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
                c(k10_var_1L_chr, utilities_chr) %>% purrr::reduce(.init = Z_Ready4useDyad@ds_tb, 
                  ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                    !!rlang::sym(paste0(.y, "_previous"))), `:=`(!!rlang::sym(paste0(.y, 
                    "_previous")), !!rlang::sym(paste0(.y, "_start"))))))
            Z_Ready4useDyad <- add_utility_event(Z_Ready4useDyad, 
                add_qalys_1L_lgl = T, add_sensitivity_1L_lgl = add_sensitivity_1L_lgl, 
                adjustment_1L_dbl = adjustment_1L_dbl, models_ls = inputs_ls$models_ls, 
                iterations_int = iterations_int, rewind_chr = k10_var_1L_chr, 
                tfmn_ls = tfmn_ls, utilities_chr = utilities_chr, 
                what_1L_chr = "new")
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
#' Add outcomes update
#' @description add_outcomes_update() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add outcomes update. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param assert_1L_lgl Assert (a logical vector of length one)
#' @param k10_mdl K10 (a model)
#' @param k10_var_1L_chr K10 variable (a character vector of length one)
#' @param iterations_int Iterations (an integer vector)
#' @param params_tb Parameters (a tibble)
#' @param sensitivities_ls Sensitivities (a list)
#' @param tfmn_ls Transformation (a list)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one)
#' @param update_1L_int Update (an integer vector of length one)
#' @param utilities_chr Utilities (a character vector)
#' @param utility_fns_ls Utility functions (a list)
#' @param types_chr Types (a character vector), Default: c("Model", "Function")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_outcomes_update
#' @export 
#' @importFrom lubridate days weeks
#' @keywords internal
add_outcomes_update <- function (X_Ready4useDyad, assert_1L_lgl, k10_mdl, k10_var_1L_chr, 
    iterations_int, params_tb, sensitivities_ls, tfmn_ls, tx_prefix_1L_chr, 
    update_1L_int, utilities_chr, utility_fns_ls, types_chr = c("Model", 
        "Function")) 
{
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateK10", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_k10_event(X_Ready4useDyad, k10_mdl = k10_mdl, 
        k10_var_1L_chr = k10_var_1L_chr, iterations_int = iterations_int, 
        params_tb = params_tb, sensitivities_ls = sensitivities_ls, 
        tfmn_ls = tfmn_ls, type_1L_chr = types_chr[1], tx_prefix_1L_chr = tx_prefix_1L_chr, 
        update_1L_int = update_1L_int)
    print_errors(X_Ready4useDyad, vars_chr = k10_var_1L_chr, 
        assert_1L_lgl = assert_1L_lgl, invalid_fn = function(x) (is.na(x) | 
            is.nan(x) | is.null(x) | x < 10 | x > 50))
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateUtility", 
        step_dtm = lubridate::weeks(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, 
        add_sensitivity_1L_lgl = F, iterations_int = iterations_int, 
        tidy_cols_1L_lgl = T, type_1L_chr = types_chr[2], update_1L_int = update_1L_int, 
        utilities_chr = utilities_chr, utility_fns_ls = utility_fns_ls, 
        what_1L_chr = "new")
    print_errors(X_Ready4useDyad, vars_chr = utilities_chr, assert_1L_lgl = assert_1L_lgl, 
        invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | 
            x < -1 | x > 1))
    return(X_Ready4useDyad)
}
#' Add project 1 K10 draws
#' @description add_project_1_k10_draws() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 1 k10 draws. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'k10'
#' @param k10_vars_chr K10 variables (a character vector), Default: character(0)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'treatment'
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param severity_ls Severity (a list), Default: make_k10_severity_cuts()
#' @param var_1L_chr Variable (a character vector of length one), Default: 'k10_12_Weeks'
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_1_k10_draws
#' @export 
#' @importFrom dplyr select where mutate filter across pull case_when arrange
#' @importFrom rlang sym
#' @importFrom purrr map map2_dfr reduce
#' @importFrom tidyselect all_of
#' @importFrom stats setNames
#' @keywords internal
add_project_1_k10_draws <- function (X_Ready4useDyad, iterations_int = 1:100L, k10_var_1L_chr = "k10", 
    k10_vars_chr = character(0), prefix_1L_chr = "treatment", 
    sensitivities_ls = make_sensitivities_ls(), severity_ls = make_k10_severity_cuts(), 
    var_1L_chr = "k10_12_Weeks", ...) 
{
    if (identical(k10_vars_chr, character(0))) {
        k10_vars_chr <- X_Ready4useDyad@ds_tb %>% dplyr::select(dplyr::where(~inherits(.x, 
            "youthvars_k10_aus"))) %>% names()
    }
    if (!paste0(k10_var_1L_chr, "_part") %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(k10_var_1L_chr, 
                "_part")), 0)))
    }
    k10_ls <- iterations_int %>% purrr::map(~{
        data_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(Iteration == 
            .x) %>% dplyr::mutate(dplyr::across(tidyselect::all_of(k10_vars_chr), 
            ~as.integer(.x)))
        quantiles_dbl <- round(quantile(data_tb %>% dplyr::pull(k10_var_1L_chr), 
            probs = c(0.2, 0.4, 0.6, 0.8)), 0) %>% unname()
        rtm_ls <- list(c(10, quantiles_dbl[1]), c(quantiles_dbl[1] + 
            1, quantiles_dbl[2]), c(quantiles_dbl[2] + 1, quantiles_dbl[3]), 
            c(quantiles_dbl[3] + 1, quantiles_dbl[4]), c(quantiles_dbl[4] + 
                1, 50)) %>% stats::setNames(paste0("ParamRTM_Q", 
            1:5))
        data_tb <- severity_ls %>% purrr::map2_dfr(names(severity_ls), 
            ~{
                cut_tb <- data_tb %>% dplyr::filter(as.integer(!!rlang::sym(k10_var_1L_chr)) >= 
                  .x[1] & as.integer(!!rlang::sym(k10_var_1L_chr)) <= 
                  .x[2])
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
                      "ParamK10ChangeSpecialistAll_mean"]], sd = cut_tb[[1, 
                      "ParamK10ChangeSpecialistAll_sd"]])))
                  cut_tb <- cut_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")), dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, 
                    "_start"))) & (!!rlang::sym(paste0(prefix_1L_chr, 
                    "_measurement")) > CurrentDate) ~ !!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")) + 1, !is.na(!!rlang::sym(paste0(prefix_1L_chr, 
                    "_start"))) & (!!rlang::sym(paste0(prefix_1L_chr, 
                    "_measurement")) <= CurrentDate) & floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part"))) == 1 ~ !!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")) + 1, !is.na(!!rlang::sym(paste0(prefix_1L_chr, 
                    "_start"))) & (!!rlang::sym(paste0(prefix_1L_chr, 
                    "_measurement")) <= CurrentDate) & floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part"))) != 1 ~ !!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")) + 2, T ~ !!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")) + 0.001))) %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                    dplyr::case_when((!is.na(!!rlang::sym(paste0(prefix_1L_chr, 
                      "_start"))) & floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                      "_part"))) == 2) ~ as.integer(round((as.double(!!rlang::sym(k10_var_1L_chr)) - 
                      Improvement * !!rlang::sym(paste0(prefix_1L_chr, 
                        "_fraction"))), 0)), T ~ as.integer(!!rlang::sym(k10_var_1L_chr))))) %>% 
                    dplyr::select(-Improvement)
                }
                cut_tb
            })
        data_tb <- rtm_ls %>% purrr::map2_dfr(names(rtm_ls), 
            ~{
                cut_tb <- data_tb %>% dplyr::filter(!!rlang::sym(k10_var_1L_chr) >= 
                  .x[1] & !!rlang::sym(k10_var_1L_chr) <= .x[2])
                if (nrow(cut_tb) > 0) {
                  cut_tb <- dplyr::mutate(cut_tb, RTM = dplyr::case_when((as.character(clinic_type) == 
                    "headspace" & floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part"))) %in% 1:2) | ((!!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")) - floor(!!rlang::sym(paste0(k10_var_1L_chr, 
                    "_part")))) <= 0.001) ~ rnorm(nrow(cut_tb), 
                    mean = cut_tb[[1, paste0(.y, "_mean")]], 
                    sd = cut_tb[[1, paste0(.y, "_sd")]]), T ~ 
                    0))
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
    return(X_Ready4useDyad)
}
#' Add project 2 cost sensitivity analysis 1
#' @description add_project_2_cost_sa_1() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 cost sensitivity analysis 1. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_intervention_costs_chr Arms for intervention costs (a character vector), Default: 'Intervention'
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: '_S1'
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_2_cost_sa_1
#' @export 
#' @importFrom dplyr rowwise mutate across ungroup case_when
#' @importFrom rlang sym
#' @keywords internal
add_project_2_cost_sa_1 <- function (X_Ready4useDyad, arms_for_intervention_costs_chr = "Intervention", 
    disciplines_chr = make_disciplines(), suffix_1L_chr = "_S1", 
    ...) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::rowwise() %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("Cost", 
        suffix_1L_chr)), sum(dplyr::across(c(paste0(disciplines_chr, 
        "Cost"), "ExternalIARCost"))))) %>% dplyr::ungroup() %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0("UnmeasuredCosts", 
            suffix_1L_chr)), dplyr::case_when(Arm %in% arms_for_intervention_costs_chr ~ 
            ParamInterventionFixedCost * (1 - as.numeric(NonHelpSeeking)) + 
                ParamInterventionFixedCostIARAdjustment * (1 - 
                  as.numeric(NonHelpSeeking)) + as.numeric(HasIAR) * 
                (1 - as.numeric(ExternalIAR)) * ParamExternalIARCost, 
            T ~ ParamComparatorFixedCost * (1 - as.numeric(NonHelpSeeking)) + 
                ParamComparatorFixedCostIARAdjustment * (1 - 
                  as.numeric(NonHelpSeeking)) + as.numeric(HasIAR) * 
                (1 - as.numeric(ExternalIAR)) * ParamExternalIARCost))) %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0("Cost", suffix_1L_chr)), 
            !!rlang::sym(paste0("Cost", suffix_1L_chr)) + !!rlang::sym(paste0("UnmeasuredCosts", 
                suffix_1L_chr)) + AmbulanceOffsetCost)))
    return(X_Ready4useDyad)
}
#' Add project 2 cost sensitivity analysis 2
#' @description add_project_2_cost_sa_2() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 cost sensitivity analysis 2. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_intervention_costs_chr Arms for intervention costs (a character vector), Default: 'Intervention'
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: '_S2'
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_2_cost_sa_2
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate case_when rowwise across ungroup
#' @importFrom rlang sym
#' @keywords internal
add_project_2_cost_sa_2 <- function (X_Ready4useDyad, arms_for_intervention_costs_chr = "Intervention", 
    disciplines_chr = make_disciplines(), suffix_1L_chr = "_S2", 
    ...) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", disciplines_chr %>% 
        purrr::reduce(.init = X_Ready4useDyad@ds_tb, ~.x %>% 
            dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, "Cost", 
                suffix_1L_chr)), dplyr::case_when(Arm %in% arms_for_intervention_costs_chr ~ 
                !!rlang::sym(paste0(.y, "UseMins")) * !!rlang::sym(paste0("Param", 
                  .y, "CostPerMin")) * ParamInterventionVariableToTotal, 
                T ~ !!rlang::sym(paste0(.y, "UseMins")) * !!rlang::sym(paste0("Param", 
                  .y, "CostPerMin")) * ParamComparatorVariableToTotal)))))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::rowwise() %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("Cost", 
        suffix_1L_chr)), sum(dplyr::across(c(paste0(disciplines_chr, 
        paste0("Cost", suffix_1L_chr)), "ExternalIARCost", "AmbulanceOffsetCost"))))) %>% 
        dplyr::ungroup())
    return(X_Ready4useDyad)
}
#' Add project 2 costs
#' @description add_project_2_costs() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 costs. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_intervention_costs_chr Arms for intervention costs (a character vector)
#' @param arms_for_offsets_chr Arms for offsets (a character vector), Default: character(0)
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param sensitivities_ls Sensitivities (a list), Default: make_project_2_sensitivities_ls()
#' @param total_1L_lgl Total (a logical vector of length one), Default: T
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_2_costs
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate case_when rowwise across ungroup
#' @importFrom rlang sym
#' @keywords internal
add_project_2_costs <- function (X_Ready4useDyad, arms_for_intervention_costs_chr, arms_for_offsets_chr = character(0), 
    disciplines_chr = make_disciplines(), intervention_1L_chr = "Intervention", 
    sensitivities_ls = make_project_2_sensitivities_ls(), total_1L_lgl = T) 
{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", disciplines_chr %>% 
        purrr::reduce(.init = X_Ready4useDyad@ds_tb, ~.x %>% 
            dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, "Cost")), 
                !!rlang::sym(paste0(.y, "UseMins")) * !!rlang::sym(paste0("Param", 
                  .y, "CostPerMin"))))))
    if (total_1L_lgl) {
        if (!"ExternalIARCost" %in% names(X_Ready4useDyad@ds_tb)) {
            X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::mutate(ExternalIARCost = 0))
        }
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(ExternalIAR = runif(n = nrow(X_Ready4useDyad@ds_tb))) %>% 
                dplyr::mutate(ExternalIAR = dplyr::case_when(Arm %in% 
                  arms_for_intervention_costs_chr ~ ExternalIAR > 
                  !!rlang::sym(paste0("ParamInHouseIAR", "Intervention")), 
                  T ~ ExternalIAR > ParamInHouseIARComparator)) %>% 
                dplyr::mutate(ExternalIARCost = dplyr::case_when(HasIAR ~ 
                  ExternalIARCost + as.numeric(ExternalIAR) * 
                    ParamExternalIARCost, T ~ ExternalIARCost)) %>% 
                dplyr::mutate(ExternalIAR = dplyr::case_when(HasIAR ~ 
                  ExternalIAR, T ~ 0)))
        X_Ready4useDyad <- add_project_2_offsets(X_Ready4useDyad, 
            arms_for_offsets_chr = arms_for_offsets_chr)
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::rowwise() %>% dplyr::mutate(Cost = sum(dplyr::across(c(paste0(disciplines_chr, 
                "Cost"), "ExternalIARCost")))) %>% dplyr::ungroup() %>% 
                dplyr::mutate(UnmeasuredCosts = dplyr::case_when(Arm %in% 
                  arms_for_intervention_costs_chr ~ ParamInterventionFixedCost * 
                  (1 - as.numeric(NonHelpSeeking)) + ParamInterventionFixedCostExcludedAdjustment * 
                  (1 - as.numeric(NonHelpSeeking)) + ParamInterventionFixedCostIARAdjustment * 
                  (1 - as.numeric(NonHelpSeeking)) + as.numeric(HasIAR) * 
                  (1 - as.numeric(ExternalIAR)) * ParamExternalIARCost, 
                  T ~ ParamComparatorFixedCost * (1 - as.numeric(NonHelpSeeking)) + 
                    ParamComparatorFixedCostExcludedAdjustment * 
                      (1 - as.numeric(NonHelpSeeking)) + ParamComparatorFixedCostIARAdjustment * 
                    (1 - as.numeric(NonHelpSeeking)) + as.numeric(HasIAR) * 
                    (1 - as.numeric(ExternalIAR)) * ParamExternalIARCost)) %>% 
                dplyr::mutate(Cost = Cost + UnmeasuredCosts + 
                  AmbulanceOffsetCost))
        X_Ready4useDyad <- sensitivities_ls$costs_ls %>% purrr::reduce(.init = X_Ready4useDyad, 
            ~{
                cost_fn <- .y
                .x %>% cost_fn(disciplines_chr = disciplines_chr, 
                  arms_for_intervention_costs_chr = arms_for_intervention_costs_chr)
            })
    }
    return(X_Ready4useDyad)
}
#' Add project 2 K10 draws
#' @description add_project_2_k10_draws() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 k10 draws. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param k10_severity_cuts_ls K10 severity cuts (a list), Default: make_k10_severity_cuts()
#' @param k10_var_1L_chr K10 variable (a character vector of length one), Default: 'K10'
#' @param scale_1L_dbl Scale (a double vector of length one), Default: 1
#' @param var_1L_chr Variable (a character vector of length one), Default: 'K10'
#' @param ... Additional arguments
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_2_k10_draws
#' @export 
#' @importFrom purrr reduce map_dbl
#' @importFrom dplyr filter bind_rows mutate case_when first
#' @importFrom rlang sym
#' @keywords internal
add_project_2_k10_draws <- function (X_Ready4useDyad, k10_severity_cuts_ls = make_k10_severity_cuts(), 
    k10_var_1L_chr = "K10", scale_1L_dbl = 1, var_1L_chr = "K10", 
    ...) 
{
    iterations_int <- X_Ready4useDyad@ds_tb$Iteration %>% unique()
    X_Ready4useDyad@ds_tb <- iterations_int %>% purrr::reduce(.init = X_Ready4useDyad@ds_tb %>% 
        dplyr::filter(FALSE), ~{
        updated_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(Iteration == 
            .y)
        .x %>% dplyr::bind_rows(updated_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            dplyr::case_when(round(!!rlang::sym(k10_var_1L_chr), 
                0) %in% k10_severity_cuts_ls$Low[1]:k10_severity_cuts_ls$Low[2] ~ 
                !!rlang::sym(k10_var_1L_chr) + round(scale_1L_dbl * 
                  rnorm(nrow(updated_tb), mean = dplyr::first(ParamK102YearRTMLow), 
                    sd = dplyr::first(ParamK102YearRTMLow_sd)), 
                  0), round(!!rlang::sym(k10_var_1L_chr), 0) %in% 
                k10_severity_cuts_ls$Moderate[1]:k10_severity_cuts_ls$Moderate[2] ~ 
                !!rlang::sym(k10_var_1L_chr) + round(scale_1L_dbl * 
                  rnorm(nrow(updated_tb), mean = dplyr::first(ParamK102YearRTMModerate), 
                    sd = dplyr::first(ParamK102YearRTMModerate_sd)), 
                  0), round(!!rlang::sym(k10_var_1L_chr), 0) %in% 
                k10_severity_cuts_ls$High[1]:k10_severity_cuts_ls$High[2] ~ 
                !!rlang::sym(k10_var_1L_chr) + round(scale_1L_dbl * 
                  rnorm(nrow(updated_tb), mean = dplyr::first(ParamK102YearRTMHigh), 
                    sd = dplyr::first(ParamK102YearRTMHigh_sd)), 
                  0), round(!!rlang::sym(k10_var_1L_chr), 0) %in% 
                k10_severity_cuts_ls$VeryHigh[1]:k10_severity_cuts_ls$VeryHigh[2] ~ 
                !!rlang::sym(k10_var_1L_chr) + round(scale_1L_dbl * 
                  rnorm(nrow(updated_tb), mean = dplyr::first(ParamK102YearRTMVeryHigh), 
                    sd = dplyr::first(ParamK102YearRTMVeryHigh_sd)), 
                  0), T ~ Inf))) %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            !!rlang::sym(var_1L_chr) %>% purrr::map_dbl(~min(max(.x, 
                10), 50)))))
    })
    return(X_Ready4useDyad)
}
#' Add project 2 model data
#' @description add_project_2_model_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 model data. The function returns Model data (a list).
#' @param model_data_ls Model data (a list)
#' @param sample_ls Sample (a list)
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param cut_off_date_1L_chr Cut off date (a character vector of length one), Default: '2025-01-01'
#' @return Model data (a list)
#' @rdname add_project_2_model_data
#' @export 
#' @importFrom dplyr filter mutate across where group_by arrange lag ungroup
#' @keywords internal
add_project_2_model_data <- function (model_data_ls, sample_ls, intervention_1L_chr = "Intervention", 
    cut_off_date_1L_chr = "2025-01-01") 
{
    model_data_ls$imputed_ls$Modelling_r4 <- model_data_ls$imputed_ls$Z_Ready4useDyad %>% 
        update_mds_modelling_ds(sample_ls = sample_ls)
    model_data_ls$imputed_ls$Wait_r4 <- model_data_ls$imputed_ls$Modelling_r4 %>% 
        renewSlot("ds_tb", .@ds_tb %>% dplyr::filter(!is.na(WaitInDays)))
    model_data_ls$imputed_ls$EpisodeDuration_r4 <- model_data_ls$imputed_ls$Modelling_r4 %>% 
        renewSlot("ds_tb", .@ds_tb %>% dplyr::filter(!is.na(EpisodeDurationDays)))
    model_data_ls$imputed_ls$ProviderMinutes_r4 <- model_data_ls$imputed_ls$Modelling_r4 %>% 
        renewSlot("ds_tb", .@ds_tb %>% update_minute_var_nms(type_1L_chr = "do"))
    model_data_ls$imputed_ls$K10_r4 <- model_data_ls$imputed_ls$Modelling_r4 %>% 
        renewSlot("ds_tb", .@ds_tb %>% dplyr::filter(!is.na(K10_End)) %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.character), 
                ~as.factor(.x))))
    model_data_ls$imputed_ls$Representations_r4 <- model_data_ls$imputed_ls$Modelling_r4 %>% 
        renewSlot("ds_tb", model_data_ls$imputed_ls$Modelling_r4@ds_tb %>% 
            transform_project_2_model_ds(cut_off_date_1L_chr = cut_off_date_1L_chr, 
                intervention_1L_chr = intervention_1L_chr, type_1L_chr = "representation"))
    model_data_ls$imputed_ls$K10Relapse_r4 <- model_data_ls$imputed_ls$Modelling_r4 %>% 
        renewSlot("ds_tb", .@ds_tb %>% dplyr::group_by(UID) %>% 
            dplyr::arrange(UID, Episode) %>% dplyr::mutate(K10Discharge = dplyr::lag(K10_End, 
            default = NA_integer_), K10ChangeDischarge = dplyr::lag(K10_change, 
            default = NA_integer_), HasIAR = dplyr::lag(HasIAR, 
            default = NA), Diagnosis = dplyr::lag(Diagnosis, 
            default = NA), SuicideRisk = dplyr::lag(SuicideRisk, 
            default = NA)) %>% dplyr::ungroup() %>% dplyr::filter(!is.na(K10) & 
            !is.na(K10Discharge) & !is.na(K10ChangeDischarge)) %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.character), 
                ~as.factor(.x))))
    return(model_data_ls)
}
#' Add project 2 model wrap up
#' @description add_project_2_model_wrap_up() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 model wrap up. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_intervention_costs_chr Arms for intervention costs (a character vector)
#' @param arms_for_offsets_chr Arms for offsets (a character vector), Default: character(0)
#' @param disciplines_chr Disciplines (a character vector)
#' @param inputs_ls Inputs (a list)
#' @param iterations_int Iterations (an integer vector)
#' @param sensitivities_ls Sensitivities (a list)
#' @param tfmn_ls Transformation (a list)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one)
#' @param utilities_chr Utilities (a character vector)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_2_model_wrap_up
#' @export 
#' @importFrom dplyr mutate
#' @importFrom lubridate days
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
add_project_2_model_wrap_up <- function (X_Ready4useDyad, arms_for_intervention_costs_chr, arms_for_offsets_chr = character(0), 
    disciplines_chr, inputs_ls, iterations_int, sensitivities_ls, 
    tfmn_ls, tx_prefix_1L_chr, utilities_chr) 
{
    if (!"Intervention" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(Intervention = Arm))
    }
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateK10", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_k10_event(X_Ready4useDyad, k10_mdl = NULL, 
        k10_var_1L_chr = "K10", iterations_int = iterations_int, 
        params_tb = inputs_ls$params_tb, sensitivities_ls = sensitivities_ls, 
        tfmn_ls = tfmn_ls, type_1L_chr = "Project", tx_prefix_1L_chr = tx_prefix_1L_chr, 
        update_1L_int = 2)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateUtility", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, 
        add_sensitivity_1L_lgl = T, iterations_int = 1:iterations_int, 
        sensitivities_ls = sensitivities_ls, tfmn_ls = 1:length(utilities_chr) %>% 
            purrr::map(~identity) %>% stats::setNames(utilities_chr), 
        tidy_cols_1L_lgl = T, type_1L_chr = "Project", utilities_chr = utilities_chr, 
        utility_fns_ls = utility_fns_ls, what_1L_chr = "new")
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "UpdateCosts", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_project_2_costs(X_Ready4useDyad, arms_for_offsets_chr = arms_for_offsets_chr, 
        disciplines_chr = disciplines_chr, arms_for_intervention_costs_chr = arms_for_intervention_costs_chr, 
        total_1L_lgl = T)
    X_Ready4useDyad <- add_time_to_event(X_Ready4useDyad, event_1L_chr = "LeaveModel", 
        step_dtm = lubridate::days(0))
    X_Ready4useDyad <- update_current_date(X_Ready4useDyad)
    X_Ready4useDyad <- update_current_event(X_Ready4useDyad)
    X_Ready4useDyad <- add_leave_model_event(X_Ready4useDyad)
    return(X_Ready4useDyad)
}
#' Add project 2 offsets
#' @description add_project_2_offsets() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 offsets. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arms_for_offsets_chr Arms for offsets (a character vector), Default: character(0)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_project_2_offsets
#' @export 
#' @importFrom dplyr mutate case_when select
#' @keywords internal
add_project_2_offsets <- function (X_Ready4useDyad, arms_for_offsets_chr = character(0)) 
{
    if (identical(arms_for_offsets_chr, character(0))) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(OffsetAmbulance = 0, 
                AmbulanceOffsetCost = 0))
    }
    else {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(DrawsForAmbulanceOffset = runif(nrow(.))) %>% 
                dplyr::mutate(OffsetAmbulance = dplyr::case_when((Arm %in% 
                  arms_for_offsets_chr) ~ as.numeric((DrawsForAmbulanceOffset < 
                  abs(ParamAmbulanceOffsetProbProxy))), T ~ 0)) %>% 
                dplyr::mutate(AmbulanceOffsetCost = dplyr::case_when(ParamAmbulanceOffsetProbProxy >= 
                  0 ~ OffsetAmbulance * ParamAmbulanceOffsetCost * 
                  -1, T ~ OffsetAmbulance * ParamAmbulanceOffsetCost)) %>% 
                dplyr::select(-DrawsForAmbulanceOffset))
    }
    return(X_Ready4useDyad)
}
#' Add project 2 parameters
#' @description add_project_2_parameters() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project 2 parameters. The function returns Parameters (a tibble).
#' @param params_tb Parameters (a tibble), Default: NULL
#' @param additions_tb Additions (a tibble), Default: NULL
#' @param comparator_1L_chr Comparator (a character vector of length one), Default: character(0)
#' @param comparator_filter_fn Comparator filter (a function), Default: identity
#' @param comparator_int Comparator (an integer vector), Default: integer(0)
#' @param comparator_share_1L_dbl Comparator share (a double vector of length one), Default: 1
#' @param cost_1L_dbl Cost (a double vector of length one), Default: numeric(0)
#' @param data_ls Data (a list), Default: NULL
#' @param denominator_1L_dbl Denominator (a double vector of length one), Default: 1e+05
#' @param exposed_1L_dbl Exposed (a double vector of length one), Default: numeric()
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: character(0)
#' @param intervention_filter_fn Intervention filter (a function), Default: identity
#' @param intervention_share_1L_dbl Intervention share (a double vector of length one), Default: 1
#' @param n_1L_dbl N (a double vector of length one), Default: numeric()
#' @param rate_1L_dbl Rate (a double vector of length one), Default: numeric()
#' @param source_1L_chr Source (a character vector of length one), Default: 'add_project_2_parameters'
#' @param test_1L_chr Test (a character vector of length one), Default: character(0)
#' @param time_1L_dbl Time (a double vector of length one), Default: numeric(0)
#' @param values_dbl Values (a double vector), Default: numeric(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("default", "costs", "iar", "offsets")
#' @return Parameters (a tibble)
#' @rdname add_project_2_parameters
#' @export 
#' @importFrom tibble tribble add_case tibble
#' @importFrom dplyr filter mutate case_when select bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom ready4 get_from_lup_obj
#' @importFrom purrr reduce map_dbl
#' @keywords internal
add_project_2_parameters <- function (params_tb = NULL, additions_tb = NULL, comparator_1L_chr = character(0), 
    comparator_filter_fn = identity, comparator_int = integer(0), 
    comparator_share_1L_dbl = 1, cost_1L_dbl = numeric(0), data_ls = NULL, 
    denominator_1L_dbl = 1e+05, exposed_1L_dbl = numeric(), intervention_1L_chr = character(0), 
    intervention_filter_fn = identity, intervention_share_1L_dbl = 1, 
    n_1L_dbl = numeric(), rate_1L_dbl = numeric(), source_1L_chr = "add_project_2_parameters", 
    test_1L_chr = character(0), time_1L_dbl = numeric(0), values_dbl = numeric(0), 
    what_1L_chr = c("default", "costs", "iar", "offsets")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (is.null(params_tb)) {
        params_tb <- tibble::tribble(~Parameter, ~Mean, ~SE, 
            ~SD, ~Source, NA_character_, NA_real_, NA_real_, 
            NA_real_, NA_character_) %>% dplyr::filter(F)
    }
    if (what_1L_chr == "costs") {
        additions_tb <- data_ls$unit_cost_params_tb %>% dplyr::mutate(SE = 0, 
            SD = NA_real_, Source = "make_cost_per_mins_tb")
        additions_tb <- add_project_2_parameters(additions_tb, 
            additions_tb = data_ls$COSTS_r4@ds_tb %>% dplyr::mutate(Intervention = dplyr::case_when(Intervention == 
                comparator_1L_chr ~ "Comparator", Intervention == 
                intervention_1L_chr ~ "Intervention", T ~ Intervention)) %>% 
                dplyr::select(Intervention, FixedPerClient, VariableToTotalMultiplier) %>% 
                tidyr::pivot_longer(cols = c("FixedPerClient", 
                  "VariableToTotalMultiplier")) %>% dplyr::mutate(Parameter = dplyr::case_when(name == 
                "FixedPerClient" ~ paste0(Intervention, "FixedCost"), 
                T ~ paste0(Intervention, "VariableToTotal"))) %>% 
                dplyr::mutate(Mean = value, SE = 0, SD = NA_real_, 
                  Source = "make_mds_costing_ds") %>% dplyr::select(-c(Intervention, 
                name, value)))
        if (identical(cost_1L_dbl, numeric(0))) {
            cost_1L_dbl <- ready4::get_from_lup_obj(params_tb, 
                match_var_nm_1L_chr = "Parameter", match_value_xx = "ExternalIARCost", 
                target_var_nm_1L_chr = "Mean")
        }
        additions_tb <- c("Intervention", "Comparator") %>% purrr::reduce(.init = additions_tb, 
            ~{
                has_iar_1L_dbl <- ready4::get_from_lup_obj(params_tb, 
                  match_var_nm_1L_chr = "Parameter", match_value_xx = paste0("HasIAR", 
                    .y), target_var_nm_1L_chr = "Mean")
                in_house_1L_dbl <- ready4::get_from_lup_obj(params_tb, 
                  match_var_nm_1L_chr = "Parameter", match_value_xx = paste0("InHouseIAR", 
                    .y), target_var_nm_1L_chr = "Mean")
                fixed_1L_dbl <- ready4::get_from_lup_obj(additions_tb, 
                  match_var_nm_1L_chr = "Parameter", match_value_xx = paste0(.y, 
                    "FixedCost"), target_var_nm_1L_chr = "Mean")
                proportion_1L_dbl <- 1 - c(intervention_share_1L_dbl, 
                  comparator_share_1L_dbl)[which(c("Intervention", 
                  "Comparator") == .y)]
                .x %>% tibble::add_case(Parameter = paste0(.y, 
                  "FixedCostIARAdjustment"), Mean = -has_iar_1L_dbl * 
                  in_house_1L_dbl * cost_1L_dbl, SE = 0, SD = NA_real_, 
                  Source = "add_iar_params") %>% tibble::add_case(Parameter = paste0(.y, 
                  "FixedCostExcludedAdjustment"), Mean = -(fixed_1L_dbl * 
                  proportion_1L_dbl), SE = 0, SD = NA_real_, 
                  Source = "add_iar_params")
            })
        params_tb <- params_tb %>% dplyr::filter(Parameter != 
            "HasIARIntervention")
    }
    if (what_1L_chr == "offsets") {
        prob_proxies_dbl <- rnorm(1000, mean = values_dbl[1], 
            sd = values_dbl[2]) %>% purrr::map_dbl(~{
            calculate_offset_prob_proxy(area_erp_1L_dbl = n_1L_dbl, 
                denominator_1L_dbl = denominator_1L_dbl, exposed_1L_dbl = exposed_1L_dbl, 
                risk_1L_dbl = .x, rate_1L_dbl = rate_1L_dbl, 
                time_1L_dbl = time_1L_dbl)
        })
        additions_tb <- tibble::tibble(Parameter = c("AmbulanceOffsetProbProxy", 
            "AmbulanceOffsetCost"), Mean = c(mean(prob_proxies_dbl), 
            cost_1L_dbl), SE = c(sd(prob_proxies_dbl), 0), SD = NA_real_, 
            Source = c("calculate_offset_prob_proxy", source_1L_chr))
    }
    if (what_1L_chr == "iar") {
        params_tb <- add_iar_params(params_tb = params_tb, model_data_ls = data_ls$model_data_ls, 
            processed_ls = data_ls$processed_ls, raw_mds_data_ls = data_ls$raw_mds_data_ls, 
            test_1L_chr = test_1L_chr, comparator_1L_chr = comparator_1L_chr, 
            comparator_int = comparator_int, comparator_filter_fn = comparator_filter_fn, 
            cost_1L_dbl = cost_1L_dbl, intervention_1L_chr = intervention_1L_chr, 
            intervention_filter_fn = intervention_filter_fn)
    }
    else {
        params_tb <- params_tb %>% dplyr::bind_rows(additions_tb) %>% 
            dplyr::arrange(Parameter)
    }
    return(params_tb)
}
#' Add project assessments
#' @description add_project_assessments() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project assessments. The function returns Regressions (a list).
#' @param regressions_ls Regressions (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param colours_chr Colours (a character vector), Default: ready4use::get_colour_codes(9, style_1L_chr = "monash_2", type_1L_chr = "unicol")[c(9, 
#'    1, 5)]
#' @param confusion_1L_lgl Confusion (a logical vector of length one), Default: F
#' @param exclude_int Exclude (an integer vector), Default: integer(0)
#' @param group_ls Group (a list), Default: list(Treatments = c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged"))
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param rank_1L_lgl Rank (a logical vector of length one), Default: TRUE
#' @param residual_1L_chr Residual (a character vector of length one), Default: 'normal'
#' @param two_part_1L_lgl Two part (a logical vector of length one), Default: FALSE
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "tests", "models")
#' @param var_1L_chr Variable (a character vector of length one), Default: character(0)
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @return Regressions (a list)
#' @rdname add_project_assessments
#' @export 
#' @importFrom ready4use get_colour_codes Ready4useDyad
#' @importFrom purrr pluck map assign_in
#' @importFrom stats setNames
#' @importFrom stringr str_remove
#' @keywords internal
add_project_assessments <- function (regressions_ls, what_1L_chr, colours_chr = ready4use::get_colour_codes(9, 
    style_1L_chr = "monash_2", type_1L_chr = "unicol")[c(9, 1, 
    5)], confusion_1L_lgl = F, exclude_int = integer(0), group_ls = list(Treatments = c("Tx_Waitlist", 
    "Tx_Treatment", "Tx_Discharged")), model_1L_int = integer(0), 
    rank_1L_lgl = TRUE, residual_1L_chr = "normal", two_part_1L_lgl = FALSE, 
    type_1L_chr = c("candidates", "tests", "models"), var_1L_chr = character(0), 
    X_Ready4useDyad = ready4use::Ready4useDyad()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (what_1L_chr %in% names(group_ls)) {
        updated_ls <- group_ls %>% purrr::pluck(what_1L_chr) %>% 
            purrr::map(~make_regression_report(regressions_ls, 
                colours_chr = colours_chr, X_Ready4useDyad = X_Ready4useDyad, 
                exclude_int = exclude_int, model_1L_int = model_1L_int, 
                report_1L_chr = ifelse(confusion_1L_lgl, "all", 
                  "main"), rank_1L_lgl = rank_1L_lgl, residual_1L_chr = residual_1L_chr, 
                type_1L_chr = type_1L_chr, what_1L_chr = .x, 
                var_1L_chr = var_1L_chr))
    }
    else {
        if (two_part_1L_lgl) {
            updated_ls <- 1:2 %>% purrr::map(~make_regression_report(regressions_ls, 
                colours_chr = colours_chr, X_Ready4useDyad = X_Ready4useDyad, 
                exclude_int = exclude_int, model_1L_int = model_1L_int, 
                report_1L_chr = ifelse(confusion_1L_lgl, "all", 
                  "main"), part_1L_int = .x, rank_1L_lgl = rank_1L_lgl, 
                residual_1L_chr = residual_1L_chr, type_1L_chr = type_1L_chr, 
                what_1L_chr = what_1L_chr, var_1L_chr = var_1L_chr)) %>% 
                stats::setNames(paste0("part_", 1:2, "_ls"))
        }
        else {
            updated_ls <- make_regression_report(regressions_ls, 
                colours_chr = colours_chr, X_Ready4useDyad = X_Ready4useDyad, 
                exclude_int = exclude_int, model_1L_int = model_1L_int, 
                report_1L_chr = ifelse(confusion_1L_lgl, "all", 
                  "main"), rank_1L_lgl = rank_1L_lgl, residual_1L_chr = residual_1L_chr, 
                type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr, 
                var_1L_chr = var_1L_chr)
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
#' Add project offset logic
#' @description add_project_offset_logic() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project offset logic. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @return Data (a tibble)
#' @rdname add_project_offset_logic
#' @export 
#' @importFrom dplyr mutate case_when
#' @keywords internal
add_project_offset_logic <- function (data_tb) 
{
    data_tb <- data_tb %>% dplyr::mutate(OffsetLogicHeadspace = dplyr::case_when(as.character(clinic_type) == 
        "headspace" ~ 1, T ~ 0), OffsetLogicSpecialist = dplyr::case_when(as.character(clinic_type) == 
        "Specialist Services" ~ 1, T ~ 0))
    return(data_tb)
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
#' @param utility_1L_chr Utility (a character vector of length one), Default: c("AQoL6D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "legacy")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_qalys_sensitivities
#' @export 
#' @importFrom purrr pluck map_dbl reduce
#' @importFrom dplyr mutate arrange
#' @importFrom rlang sym
#' @keywords internal
add_qalys_sensitivities <- function (X_Ready4useDyad, end_var_1L_chr = character(0), sensitivities_ls = make_sensitivities_ls(), 
    start_var_1L_chr = character(0), utility_1L_chr = c("AQoL6D"), 
    type_1L_chr = c("main", "legacy")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
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
#' Add regression to mean
#' @description add_regression_to_mean() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add regression to mean. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param inputs_ls Inputs (a list)
#' @param iterations_int Iterations (an integer vector)
#' @param k10_draws_fn K10 draws (a function)
#' @param add_sensitivity_1L_lgl Add sensitivity (a logical vector of length one), Default: FALSE
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param tfmn_ls Transformation (a list), Default: make_class_tfmns()
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: 'Treatment'
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2")
#' @param utility_fns_ls Utility functions (a list), Default: NULL
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_regression_to_mean
#' @export 
#' @keywords internal
add_regression_to_mean <- function (X_Ready4useDyad, inputs_ls, iterations_int, k10_draws_fn, 
    add_sensitivity_1L_lgl = FALSE, sensitivities_ls = make_sensitivities_ls(), 
    tfmn_ls = make_class_tfmns(), tx_prefix_1L_chr = "Treatment", 
    utilities_chr = c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2"), 
    utility_fns_ls = NULL) 
{
    if (is.null(utility_fns_ls)) {
        utility_fns_ls <- make_utility_fns_ls(utilities_chr = utilities_chr)
    }
    X_Ready4useDyad <- add_k10_event(X_Ready4useDyad, k10_draws_fn = k10_draws_fn, 
        k10_mdl = NULL, k10_var_1L_chr = "K10", iterations_int = iterations_int, 
        params_tb = inputs_ls$params_tb, sensitivities_ls = sensitivities_ls, 
        suffix_1L_chr = "Update", tfmn_ls = tfmn_ls, type_1L_chr = "Table", 
        tx_prefix_1L_chr = tx_prefix_1L_chr, update_1L_int = 1)
    X_Ready4useDyad <- add_utility_event(X_Ready4useDyad, add_qalys_1L_lgl = T, 
        add_sensitivity_1L_lgl = add_sensitivity_1L_lgl, iterations_int = 1:iterations_int, 
        sensitivities_ls = sensitivities_ls, tidy_cols_1L_lgl = T, 
        type_1L_chr = "Function", update_1L_int = 1, utilities_chr = utilities_chr, 
        utility_fns_ls = utility_fns_ls, what_1L_chr = "new")
    return(X_Ready4useDyad)
}
#' Add regressions
#' @description add_regressions() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add regressions. The function returns Regressions (a list).
#' @param regressions_ls Regressions (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param fn_args_ls Function arguments (a list), Default: list()
#' @param model_fn Model (a function), Default: NULL
#' @param named_1L_lgl Named (a logical vector of length one), Default: FALSE
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "tests", "models")
#' @return Regressions (a list)
#' @rdname add_regressions
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom rlang exec
#' @importFrom purrr assign_in map
#' @importFrom stringr str_remove
#' @keywords internal
add_regressions <- function (regressions_ls, what_1L_chr, model_1L_int = integer(0), 
    fn_args_ls = list(), model_fn = NULL, named_1L_lgl = FALSE, 
    X_Ready4useDyad = ready4use::Ready4useDyad(), type_1L_chr = c("candidates", 
        "tests", "models")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
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
#' Add Short Form - Six Dimension from draws
#' @description add_sf6d_from_draws() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add short form - six dimension from draws. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param female_values_chr Female values (a character vector), Default: c("Female", "female", "F", "f", "FEMALE")
#' @param male_values_chr Male values (a character vector), Default: c("Male", "male", "M", "m", "MALE")
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'ParamSF6DBeta'
#' @param var_1L_chr Variable (a character vector of length one), Default: 'SF6D'
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_sf6d_from_draws
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @keywords internal
add_sf6d_from_draws <- function (X_Ready4useDyad, correspondences_r3 = ready4show::ready4show_correspondences(), 
    female_values_chr = c("Female", "female", "F", "f", "FEMALE"), 
    male_values_chr = c("Male", "male", "M", "m", "MALE"), prefix_1L_chr = "ParamSF6DBeta", 
    var_1L_chr = "SF6D") 
{
    X_Ready4useDyad <- X_Ready4useDyad %>% add_iteration_values_set(value_with_fn = add_sf6d_from_k10, 
        value_with_args_ls = list(correspondences_r3 = correspondences_r3, 
            female_values_chr = female_values_chr, male_values_chr = male_values_chr, 
            prefix_1L_chr = prefix_1L_chr, var_1L_chr = var_1L_chr, 
            type_1L_chr = "internal"))
    return(X_Ready4useDyad)
}
#' Add Short Form - Six Dimension from K10
#' @description add_sf6d_from_k10() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add short form - six dimension from k10. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param beta_female_moderate_1L_dbl Beta female moderate (a double vector of length one), Default: -0.059
#' @param beta_female_high_1L_dbl Beta female high (a double vector of length one), Default: -0.124
#' @param beta_male_moderate_1L_dbl Beta male moderate (a double vector of length one), Default: -0.055
#' @param beta_male_high_1L_dbl Beta male high (a double vector of length one), Default: -0.123
#' @param beta_constant_1L_dbl Beta constant (a double vector of length one), Default: 0.805
#' @param female_values_chr Female values (a character vector), Default: c("Female", "female", "F", "f", "FEMALE")
#' @param male_values_chr Male values (a character vector), Default: c("Male", "male", "M", "m", "MALE")
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'ParamSF6DBeta'
#' @param var_1L_chr Variable (a character vector of length one), Default: 'SF6D'
#' @param source_1L_chr Source (a character vector of length one), Default: c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036")
#' @param tidy_cols_1L_lgl Tidy columns (a logical vector of length one), Default: FALSE
#' @param type_1L_chr Type (a character vector of length one), Default: c("internal", "external")
#' @return Data (an output object of multiple potential types)
#' @rdname add_sf6d_from_k10
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom serious transform_data_fmt
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter arrange mutate select
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @importFrom purrr pmap_dbl map2_dbl
#' @keywords internal
add_sf6d_from_k10 <- function (data_xx, correspondences_r3 = ready4show::ready4show_correspondences(), 
    beta_female_moderate_1L_dbl = -0.059, beta_female_high_1L_dbl = -0.124, 
    beta_male_moderate_1L_dbl = -0.055, beta_male_high_1L_dbl = -0.123, 
    beta_constant_1L_dbl = 0.805, female_values_chr = c("Female", 
        "female", "F", "f", "FEMALE"), male_values_chr = c("Male", 
        "male", "M", "m", "MALE"), prefix_1L_chr = "ParamSF6DBeta", 
    var_1L_chr = "SF6D", source_1L_chr = c("10.1016/j.jval.2024.12.002", 
        "10.1192/bjp.bp.113.136036"), tidy_cols_1L_lgl = FALSE, 
    type_1L_chr = c("internal", "external")) 
{
    source_1L_chr <- match.arg(source_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    X_Ready4useDyad <- serious::transform_data_fmt(data_xx, type_1L_chr = "input")
    data_tb <- X_Ready4useDyad@ds_tb
    if (identical(correspondences_r3, ready4show::ready4show_correspondences())) {
        correspondences_r3 <- correspondences_r3 %>% ready4show::renew.ready4show_correspondences(old_nms_chr = c("Sex", 
            "K10"), new_nms_chr = c("Sex", "K10"))
    }
    test_1L_lgl <- assertthat::assert_that(length(intersect(correspondences_r3$old_nms_chr, 
        c("Sex", "K10"))) == 2)
    correspondences_r3 <- correspondences_r3 %>% dplyr::filter(old_nms_chr %in% 
        c("Sex", "K10")) %>% dplyr::arrange(old_nms_chr)
    if (type_1L_chr == "internal") {
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            data_tb %>% dplyr::select(tidyselect::all_of(c(correspondences_r3$new_nms_chr, 
                paste0(prefix_1L_chr, "Constant"), paste0(paste0(prefix_1L_chr, 
                  "Female"), c("Moderate", "High")), paste0(paste0(prefix_1L_chr, 
                  "Male"), c("Moderate", "High"))))) %>% purrr::pmap_dbl(~calculate_sf6d_from_k10(female_1L_lgl = ifelse(..2 %in% 
                female_values_chr, T, ifelse(..2 %in% male_values_chr, 
                F, runif(1) < 0.5)), k10_1L_dbl = ..1, beta_female_moderate_1L_dbl = ..4, 
                beta_female_high_1L_dbl = ..5, beta_male_moderate_1L_dbl = ..6, 
                beta_male_high_1L_dbl = ..7, beta_constant_1L_dbl = ..3))))
    }
    else {
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            !!rlang::sym(correspondences_r3$new_nms_chr[1]) %>% 
                purrr::map2_dbl(!!rlang::sym(correspondences_r3$new_nms_chr[2]), 
                  ~{
                    calculate_sf6d_from_k10(female_1L_lgl = ifelse(.y %in% 
                      female_values_chr, T, ifelse(.y %in% male_values_chr, 
                      F, runif(1) < 0.5)), k10_1L_dbl = .x, beta_female_moderate_1L_dbl = beta_female_moderate_1L_dbl, 
                      beta_female_high_1L_dbl = beta_female_high_1L_dbl, 
                      beta_male_moderate_1L_dbl = beta_male_moderate_1L_dbl, 
                      beta_male_high_1L_dbl = beta_male_high_1L_dbl, 
                      beta_constant_1L_dbl = beta_constant_1L_dbl, 
                      source_1L_chr = source_1L_chr)
                  })))
    }
    X_Ready4useDyad@ds_tb <- data_tb
    if (tidy_cols_1L_lgl) {
        X_Ready4useDyad <- update_order(X_Ready4useDyad, type_1L_chr = "columns")
    }
    data_xx <- serious::transform_data_fmt(data_xx, X_Ready4useDyad = X_Ready4useDyad)
    return(data_xx)
}
#' Add simulated data
#' @description add_simulated_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add simulated data. The function is called for its side effects and does not return a value.
#' @param model_mdl Model (a model)
#' @param var_1L_chr Variable (a character vector of length one)
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param rewind_chr Rewind (a character vector), Default: character(0)
#' @param tfmn_1L_chr Transformation (a character vector of length one), Default: 'NTF'
#' @param type_1L_chr Type (a character vector of length one), Default: c("first", "second", "third", "fourth")
#' @param what_1L_chr What (a character vector of length one), Default: c("old", "new")
#' @return Y (A dataset and data dictionary pair.)
#' @rdname add_simulated_data
#' @export 
#' @importFrom purrr reduce map_dfc map_dfr
#' @importFrom dplyr mutate select across everything rename_with inner_join filter starts_with
#' @importFrom rlang sym
#' @importFrom tidyr all_of any_of
#' @importFrom didgformula sim
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom specific calculate_depnt_var_tfmn
#' @importFrom tidyselect any_of
#' @keywords internal
add_simulated_data <- function (model_mdl, var_1L_chr, Y_Ready4useDyad, iterations_int = 1:100L, 
    join_with_chr = character(0), rewind_chr = character(0), 
    tfmn_1L_chr = "NTF", type_1L_chr = c("first", "second", "third", 
        "fourth"), what_1L_chr = c("old", "new")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    iterations_1L_int <- length(unique(iterations_int))
    if (type_1L_chr == "first") {
        new_data_tb <- Y_Ready4useDyad@ds_tb
        if (!identical(rewind_chr, character(0))) {
            new_data_tb <- rewind_chr %>% purrr::reduce(.init = new_data_tb, 
                ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                  !!rlang::sym(.y) - !!rlang::sym(paste0(.y, 
                    "_change")))))
        }
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
                      .y)), iterations_int = 1L, rewind_chr = rewind_chr, 
                  what_1L_chr = what_1L_chr) %>% procureSlot("ds_tb") %>% 
                  dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                    !!rlang::sym(paste0(var_1L_chr, "_sim_", 
                      ifelse(.y == 0, "mean", "1"))))) %>% dplyr::select(-tidyr::all_of(paste0(var_1L_chr, 
                  "_sim_", c("1", "mean")))))
            }))
    }
    if (type_1L_chr == "third") {
        Y_Ready4useDyad <- add_simulated_data(model_mdl = model_mdl, 
            var_1L_chr = var_1L_chr, Y_Ready4useDyad = Y_Ready4useDyad, 
            iterations_int = iterations_int, join_with_chr = join_with_chr, 
            rewind_chr = rewind_chr, what_1L_chr = what_1L_chr)
        Y_Ready4useDyad <- add_simulated_data(model_mdl = model_mdl, 
            var_1L_chr = var_1L_chr, Y_Ready4useDyad = Y_Ready4useDyad, 
            iterations_int = iterations_int, type_1L_chr = "fourth", 
            rewind_chr = rewind_chr, what_1L_chr = what_1L_chr)
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
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'treatment'
#' @param tx_duration_dtm Treatment duration (a date vector), Default: lubridate::weeks(12)
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_treatment_event
#' @export 
#' @importFrom lubridate weeks time_length
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
#' @keywords internal
add_treatment_event <- function (X_Ready4useDyad, tx_models_ls, adjustment_1L_dbl = -2, 
    bl_week_1L_dbl = 0, iterations_int = 1:100L, measurement_1L_int = integer(0), 
    prefix_1L_chr = "treatment", tx_duration_dtm = lubridate::weeks(12)) 
{
    if (!"Period" %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(Period = "0 to 0 Weeks"))
    }
    if (!paste0(prefix_1L_chr, "_change") %in% names(X_Ready4useDyad@ds_tb)) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(prefix_1L_chr, 
                "_change")), NA_character_)))
    }
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = c(paste0(prefix_1L_chr, 
        "_status"), paste0(prefix_1L_chr, "_change")))
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
    X_Ready4useDyad <- update_tx_start_end(X_Ready4useDyad, prefix_1L_chr = prefix_1L_chr, 
        tx_duration_dtm = tx_duration_dtm) %>% update_order()
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
#' Add unset variables
#' @description add_unset_vars() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add unset variables. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param var_names_chr Variable names (a character vector)
#' @param value_xx Value (an output object of multiple potential types), Default: 0
#' @return Data (a tibble)
#' @rdname add_unset_vars
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_unset_vars <- function (data_tb, var_names_chr, value_xx = 0) 
{
    need_to_set_chr <- setdiff(var_names_chr, names(data_tb))
    if (!identical(need_to_set_chr, character(0))) {
        data_tb <- need_to_set_chr %>% purrr::reduce(.init = data_tb, 
            ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), value_xx)))
    }
    return(data_tb)
}
#' Add utility event
#' @description add_utility_event() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add utility event. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param add_qalys_1L_lgl Add Quality Adjusted Life Years (a logical vector of length one), Default: FALSE
#' @param add_sensitivity_1L_lgl Add sensitivity (a logical vector of length one), Default: FALSE
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: 0
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: integer(0)
#' @param utility_fns_ls Utility functions (a list), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: 1:100L
#' @param maintain_for_1L_int Maintain for (an integer vector of length one), Default: 0
#' @param models_ls Models (a list), Default: NULL
#' @param rewind_chr Rewind (a character vector), Default: character(0)
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param simulate_1L_lgl Simulate (a logical vector of length one), Default: TRUE
#' @param tfmn_ls Transformation (a list), Default: NULL
#' @param tidy_1L_lgl Tidy (a logical vector of length one), Default: TRUE
#' @param tidy_cols_1L_lgl Tidy columns (a logical vector of length one), Default: FALSE
#' @param update_1L_int Update (an integer vector of length one), Default: integer(0)
#' @param utilities_chr Utilities (a character vector), Default: c("CHU9D", "AQoL6D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("Model", "Function", "Project")
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
    adjustment_1L_dbl = 0, follow_up_1L_int = integer(0), utility_fns_ls = NULL, 
    iterations_int = 1:100L, maintain_for_1L_int = 0L, models_ls = NULL, 
    rewind_chr = character(0), sensitivities_ls = make_sensitivities_ls(), 
    simulate_1L_lgl = TRUE, tfmn_ls = NULL, tidy_1L_lgl = TRUE, 
    tidy_cols_1L_lgl = FALSE, update_1L_int = integer(0), utilities_chr = c("CHU9D", 
        "AQoL6D"), type_1L_chr = c("Model", "Function", "Project"), 
    what_1L_chr = c("old", "new")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    suffix_1L_chr <- make_suffix(X_Ready4useDyad, adjustment_1L_dbl = adjustment_1L_dbl, 
        follow_up_1L_int = follow_up_1L_int, sensitivities_ls = sensitivities_ls, 
        type_1L_chr = type_1L_chr, update_1L_int = update_1L_int)
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
            if (type_1L_chr %in% c("Model", "Function") & simulate_1L_lgl) {
                if (type_1L_chr == "Model") {
                  Y_Ready4useDyad <- add_simulated_data(models_ls %>% 
                    purrr::pluck(paste0(.y, "_mdl")), var_1L_chr = paste0(.y, 
                    suffix_1L_chr), Y_Ready4useDyad = .x, iterations_int = iterations_int, 
                    join_with_chr = "Iteration", rewind_chr = rewind_chr, 
                    type_1L_chr = "second", what_1L_chr = what_1L_chr)
                }
                else {
                  calculator_fn <- utility_fns_ls %>% purrr::pluck(.y)
                  Y_Ready4useDyad <- calculator_fn(.x, var_1L_chr = paste0(.y, 
                    suffix_1L_chr))
                }
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
            if (is.null(tfmn_fn)) {
                tfmn_fn <- identity
            }
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
    if (tidy_cols_1L_lgl) {
        X_Ready4useDyad <- update_order(X_Ready4useDyad, type_1L_chr = "columns")
    }
    return(X_Ready4useDyad)
}
