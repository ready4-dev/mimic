#' Make actives tibble
#' @description make_actives_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make actives tibble. The function returns Actives (an output object of multiple potential types).
#' @param model_data_ls Model data (a list)
#' @param as_tsibble_1L_lgl As tsibble (a logical vector of length one), Default: FALSE
#' @param date_end_dtm Date end (a date vector), Default: NULL
#' @param date_start_dtm Date start (a date vector), Default: NULL
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: make_temporal_vars()
#' @param periods_1L_int Periods (an integer vector of length one), Default: integer(0)
#' @return Actives (an output object of multiple potential types)
#' @rdname make_actives_tb
#' @export 
#' @importFrom dplyr select group_by summarise filter left_join mutate lag pull across where
#' @importFrom rlang sym
#' @importFrom lubridate NA_Date_
#' @importFrom purrr map_dfr flatten_chr map2 map_int
#' @importFrom tsibble as_tsibble
#' @keywords internal
make_actives_tb <- function (model_data_ls, as_tsibble_1L_lgl = FALSE, date_end_dtm = NULL, 
    date_start_dtm = NULL, date_var_1L_chr = make_temporal_vars(), 
    periods_1L_int = integer(0)) 
{
    date_var_1L_chr <- match.arg(date_var_1L_chr)
    onboarded_tb <- processed_ls$overview@ds_tb %>% dplyr::select(UID, 
        onboarding_date) %>% add_temporal_vars(date_var_1L_chr = "onboarding_date", 
        temporal_vars_chr = c(date_var_1L_chr)) %>% dplyr::group_by(!!rlang::sym(date_var_1L_chr)) %>% 
        dplyr::summarise(Onboarded = list(unique(UID)))
    actives_tb <- model_data_ls$imputed_ls$MicroLong_r4@ds_tb %>% 
        dplyr::group_by(!!rlang::sym(date_var_1L_chr)) %>% dplyr::filter(Activity == 
        "Contact") %>% dplyr::summarise(Contacters = list(unique(UID))) %>% 
        dplyr::left_join(onboarded_tb) %>% dplyr::filter(!is.null(Onboarded) | 
        !is.null(Contacters))
    if (!identical(periods_1L_int, integer(0))) {
        date_fn <- get_temporal_fn(date_var_1L_chr)
        period_tb <- onboarded_tb %>% dplyr::mutate(Period_Starts = dplyr::lag(!!rlang::sym(date_var_1L_chr), 
            n = periods_1L_int, default = date_fn(lubridate::NA_Date_)))
        period_tb <- period_tb %>% na.omit() %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)) %>% 
            purrr::map_dfr(~{
                filtered_tb <- period_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) == 
                  .x)
                period_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >= 
                  filtered_tb$Period_Starts & !!rlang::sym(date_var_1L_chr) <= 
                  .x) %>% dplyr::summarise(`:=`(!!rlang::sym(date_var_1L_chr), 
                  .x), Onboarded_Same_Period = list(purrr::flatten_chr(Onboarded)), 
                  Period_Starts = filtered_tb$Period_Starts)
            })
        actives_tb <- dplyr::left_join(actives_tb, period_tb)
        use_1L_chr <- "Onboarded_Same_Period"
    }
    else {
        use_1L_chr <- "Onboarded"
    }
    if (!is.null(date_start_dtm)) {
        actives_tb <- actives_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >= 
            date_start_dtm)
    }
    if (!is.null(date_end_dtm)) {
        actives_tb <- actives_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) <= 
            date_end_dtm)
    }
    actives_tb <- actives_tb %>% dplyr::mutate(Retained = purrr::map2(Contacters, 
        !!rlang::sym(use_1L_chr), ~setdiff(.x, .y)), Inactive = purrr::map2(Contacters, 
        Onboarded, ~setdiff(.y, .x))) %>% dplyr::mutate(dplyr::across(dplyr::where(is.list), 
        ~.x %>% purrr::map_int(~length(.x))))
    if (as_tsibble_1L_lgl) {
        actives_xx <- actives_tb %>% tsibble::as_tsibble(index = date_var_1L_chr)
    }
    else {
        actives_xx <- actives_tb
    }
    return(actives_xx)
}
#' Make annual overview
#' @description make_annual_overview() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make annual overview. The function returns Annual (a tibble).
#' @param processed_ls Processed (a list)
#' @return Annual (a tibble)
#' @rdname make_annual_overview
#' @export 
#' @importFrom serious add_temporal_vars
#' @importFrom dplyr group_by select summarise across where
#' @keywords internal
make_annual_overview <- function (processed_ls) 
{
    annual_tb <- processed_ls$overview@ds_tb %>% serious::add_temporal_vars(date_var_1L_chr = "onboarding_date") %>% 
        dplyr::group_by(FiscalYear) %>% dplyr::select(-c(Age, 
        Year, FiscalQuarter)) %>% dplyr::summarise(Clients = length(unique(UID)), 
        dplyr::across(dplyr::where(is.numeric), ~sum(.x, na.rm = T)))
    return(annual_tb)
}
#' Make Australia price years
#' @description make_aus_price_years() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make australia price years. The function returns Price indices (a double vector).
#' @param start_1L_chr Start (a character vector of length one), Default: '2021-22'
#' @param end_1L_chr End (a character vector of length one), Default: '2024-25'
#' @return Price indices (a double vector)
#' @rdname make_aus_price_years
#' @export 
#' @keywords internal
make_aus_price_years <- function (start_1L_chr = "2021-22", end_1L_chr = "2024-25") 
{
    price_indices_dbl <- c(`2020-21` = 91.4, `2021-22` = 93.3, 
        `2022-23` = 96, `2023-24` = 100, `2024-25` = 100 * 102.5514/100.7796)
    price_indices_dbl <- price_indices_dbl[which(names(price_indices_dbl) == 
        start_1L_chr):which(names(price_indices_dbl) == end_1L_chr)]
    return(price_indices_dbl)
}
#' Make class transformations
#' @description make_class_tfmns() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make class transformations. The function returns Transformations (a list).
#' @param force_1L_lgl Force (a logical vector of length one), Default: FALSE
#' @return Transformations (a list)
#' @rdname make_class_tfmns
#' @export 
#' @importFrom youthvars youthvars_k10_aus youthvars_aqol6d_adol youthvars_chu9d_adolaus
#' @importFrom purrr map_int map_dbl
#' @keywords internal
make_class_tfmns <- function (force_1L_lgl = FALSE) 
{
    if (force_1L_lgl) {
        tfmns_ls <- list(UID = as.integer, K10 = function(x) {
            youthvars::youthvars_k10_aus(purrr::map_int(x, ~as.integer(max(10, 
                min(round(.x, 0), 50)))))
        }, AQoL6D = function(x) {
            youthvars::youthvars_aqol6d_adol(purrr::map_dbl(x, 
                ~max(0.03, min(.x, 1))))
        }, CHU9D = function(x) {
            youthvars::youthvars_chu9d_adolaus(purrr::map_dbl(x, 
                ~max(-0.2118, min(.x, 1))))
        })
    }
    else {
        tfmns_ls <- list(UID = as.integer, k10 = function(x) {
            youthvars::youthvars_k10_aus(as.integer(x))
        }, AQoL6D = youthvars::youthvars_aqol6d_adol, CHU9D = youthvars::youthvars_chu9d_adolaus)
    }
    return(tfmns_ls)
}
#' Make clinic summary
#' @description make_clinic_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make clinic summary. The function returns Clinics (a tibble).
#' @param processed_ls Processed (a list)
#' @param type_1L_chr Type (a character vector of length one), Default: serious::make_temporal_vars()
#' @return Clinics (a tibble)
#' @rdname make_clinic_summary
#' @export 
#' @importFrom serious make_temporal_vars add_temporal_vars
#' @importFrom dplyr group_by summarise mutate across where
#' @importFrom rlang sym
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_int
#' @keywords internal
make_clinic_summary <- function (processed_ls, type_1L_chr = serious::make_temporal_vars()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    clinics_tb <- processed_ls$overview@ds_tb %>% serious::add_temporal_vars(date_var_1L_chr = "onboarding_date") %>% 
        dplyr::group_by(!!rlang::sym(type_1L_chr), clinic_type) %>% 
        dplyr::summarise(Clinics = length(unique(clinic_postcode))) %>% 
        tidyr::pivot_wider(names_from = clinic_type, values_from = Clinics) %>% 
        dplyr::mutate(dplyr::across(dplyr::where(is.integer), 
            ~.x %>% purrr::map_int(~ifelse(is.na(.x), 0, .x))))
    return(clinics_tb)
}
#' Make composite results
#' @description make_composite_results() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make composite results. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param Z_Ready4useDyad PARAM_DESCRIPTION
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param exclude_suffixes_chr Exclude suffixes (a character vector), Default: character(0)
#' @param keep_chr Keep (a character vector), Default: character(0)
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param start_suffix_1L_chr Start suffix (a character vector of length one), Default: '_start'
#' @param type_1L_chr Type (a character vector of length one), Default: c("AB", "C", "D")
#' @return A (A dataset and data dictionary pair.)
#' @rdname make_composite_results
#' @export 
#' @importFrom purrr map_dfr reduce
#' @importFrom dplyr select all_of mutate ends_with inner_join bind_rows rename
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom stringr str_remove_all
#' @importFrom rlang sym
#' @keywords internal
make_composite_results <- function (X_Ready4useDyad, Y_Ready4useDyad, Z_Ready4useDyad, 
    exclude_chr = character(0), exclude_suffixes_chr = character(0), 
    keep_chr = character(0), modifiable_chr = character(0), start_suffix_1L_chr = "_start", 
    type_1L_chr = c("AB", "C", "D")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    outcomes_chr <- make_outcomes_vars(X_Ready4useDyad, Y_Ready4useDyad = Y_Ready4useDyad, 
        Z_Ready4useDyad = Z_Ready4useDyad, exclude_chr = exclude_chr, 
        exclude_suffixes_chr = exclude_suffixes_chr, modifiable_chr = modifiable_chr)
    A_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", list(Y_Ready4useDyad, 
        Z_Ready4useDyad) %>% purrr::map_dfr(~.x@ds_tb %>% dplyr::select(dplyr::all_of(c("Iteration", 
        "UID", "Arm", keep_chr, outcomes_chr)))))
    if (type_1L_chr %in% c("C", "D")) {
        numeric_chr <- make_outcomes_vars(X_Ready4useDyad, Y_Ready4useDyad = Y_Ready4useDyad, 
            Z_Ready4useDyad = Z_Ready4useDyad, exclude_chr = exclude_chr, 
            exclude_suffixes_chr = exclude_suffixes_chr, modifiable_chr = modifiable_chr, 
            numeric_only_1L_lgl = TRUE)
        D_Ready4useDyad <- renewSlot(A_Ready4useDyad, "ds_tb", 
            A_Ready4useDyad@ds_tb %>% tidyr::pivot_wider(id_cols = tidyselect::any_of(c("Iteration", 
                "UID", keep_chr)), names_from = Arm, values_from = numeric_chr))
        D_Ready4useDyad <- renewSlot(D_Ready4useDyad, "ds_tb", 
            purrr::reduce(intersect(outcomes_chr, names(D_Ready4useDyad@ds_tb) %>% 
                stringr::str_remove_all("_Intervention") %>% 
                stringr::str_remove_all("_Comparator")), .init = D_Ready4useDyad@ds_tb, 
                ~dplyr::mutate(.x, `:=`(!!rlang::sym(.y), !!rlang::sym(paste0(.y, 
                  "_Intervention")) - !!rlang::sym(paste0(.y, 
                  "_Comparator"))))))
        D_Ready4useDyad <- renewSlot(D_Ready4useDyad, "ds_tb", 
            D_Ready4useDyad@ds_tb %>% dplyr::select(-tidyselect::any_of(dplyr::ends_with("_Intervention"))) %>% 
                dplyr::select(-tidyselect::any_of(dplyr::ends_with("_Comparator"))) %>% 
                dplyr::mutate(Arm = "Difference"))
        if (type_1L_chr == "D") {
            reset_chr <- names(A_Ready4useDyad@ds_tb)[endsWith(names(A_Ready4useDyad@ds_tb), 
                start_suffix_1L_chr)]
            D_Ready4useDyad <- renewSlot(D_Ready4useDyad, "ds_tb", 
                D_Ready4useDyad@ds_tb %>% dplyr::select(-tidyselect::any_of(reset_chr)) %>% 
                  dplyr::inner_join(Z_Ready4useDyad@ds_tb %>% 
                    dplyr::select(tidyselect::any_of(c("Iteration", 
                      "UID", reset_chr)))))
        }
        A_Ready4useDyad <- renewSlot(D_Ready4useDyad, "ds_tb", 
            A_Ready4useDyad@ds_tb %>% dplyr::bind_rows(D_Ready4useDyad@ds_tb) %>% 
                dplyr::rename(Data = Arm))
    }
    return(A_Ready4useDyad)
}
#' Make conditional variables
#' @description make_conditional_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make conditional variables. The function returns Variable (a character vector of length one).
#' @param outcome_1L_chr Outcome (a character vector of length one)
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: integer(0)
#' @param fup_var_1L_chr Follow-up variable (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("end", "fup", "start", "years")
#' @return Variable (a character vector of length one)
#' @rdname make_conditional_vars
#' @export 
#' @importFrom lubridate weeks years
#' @keywords internal
make_conditional_vars <- function (outcome_1L_chr, follow_up_1L_int = integer(0), fup_var_1L_chr = character(0), 
    type_1L_chr = c("end", "fup", "start", "years")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (identical(fup_var_1L_chr, character(0))) {
        fup_var_1L_chr <- paste0(outcome_1L_chr, "_", follow_up_1L_int, 
            "_Weeks")
    }
    if (!identical(follow_up_1L_int, integer(0))) {
        start_var_1L_chr <- outcome_1L_chr
        end_var_1L_chr <- fup_var_1L_chr
        yrs_1L_dbl <- lubridate::weeks(follow_up_1L_int)/lubridate::years(1)
    }
    else {
        start_var_1L_chr <- paste0(outcome_1L_chr, "_previous")
        end_var_1L_chr <- outcome_1L_chr
        yrs_1L_dbl <- numeric(0)
    }
    var_1L_chr <- switch(type_1L_chr, end = end_var_1L_chr, fup = fup_var_1L_chr, 
        start = start_var_1L_chr, years = yrs_1L_dbl)
    return(var_1L_chr)
}
#' Make confusion list
#' @description make_confusion_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make confusion list. The function returns Confusion (a list).
#' @param regressions_ls Regressions (a list)
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param var_1L_chr Variable (a character vector of length one)
#' @param high_1L_chr High (a character vector of length one), Default: '#2E86C1'
#' @param low_1L_chr Low (a character vector of length one), Default: '#D6EAF8'
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param named_1L_lgl Named (a logical vector of length one), Default: FALSE
#' @param part_1L_int Part (an integer vector of length one), Default: integer(0)
#' @param plot_1L_lgl Plot (a logical vector of length one), Default: FALSE
#' @param tfmn_fn Transformation (a function), Default: identity
#' @param tfmn_args_ls Transformation arguments (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "tests", "models")
#' @param what_1L_chr What (a character vector of length one), Default: c("AQoL6D", "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
#'    "Tx_Treatment", "Tx_Discharged")
#' @return Confusion (a list)
#' @rdname make_confusion_ls
#' @export 
#' @importFrom rlang exec sym
#' @importFrom caret confusionMatrix
#' @importFrom stats predict
#' @importFrom dplyr pull rename mutate
#' @importFrom yardstick conf_mat
#' @importFrom ggplot2 autoplot scale_fill_gradient
#' @keywords internal
make_confusion_ls <- function (regressions_ls, X_Ready4useDyad, var_1L_chr, high_1L_chr = "#2E86C1", 
    low_1L_chr = "#D6EAF8", model_1L_int = integer(0), named_1L_lgl = FALSE, 
    part_1L_int = integer(0), plot_1L_lgl = FALSE, tfmn_fn = identity, 
    tfmn_args_ls = NULL, type_1L_chr = c("candidates", "tests", 
        "models"), what_1L_chr = c("AQoL6D", "CHU9D", "K10", 
        "Minutes", "Treatments", "Tx_Waitlist", "Tx_Treatment", 
        "Tx_Discharged")) 
{
    model_mdl <- get_regression(regressions_ls, model_1L_int = model_1L_int, 
        named_1L_lgl = named_1L_lgl, part_1L_int = part_1L_int, 
        type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)
    if (!is.null(tfmn_args_ls)) {
        data_tb <- rlang::exec(tfmn_fn, X_Ready4useDyad@ds_tb, 
            !!!tfmn_args_ls)
    }
    else {
        data_tb <- X_Ready4useDyad@ds_tb %>% tfmn_fn()
    }
    confusion_ls <- list(summary_ls = caret::confusionMatrix(stats::predict(model_mdl, 
        data_tb) %>% dplyr::pull(.pred_class), data_tb %>% dplyr::pull(!!rlang::sym(var_1L_chr))), 
        matrix_plt = yardstick::conf_mat(stats::predict(model_mdl, 
            data_tb) %>% dplyr::rename(Predicted = .pred_class) %>% 
            dplyr::mutate(Observed = data_tb %>% dplyr::pull(!!rlang::sym(var_1L_chr))), 
            Observed, Predicted, dnn = c("Predicted", "Observed")) %>% 
            ggplot2::autoplot(type = "heatmap") + ggplot2::scale_fill_gradient(low = low_1L_chr, 
            high = high_1L_chr))
    return(confusion_ls)
}
#' Make contacters series
#' @description make_contacters_series() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make contacters series. The function is called for its side effects and does not return a value.
#' @param model_data_ls Model data (a list)
#' @return X (A dataset and data dictionary pair.)
#' @rdname make_contacters_series
#' @export 
#' @importFrom dplyr filter pull mutate case_when
#' @keywords internal
make_contacters_series <- function (model_data_ls) 
{
    end_dtm <- model_data_ls$unimputed_ls$Joiners_r4@ds_tb$Date %>% 
        max()
    start_dtm <- model_data_ls$imputed_ls$Series_r4@ds_tb %>% 
        dplyr::filter(Minutes > 0) %>% dplyr::pull(Date) %>% 
        min()
    X_Ready4useDyad <- renewSlot(model_data_ls$imputed_ls$Series_r4, 
        "ds_tb", model_data_ls$imputed_ls$Series_r4@ds_tb %>% 
            dplyr::mutate(Contacts = dplyr::case_when(direct_mins > 
                0 ~ 1, T ~ 0)) %>% dplyr::filter(Date >= start_dtm, 
            Date <= end_dtm))
    return(X_Ready4useDyad)
}
#' Make contacters summary
#' @description make_contacters_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make contacters summary. The function returns Contacters (a tibble).
#' @param processed_ls Processed (a list)
#' @param as_tsibble_1L_lgl As tsibble (a logical vector of length one), Default: FALSE
#' @param type_1L_chr Type (a character vector of length one), Default: serious::make_temporal_vars()
#' @return Contacters (a tibble)
#' @rdname make_contacters_summary
#' @export 
#' @importFrom serious make_temporal_vars add_temporal_vars
#' @importFrom dplyr group_by summarise mutate
#' @importFrom rlang sym
#' @importFrom tsibble as_tsibble
#' @keywords internal
make_contacters_summary <- function (processed_ls, as_tsibble_1L_lgl = FALSE, type_1L_chr = serious::make_temporal_vars()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    contacters_tb <- processed_ls$contacts@ds_tb %>% serious::add_temporal_vars(date_var_1L_chr = "date_contacted") %>% 
        dplyr::group_by(!!rlang::sym(type_1L_chr)) %>% dplyr::summarise(Minutes = sum(Minutes), 
        Clients = length(unique(UID))) %>% dplyr::mutate(`Minutes per Client` = Minutes/Clients)
    if (as_tsibble_1L_lgl) {
        contacters_tb <- contacters_tb %>% tsibble::as_tsibble(index = type_1L_chr)
    }
    return(contacters_tb)
}
#' Make cost per minimums tibble
#' @description make_cost_per_mins_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cost per minimums tibble. The function returns Data (a tibble).
#' @param mbs_tb Medicare Benefits Schedule (a tibble), Default: NULL
#' @param cost_types_chr Cost types (a character vector), Default: character(0)
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param mbs_fl_nm_1L_chr Medicare Benefits Schedule file name (a character vector of length one), Default: character(0)
#' @param mbs_sheet_1L_chr Medicare Benefits Schedule sheet (a character vector of length one), Default: 'Table EXP.14'
#' @param mbs_skip_1L_int Medicare Benefits Schedule skip (an integer vector of length one), Default: 4
#' @param unit_cost_fl_nm_1L_chr Unit cost file name (a character vector of length one), Default: character(0)
#' @param unit_cost_tb_ls Unit cost (a list of tibbles), Default: NULL
#' @return Data (a tibble)
#' @rdname make_cost_per_mins_tb
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_dfr pluck map2_dbl
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames
#' @importFrom dplyr select filter mutate case_when group_by summarise nth first left_join
#' @importFrom stringr str_replace
#' @keywords internal
make_cost_per_mins_tb <- function (mbs_tb = NULL, cost_types_chr = character(0), path_1L_chr = character(0), 
    mbs_fl_nm_1L_chr = character(0), mbs_sheet_1L_chr = "Table EXP.14", 
    mbs_skip_1L_int = 4, unit_cost_fl_nm_1L_chr = character(0), 
    unit_cost_tb_ls = NULL) 
{
    if (identical(mbs_fl_nm_1L_chr, character(0))) {
        mbs_fl_nm_1L_chr <- "Expenditure on mental health services 2022-23.xlsx"
    }
    if (identical(cost_types_chr, character(0))) {
        cost_types_chr <- c("Allied health costs", "Psychologist costs", 
            "psychaitrist costs", "GP costs")
    }
    if (is.null(unit_cost_tb_ls)) {
        test_1L_lgl <- assertthat::assert_that(!identical(unit_cost_fl_nm_1L_chr, 
            character(0)), msg = "A non empty value for unit_cost_fl_nm_1L_chr must be supplied in order to read the unit cost file.")
        unit_cost_tb_ls <- cost_types_chr %>% purrr::map(~readxl::read_xlsx(paste0(path_1L_chr, 
            "/", unit_cost_fl_nm_1L_chr), sheet = .x)) %>% stats::setNames(cost_types_chr)
    }
    if (is.null(mbs_tb)) {
        mbs_tb <- readxl::read_xlsx(paste0(path_1L_chr, "/", 
            mbs_fl_nm_1L_chr), sheet = mbs_sheet_1L_chr, skip = mbs_skip_1L_int)
    }
    data_tb <- cost_types_chr %>% purrr::map_dfr(~{
        data_tb <- unit_cost_tb_ls %>% purrr::pluck(.x)
        data_tb <- data_tb %>% dplyr::select(c(Professional, 
            Duration, Interaction, Fee)) %>% dplyr::filter(!is.na(Duration)) %>% 
            dplyr::filter(Duration != "-") %>% dplyr::filter(Interaction == 
            "individual") %>% dplyr::mutate(Duration = Duration %>% 
            stringr::str_replace("1hour", "60mins") %>% stringr::str_replace("minuts", 
            "mins") %>% stringr::str_replace("minutes", "mins") %>% 
            stringr::str_replace(" mins", "") %>% stringr::str_replace("mins", 
            "") %>% stringr::str_replace(">", "greater than ") %>% 
            stringr::str_replace("<", "less than ") %>% stringr::str_replace("At least ", 
            "greater than ")) %>% dplyr::mutate(Multiplier = dplyr::case_when(startsWith(Duration, 
            "greater than ") ~ 1.1, startsWith(Duration, "less than ") ~ 
            1/1.1, T ~ 1)) %>% dplyr::mutate(Duration = stringr::str_replace(Duration, 
            "greater than ", "") %>% stringr::str_replace("less than ", 
            "")) %>% dplyr::mutate(MeanMinutes = strsplit(Duration, 
            "-") %>% purrr::map2_dbl(Multiplier, ~mean(as.numeric(.x) * 
            .y))) %>% dplyr::mutate(PerMinute = Fee/MeanMinutes) %>% 
            dplyr::group_by(Professional) %>% dplyr::summarise(Fee = mean(Fee), 
            Minutes = mean(MeanMinutes), PerMinute = mean(PerMinute))
        data_tb
    })
    mbs_tb <- mbs_tb %>% dplyr::select(c(Provider, `Expenditure type`, 
        `Expenditure measure`, `2023–24`)) %>% dplyr::filter(`Expenditure measure` == 
        "Constant price ($'000)") %>% dplyr::filter(`Expenditure type` != 
        "Total fees charged")
    mbs_tb <- mbs_tb %>% dplyr::group_by(Provider) %>% dplyr::summarise(CopaymentMultiplier = dplyr::nth(`2023–24`, 
        n = 2)/dplyr::first(`2023–24`))
    data_tb <- data_tb %>% dplyr::mutate(Provider = dplyr::case_when(Professional %in% 
        c("OT", "social worker", "Psychologist") ~ "Other allied health", 
        Professional == "Clinical Psychologist" ~ "Clinical psychologists", 
        Professional == "GP" ~ "General practitioners", Professional == 
            "Psychiatrist" ~ "Psychiatrists")) %>% dplyr::left_join(mbs_tb) %>% 
        dplyr::mutate(CopaymentPerMinute = PerMinute * CopaymentMultiplier) %>% 
        dplyr::group_by(Provider) %>% dplyr::summarise(Subsidy = mean(PerMinute), 
        OOP = mean(CopaymentPerMinute)) %>% dplyr::mutate(Total = Subsidy + 
        OOP)
    return(data_tb)
}
#' Make disciplines
#' @description make_disciplines() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make disciplines. The function returns Disciplines (a character vector).
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: c("default", "semi", "ordered")
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @return Disciplines (a character vector)
#' @rdname make_disciplines
#' @export 
#' @keywords internal
make_disciplines <- function (arrange_1L_chr = c("default", "semi", "ordered"), exclude_chr = character(0)) 
{
    arrange_1L_chr <- match.arg(arrange_1L_chr)
    if (arrange_1L_chr == "default") {
        medical_chr <- c("Psychiatrist", "GP", "OtherMedical")
    }
    else {
        medical_chr <- c("GP", "Psychiatrist", "OtherMedical")
    }
    disciplines_chr <- c("ClinicalPsychologist", medical_chr, 
        "Nurse", "Other")
    if (arrange_1L_chr == "ordered") {
        disciplines_chr <- sort(disciplines_chr)
    }
    if (!identical(exclude_chr, character(0))) {
        disciplines_chr <- setdiff(disciplines_chr, exclude_chr)
    }
    return(disciplines_chr)
}
#' Make draws tibble
#' @description make_draws_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make draws tibble. The function returns Draws (a tibble).
#' @param inputs_ls Inputs (a list)
#' @param iterations_int Iterations (an integer vector), Default: 1:100
#' @param scale_1L_int Scale (an integer vector of length one), Default: 1
#' @param seed_1L_int Seed (an integer vector of length one), Default: integer(0)
#' @return Draws (a tibble)
#' @rdname make_draws_tb
#' @export 
#' @importFrom janitor row_to_names
#' @importFrom dplyr mutate across everything rename_with where reframe first nth last select
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @keywords internal
make_draws_tb <- function (inputs_ls, iterations_int = 1:100, scale_1L_int = 1L, 
    seed_1L_int = integer(0)) 
{
    if (!identical(seed_1L_int, integer(0))) {
        set.seed(seed_1L_int)
    }
    iterations_1L_int <- length(unique(iterations_int))
    params_tb <- inputs_ls$params_tb
    reshaped_tb <- params_tb %>% as.data.frame() %>% t() %>% 
        janitor::row_to_names(1) %>% as.data.frame() %>% dplyr::mutate(dplyr::across(dplyr::everything(), 
        ~as.numeric(.x))) %>% tibble::rownames_to_column("Statistic") %>% 
        tibble::as_tibble() %>% dplyr::rename_with(.fn = ~paste0("Param", 
        .x), .cols = dplyr::where(is.numeric))
    draws_tb <- reshaped_tb %>% dplyr::reframe(Iteration = iterations_int, 
        dplyr::across(dplyr::where(is.numeric), list(mean = ~rnorm(iterations_1L_int, 
            mean = dplyr::first(.x), sd = dplyr::nth(.x, 2)), 
            sd = ~dplyr::last(.x))))
    if (!is.null(inputs_ls$pooled_ls)) {
        draws_tb <- 1:length(inputs_ls$pooled_ls) %>% purrr::reduce(.init = draws_tb, 
            ~{
                pooled_mdl <- inputs_ls$pooled_ls[[.y]]$model_ls
                args_ls <- inputs_ls$pooled_ls[[.y]]$arguments_ls
                name_1L_chr <- names(inputs_ls$pooled_ls)[.y]
                predictions_dbl <- predict_from_pool(pooled_mdl, 
                  adjustment_1L_dbl = args_ls$adjustment_1L_dbl, 
                  distributions_chr = args_ls$distributions_chr, 
                  n_1L_int = iterations_1L_int * scale_1L_int, 
                  seed_1L_int = seed_1L_int, resample_1L_lgl = T, 
                  what_1L_chr = name_1L_chr)
                .x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("ParamPool", 
                  name_1L_chr)), sample(predictions_dbl, size = iterations_1L_int)))
            })
    }
    draws_tb <- draws_tb %>% dplyr::select(-tidyselect::any_of(c("Iteration_mean", 
        "Iteration_sd")))
    return(draws_tb)
}
#' Make economic summary
#' @description make_economic_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make economic summary. The function returns Economic (an output object of multiple potential types).
#' @param sim_results_ls Sim results (a list)
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param costs_1L_chr Costs (a character vector of length one), Default: 'Cost'
#' @param effects_1L_chr Effects (a character vector of length one), Default: 'QALYs'
#' @param reference_1L_chr Reference (a character vector of length one), Default: 'Intervention'
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @param what_1L_chr What (a character vector of length one), Default: 'total'
#' @return Economic (an output object of multiple potential types)
#' @rdname make_economic_summary
#' @export 
#' @importFrom ready4show ready4show_correspondences manufacture.ready4show_correspondences
#' @importFrom purrr pluck
#' @importFrom dplyr filter
#' @importFrom BCEA bcea
#' @keywords internal
make_economic_summary <- function (sim_results_ls, correspondences_r3 = ready4show::ready4show_correspondences(), 
    costs_1L_chr = "Cost", effects_1L_chr = "QALYs", reference_1L_chr = "Intervention", 
    threshold_1L_dbl = 96000, what_1L_chr = "total") 
{
    data_tb <- procureSlot(sim_results_ls %>% purrr::pluck(paste0(what_1L_chr, 
        "_ls")) %>% purrr::pluck("X"), "ds_tb") %>% dplyr::filter(Data != 
        "Difference")
    names_chr <- data_tb$Data %>% unique() %>% sort()
    effects_mat <- make_results_matrix(data_tb, names_chr = names_chr, 
        var_1L_chr = effects_1L_chr)
    costs_mat <- make_results_matrix(data_tb, names_chr = names_chr, 
        var_1L_chr = costs_1L_chr)
    reference_1L_int <- which(names_chr == reference_1L_chr)
    if (!identical(correspondences_r3, ready4show::ready4show_correspondences())) {
        names_chr <- ready4show::manufacture.ready4show_correspondences(correspondences_r3, 
            names_chr, flatten_1L_lgl = T)
    }
    economic_xx <- BCEA::bcea(effects_mat, costs_mat, ref = reference_1L_int, 
        interventions = names_chr, Kmax = threshold_1L_dbl)
    return(economic_xx)
}
#' Make enhanced pool
#' @description make_enhanced_pool() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make enhanced pool. The function returns Pooled (a list).
#' @param pooled_fits_ls Pooled fits (a list)
#' @param arguments_ls Arguments (a list)
#' @return Pooled (a list)
#' @rdname make_enhanced_pool
#' @export 
#' @importFrom purrr map pluck
#' @importFrom stats setNames
#' @keywords internal
make_enhanced_pool <- function (pooled_fits_ls, arguments_ls) 
{
    pooled_ls <- names(pooled_fits_ls) %>% purrr::map(~list(model_ls = pooled_fits_ls %>% 
        purrr::pluck(.x), arguments_ls = arguments_ls %>% purrr::pluck(.x))) %>% 
        stats::setNames(names(pooled_fits_ls))
    return(pooled_ls)
}
#' Make episodes lookup table
#' @description make_episodes_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make episodes lookup table. The function returns Episodes lookup table (a tibble).
#' @param processed_ls Processed (a list)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param add_programs_int Add programs (an integer vector), Default: integer(0)
#' @param distinct_orgs_1L_lgl Distinct organisations (a logical vector of length one), Default: TRUE
#' @param drop_chr Drop (a character vector), Default: character(0)
#' @param filter_1L_lgl Filter (a logical vector of length one), Default: TRUE
#' @param filter_fn Filter (a function), Default: identity
#' @param keep_chr Keep (a character vector), Default: c("client_key", "program_type", "referral_date")
#' @param phn_code_1L_chr Primary Health Network code (a character vector of length one), Default: 'PHN_code'
#' @param phn_name_1L_chr Primary Health Network name (a character vector of length one), Default: 'PHN_area_name'
#' @param program_services_lup Program services (a lookup table), Default: NULL
#' @param program_true_1L_chr Program true (a character vector of length one), Default: 'is_program'
#' @param program_true_int Program true (an integer vector), Default: integer(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("one", "two")
#' @return Episodes lookup table (a tibble)
#' @rdname make_episodes_lup
#' @export 
#' @importFrom dplyr filter select distinct pull left_join bind_rows mutate
#' @importFrom rlang sym
#' @importFrom tidyselect all_of any_of
#' @keywords internal
make_episodes_lup <- function (processed_ls, provider_lup_tb, add_programs_int = integer(0), 
    distinct_orgs_1L_lgl = TRUE, drop_chr = character(0), filter_1L_lgl = TRUE, 
    filter_fn = identity, keep_chr = c("client_key", "program_type", 
        "referral_date"), phn_code_1L_chr = "PHN_code", phn_name_1L_chr = "PHN_area_name", 
    program_services_lup = NULL, program_true_1L_chr = "is_program", 
    program_true_int = integer(0), type_1L_chr = c("one", "two")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "two") {
        service_centres_lup <- processed_ls$outcomes_tb %>% dplyr::filter(InScope) %>% 
            dplyr::select(service_centre, organisation_path) %>% 
            dplyr::distinct()
        organisations_chr <- service_centres_lup %>% dplyr::pull(organisation_path) %>% 
            unique()
        episodes_lup_tb <- processed_ls$contacts_tb %>% dplyr::filter(organisation_path %in% 
            organisations_chr) %>% dplyr::select(organisation_path, 
            episode_key) %>% dplyr::distinct() %>% dplyr::left_join(service_centres_lup)
        if (!identical(add_programs_int, integer(0))) {
            additional_lup <- processed_ls$episodes_tb %>% dplyr::filter(program_type %in% 
                add_programs_int) %>% dplyr::select(c(organisation_path, 
                episode_key, program_type)) %>% dplyr::filter(!episode_key %in% 
                unique(episodes_lup_tb$episode_key))
            if (distinct_orgs_1L_lgl) {
                additional_lup <- additional_lup %>% dplyr::filter(!organisation_path %in% 
                  organisations_chr)
            }
        }
        episodes_lup_tb <- episodes_lup_tb %>% dplyr::bind_rows(additional_lup)
    }
    else {
        episodes_lup_tb <- processed_ls$episodes_tb
        if (!identical(program_true_int, integer(0))) {
            episodes_lup_tb <- episodes_lup_tb %>% add_mds_org_vars(provider_lup_tb = provider_lup_tb, 
                phn_code_1L_chr = phn_code_1L_chr, phn_name_1L_chr = phn_name_1L_chr)
            episodes_lup_tb <- episodes_lup_tb %>% dplyr::mutate(`:=`(!!rlang::sym(program_true_1L_chr), 
                program_type %in% program_true_int))
            episodes_lup_tb <- episodes_lup_tb %>% dplyr::left_join(program_services_lup %>% 
                dplyr::filter(!!rlang::sym(program_true_1L_chr)) %>% 
                dplyr::select(tidyselect::all_of(c(names(program_services_lup)[1:3], 
                  program_true_1L_chr, "extra_logic"))))
            if (filter_1L_lgl) {
                episodes_lup_tb <- episodes_lup_tb %>% filter_fn()
            }
            episodes_lup_tb <- episodes_lup_tb %>% dplyr::select(-extra_logic)
        }
        if (!identical(keep_chr, character(0))) {
            episodes_lup_tb <- dplyr::select(episodes_lup_tb, 
                tidyselect::any_of(unique(c("episode_key", keep_chr, 
                  program_true_1L_chr, "is_eligible_service"))))
        }
        if (!identical(drop_chr, character(0))) {
            episodes_lup_tb <- dplyr::select(episodes_lup_tb, 
                -tidyselect::all_of(drop_chr))
        }
    }
    return(episodes_lup_tb)
}
#' Make experts tibble
#' @description make_experts_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make experts tibble. The function returns Experts (a tibble).
#' @param experts_ls Experts (a list)
#' @param anonymise_1L_lgl Anonymise (a logical vector of length one), Default: T
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Expert '
#' @return Experts (a tibble)
#' @rdname make_experts_tb
#' @export 
#' @importFrom purrr map2_dfr reduce
#' @importFrom dplyr mutate select everything rename arrange
#' @importFrom stringr str_sub
#' @importFrom rlang sym
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences manufacture.ready4show_correspondences
#' @keywords internal
make_experts_tb <- function (experts_ls, anonymise_1L_lgl = T, prefix_1L_chr = "Expert ") 
{
    experts_tb <- experts_ls %>% purrr::map2_dfr(names(experts_ls), 
        ~{
            data_tb <- .x %>% dplyr::mutate(Question = .y) %>% 
                dplyr::select(Question, dplyr::everything())
            fix_chr <- intersect(names(data_tb), c("Name", "ExpertId"))
            if (!identical(fix_chr, character(0))) {
                data_tb <- fix_chr %>% purrr::reduce(.init = data_tb, 
                  ~{
                    if (.y %in% names(.x)) {
                      new_1L_chr <- tolower(.y)
                      if (stringr::str_sub(.y, start = -2) == 
                        "Id") {
                        new_1L_chr <- paste0(stringr::str_sub(new_1L_chr, 
                          end = -3), "Id")
                      }
                      .x %>% dplyr::rename(`:=`(!!rlang::sym(new_1L_chr), 
                        !!rlang::sym(.y)))
                    }
                    else {
                      .x
                    }
                  })
            }
            data_tb
        })
    if (anonymise_1L_lgl) {
        experts_chr <- experts_tb$name %>% unique() %>% sample()
        new_chr <- paste0(prefix_1L_chr, 1:length(experts_chr))
        correspondences_r3 <- ready4show::ready4show_correspondences() %>% 
            ready4show::renew.ready4show_correspondences(old_nms_chr = experts_chr, 
                new_nms_chr = new_chr)
        experts_tb <- experts_tb %>% dplyr::mutate(name = ready4show::manufacture.ready4show_correspondences(correspondences_r3, 
            data_ls = list(name), flatten_1L_lgl = T))
        experts_tb <- experts_tb %>% dplyr::arrange(Question, 
            name)
    }
    return(experts_tb)
}
#' Make Initial Assessment andeferral parameters
#' @description make_iar_params() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make initial assessment andeferral parameters. The function returns Parameters (a double vector).
#' @param processed_ls Processed (a list)
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param test_1L_chr Test (a character vector of length one)
#' @param comparator_1L_chr Comparator (a character vector of length one), Default: 'Comparator'
#' @param comparator_int Comparator (an integer vector), Default: integer(0)
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param type_1L_chr Type (a character vector of length one), Default: c(intervention_1L_chr, comparator_1L_chr)
#' @param what_1L_chr What (a character vector of length one), Default: 'InHouseIAR'
#' @return Parameters (a double vector)
#' @rdname make_iar_params
#' @export 
#' @importFrom dplyr filter pull mutate case_when
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @keywords internal
make_iar_params <- function (processed_ls, raw_mds_data_ls, test_1L_chr, comparator_1L_chr = "Comparator", 
    comparator_int = integer(0), intervention_1L_chr = "Intervention", 
    type_1L_chr = c(intervention_1L_chr, comparator_1L_chr), 
    what_1L_chr = "InHouseIAR") 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "InHouseIAR") {
        if (type_1L_chr == intervention_1L_chr) {
            intake_keys_chr <- processed_ls$outcomes_tb %>% dplyr::filter(!is.na(intake_key), 
                InScope, Mature) %>% dplyr::pull(intake_key) %>% 
                unique()
            org_paths_chr <- processed_ls$outcomes_tb %>% dplyr::filter(!is.na(intake_key), 
                InScope, Mature) %>% dplyr::pull(organisation_path) %>% 
                unique()
        }
        if (type_1L_chr == comparator_1L_chr) {
            intake_keys_chr <- processed_ls$outcomes_tb %>% dplyr::filter(!is.na(intake_key), 
                !(!!rlang::sym(test_1L_chr)), Mature) %>% dplyr::filter(program_type %in% 
                comparator_int) %>% dplyr::pull(intake_key) %>% 
                unique()
            org_paths_chr <- processed_ls$outcomes_tb %>% dplyr::filter(!is.na(intake_key), 
                !(!!rlang::sym(test_1L_chr)), Mature) %>% dplyr::filter(program_type %in% 
                comparator_int) %>% dplyr::pull(organisation_path) %>% 
                unique()
        }
        intakes_tb <- raw_mds_data_ls$intakes %>% dplyr::filter(intake_key %in% 
            intake_keys_chr) %>% tibble::as_tibble()
        intakes_tb <- intakes_tb %>% dplyr::mutate(InHouseIAR = dplyr::case_when(organisation_path %in% 
            org_paths_chr ~ T, !organisation_path %in% org_paths_chr ~ 
            F, T ~ NA))
        mean_1L_dbl <- intakes_tb$InHouseIAR %>% mean(na.rm = T)
        n_1L_dbl <- intakes_tb$InHouseIAR %>% stats::na.omit() %>% 
            as.vector() %>% length()
        se_1L_dbl <- sqrt((mean_1L_dbl * (1 - mean_1L_dbl))/n_1L_dbl)
        parameters_dbl <- c(mean_1L_dbl, se_1L_dbl, n_1L_dbl)
    }
    return(parameters_dbl)
}
#' Make interactions summary
#' @description make_interactions_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make interactions summary. The function returns Interactions (a tibble).
#' @param processed_ls Processed (a list)
#' @return Interactions (a tibble)
#' @rdname make_interactions_summary
#' @export 
#' @importFrom serious add_temporal_vars
#' @importFrom dplyr filter group_by select summarise across where
#' @keywords internal
make_interactions_summary <- function (processed_ls) 
{
    interactions_tb <- processed_ls$overview@ds_tb %>% serious::add_temporal_vars(date_var_1L_chr = "onboarding_date") %>% 
        dplyr::filter(FiscalYear == "2023-2024") %>% dplyr::group_by(Month) %>% 
        dplyr::select(-c(Age, Year, FiscalQuarter)) %>% dplyr::summarise(Clients = length(unique(UID)), 
        dplyr::across(dplyr::where(is.numeric), ~sum(.x, na.rm = T)))
    return(interactions_tb)
}
#' Make joiners series
#' @description make_joiners_series() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make joiners series. The function is called for its side effects and does not return a value.
#' @param model_data_ls Model data (a list)
#' @return X (A dataset and data dictionary pair.)
#' @rdname make_joiners_series
#' @export 
#' @importFrom dplyr select filter
#' @keywords internal
make_joiners_series <- function (model_data_ls) 
{
    end_dtm <- model_data_ls$unimputed_ls$Joiners_r4@ds_tb$Date %>% 
        max()
    X_Ready4useDyad <- renewSlot(model_data_ls$imputed_ls$Series_r4, 
        "ds_tb", model_data_ls$imputed_ls$Series_r4@ds_tb %>% 
            dplyr::select(-Minutes) %>% dplyr::filter(Date <= 
            end_dtm))
    return(X_Ready4useDyad)
}
#' Make K10 change tibble
#' @description make_k10_change_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make k10 change tibble. The function returns K10 change (a tibble).
#' @param population_k10_tb Population K10 (a tibble)
#' @return K10 change (a tibble)
#' @rdname make_k10_change_tb
#' @export 
#' @importFrom dplyr group_by summarise nth first
#' @keywords internal
make_k10_change_tb <- function (population_k10_tb) 
{
    k10_change_tb <- population_k10_tb %>% dplyr::group_by(Area, 
        Treatment) %>% dplyr::summarise(`K10 change W17 to W19` = dplyr::nth(Mean, 
        2) - dplyr::first(Mean), `K10 change W21 to W23` = dplyr::nth(Mean, 
        4) - dplyr::nth(Mean, 3))
    return(k10_change_tb)
}
#' Make K10 severity cuts
#' @description make_k10_severity_cuts() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make k10 severity cuts. The function returns Severity cuts (a list).
#' @param mild_int Mild (an integer vector), Default: c(10, 15)
#' @param moderate_int Moderate (an integer vector), Default: c(16, 21)
#' @param high_int High (an integer vector), Default: c(22, 29)
#' @param very_high_int Very high (an integer vector), Default: c(30, 50)
#' @return Severity cuts (a list)
#' @rdname make_k10_severity_cuts
#' @export 
#' @keywords internal
make_k10_severity_cuts <- function (mild_int = c(10, 15), moderate_int = c(16, 21), high_int = c(22, 
    29), very_high_int = c(30, 50)) 
{
    severity_cuts_ls <- list(Low = mild_int, Moderate = moderate_int, 
        High = high_int, VeryHigh = very_high_int)
    return(severity_cuts_ls)
}
#' Make mapping parameters tibble
#' @description make_mapping_params_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make mapping parameters tibble. The function returns Mapping parameters (a tibble).

#' @return Mapping parameters (a tibble)
#' @rdname make_mapping_params_tb
#' @export 
#' @importFrom tibble tribble
#' @importFrom dplyr arrange
#' @keywords internal
make_mapping_params_tb <- function () 
{
    mapping_params_tb <- tibble::tribble(~Parameter, ~Mean, ~SE, 
        ~SD, ~Source, "EQ5DBetaAge", -0.01382, 0.00202, NA_real_, 
        "doi:10.1192/bjo.2018.21", "EQ5DBetaConstant", 3.5222, 
        0.13543, NA_real_, "doi:10.1192/bjo.2018.21", "EQ5DBetaK10", 
        -0.06476, 0.00337, NA_real_, "doi:10.1192/bjo.2018.21", 
        "SF6DBetaConstant", 0.805, 0.12/((7907 + 6792) * 0.596), 
        NA_real_, "https://doi.org/10.1016/j.jval.2024.12.002", 
        "SF6DBetaFemaleModerate", -0.059, 0.002, NA_real_, "https://doi.org/10.1016/j.jval.2024.12.002", 
        "SF6DBetaFemaleHigh", -0.124, 0.003, NA_real_, "https://doi.org/10.1016/j.jval.2024.12.002", 
        "SF6DBetaMaleModerate", -0.055, 0.002, NA_real_, "https://doi.org/10.1016/j.jval.2024.12.002", 
        "SF6DBetaMaleHigh", -0.123, 0.0035, NA_real_, "https://doi.org/10.1016/j.jval.2024.12.002") %>% 
        dplyr::arrange(Parameter)
    return(mapping_params_tb)
}
#' Make Minimum Dataset clients tibble
#' @description make_mds_clients_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset clients tibble. The function returns a Minimum Dataset clients (a tibble).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @return a Minimum Dataset clients (a tibble)
#' @rdname make_mds_clients_tb
#' @export 
#' @importFrom dplyr mutate case_when select
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @keywords internal
make_mds_clients_tb <- function (raw_mds_data_ls) 
{
    mds_clients_tb <- raw_mds_data_ls$clients %>% dplyr::mutate(date_of_birth = transform_integer_dates(date_of_birth), 
        est_date_of_birth = as.character(est_date_of_birth) %>% 
            purrr::map_chr(~switch(.x, `1` = "Accurate", `2` = "Estimate", 
                `8` = "Dummy", `9` = "Unknown")), client_gender = as.character(client_gender) %>% 
            purrr::map_chr(~switch(.x, `0` = "Unknown", `1` = "Male", 
                `2` = "Female", `3` = "Other")), Australian = dplyr::case_when(country_of_birth %in% 
            c(1101) ~ T, country_of_birth %in% c(9999) ~ NA, 
            T ~ FALSE), EnglishMain = dplyr::case_when(main_lang_at_home %in% 
            c(1201) ~ T, main_lang_at_home %in% c(9999) ~ NA, 
            T ~ FALSE), ProficientEnglish = dplyr::case_when(prof_english %in% 
            0:2 ~ T, prof_english == 9 ~ NA, T ~ FALSE), COVID = dplyr::case_when(X.covid19 == 
            "Yes" ~ T, X.covid19 == "No" ~ F, T ~ NA)) %>% dplyr::select(organisation_path, 
        client_key, slk, date_of_birth, est_date_of_birth, client_gender, 
        Australian, EnglishMain, ProficientEnglish, COVID) %>% 
        tibble::as_tibble()
    return(mds_clients_tb)
}
#' Make Minimum Dataset collection tibble
#' @description make_mds_collection_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset collection tibble. The function returns Collection (a tibble).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param drop_chr Drop (a character vector), Default: c("collection_occasion_tags", "X.covid19")
#' @return Collection (a tibble)
#' @rdname make_mds_collection_tb
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr map_chr
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @keywords internal
make_mds_collection_tb <- function (raw_mds_data_ls, drop_chr = c("collection_occasion_tags", 
    "X.covid19")) 
{
    collection_tb <- raw_mds_data_ls$collection_occasions %>% 
        dplyr::mutate(collection_occasion_date = collection_occasion_date %>% 
            transform_integer_dates(), reason_for_collection = as.character(reason_for_collection) %>% 
            purrr::map_chr(~switch(.x, `1` = "Start", `2` = "Review", 
                `3` = "End")))
    if (!identical(drop_chr, character(0))) {
        collection_tb <- collection_tb %>% dplyr::select(-tidyselect::all_of(drop_chr))
    }
    collection_tb <- tibble::as_tibble(collection_tb)
    return(collection_tb)
}
#' Make Minimum Dataset costing dataset
#' @description make_mds_costing_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset costing dataset. The function returns COSTS (a ready4 module).
#' @param processed_ls Processed (a list)
#' @param expenditure_tb Expenditure (a tibble)
#' @param params_tb Parameters (a tibble)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param add_programs_int Add programs (an integer vector), Default: integer(0)
#' @param comparator_1L_chr Comparator (a character vector of length one), Default: 'Comp'
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param end_dtm End (a date vector), Default: as.Date("2024-06-30")
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intv'
#' @param intervention_long_1L_chr Intervention long (a character vector of length one), Default: 'The intervention'
#' @param jurisdiction_1L_chr Jurisdiction (a character vector of length one), Default: 'Jurisdiction'
#' @param mds_programs_lup Minimum Dataset programs (a lookup table), Default: make_mds_program_lup()
#' @param start_dtm Start (a date vector), Default: as.Date("2023-07-01")
#' @param summary_1L_lgl Summary (a logical vector of length one), Default: T
#' @return COSTS (a ready4 module)
#' @rdname make_mds_costing_ds
#' @export 
#' @importFrom dplyr filter select distinct pull left_join mutate everything case_when relocate group_by summarise across where bind_rows all_of n
#' @importFrom tidyselect all_of any_of
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @importFrom collapse na_omit
#' @importFrom ready4use Ready4useDyad add_dictionary
#' @keywords internal
make_mds_costing_ds <- function (processed_ls, expenditure_tb, params_tb, provider_lup_tb, 
    add_programs_int = integer(0), comparator_1L_chr = "Comp", 
    disciplines_chr = make_disciplines(), end_dtm = as.Date("2024-06-30"), 
    intervention_1L_chr = "Intv", intervention_long_1L_chr = "The intervention", 
    jurisdiction_1L_chr = "Jurisdiction", mds_programs_lup = make_mds_program_lup(), 
    start_dtm = as.Date("2023-07-01"), summary_1L_lgl = T) 
{
    service_centres_lup <- processed_ls$outcomes_tb %>% dplyr::filter(InScope) %>% 
        dplyr::select(service_centre, organisation_path) %>% 
        dplyr::distinct()
    organisations_chr <- service_centres_lup %>% dplyr::pull(organisation_path) %>% 
        unique()
    episodes_lup <- make_episodes_lup(processed_ls, add_programs_int = add_programs_int, 
        distinct_orgs_1L_lgl = FALSE, provider_lup_tb = provider_lup_tb, 
        type_1L_chr = "two")
    episode_keys_chr <- episodes_lup %>% dplyr::pull(episode_key) %>% 
        unique()
    clients_lup <- processed_ls$episodes_tb %>% dplyr::filter(episode_key %in% 
        episode_keys_chr) %>% dplyr::select(episode_key, client_key) %>% 
        dplyr::distinct()
    services_tb <- make_mds_services_tb(processed_ls, end_dtm = end_dtm, 
        episode_keys_chr = episode_keys_chr, start_dtm = start_dtm, 
        summarise_1L_lgl = T)
    services_tb <- services_tb %>% dplyr::select(-tidyselect::all_of(names(services_tb)[endsWith(names(services_tb), 
        "Prop")]))
    services_tb <- services_tb %>% dplyr::left_join(episodes_lup)
    params_tb <- params_tb %>% dplyr::filter(endsWith(Parameter, 
        "PerMin")) %>% dplyr::select(Parameter, Mean)
    services_tb <- 1:nrow(params_tb) %>% purrr::reduce(.init = services_tb, 
        ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("Param", 
            params_tb$Parameter[.y])), params_tb$Mean[.y])))
    services_tb <- services_tb %>% dplyr::left_join(mds_programs_lup) %>% 
        dplyr::select(c(episode_key, program_type, program_name, 
            service_centre, dplyr::everything())) %>% dplyr::mutate(Program = dplyr::case_when(is.na(program_name) & 
        !is.na(service_centre) ~ intervention_long_1L_chr, !is.na(program_name) ~ 
        program_name, T ~ NA_character_)) %>% dplyr::relocate(Program, 
        .after = episode_key) %>% dplyr::select(-c(program_type, 
        program_name))
    complete_tb <- services_tb %>% dplyr::select(tidyselect::any_of(c("episode_key", 
        "Program", "service_centre", setdiff(names(services_tb)[names(services_tb) %>% 
            endsWith("UseMins")], "TotalUseMins")))) %>% collapse::na_omit(cols = setdiff(names(services_tb)[names(services_tb) %>% 
        endsWith("UseMins")], "TotalUseMins"))
    mean_mins_tb <- complete_tb %>% dplyr::group_by(Program) %>% 
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
            ~mean(.x, na.rm = T)))
    missing_tb <- services_tb %>% dplyr::select(Program, service_centre, 
        episode_key, tidyselect::all_of(setdiff(names(services_tb)[names(services_tb) %>% 
            endsWith("UseMins")], "TotalUseMins"))) %>% dplyr::filter(!episode_key %in% 
        complete_tb$episode_key) %>% dplyr::select(Program, service_centre, 
        episode_key)
    imputed_services_tb <- dplyr::bind_rows(complete_tb, missing_tb %>% 
        dplyr::left_join(mean_mins_tb)) %>% dplyr::left_join(services_tb %>% 
        dplyr::select(tidyselect::all_of(dplyr::all_of(c("Program", 
            "service_centre", names(services_tb)[startsWith(names(services_tb), 
                "Param")])))) %>% dplyr::distinct())
    clients_lup <- services_tb %>% dplyr::select(Program, service_centre, 
        episode_key) %>% dplyr::left_join(clients_lup) %>% dplyr::select(c(Program, 
        service_centre, client_key)) %>% dplyr::distinct() %>% 
        dplyr::group_by(Program) %>% dplyr::summarise(Clients = dplyr::n())
    COSTS_r4 <- ready4use::Ready4useDyad(ds_tb = imputed_services_tb)
    COSTS_r4 <- add_project_2_costs(COSTS_r4, disciplines_chr = disciplines_chr, 
        intervention_1L_chr = intervention_1L_chr, total_1L_lgl = F)
    COSTS_r4 <- renewSlot(COSTS_r4, "ds_tb", COSTS_r4@ds_tb %>% 
        dplyr::group_by(Program) %>% dplyr::summarise(dplyr::across(tidyselect::all_of(names(COSTS_r4@ds_tb)[endsWith(names(COSTS_r4@ds_tb), 
        "Cost")]), ~sum(.x, na.rm = T))) %>% dplyr::mutate(TotalVariable = dplyr::select(., 
        dplyr::where(is.numeric)) %>% rowSums(na.rm = TRUE)) %>% 
        dplyr::left_join(expenditure_tb) %>% dplyr::mutate(TotalFixed = Expenditure - 
        TotalVariable) %>% dplyr::left_join(clients_lup) %>% 
        dplyr::select(Program, Year, dplyr::everything()))
    if (summary_1L_lgl) {
        COSTS_r4 <- renewSlot(COSTS_r4, "ds_tb", COSTS_r4@ds_tb %>% 
            dplyr::mutate(Intervention = dplyr::case_when(Program == 
                intervention_long_1L_chr ~ intervention_1L_chr, 
                T ~ comparator_1L_chr)) %>% dplyr::group_by(Intervention) %>% 
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
                ~sum(.x))) %>% dplyr::mutate(FixedPerClient = TotalFixed/Clients, 
            VariablePerClient = TotalVariable/Clients, TotalPerClient = Expenditure/Clients, 
            VariableToTotalMultiplier = Expenditure/TotalVariable))
    }
    COSTS_r4 <- COSTS_r4 %>% ready4use::add_dictionary()
    return(COSTS_r4)
}
#' Make Minimum Dataset episodes tibble
#' @description make_mds_episodes_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset episodes tibble. The function returns Episodes (a tibble).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param processed_ls Processed (a list)
#' @return Episodes (a tibble)
#' @rdname make_mds_episodes_tb
#' @export 
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across case_when group_by summarise left_join
#' @importFrom tidyselect all_of
#' @keywords internal
make_mds_episodes_tb <- function (raw_mds_data_ls, processed_ls) 
{
    episodes_tb <- raw_mds_data_ls$episodes %>% tibble::as_tibble() %>% 
        dplyr::mutate(dplyr::across(tidyselect::all_of(c("episode_end_date", 
            "referral_date")), ~transform_integer_dates(.x)), 
            COVID = dplyr::case_when(X.covid19 == "Yes" ~ T, 
                X.covid19 == "No" ~ F, T ~ NA))
    dates_tb <- processed_ls$contacts_tb %>% dplyr::group_by(episode_key) %>% 
        dplyr::summarise(first_service_date = min(service_contact_date), 
            last_service_date = max(service_contact_date))
    episodes_tb <- dplyr::left_join(episodes_tb, dates_tb)
    return(episodes_tb)
}
#' Make Minimum Dataset expenditure tibble
#' @description make_mds_expenditure_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset expenditure tibble. The function returns Expenditure (a tibble).
#' @param raw_expenditure_tb Raw expenditure (a tibble), Default: NULL
#' @param path_to_param_data_1L_chr Path to parameter data (a character vector of length one), Default: character(0)
#' @param file_nm_1L_chr File name (a character vector of length one), Default: character(0)
#' @param program_1L_chr Program (a character vector of length one)
#' @param sheet_1L_chr Sheet (a character vector of length one), Default: character(0)
#' @param skip_1L_int Skip (an integer vector of length one), Default: 0
#' @param additional_tb Additional (a tibble), Default: NULL
#' @param price_indices_dbl Price indices (a double vector), Default: make_aus_price_years("2021-22")
#' @param program_services_lup Program services (a lookup table), Default: NULL
#' @param price_ref_1L_int Price reference (an integer vector of length one), Default: 3
#' @param year_1L_chr Year (a character vector of length one), Default: '2023-24'
#' @return Expenditure (a tibble)
#' @rdname make_mds_expenditure_tb
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter pull select mutate summarise bind_rows rename
#' @importFrom rlang sym
#' @importFrom serious update_for_price_year
#' @keywords internal
make_mds_expenditure_tb <- function (raw_expenditure_tb = NULL, path_to_param_data_1L_chr = character(0), 
    file_nm_1L_chr = character(0), program_1L_chr, sheet_1L_chr = character(0), 
    skip_1L_int = 0, additional_tb = NULL, price_indices_dbl = make_aus_price_years("2021-22"), 
    program_services_lup = NULL, price_ref_1L_int = 3, year_1L_chr = "2023-24") 
{
    if (is.null(raw_expenditure_tb)) {
        raw_expenditure_tb <- readxl::read_xlsx(paste0(path_to_param_data_1L_chr, 
            "/", file_nm_1L_chr), sheet = sheet_1L_chr, skip = skip_1L_int)
    }
    expenditure_tb <- raw_expenditure_tb
    if (!is.null(program_services_lup)) {
        locations_chr <- program_services_lup %>% dplyr::filter(include) %>% 
            dplyr::pull(service_centre)
        expenditure_tb <- expenditure_tb %>% dplyr::filter(Location %in% 
            locations_chr)
    }
    expenditure_tb <- expenditure_tb %>% dplyr::select(Location, 
        !!rlang::sym(year_1L_chr)) %>% dplyr::mutate(`:=`(!!rlang::sym(year_1L_chr), 
        as.numeric(!!rlang::sym(year_1L_chr)))) %>% dplyr::summarise(Program = program_1L_chr, 
        Year = year_1L_chr, Cost = sum(!!rlang::sym(year_1L_chr)))
    if (!is.null(additional_tb)) {
        expenditure_tb <- expenditure_tb %>% dplyr::bind_rows(additional_tb)
    }
    expenditure_tb <- serious::update_for_price_year(expenditure_tb, 
        price_indices_dbl = price_indices_dbl, price_ref_1L_int = price_ref_1L_int, 
        time_var_1L_chr = "Year")
    expenditure_tb <- dplyr::rename(expenditure_tb, Expenditure = Cost)
    return(expenditure_tb)
}
#' Make Minimum Dataset modelling dataset
#' @description make_mds_modelling_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset modelling dataset. The function returns Model data (a list).
#' @param processed_ls Processed (a list)
#' @param outcomes_ls Outcomes (a list)
#' @param program_services_lup Program services (a lookup table)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param after_dtm After (a date vector), Default: as.Date("2022-07-01")
#' @param add_programs_int Add programs (an integer vector), Default: c(1, 4)
#' @param age_max_1L_int Age maximum (an integer vector of length one), Default: integer(0)
#' @param age_min_1L_int Age minimum (an integer vector of length one), Default: integer(0)
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param distinct_orgs_1L_lgl Distinct organisations (a logical vector of length one), Default: TRUE
#' @param impute_below_1L_dbl Impute below (a double vector of length one), Default: 40
#' @param imputations_1L_int Imputations (an integer vector of length one), Default: 1
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intv'
#' @param jurisdiction_1L_chr Jurisdiction (a character vector of length one), Default: 'Jurisdiction'
#' @param mature_only_1L_lgl Mature only (a logical vector of length one), Default: TRUE
#' @param max_iterations_1L_int Maximum iterations (an integer vector of length one), Default: 2
#' @param missing_after_dtm Missing after (a date vector), Default: Sys.Date()
#' @param postcode_lup Postcode (a lookup table), Default: NULL
#' @param program_true_1L_chr Program true (a character vector of length one), Default: 'is_program'
#' @param require_complete_chr Require complete (a character vector), Default: character(0)
#' @return Model data (a list)
#' @rdname make_mds_modelling_ds
#' @export 
#' @importFrom dplyr filter left_join mutate case_when select relocate arrange rename rename_with pull where
#' @importFrom rlang sym
#' @importFrom lubridate NA_Date_
#' @importFrom collapse na_omit
#' @importFrom tibble tibble
#' @importFrom ready4use Ready4useDyad add_dictionary
#' @importFrom purrr map flatten_chr
#' @keywords internal
make_mds_modelling_ds <- function (processed_ls, outcomes_ls, program_services_lup, provider_lup_tb, 
    after_dtm = as.Date("2022-07-01"), add_programs_int = c(1, 
        4), age_max_1L_int = integer(0), age_min_1L_int = integer(0), 
    disciplines_chr = make_disciplines(), distinct_orgs_1L_lgl = TRUE, 
    impute_below_1L_dbl = 40, imputations_1L_int = 1, intervention_1L_chr = "Intv", 
    jurisdiction_1L_chr = "Jurisdiction", mature_only_1L_lgl = TRUE, 
    max_iterations_1L_int = 2, missing_after_dtm = Sys.Date(), 
    postcode_lup = NULL, program_true_1L_chr = "is_program", 
    require_complete_chr = character(0)) 
{
    data_tb <- make_tfd_mds_outcomes_tb(processed_ls, jurisdiction_1L_chr = jurisdiction_1L_chr, 
        outcomes_ls = outcomes_ls, missing_after_dtm = missing_after_dtm, 
        postcode_lup = postcode_lup, referrals_1L_lgl = F, reviews_1L_lgl = F)
    episodes_lup <- make_episodes_lup(processed_ls, add_programs_int = add_programs_int, 
        distinct_orgs_1L_lgl = distinct_orgs_1L_lgl, program_services_lup = program_services_lup %>% 
            dplyr::filter(!!rlang::sym(program_true_1L_chr)), 
        provider_lup_tb = provider_lup_tb, type_1L_chr = "two")
    episode_keys_chr <- unique(intersect(episodes_lup$episode_key, 
        processed_ls$outcomes_tb$episode_key))
    services_tb <- make_mds_services_tb(processed_ls, episode_keys_chr = episode_keys_chr, 
        summarise_1L_lgl = T)
    data_tb <- data_tb %>% dplyr::left_join(services_tb)
    if (!identical(age_max_1L_int, integer(0))) {
        data_tb <- data_tb %>% dplyr::filter(Age <= age_max_1L_int)
    }
    if (!identical(age_min_1L_int, integer(0))) {
        data_tb <- data_tb %>% dplyr::filter(Age >= age_min_1L_int)
    }
    data_tb <- data_tb %>% dplyr::mutate(Gender = dplyr::case_when(Gender == 
        "Unknown" ~ NA_character_, T ~ Gender))
    if (mature_only_1L_lgl) {
        data_tb <- data_tb %>% dplyr::filter(MatureService)
    }
    if (!identical(after_dtm, lubridate::NA_Date_)) {
        data_tb <- data_tb %>% dplyr::filter(first_service_date >= 
            after_dtm)
    }
    if (!identical(require_complete_chr, character(0))) {
        data_tb <- data_tb %>% collapse::na_omit(cols = require_complete_chr, 
            prop = 1)
    }
    if ("iar_dst_practitioner_level_of_care" %in% names(data_tb)) {
        data_tb <- data_tb %>% dplyr::mutate(HasIAR = dplyr::case_when(!is.na(iar_dst_practitioner_level_of_care) ~ 
            T, is.na(iar_dst_practitioner_level_of_care) ~ F, 
            T ~ NA))
    }
    data_tb <- data_tb %>% dplyr::select(-episode_key)
    new_ids_tb <- tibble::tibble(client_key = unique(data_tb$client_key), 
        UID = 1:length(unique(data_tb$client_key)) %>% sample())
    data_tb <- data_tb %>% dplyr::left_join(new_ids_tb) %>% dplyr::relocate(UID, 
        .after = InterventionGroup) %>% dplyr::arrange(UID, Episode)
    data_tb <- data_tb %>% dplyr::select(-client_key)
    data_tb <- data_tb %>% dplyr::rename(`:=`(!!rlang::sym(intervention_1L_chr), 
        service_centre))
    data_tb <- data_tb %>% dplyr::rename_with(~gsub("k10p_score", 
        "K10", .x)) %>% dplyr::rename_with(~gsub("K10_Start", 
        "K10", .x))
    data_tb <- data_tb %>% dplyr::mutate(RecordID = paste0(UID, 
        "_", Episode))
    data_tb <- data_tb %>% dplyr::select(-(names(data_tb)[endsWith(names(data_tb), 
        "MinsProp")]))
    X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_tb) %>% 
        ready4use::add_dictionary()
    missing_tb <- list(X_Ready4useDyad = X_Ready4useDyad) %>% 
        make_missing_report()
    do_not_impute_chr <- missing_tb %>% dplyr::filter(pct_miss >= 
        impute_below_1L_dbl) %>% dplyr::pull(variable)
    do_not_impute_chr <- c(do_not_impute_chr, c(disciplines_chr, 
        "Total") %>% purrr::map(~c(.x, paste0(.x, "ContactMins"))) %>% 
        purrr::flatten_chr() %>% intersect(names(X_Ready4useDyad@ds_tb)))
    impute_chr <- setdiff(missing_tb %>% dplyr::filter(pct_miss < 
        impute_below_1L_dbl) %>% dplyr::pull(variable), do_not_impute_chr)
    model_data_ls <- make_project_imputations(X_Ready4useDyad, 
        characteristics_chr = intersect(X_Ready4useDyad@ds_tb %>% 
            dplyr::select(dplyr::where(is.character)) %>% names(), 
            impute_chr), date_vars_chr = X_Ready4useDyad@ds_tb %>% 
            dplyr::select(dplyr::where(function(x) inherits(x, 
                "Date"))) %>% names(), extras_chr = character(0), 
        ignore_x_chr = do_not_impute_chr, imputations_1L_int = imputations_1L_int, 
        max_iterations_1L_int = max_iterations_1L_int, method_1L_chr = "rf", 
        post_imputation_fn = function(x) add_mds_minutes_totals(x, 
            add_chr = "Use"), uid_var_1L_chr = "RecordID")
    model_data_ls$unimputed_ls$missing_tb <- missing_tb
    return(model_data_ls)
}
#' Make Minimum Dataset outcomes tibble
#' @description make_mds_outcomes_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset outcomes tibble. The function returns Outcomes (a tibble).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param processed_ls Processed (a list)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param add_start_date_1L_lgl Add start date (a logical vector of length one)
#' @param filter_fn Filter (a function), Default: identity
#' @param program_services_lup Program services (a lookup table)
#' @param program_true_chr Program true (a character vector)
#' @param program_type_ls Program type (a list)
#' @param as_wide_1L_lgl As wide (a logical vector of length one), Default: FALSE
#' @param frequencies_chr Frequencies (a character vector), Default: paste0("k10p_item", c(1:10, 14))
#' @param integers_chr Integers (a character vector), Default: c("k10p_score", paste0("k10p_item", 11:13))
#' @param mature_after_dtm Mature after (a date vector), Default: lubridate::years(1)
#' @param outcomes_ls Outcomes (a list), Default: list(k10p = c("k10p_score", paste0("k10p_item", 11:14)))
#' @param remove_duplicates_1L_lgl Remove duplicates (a logical vector of length one), Default: TRUE
#' @param review_target_dtm Review target (a date vector), Default: lubridate::days(90)
#' @return Outcomes (a tibble)
#' @rdname make_mds_outcomes_tb
#' @export 
#' @importFrom lubridate years days
#' @importFrom dplyr select distinct left_join filter bind_rows mutate across case_when arrange
#' @importFrom tibble as_tibble
#' @importFrom purrr map pluck reduce flatten_chr
#' @importFrom tidyselect any_of all_of
#' @importFrom tidyr pivot_wider
#' @keywords internal
make_mds_outcomes_tb <- function (raw_mds_data_ls, processed_ls, provider_lup_tb, add_start_date_1L_lgl, 
    filter_fn = identity, program_services_lup, program_true_chr, 
    program_type_ls, as_wide_1L_lgl = FALSE, frequencies_chr = paste0("k10p_item", 
        c(1:10, 14)), integers_chr = c("k10p_score", paste0("k10p_item", 
        11:13)), mature_after_dtm = lubridate::years(1), outcomes_ls = list(k10p = c("k10p_score", 
        paste0("k10p_item", 11:14))), remove_duplicates_1L_lgl = TRUE, 
    review_target_dtm = lubridate::days(90)) 
{
    collection_tb <- processed_ls$collection_tb
    intake_tb <- raw_mds_data_ls$intake_episodes %>% dplyr::select("episode_key", 
        "intake_key") %>% tibble::as_tibble() %>% dplyr::distinct()
    collection_tb <- collection_tb %>% dplyr::left_join(intake_tb)
    prefixes_chr <- setdiff(names(outcomes_ls), "iar_dst")
    outcomes_ls <- 1:length(outcomes_ls) %>% purrr::map(~{
        ds_tb <- raw_mds_data_ls %>% purrr::pluck(names(outcomes_ls)[.x])
        ds_tb <- dplyr::select(ds_tb, tidyselect::any_of(unique(c("organisation_path", 
            "measure_key", "collection_occasion_key", "intake_key", 
            outcomes_ls[[.x]]))))
        if (names(outcomes_ls)[.x] == "iar_dst") {
            ds_tb <- ds_tb %>% dplyr::select(tidyselect::any_of(c("intake_key", 
                outcomes_ls[[.x]]))) %>% tibble::as_tibble() %>% 
                dplyr::distinct()
            dplyr::left_join(collection_tb, ds_tb %>% dplyr::filter(!is.na(intake_key)) %>% 
                dplyr::filter(!duplicated(intake_key)))
        }
        else {
            dplyr::left_join(ds_tb, collection_tb) %>% tibble::as_tibble()
        }
    })
    outcomes_tb <- outcomes_ls %>% purrr::reduce(.init = collection_tb %>% 
        dplyr::filter(F), ~{
        joined_tb <- dplyr::left_join(.y, .x)
        distinct_tb <- .y %>% dplyr::bind_rows(joined_tb %>% 
            dplyr::select(tidyselect::all_of(intersect(names(joined_tb), 
                names(.y))))) %>% dplyr::distinct()
        joined_tb <- distinct_tb %>% dplyr::left_join(joined_tb)
        distinct_tb <- .x %>% dplyr::bind_rows(joined_tb %>% 
            dplyr::select(tidyselect::all_of(intersect(names(joined_tb), 
                names(.x))))) %>% dplyr::distinct()
        joined_tb <- distinct_tb %>% dplyr::left_join(joined_tb)
    })
    integer_updates_chr <- intersect(names(outcomes_tb), integers_chr)
    if (!identical(integer_updates_chr, character(0))) {
        outcomes_tb <- outcomes_tb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(integer_updates_chr), 
            ~dplyr::case_when(.x == 99 ~ NA_integer_, T ~ .x)))
    }
    frequency_updates_chr <- intersect(names(outcomes_tb), frequencies_chr)
    if (!identical(frequency_updates_chr, character(0))) {
        outcomes_tb <- outcomes_tb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(frequency_updates_chr), 
            ~dplyr::case_when(.x == 9 ~ NA_character_, .x == 
                1 ~ "None of the time", .x == 2 ~ "A little of the time", 
                .x == 3 ~ "Some of the time", .x == 4 ~ "Most of the time", 
                .x == 5 ~ "All of the time", T ~ NA_character_)))
    }
    outcomes_tb <- tibble::as_tibble(outcomes_tb)
    outcomes_tb <- outcomes_tb %>% add_mds_org_vars(provider_lup_tb = provider_lup_tb)
    outcomes_tb <- outcomes_tb %>% add_mds_program_vars(processed_ls = processed_ls, 
        add_start_date_1L_lgl = add_start_date_1L_lgl, filter_fn = filter_fn, 
        mature_after_dtm = mature_after_dtm, program_services_lup = program_services_lup, 
        program_true_chr = program_true_chr, program_type_ls = program_type_ls, 
        provider_lup_tb = provider_lup_tb)
    if (remove_duplicates_1L_lgl) {
        outcomes_tb <- outcomes_tb %>% transform_to_remove_duplicates()
        outcomes_tb <- outcomes_tb %>% make_mds_unique_measures(episodes_tb = processed_ls$episodes_tb, 
            review_target_dtm = review_target_dtm, prefixes_chr = prefixes_chr)
    }
    if (as_wide_1L_lgl) {
        outcomes_chr <- purrr::map(prefixes_chr, ~names(outcomes_tb)[startsWith(names(outcomes_tb), 
            .x)]) %>% purrr::flatten_chr()
        outcomes_tb <- outcomes_tb %>% dplyr::select(-tidyselect::any_of(c("measure_key", 
            "collection_occasion_key", "N", "MISSINGOUTCOMES"))) %>% 
            tidyr::pivot_wider(names_from = "reason_for_collection", 
                values_from = c("collection_occasion_date", outcomes_chr)) %>% 
            dplyr::arrange(episode_key)
    }
    return(outcomes_tb)
}
#' Make Minimum Dataset processed list
#' @description make_mds_processed_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset processed list. The function returns Processed (a list).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param add_start_date_1L_lgl Add start date (a logical vector of length one)
#' @param program_services_lup Program services (a lookup table)
#' @param program_true_chr Program true (a character vector)
#' @param program_type_ls Program type (a list)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param filter_fn Filter (a function), Default: identity
#' @param frequencies_chr Frequencies (a character vector), Default: c(paste0("k10p_item", c(1:10, 14)))
#' @param integers_chr Integers (a character vector), Default: c("k10p_score", paste0("k10p_item", 11:13), "iar_dst_practitioner_level_of_care")
#' @param mature_after_dtm Mature after (a date vector), Default: lubridate::years(1)
#' @param outcomes_ls Outcomes (a list), Default: make_project_2_outcomes_ls()
#' @param review_target_dtm Review target (a date vector), Default: lubridate::days(90)
#' @return Processed (a list)
#' @rdname make_mds_processed_ls
#' @export 
#' @importFrom lubridate years days
#' @keywords internal
make_mds_processed_ls <- function (raw_mds_data_ls, add_start_date_1L_lgl, program_services_lup, 
    program_true_chr, program_type_ls, provider_lup_tb, filter_fn = identity, 
    frequencies_chr = c(paste0("k10p_item", c(1:10, 14))), integers_chr = c("k10p_score", 
        paste0("k10p_item", 11:13), "iar_dst_practitioner_level_of_care"), 
    mature_after_dtm = lubridate::years(1), outcomes_ls = make_project_2_outcomes_ls(), 
    review_target_dtm = lubridate::days(90)) 
{
    processed_ls <- list(clients_tb = make_mds_clients_tb(raw_mds_data_ls))
    processed_ls$collection_tb <- make_mds_collection_tb(raw_mds_data_ls)
    processed_ls$contacts_tb <- make_mds_service_contacts(raw_mds_data_ls)
    processed_ls$episodes_tb <- make_mds_episodes_tb(raw_mds_data_ls, 
        processed_ls = processed_ls)
    processed_ls$outcomes_tb <- raw_mds_data_ls %>% make_mds_outcomes_tb(processed_ls = processed_ls, 
        add_start_date_1L_lgl = add_start_date_1L_lgl, as_wide_1L_lgl = T, 
        filter_fn = filter_fn, frequencies_chr = frequencies_chr, 
        integers_chr = integers_chr, mature_after_dtm = mature_after_dtm, 
        outcomes_ls = outcomes_ls, program_services_lup = program_services_lup, 
        program_true_chr = program_true_chr, program_type_ls = program_type_ls, 
        provider_lup_tb = provider_lup_tb, review_target_dtm = review_target_dtm)
    processed_ls$providers_tb <- make_mds_providers_tb(raw_mds_data_ls)
    return(processed_ls)
}
#' Make Minimum Dataset program lookup table
#' @description make_mds_program_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset program lookup table. The function returns Program (a lookup table).

#' @return Program (a lookup table)
#' @rdname make_mds_program_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_mds_program_lup <- function () 
{
    program_lup <- tibble::tibble(program_type = c(1:5, 7), program_name = c("Flexible Funding Pool", 
        "Head to Health", "AMHC", "Psychosocial", "Bushfire Recovery 2020", 
        "Supporting Recovery"))
    return(program_lup)
}
#' Make Minimum Dataset program starts
#' @description make_mds_program_starts() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset program starts. The function returns Start up (a lookup table).
#' @param processed_ls Processed (a list)
#' @param filter_fn Filter (a function)
#' @param program_services_lup Program services (a lookup table)
#' @param program_true_chr Program true (a character vector)
#' @param program_type_ls Program type (a list)
#' @param provider_lup_tb Provider lookup table (a tibble)
#' @param add_start_date_1L_lgl Add start date (a logical vector of length one), Default: FALSE
#' @param mature_after_dtm Mature after (a date vector), Default: lubridate::years(1)
#' @return Start up (a lookup table)
#' @rdname make_mds_program_starts
#' @export 
#' @importFrom lubridate years
#' @importFrom dplyr select bind_rows filter group_by summarise
#' @importFrom rlang sym
#' @keywords internal
make_mds_program_starts <- function (processed_ls, filter_fn, program_services_lup, program_true_chr, 
    program_type_ls, provider_lup_tb, add_start_date_1L_lgl = FALSE, 
    mature_after_dtm = lubridate::years(1)) 
{
    start_up_lup <- add_mds_program_vars(processed_ls$episodes_tb %>% 
        add_mds_org_vars(provider_lup_tb = provider_lup_tb), 
        processed_ls = processed_ls, add_start_date_1L_lgl = add_start_date_1L_lgl, 
        filter_fn = filter_fn, mature_after_dtm = mature_after_dtm, 
        program_services_lup = program_services_lup, program_true_chr = program_true_chr, 
        program_type_ls = program_type_ls, provider_lup_tb = provider_lup_tb)
    start_up_lup <- start_up_lup %>% dplyr::select(c(organisation_path, 
        episode_key, referral_date, !!rlang::sym(names(program_services_lup)[1])))
    start_up_lup <- dplyr::bind_rows(start_up_lup %>% dplyr::filter(!is.na(!!rlang::sym(names(program_services_lup)[1]))) %>% 
        dplyr::group_by(!!rlang::sym(names(program_services_lup)[1])) %>% 
        dplyr::summarise(organisation_path = unique(organisation_path), 
            start_date = min(referral_date)), start_up_lup %>% 
        dplyr::filter(is.na(!!rlang::sym(names(program_services_lup)[1]))) %>% 
        dplyr::group_by(organisation_path) %>% dplyr::summarise(start_date = min(referral_date)))
    return(start_up_lup)
}
#' Make Minimum Dataset providers tibble
#' @description make_mds_providers_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset providers tibble. The function returns Providers (a tibble).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param additional_chr Additional (a character vector), Default: character(0)
#' @return Providers (a tibble)
#' @rdname make_mds_providers_tb
#' @export 
#' @importFrom dplyr left_join select filter group_by summarise distinct
#' @importFrom tidyselect any_of
#' @importFrom tibble as_tibble
#' @keywords internal
make_mds_providers_tb <- function (raw_mds_data_ls, additional_chr = character(0)) 
{
    providers_tb <- raw_mds_data_ls$service_contact_practitioners %>% 
        dplyr::left_join(raw_mds_data_ls$practitioners %>% dplyr::select(tidyselect::any_of(c("organisation_path", 
            "practitioner_key", "practitioner_category", additional_chr))))
    providers_tb <- update_providers_tb(providers_tb)
    primary_providers_tb <- providers_tb %>% dplyr::filter(primary_practitioner_indicator == 
        1) %>% dplyr::select(-primary_practitioner_indicator)
    secondary_providers_tb <- providers_tb %>% dplyr::filter(primary_practitioner_indicator != 
        1) %>% dplyr::group_by(service_contact_key) %>% dplyr::summarise(secondary_providers = paste0(practitioner_category, 
        collapse = ", "))
    providers_tb <- primary_providers_tb %>% dplyr::left_join(secondary_providers_tb)
    providers_tb <- tibble::as_tibble(providers_tb) %>% dplyr::distinct()
    return(providers_tb)
}
#' Make Minimum Dataset service contacts
#' @description make_mds_service_contacts() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset service contacts. The function returns Data (a tibble).
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @return Data (a tibble)
#' @rdname make_mds_service_contacts
#' @export 
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl
#' @keywords internal
make_mds_service_contacts <- function (raw_mds_data_ls) 
{
    data_tb <- raw_mds_data_ls$service_contacts %>% tibble::as_tibble()
    data_tb <- data_tb %>% dplyr::mutate(service_contact_date = transform_integer_dates(service_contact_date))
    data_tb <- data_tb %>% dplyr::mutate(service_contact_copayment = service_contact_copayment %>% 
        as.numeric() %>% purrr::map_dbl(~ifelse(.x == 9999, NA_real_, 
        .x)))
    return(data_tb)
}
#' Make Minimum Dataset services tibble
#' @description make_mds_services_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset services tibble. The function returns Services (a tibble).
#' @param processed_ls Processed (a list)
#' @param end_dtm End (a date vector), Default: lubridate::NA_Date_
#' @param episode_keys_chr Episode keys (a character vector), Default: character(0)
#' @param start_dtm Start (a date vector), Default: lubridate::NA_Date_
#' @param summarise_1L_lgl Summarise (a logical vector of length one), Default: FALSE
#' @return Services (a tibble)
#' @rdname make_mds_services_tb
#' @export 
#' @importFrom lubridate NA_Date_
#' @importFrom dplyr filter left_join select mutate case_when rename across starts_with group_by summarise where
#' @importFrom purrr map map_int map2_lgl
#' @importFrom stringr str_replace_all
#' @keywords internal
make_mds_services_tb <- function (processed_ls, end_dtm = lubridate::NA_Date_, episode_keys_chr = character(0), 
    start_dtm = lubridate::NA_Date_, summarise_1L_lgl = FALSE) 
{
    services_tb <- processed_ls$contacts_tb
    if (!is.na(start_dtm)) {
        services_tb <- services_tb %>% dplyr::filter(service_contact_date >= 
            start_dtm)
    }
    if (!is.na(end_dtm)) {
        services_tb <- services_tb %>% dplyr::filter(service_contact_date <= 
            end_dtm)
    }
    if (!identical(episode_keys_chr, character(0))) {
        services_tb <- services_tb %>% dplyr::filter(episode_key %in% 
            episode_keys_chr)
    }
    services_tb <- services_tb %>% dplyr::left_join(processed_ls$providers_tb %>% 
        dplyr::select(-c(service_contact_practitioner_key, practitioner_key)))
    services_tb <- services_tb %>% dplyr::mutate(ClinicalPsychologist = 0, 
        GP = 0, Psychiatrist = 0, OtherMedical = 0, Nurse = 0, 
        Other = 0)
    services_tb <- services_tb %>% dplyr::mutate(ClinicalPsychologist = dplyr::case_when(practitioner_category == 
        "Clinical Psychologist" ~ ClinicalPsychologist + 1, is.na(practitioner_category) ~ 
        NA_real_, T ~ ClinicalPsychologist), GP = dplyr::case_when(practitioner_category == 
        "General Practitioner" ~ GP + 1, is.na(practitioner_category) ~ 
        NA_real_, T ~ GP), Psychiatrist = dplyr::case_when(practitioner_category == 
        "Psychiatrist" ~ Psychiatrist + 1, is.na(practitioner_category) ~ 
        NA_real_, T ~ Psychiatrist), OtherMedical = dplyr::case_when(practitioner_category == 
        "Other Medical" ~ OtherMedical + 1, is.na(practitioner_category) ~ 
        NA_real_, T ~ OtherMedical), Nurse = dplyr::case_when(practitioner_category == 
        "Mental Health Nurse" ~ Nurse + 1, is.na(practitioner_category) ~ 
        NA_real_, T ~ Nurse), Other = dplyr::case_when(practitioner_category %in% 
        c("Aboriginal and Torres Strait", "General Psychologist", 
            "Islander Health/Mental Health Worker", "Low Intensity Mental Health Worker", 
            "Occupational Therapist", "Other", "Psychosocial Support Worker", 
            "Social Worker") ~ Other + 1, is.na(practitioner_category) ~ 
        NA_real_, T ~ Other))
    services_tb <- services_tb %>% dplyr::mutate(SecondaryProviders = secondary_providers %>% 
        purrr::map(~if (is.na(.x)) {
            NA_character_
        }
        else {
            strsplit(.x, ", ") %>% unlist() %>% stringr::str_replace_all("Aboriginal and Torres Strait", 
                "Other") %>% stringr::str_replace_all("General Psychologist", 
                "Other") %>% stringr::str_replace_all("Islander Health/Mental Health Worker", 
                "Other") %>% stringr::str_replace_all("Low Intensity Mental Health Worker", 
                "Other") %>% stringr::str_replace_all("Occupational Therapist", 
                "Other") %>% stringr::str_replace_all("Psychosocial Support Worker", 
                "Other") %>% stringr::str_replace_all("Social Worker", 
                "Other") %>% stringr::str_replace_all("Peer Support Worker", 
                "Other") %>% sort()
        }))
    services_tb <- services_tb %>% dplyr::mutate(ClinicalPsychologist = dplyr::case_when(!is.na(secondary_providers) & 
        !is.na(ClinicalPsychologist) ~ ClinicalPsychologist + 
        (SecondaryProviders %>% purrr::map_int(~sum(.x == "Clinical Psychologist"))), 
        T ~ ClinicalPsychologist), GP = dplyr::case_when(!is.na(secondary_providers) & 
        !is.na(GP) ~ GP + (SecondaryProviders %>% purrr::map_int(~sum(.x == 
        "General Practitioner"))), T ~ GP), Psychiatrist = dplyr::case_when(!is.na(secondary_providers) & 
        !is.na(Psychiatrist) ~ Psychiatrist + (SecondaryProviders %>% 
        purrr::map_int(~sum(.x == "Psychiatrist"))), T ~ Psychiatrist), 
        OtherMedical = dplyr::case_when(!is.na(secondary_providers) & 
            !is.na(OtherMedical) ~ OtherMedical + (SecondaryProviders %>% 
            purrr::map_int(~sum(.x == "Other Medical"))), T ~ 
            OtherMedical), Nurse = dplyr::case_when(!is.na(secondary_providers) & 
            !is.na(Nurse) ~ Nurse + (SecondaryProviders %>% purrr::map_int(~sum(.x == 
            "Mental Health Nurse"))), T ~ Nurse), Other = dplyr::case_when(!is.na(secondary_providers) & 
            !is.na(Other) ~ Other + (SecondaryProviders %>% purrr::map_int(~sum(.x == 
            "Other"))), T ~ Other))
    services_tb <- services_tb %>% dplyr::mutate(ClientContact = service_contact_participants %>% 
        purrr::map2_lgl(service_contact_no_show, ~ifelse(is.na(.x) | 
            is.na(.y), NA, ifelse(.x %in% 1:3 & .y == 2, T, F))))
    services_tb <- services_tb %>% dplyr::mutate(GroupDelivery = dplyr::case_when(service_contact_participants == 
        2 ~ T, is.na(service_contact_participants) ~ NA, T ~ 
        F))
    services_tb <- services_tb %>% dplyr::mutate(GroupMultiplier = dplyr::case_when(GroupDelivery ~ 
        0.25, !GroupDelivery ~ 1, T ~ NA_real_))
    services_tb <- services_tb %>% dplyr::mutate(Interpreter = dplyr::case_when(service_contact_interpreter == 
        1 ~ T, service_contact_interpreter == 2 ~ F, T ~ NA))
    services_tb <- services_tb %>% dplyr::mutate(Duration = dplyr::case_when(service_contact_duration == 
        0 ~ 0, service_contact_duration == 1 ~ (1 + 15)/2, service_contact_duration == 
        2 ~ (16 + 30)/2, service_contact_duration == 3 ~ (31 + 
        45)/2, service_contact_duration == 4 ~ (46 + 60)/2, service_contact_duration == 
        5 ~ (61 + 75)/2, service_contact_duration == 6 ~ (76 + 
        90)/2, service_contact_duration == 7 ~ (91 + 105)/2, 
        service_contact_duration == 8 ~ (106 + 120)/2, service_contact_duration == 
            9 ~ 150))
    services_tb <- services_tb %>% dplyr::rename(Copayment = service_contact_copayment)
    services_tb <- services_tb %>% dplyr::mutate(dplyr::across(c(ClinicalPsychologist, 
        GP, Psychiatrist, OtherMedical, Nurse, Other), ~.x * 
        Duration * GroupMultiplier, .names = "{col}UseMins"))
    services_tb <- services_tb %>% dplyr::mutate(dplyr::across(c(ClinicalPsychologist, 
        GP, Psychiatrist, OtherMedical, Nurse, Other), ~.x * 
        Duration * ClientContact, .names = "{col}ContactMins"))
    services_tb <- add_mds_minutes_totals(services_tb, type_1L_chr = "total")
    services_tb <- services_tb %>% dplyr::select(organisation_path, 
        service_contact_key, episode_key, service_contact_date, 
        Copayment, ClientContact, Interpreter, dplyr::starts_with("ClinicalPsychologist"), 
        dplyr::starts_with("GP"), dplyr::starts_with("Psychiatrist"), 
        dplyr::starts_with("OtherMedical"), dplyr::starts_with("Nurse"), 
        dplyr::starts_with("Other"), "TotalContactMins", "TotalUseMins")
    if (summarise_1L_lgl) {
        services_tb <- services_tb %>% dplyr::group_by(episode_key) %>% 
            dplyr::summarise(dplyr::across(dplyr::where(function(x) is.numeric(x) | 
                is.logical(x)), sum))
    }
    services_tb <- add_mds_minutes_totals(services_tb, type_1L_chr = "prop")
    return(services_tb)
}
#' Make Minimum Dataset unique measures
#' @description make_mds_unique_measures() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make minimum dataset unique measures. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param episodes_tb Episodes (a tibble)
#' @param review_target_dtm Review target (a date vector), Default: lubridate::days(90)
#' @param prefixes_chr Prefixes (a character vector), Default: c("k10p_", "iar_dst_")
#' @return Data (a tibble)
#' @rdname make_mds_unique_measures
#' @export 
#' @importFrom lubridate days
#' @importFrom dplyr inner_join select arrange filter ungroup mutate case_when group_by summarise across everything first bind_rows last sample_n
#' @importFrom purrr map flatten_chr
#' @importFrom collapse na_omit
#' @importFrom tidyselect all_of any_of
#' @keywords internal
make_mds_unique_measures <- function (data_tb, episodes_tb, review_target_dtm = lubridate::days(90), 
    prefixes_chr = c("k10p_", "iar_dst_")) 
{
    data_tb <- data_tb %>% dplyr::inner_join(episodes_tb %>% 
        dplyr::select(episode_key, first_service_date, last_service_date))
    outcomes_chr <- prefixes_chr %>% purrr::map(~names(data_tb)[startsWith(names(data_tb), 
        .x)]) %>% purrr::flatten_chr()
    starts_tb <- data_tb %>% get_duplicated_measures(group_by_chr = c("episode_key", 
        "reason_for_collection")) %>% dplyr::arrange(episode_key, 
        reason_for_collection) %>% dplyr::filter(reason_for_collection == 
        "Start")
    new_starts_tb <- starts_tb %>% dplyr::ungroup() %>% dplyr::filter(reason_for_collection == 
        "Start") %>% collapse::na_omit(cols = outcomes_chr, prop = 1)
    new_starts_tb <- new_starts_tb %>% dplyr::mutate(reason_for_collection = dplyr::case_when(collection_occasion_date == 
        referral_date & (collection_occasion_date < first_service_date) ~ 
        "Referral", collection_occasion_date != referral_date & 
        (collection_occasion_date > first_service_date) ~ "Review", 
        T ~ reason_for_collection))
    new_starts_tb <- new_starts_tb %>% dplyr::mutate(MISSINGOUTCOMES = new_starts_tb %>% 
        dplyr::select(tidyselect::all_of(outcomes_chr)) %>% is.na() %>% 
        rowSums()) %>% dplyr::group_by(episode_key, reason_for_collection) %>% 
        dplyr::filter(MISSINGOUTCOMES == min(MISSINGOUTCOMES)) %>% 
        dplyr::ungroup()
    changed_from_starts_tb <- new_starts_tb %>% dplyr::filter(reason_for_collection != 
        "Start")
    new_starts_tb <- new_starts_tb %>% dplyr::filter(reason_for_collection == 
        "Start") %>% dplyr::group_by(episode_key) %>% dplyr::arrange(collection_occasion_date) %>% 
        dplyr::summarise(dplyr::across(dplyr::everything(), ~dplyr::first(.x)))
    data_tb <- data_tb %>% dplyr::filter(!(reason_for_collection == 
        "Start" & episode_key %in% starts_tb$episode_key)) %>% 
        dplyr::bind_rows(new_starts_tb) %>% dplyr::bind_rows(changed_from_starts_tb)
    ends_tb <- data_tb %>% get_duplicated_measures(group_by_chr = c("episode_key", 
        "reason_for_collection")) %>% dplyr::arrange(episode_key, 
        reason_for_collection) %>% dplyr::filter(reason_for_collection == 
        "End")
    new_ends_tb <- ends_tb %>% dplyr::ungroup() %>% dplyr::filter(reason_for_collection == 
        "End") %>% collapse::na_omit(cols = outcomes_chr, prop = 1)
    new_ends_tb <- new_ends_tb %>% dplyr::mutate(reason_for_collection = dplyr::case_when(collection_occasion_date < 
        last_service_date ~ "Review", T ~ reason_for_collection))
    new_ends_tb <- new_ends_tb %>% dplyr::mutate(MISSINGOUTCOMES = new_ends_tb %>% 
        dplyr::select(tidyselect::all_of(outcomes_chr)) %>% is.na() %>% 
        rowSums()) %>% dplyr::group_by(episode_key, reason_for_collection) %>% 
        dplyr::filter(MISSINGOUTCOMES == min(MISSINGOUTCOMES)) %>% 
        dplyr::ungroup()
    changed_from_ends_tb <- new_ends_tb %>% dplyr::filter(reason_for_collection != 
        "End")
    new_ends_tb <- new_ends_tb %>% dplyr::filter(reason_for_collection == 
        "End") %>% dplyr::group_by(episode_key) %>% dplyr::arrange(collection_occasion_date) %>% 
        dplyr::summarise(dplyr::across(dplyr::everything(), ~dplyr::last(.x)))
    data_tb <- data_tb %>% dplyr::filter(!(reason_for_collection == 
        "End" & episode_key %in% ends_tb$episode_key)) %>% dplyr::bind_rows(new_ends_tb) %>% 
        dplyr::bind_rows(changed_from_ends_tb)
    reviews_tb <- data_tb %>% dplyr::filter(reason_for_collection == 
        "Review") %>% get_duplicated_measures(group_by_chr = "episode_key")
    new_reviews_tb <- reviews_tb %>% dplyr::ungroup() %>% dplyr::filter(reason_for_collection == 
        "Review") %>% collapse::na_omit(cols = outcomes_chr, 
        prop = 1)
    new_reviews_tb <- new_reviews_tb %>% dplyr::mutate(MISSINGOUTCOMES = new_reviews_tb %>% 
        dplyr::select(tidyselect::all_of(outcomes_chr)) %>% is.na() %>% 
        rowSums()) %>% dplyr::group_by(episode_key, reason_for_collection) %>% 
        dplyr::filter(MISSINGOUTCOMES == min(MISSINGOUTCOMES)) %>% 
        dplyr::ungroup()
    new_reviews_tb <- new_reviews_tb %>% dplyr::group_by(episode_key) %>% 
        dplyr::filter(abs(collection_occasion_date - (first_service_date + 
            review_target_dtm)) == min(abs(collection_occasion_date - 
            (first_service_date + review_target_dtm)))) %>% dplyr::sample_n(size = 1) %>% 
        dplyr::ungroup()
    data_tb <- data_tb %>% dplyr::filter(!(reason_for_collection == 
        "Review" & episode_key %in% reviews_tb$episode_key)) %>% 
        dplyr::bind_rows(new_reviews_tb)
    data_tb <- data_tb %>% dplyr::select(-tidyselect::any_of(c("N", 
        "MISSINGOUTCOMES")))
    return(data_tb)
}
#' Make model dyad list
#' @description make_model_dyad_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make model dyad list. The function returns Model dyad (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param Y_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @return Model dyad (a list)
#' @rdname make_model_dyad_ls
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @keywords internal
make_model_dyad_ls <- function (X_Ready4useDyad = ready4use::Ready4useDyad(), Y_Ready4useDyad = ready4use::Ready4useDyad()) 
{
    model_dyad_ls <- list(X_Ready4useDyad = X_Ready4useDyad, 
        Y_Ready4useDyad = Y_Ready4useDyad)
    return(model_dyad_ls)
}
#' Make outcomes variables
#' @description make_outcomes_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make outcomes variables. The function returns Outcomes (a character vector).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param Z_Ready4useDyad PARAM_DESCRIPTION
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param exclude_suffixes_chr Exclude suffixes (a character vector), Default: character(0)
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param numeric_only_1L_lgl Numeric only (a logical vector of length one), Default: FALSE
#' @return Outcomes (a character vector)
#' @rdname make_outcomes_vars
#' @export 
#' @importFrom purrr reduce map_lgl
#' @importFrom dplyr pull
#' @keywords internal
make_outcomes_vars <- function (X_Ready4useDyad, Y_Ready4useDyad, Z_Ready4useDyad, 
    exclude_chr = character(0), exclude_suffixes_chr = character(0), 
    modifiable_chr = character(0), numeric_only_1L_lgl = FALSE) 
{
    outcomes_chr <- intersect(setdiff(names(Y_Ready4useDyad@ds_tb), 
        c(setdiff(names(X_Ready4useDyad@ds_tb), modifiable_chr), 
            make_structural_vars(), exclude_chr)) %>% sort(), 
        setdiff(names(Z_Ready4useDyad@ds_tb), c(setdiff(names(X_Ready4useDyad@ds_tb), 
            modifiable_chr), make_structural_vars(), exclude_chr) %>% 
            sort()))
    if (!identical(exclude_suffixes_chr, character(0))) {
        outcomes_chr <- exclude_suffixes_chr %>% purrr::reduce(.init = outcomes_chr, 
            ~.x[!.x %>% endsWith(.y)])
    }
    if (numeric_only_1L_lgl) {
        outcomes_chr <- outcomes_chr[outcomes_chr %>% purrr::map_lgl(~ifelse(.x %in% 
            names(Z_Ready4useDyad@ds_tb), Z_Ready4useDyad@ds_tb %>% 
            dplyr::pull(.x) %>% is.numeric(), FALSE))]
    }
    return(outcomes_chr)
}
#' Make parsnip model
#' @description make_parsnip_mdl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make parsnip model. The function returns Model (a model).
#' @param data_tb Data (a tibble)
#' @param model_fn Model (a function), Default: parsnip::multinom_reg
#' @param model_args_ls Model arguments (a list), Default: NULL
#' @param x_chr X (a character vector)
#' @param y_1L_chr Y (a character vector of length one)
#' @param ... Additional arguments
#' @return Model (a model)
#' @rdname make_parsnip_mdl
#' @export 
#' @importFrom parsnip multinom_reg fit
#' @importFrom rlang exec
#' @keywords internal
make_parsnip_mdl <- function (data_tb, model_fn = parsnip::multinom_reg, model_args_ls = NULL, 
    x_chr, y_1L_chr, ...) 
{
    fit_fn <- parsnip::fit
    if (is.null(model_args_ls)) {
        model_args_ls <- formals(model_fn)
    }
    model_xx <- rlang::exec(model_fn, !!!model_args_ls)
    x_1L_chr <- paste0(x_chr, collapse = " + ")
    model_mdl <- eval(parse(text = paste0("fit_fn(model_xx,", 
        y_1L_chr, " ~ ", x_1L_chr, ",", "data = data_tb, ...)")))
    return(model_mdl)
}
#' Make Primary Health Network lookup table
#' @description make_phn_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make primary health network lookup table. The function returns a Primary Health Network lookup table (an output object of multiple potential types).
#' @param code_1L_chr Code (a character vector of length one), Default: character(0)
#' @param name_1L_chr Name (a character vector of length one), Default: character(0)
#' @param jurisdiction_1L_chr Jurisdiction (a character vector of length one), Default: character(0)
#' @return a Primary Health Network lookup table (an output object of multiple potential types)
#' @rdname make_phn_lup
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename mutate
#' @importFrom rlang sym
#' @importFrom stringr str_sub
#' @importFrom purrr map_chr
#' @keywords internal
make_phn_lup <- function (code_1L_chr = character(0), name_1L_chr = character(0), 
    jurisdiction_1L_chr = character(0)) 
{
    phn_lup_xx <- ready4show::ready4show_correspondences() %>% 
        ready4show::renew.ready4show_correspondences(old_nms_chr = c(paste0("PHN10", 
            1:9), "PHN110"), new_nms_chr = c("Central and Eastern Sydney", 
            "Northern Sydney", "Western Sydney", "Nepean Blue Mountains", 
            "South Western Sydney", "South Eastern NSW", "Western NSW", 
            "Hunter New England and Central Coast", "North Coast", 
            "Murrumbidgee")) %>% ready4show::renew.ready4show_correspondences(old_nms_chr = paste0("PHN20", 
        1:6), new_nms_chr = c("North Western Melbourne", "Eastern Melbourne", 
        "South Eastern Melbourne", "Gippsland", "Murray", "Western Victoria")) %>% 
        ready4show::renew.ready4show_correspondences(old_nms_chr = paste0("PHN30", 
            1:7), new_nms_chr = c("Brisbane North", "Brisbane South", 
            "Goldcoast", "Darling Downs and West Moreton", "Western Queensland", 
            "Central Queensland, Wide Bay & Sunshine Coast", 
            "Nothern Queensland")) %>% ready4show::renew.ready4show_correspondences(old_nms_chr = paste0("PHN40", 
        1:2), new_nms_chr = c("Adelaide", "Country SA")) %>% 
        ready4show::renew.ready4show_correspondences(old_nms_chr = paste0("PHN50", 
            1:3), new_nms_chr = c("Perth North", "Perth South", 
            "Country WA")) %>% ready4show::renew.ready4show_correspondences(old_nms_chr = c("PHN601", 
        "PHN701", "PHN801"), new_nms_chr = c("Tasmania", "Nothern Territory", 
        "Australian Capital Territory"))
    if (!identical(jurisdiction_1L_chr, character(0))) {
        code_1L_chr <- ifelse(identical(code_1L_chr, character(0)), 
            "PHN_code", code_1L_chr)
        name_1L_chr <- ifelse(identical(name_1L_chr, character(0)), 
            "PHN_area_name", name_1L_chr)
    }
    if (!identical(code_1L_chr, character(0)) | !identical(name_1L_chr, 
        character(0))) {
        phn_lup_xx <- tibble::as_tibble(phn_lup_xx)
        if (!identical(code_1L_chr, character(0))) {
            phn_lup_xx <- dplyr::rename(phn_lup_xx, `:=`(!!rlang::sym(code_1L_chr), 
                old_nms_chr))
        }
        if (!identical(name_1L_chr, character(0))) {
            phn_lup_xx <- dplyr::rename(phn_lup_xx, `:=`(!!rlang::sym(name_1L_chr), 
                new_nms_chr))
        }
    }
    if (!identical(jurisdiction_1L_chr, character(0))) {
        phn_lup_xx <- phn_lup_xx %>% dplyr::mutate(`:=`(!!rlang::sym(jurisdiction_1L_chr), 
            !!rlang::sym(code_1L_chr) %>% stringr::str_sub(start = 4, 
                end = 4) %>% as.integer() %>% purrr::map_chr(~switch(.x, 
                "New South Wales", "Victoria", "Queensland", 
                "South Australia", "Western Australia", "Tasmania", 
                "Northern Territory", "Australian Capital Territory"))))
    }
    return(phn_lup_xx)
}
#' Make pooled fit
#' @description make_pooled_fit() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make pooled fit. The function returns Fit (an output object of multiple potential types).
#' @param experts_tb Experts (a tibble)
#' @param question_1L_chr Question (a character vector of length one), Default: character(0)
#' @return Fit (an output object of multiple potential types)
#' @rdname make_pooled_fit
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @importFrom dplyr filter select
#' @importFrom SHELF fitdist
#' @keywords internal
make_pooled_fit <- function (experts_tb, question_1L_chr = character(0)) 
{
    if (identical(question_1L_chr, character(0))) {
        questions_chr <- experts_tb$Question %>% unique()
        fit_xx <- questions_chr %>% purrr::map(~make_pooled_fit(experts_tb, 
            question_1L_chr = .x)) %>% stats::setNames(questions_chr)
    }
    else {
        experts_tb <- experts_tb %>% dplyr::filter(Question == 
            question_1L_chr)
        prob_cols_chr <- grep("^prob\\d+$", names(experts_tb), 
            value = TRUE)
        prob_mat <- t(as.matrix(experts_tb %>% dplyr::select(all_of(prob_cols_chr))))
        value_cols_chr <- grep("^bin\\d+$", names(experts_tb), 
            value = TRUE)
        values_mat <- t(as.matrix(experts_tb %>% dplyr::select(all_of(value_cols_chr))))
        fit_xx <- SHELF::fitdist(vals = values_mat, probs = prob_mat, 
            lower = experts_tb$bin1min, upper = experts_tb$bin10, 
            expertnames = experts_tb$name)
    }
    return(fit_xx)
}
#' Make predicted observed dataset
#' @description make_predd_observed_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make predicted observed dataset. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param consolidate_1L_chr Consolidate (a character vector of length one), Default: character(0)
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param new_1L_chr New (a character vector of length one), Default: 'Simulated'
#' @param old_1L_chr Old (a character vector of length one), Default: 'Observed'
#' @param select_chr Select (a character vector), Default: character(0)
#' @param slim_1L_lgl Slim (a logical vector of length one), Default: FALSE
#' @return Y (A dataset and data dictionary pair.)
#' @rdname make_predd_observed_ds
#' @export 
#' @importFrom dplyr mutate inner_join select across
#' @importFrom tidyr any_of all_of
#' @importFrom tidyselect any_of
#' @keywords internal
make_predd_observed_ds <- function (X_Ready4useDyad, Y_Ready4useDyad, consolidate_1L_chr = character(0), 
    join_with_chr = character(0), new_1L_chr = "Simulated", old_1L_chr = "Observed", 
    select_chr = character(0), slim_1L_lgl = FALSE) 
{
    new_chr <- setdiff(names(Y_Ready4useDyad@ds_tb), names(X_Ready4useDyad@ds_tb))
    bind_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(Data = old_1L_chr) %>% 
        dplyr::inner_join(Y_Ready4useDyad@ds_tb %>% dplyr::select(tidyr::any_of(c("UID", 
            new_chr, join_with_chr)))) %>% dplyr::mutate(dplyr::across(tidyr::all_of(new_chr), 
        ~NA_real_))
    if (!identical(consolidate_1L_chr, character(0))) {
        Y_Ready4useDyad <- transform_to_long_results(Y_Ready4useDyad, 
            var_1L_chr = consolidate_1L_chr, add_means_1L_lgl = FALSE, 
            tidy_1L_lgl = FALSE)
    }
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(Data = new_1L_chr) %>% rbind(bind_tb %>% 
        dplyr::mutate(Iteration = 0)))
    if (slim_1L_lgl) {
        Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            Y_Ready4useDyad@ds_tb %>% dplyr::select(tidyselect::any_of(c("Data", 
                "UID", "Iteration", join_with_chr, consolidate_1L_chr, 
                select_chr) %>% unique())))
    }
    Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    return(Y_Ready4useDyad)
}
#' Make project 2 days models
#' @description make_project_2_days_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 days models. The function returns Tpm models (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param add_chr Add (a character vector), Default: character(0)
#' @param family_2_1L_chr Family 2 (a character vector of length one), Default: 'Gamma(link = 'log')'
#' @param link_1_1L_chr Link 1 (a character vector of length one), Default: 'logit'
#' @param max_1L_dbl Maximum (a double vector of length one), Default: Inf
#' @param x_part_1_ls X part 1 (a list), Default: NULL
#' @param x_part_2_ls X part 2 (a list), Default: NULL
#' @param y_1L_chr Y (a character vector of length one), Default: 'EpisodeDurationDays'
#' @param use_1_int Use 1 (an integer vector), Default: integer(0)
#' @param use_2_int Use 2 (an integer vector), Default: integer(0)
#' @param ... Additional arguments
#' @return Tpm models (a list)
#' @rdname make_project_2_days_mdls
#' @export 
#' @importFrom purrr map2
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @keywords internal
make_project_2_days_mdls <- function (X_Ready4useDyad, add_chr = character(0), family_2_1L_chr = "Gamma(link = 'log')", 
    link_1_1L_chr = "logit", max_1L_dbl = Inf, x_part_1_ls = NULL, 
    x_part_2_ls = NULL, y_1L_chr = "EpisodeDurationDays", use_1_int = integer(0), 
    use_2_int = integer(0), ...) 
{
    if (is.null(x_part_1_ls)) {
        x_part_1_ls <- list(c("Intervention", "Gender", "Jurisdiction", 
            "IRSADQuintile", "Diagnosis", "K10", "HasIAR", "TreatmentPlan", 
            "Medication", add_chr), c("Intervention", "SubthresholdDisorder", 
            "K10", "HasIAR", "TreatmentPlan", "Medication", "SuicideRisk", 
            add_chr), c("Intervention", "SubthresholdDisorder", 
            "Age", "Gender", add_chr), c("Intervention", "Age", 
            "Gender", add_chr), c("Intervention", "SubthresholdDisorder", 
            "Employment", add_chr), c("Intervention", "SubthresholdDisorder", 
            "Gender", add_chr), c("Intervention", "SubthresholdDisorder", 
            "K10", add_chr), c("Intervention", "SubthresholdDisorder", 
            add_chr), c("Intervention", add_chr))
    }
    if (is.null(x_part_2_ls)) {
        x_part_2_ls <- x_part_1_ls
    }
    if (!identical(use_1_int, integer(0))) {
        x_part_1_ls <- x_part_1_ls[use_1_int]
    }
    if (!identical(use_2_int, integer(0))) {
        x_part_2_ls <- x_part_2_ls[use_2_int]
    }
    tpm_mdls_ls <- purrr::map2(x_part_1_ls, x_part_2_ls, ~make_two_part_mdl(data_tb = X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(y_1L_chr), dplyr::case_when(!!rlang::sym(y_1L_chr) > 
            max_1L_dbl ~ max_1L_dbl, T ~ !!rlang::sym(y_1L_chr)))), 
        family_2_1L_chr = family_2_1L_chr, link_1_1L_chr = link_1_1L_chr, 
        x_part_1_chr = .x, x_part_2_chr = .y, y_1L_chr = y_1L_chr)) %>% 
        stats::setNames(paste0("TPM_", 1:length(x_part_1_ls), 
            "_mdl"))
    return(tpm_mdls_ls)
}
#' Make project 2 K10 change models
#' @description make_project_2_k10_change_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 k10 change models. The function returns K10 (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param type_1L_chr Type (a character vector of length one), Default: c("Intervention", "Jurisdiction")
#' @return K10 (a list)
#' @rdname make_project_2_k10_change_mdls
#' @export 
#' @importFrom dplyr mutate
#' @keywords internal
make_project_2_k10_change_mdls <- function (X_Ready4useDyad, type_1L_chr = c("Intervention", "Jurisdiction")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    K10_ls <- list()
    if (type_1L_chr == "Intervention") {
        K10_ls <- list(OLS_1_mdl = lm(formula = K10_change ~ 
            Intervention, data = X_Ready4useDyad@ds_tb))
        K10_ls$OLS_2_mdl <- lm(formula = K10_change ~ Intervention + 
            K10, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_3_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + HasIAR, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_4_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile, 
            data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_5_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_6_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + HasIAR, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_7_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            HasIAR, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_8_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + HasIAR + Medication, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_9_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + HasIAR + TreatmentPlan, 
            data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_10_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + HasIAR + Medication + TreatmentPlan, 
            data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_11_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            EpisodeDurationDays + TotalUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_12_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Episode + EpisodeDurationDays + TotalUseMins, 
            data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_13_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            TotalUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_14_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_15_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Episode + HasIAR, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_16_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            HasIAR, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_17_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            HasIAR + TotalUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_18_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Episode + ClinicalPsychologistUseMins + 
            MedicalUseMins + NurseUseMins + OtherUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_19_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
            OtherUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_20_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Episode + HasIAR + ClinicalPsychologistUseMins + 
            MedicalUseMins + NurseUseMins + OtherUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_21_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            HasIAR + ClinicalPsychologistUseMins + MedicalUseMins + 
            NurseUseMins + OtherUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_22_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            HasIAR + ClinicalPsychologistUseMins + OtherProviderUseMins, 
            data = X_Ready4useDyad@ds_tb %>% dplyr::mutate(OtherProviderUseMins = TotalUseMins - 
                ClinicalPsychologistUseMins))
        K10_ls$OLS_23_mdl <- lm(formula = K10_change ~ Intervention + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Medication + Diagnosis + SuicideRisk + Episode + 
            HasIAR + MedicalUseMins + OtherProviderUseMins, data = X_Ready4useDyad@ds_tb %>% 
            dplyr::mutate(OtherProviderUseMins = TotalUseMins - 
                MedicalUseMins))
    }
    else {
        K10_ls <- list(OLS_1_mdl = lm(formula = K10_change ~ 
            Jurisdiction, data = X_Ready4useDyad@ds_tb))
        K10_ls$OLS_2_mdl <- lm(formula = K10_change ~ Jurisdiction + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + Episode, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_3_mdl <- lm(formula = K10_change ~ Jurisdiction + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + Episode + HasIAR, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_4_mdl <- lm(formula = K10_change ~ Jurisdiction + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + Episode + ClinicalPsychologistUseMins + 
            MedicalUseMins + NurseUseMins + OtherUseMins, data = X_Ready4useDyad@ds_tb)
        K10_ls$OLS_5_mdl <- lm(formula = K10_change ~ Jurisdiction + 
            K10 + Employment + Age + Gender + IRSADQuintile + 
            Diagnosis + SuicideRisk + Episode + HasIAR + ClinicalPsychologistUseMins + 
            MedicalUseMins + NurseUseMins + OtherUseMins, data = X_Ready4useDyad@ds_tb)
    }
    return(K10_ls)
}
#' Make project 2 K10 models
#' @description make_project_2_k10_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 k10 models. The function returns K10 (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return K10 (a list)
#' @rdname make_project_2_k10_mdls
#' @export 
#' @keywords internal
make_project_2_k10_mdls <- function (X_Ready4useDyad) 
{
    K10_ls <- list()
    K10_ls <- list(OLS_1_mdl = lm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        TotalUseMins, data = X_Ready4useDyad@ds_tb))
    K10_ls$GLM_GSN_2_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = gaussian())
    K10_ls$GLM_GSN_LOG_3_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = gaussian(link = "log"))
    K10_ls$GLM_GSN_INV_4_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = gaussian(link = "inverse"))
    K10_ls$GLM_GMA_5_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = Gamma())
    K10_ls$GLM_GMA_LOG_6_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = Gamma(link = "log"))
    K10_ls$GLM_GMA_INV_7_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = Gamma(link = "inverse"))
    K10_ls$GLM_ING_INV_7_mdl <- glm(formula = K10_End ~ Intervention + 
        Employment + Age + Gender + Jurisdiction + IRSADQuintile + 
        Diagnosis + K10 + HasIAR + SuicideRisk + Episode + EpisodeDurationDays + 
        ClinicalPsychologistUseMins + MedicalUseMins + NurseUseMins + 
        OtherUseMins, data = X_Ready4useDyad@ds_tb, family = inverse.gaussian(link = "inverse"))
    return(K10_ls)
}
#' Make project 2 K10 relapse models
#' @description make_project_2_k10_relapse_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 k10 relapse models. The function returns K10 (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return K10 (a list)
#' @rdname make_project_2_k10_relapse_mdls
#' @export 
#' @keywords internal
make_project_2_k10_relapse_mdls <- function (X_Ready4useDyad) 
{
    K10_ls <- list(OLS_1_mdl = lm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb))
    K10_ls$GLM_GSN_2_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = gaussian())
    K10_ls$GLM_GSN_LOG_3_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = gaussian(link = "log"))
    K10_ls$GLM_GSN_INV_4_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = gaussian(link = "inverse"))
    K10_ls$GLM_GMA_5_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = Gamma())
    K10_ls$GLM_GMA_LOG_6_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = Gamma(link = "log"))
    K10_ls$GLM_GMA_INV_7_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = Gamma(link = "inverse"))
    K10_ls$GLM_ING_8_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = inverse.gaussian())
    K10_ls$GLM_ING_INV_9_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = inverse.gaussian(link = "inverse"))
    K10_ls$GLM_ING_SQT_10_mdl <- glm(formula = K10 ~ Intervention + 
        Employment + Age + Gender + IRSADQuintile + Diagnosis + 
        HasIAR + K10Discharge + K10ChangeDischarge + SuicideRisk, 
        data = X_Ready4useDyad@ds_tb, family = inverse.gaussian(link = "1/mu^2"))
    return(K10_ls)
}
#' Make project 2 labels
#' @description make_project_2_labels() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 labels. The function returns Labels (a list).
#' @param type_1L_chr Type (a character vector of length one), Default: c("regression", "simulation")
#' @return Labels (a list)
#' @rdname make_project_2_labels
#' @export 
#' @keywords internal
make_project_2_labels <- function (type_1L_chr = c("regression", "simulation")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    labels_ls <- list(K10 = "K10 at start of episode of care", 
        K10_End = "K10 at end of episode of care", K10_change = "Change in K10", 
        IRSADQuintile = "IRSAD Quintile", SuicideRisk = "Suicide Risk", 
        HasIAR = "Has an IAR", IARPractitioner = "Practitioner IAR Recommendation", 
        WaitInDays = "Wait time, days", TotalEpisodes = "Number of episodes of care", 
        EpisodeDurationDays = "Duration of episode of care, days", 
        DaysSinceIndexService = "Days since index service event", 
        DaysToYearOneRepresentation = "Time to subsequent episode, days", 
        TreatmentPlan = "Has a GP mental health treatment plan", 
        ClinicalPsychologistUseMins = "Clinical psychologist minutes", 
        GPUseMins = "General Practitioner minutes", MedicalUseMins = "Medical minutes", 
        NurseUseMins = "Nurse minutes", OtherMedicalUseMins = "Other medical minutes", 
        OtherUseMins = "Other clinician minutes", PsychiatristUseMins = "Psychiatrist minutes", 
        TotalUseMins = "Total provider minutes")
    if (type_1L_chr == "simulation") {
        labels_ls$K10 <- "K10"
        labels_ls <- labels_ls %>% append(list(ClinicalPsychologistCost = "Clinical psychologist cost", 
            GPCost = "General Practitioner cost", MedicalCost = "Medical cost", 
            NurseCost = "Nurse cost", OtherMedicalCost = "Other medical cost", 
            OtherCost = "Other clinician cost", PsychiatristCost = "Psychiatrist cost", 
            AQoL8D = "AQoL-8D", EQ5D = "EQ-5D (algorithm 1)", 
            EQ5DM2 = "EQ-5D (algorithm 2)", SF6D = "SF-6D (algorithm 1)", 
            SF6DM2 = "SF-6D (algorithm 2)", Episode = "Episodes of care"))
    }
    return(labels_ls)
}
#' Make project 2 minutes models
#' @description make_project_2_minutes_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 minutes models. The function returns Tpm models (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param family_2_1L_chr Family 2 (a character vector of length one), Default: 'Gamma(link = 'log')'
#' @param link_1_1L_chr Link 1 (a character vector of length one), Default: 'logit'
#' @param x_part_1_ls X part 1 (a list), Default: NULL
#' @param x_part_2_ls X part 2 (a list), Default: NULL
#' @param y_1L_chr Y (a character vector of length one), Default: 'TotalUseMins_change'
#' @param use_1_int Use 1 (an integer vector), Default: integer(0)
#' @param use_2_int Use 2 (an integer vector), Default: integer(0)
#' @param ... Additional arguments
#' @return Tpm models (a list)
#' @rdname make_project_2_minutes_mdls
#' @export 
#' @importFrom purrr map2
#' @importFrom stats setNames
#' @keywords internal
make_project_2_minutes_mdls <- function (X_Ready4useDyad, disciplines_chr = make_disciplines(), 
    family_2_1L_chr = "Gamma(link = 'log')", link_1_1L_chr = "logit", 
    x_part_1_ls = NULL, x_part_2_ls = NULL, y_1L_chr = "TotalUseMins_change", 
    use_1_int = integer(0), use_2_int = integer(0), ...) 
{
    components_chr <- paste0(paste0(disciplines_chr, "UseMins"), 
        "_change")
    if (y_1L_chr %in% components_chr) {
        components_chr <- setdiff(components_chr, components_chr[which(components_chr == 
            y_1L_chr):length(components_chr)])
    }
    else {
        components_chr <- character(0)
    }
    if (is.null(x_part_1_ls)) {
        x_part_1_ls <- list(c("Intervention", "Diagnosis", "K10", 
            "EpisodeDurationCategory", components_chr), c("Intervention", 
            "SubthresholdDisorder", "K10", "EpisodeDurationCategory", 
            components_chr), c("Intervention", "EpisodeDurationCategory", 
            components_chr), c("Intervention", components_chr), 
            c("Intervention", "EpisodeDurationCategory"), "Intervention", 
            c("Intervention", "Age", "Gender", "EpisodeDurationCategory"), 
            c("Intervention", "SubthresholdDisorder", "Age", 
                "Gender", "EpisodeDurationCategory"), c("Intervention", 
                "SubthresholdDisorder", "Gender", "EpisodeDurationCategory"), 
            c("Intervention", "SubthresholdDisorder", "EpisodeDurationCategory"))
    }
    if (is.null(x_part_2_ls)) {
        x_part_2_ls <- x_part_1_ls
    }
    if (!identical(use_1_int, integer(0))) {
        x_part_1_ls <- x_part_1_ls[use_1_int]
    }
    if (!identical(use_2_int, integer(0))) {
        x_part_2_ls <- x_part_2_ls[use_2_int]
    }
    tpm_mdls_ls <- purrr::map2(x_part_1_ls, x_part_2_ls, ~make_two_part_mdl(data_tb = X_Ready4useDyad@ds_tb, 
        family_2_1L_chr = family_2_1L_chr, link_1_1L_chr = link_1_1L_chr, 
        x_part_1_chr = .x, x_part_2_chr = .y, y_1L_chr = y_1L_chr)) %>% 
        stats::setNames(paste0("TPM_", 1:length(x_part_1_ls), 
            "_mdl"))
    return(tpm_mdls_ls)
}
#' Make project 2 outcomes list
#' @description make_project_2_outcomes_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 outcomes list. The function returns Outcomes (a list).

#' @return Outcomes (a list)
#' @rdname make_project_2_outcomes_ls
#' @export 
#' @keywords internal
make_project_2_outcomes_ls <- function () 
{
    outcomes_ls <- list(k10p = c("k10p_score", paste0("k10p_item", 
        11:14)), iar_dst = c("iar_dst_recommended_level_of_care", 
        "iar_dst_practitioner_level_of_care"))
    return(outcomes_ls)
}
#' Make project 2 regressions list
#' @description make_project_2_regressions_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 regressions list. The function returns Regressions (a list).

#' @return Regressions (a list)
#' @rdname make_project_2_regressions_ls
#' @export 
#' @keywords internal
make_project_2_regressions_ls <- function () 
{
    regressions_ls <- list(EpisodeStart_ls = list(), EpisodeEnd_ls = list(), 
        ClinicalPsychologistMins_ls = list(), GPMins_ls = list(), 
        PsychiatristMins_ls = list(), OtherMedicalMins_ls = list(), 
        NurseMins_ls = list(), OtherMins_ls = list(), K10_ls = list(), 
        Representation_ls = list(), K10Relapse_ls = list()) %>% 
        make_regressions_ls(prototype_mdl = list(EpisodeStart_mdl = NULL, 
            EpisodeEnd_mdl = NULL, ClinicalPsychologistMins_mdl = NULL, 
            GPMins_mdl = NULL, PychiatristMins_mdl = NULL, OtherMedicalMins_mdl = NULL, 
            NurseMins_mdl = NULL, OtherMins_mdl = NULL, K10_mdl = NULL, 
            Representation_mdl = NULL, K10Relapse_mdl = NULL))
    return(regressions_ls)
}
#' Make project 2 report
#' @description make_project_2_report() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 report. The function returns Data (an output object of multiple potential types).
#' @param model_data_ls Model data (a list), Default: NULL
#' @param arms_chr Arms (a character vector)
#' @param params_tb Parameters (a tibble)
#' @param period_dtm Period (a date vector), Default: lubridate::years(1)
#' @param platform_1L_chr Platform (a character vector of length one), Default: 'Intervention'
#' @param processed_ls Processed (a list), Default: NULL
#' @param regressions_ls Regressions (a list), Default: NULL
#' @param select_1L_chr Select (a character vector of length one), Default: character(0)
#' @param sim_results_ls Sim results (a list), Default: NULL
#' @param timepoint_1L_chr Timepoint (a character vector of length one), Default: character(0)
#' @param transformations_chr Transformations (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'full_combos'
#' @param ungroup_1L_lgl Ungroup (a logical vector of length one), Default: FALSE
#' @param weeks_int Weeks (an integer vector), Default: integer(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("paramscost", "paramsk10", "resultsaqol", "resultseq5d", "resultseconomic", 
#'    "resultsoutcomes", "resultsutility", "serviceuse", "serviceusecost")
#' @return Data (an output object of multiple potential types)
#' @rdname make_project_2_report
#' @export 
#' @importFrom lubridate years
#' @importFrom dplyr filter mutate arrange select ungroup
#' @importFrom stringr str_detect str_replace_all
#' @keywords internal
make_project_2_report <- function (model_data_ls = NULL, arms_chr, params_tb, period_dtm = lubridate::years(1), 
    platform_1L_chr = "Intervention", processed_ls = NULL, regressions_ls = NULL, 
    select_1L_chr = character(0), sim_results_ls = NULL, timepoint_1L_chr = character(0), 
    transformations_chr = character(0), type_1L_chr = "full_combos", 
    ungroup_1L_lgl = FALSE, weeks_int = integer(0), what_1L_chr = c("paramscost", 
        "paramsk10", "resultsaqol", "resultseq5d", "resultseconomic", 
        "resultsoutcomes", "resultsutility", "serviceuse", "serviceusecost")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "paramscost") {
        data_xx <- params_tb %>% dplyr::filter(Parameter %>% 
            stringr::str_detect("Cost")) %>% dplyr::mutate(Parameter = Parameter %>% 
            stringr::str_replace_all("CostPerMin", " cost per minute"))
    }
    if (what_1L_chr == "paramsk10") {
        data_xx <- params_tb %>% dplyr::filter(!Parameter %>% 
            stringr::str_detect("Cost")) %>% dplyr::filter(!Parameter %>% 
            startsWith("RTM_"))
    }
    if (what_1L_chr == "resultsaqol") {
        data_xx <- sim_results_ls %>% make_project_2_sim_summary(arms_chr = arms_chr, 
            platform_1L_chr = platform_1L_chr, select_1L_chr = "AQoL-8D", 
            type_1L_chr = "economic", what_1L_chr = type_1L_chr)
    }
    if (what_1L_chr == "resultseq5d") {
        data_xx <- sim_results_ls %>% make_project_2_sim_summary(arms_chr = arms_chr, 
            platform_1L_chr = platform_1L_chr, select_1L_chr = "EQ-5D", 
            type_1L_chr = "economic", what_1L_chr = type_1L_chr)
    }
    if (what_1L_chr == "resultsutility") {
        data_xx <- sim_results_ls %>% make_project_2_sim_summary(type_1L_chr = "economic", 
            arms_chr = arms_chr, platform_1L_chr = platform_1L_chr, 
            what_1L_chr = type_1L_chr, select_1L_chr = select_1L_chr)
    }
    if (what_1L_chr == "resultsoutcomes") {
        data_xx <- sim_results_ls %>% make_project_2_sim_summary(arms_chr = arms_chr, 
            platform_1L_chr = platform_1L_chr) %>% dplyr::mutate(Outcome = Outcome %>% 
            stringr::str_replace_all("k10", "K10") %>% stringr::str_replace_all("_change", 
            " change from baseline") %>% stringr::str_replace_all("_", 
            " ")) %>% dplyr::arrange(Outcome)
    }
    if (what_1L_chr == "resultseconomic") {
        data_xx <- sim_results_ls %>% make_project_2_sim_summary(arms_chr = arms_chr, 
            platform_1L_chr = platform_1L_chr, select_1L_chr = select_1L_chr, 
            type_1L_chr = "economic", what_1L_chr = type_1L_chr)
        if (!identical(select_1L_chr, character(0))) {
            data_xx <- data_xx %>% dplyr::filter(QALYs == select_1L_chr) %>% 
                dplyr::select(-QALYs)
        }
    }
    if (ungroup_1L_lgl) {
        data_xx <- renewSlot(data_xx, "ds_tb", data_xx@ds_tb %>% 
            dplyr::ungroup())
    }
    return(data_xx)
}
#' Make project 2 results
#' @description make_project_2_results() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 results. The function returns Sim results (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param inputs_ls Inputs (a list)
#' @param comparator_1L_chr Comparator (a character vector of length one), Default: 'Comparator'
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param min_cell_size_1L_int Minimum cell size (an integer vector of length one), Default: 30
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param outcomes_chr Outcomes (a character vector), Default: character(0)
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @param utilities_chr Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2")
#' @return Sim results (a list)
#' @rdname make_project_2_results
#' @export 
#' @importFrom dplyr filter
#' @keywords internal
make_project_2_results <- function (X_Ready4useDyad, inputs_ls, comparator_1L_chr = "Comparator", 
    intervention_1L_chr = "Intervention", min_cell_size_1L_int = 30L, 
    modifiable_chr = character(0), outcomes_chr = character(0), 
    threshold_1L_dbl = 96000, utilities_chr = c("AQoL8D", "EQ5D", 
        "EQ5DM2", "SF6D", "SF6DM2")) 
{
    if (identical(outcomes_chr, character(0))) {
        outcomes_chr <- make_outcomes_vars(inputs_ls$Synthetic_r4, 
            Y_Ready4useDyad = renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
                  intervention_1L_chr)), Z_Ready4useDyad = renewSlot(X_Ready4useDyad, 
                "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
                  comparator_1L_chr)), exclude_chr = c("Adult", 
                "Period", "MeasurementWeek", "treatment_fraction", 
                "treatment_measurement", "treatment_start"), 
            exclude_suffixes_chr = c("_change", "_date", "_previous", 
                "52_Weeks"), modifiable_chr = modifiable_chr, 
            numeric_only_1L_lgl = T)
    }
    full_combos_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("Diagnosis", 
        "Distress"), min_cell_size_1L_int = min_cell_size_1L_int, 
        outcomes_chr = outcomes_chr, utilities_chr = utilities_chr)
    diagnosis_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("Diagnosis"), 
        min_cell_size_1L_int = min_cell_size_1L_int, outcomes_chr = outcomes_chr, 
        utilities_chr = utilities_chr)
    distress_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("Distress"), 
        min_cell_size_1L_int = min_cell_size_1L_int, outcomes_chr = outcomes_chr, 
        utilities_chr = utilities_chr)
    total_ls <- make_results_summary(X_Ready4useDyad, min_cell_size_1L_int = min_cell_size_1L_int, 
        outcomes_chr = outcomes_chr, utilities_chr = utilities_chr)
    sim_results_ls <- list(D_Ready4useDyad = X_Ready4useDyad, 
        diagnosis_ls = diagnosis_ls, distress_ls = distress_ls, 
        full_combos_ls = full_combos_ls, total_ls = total_ls, 
        size_1L_int = X_Ready4useDyad@ds_tb$UID %>% unique() %>% 
            length())
    return(sim_results_ls)
}
#' Make project 2 results synthesis
#' @description make_project_2_results_synthesis() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 results synthesis. The function is called for its side effects and does not return a value.
#' @param inputs_ls Inputs (a list)
#' @param results_ls Results (a list)
#' @param comparator_1L_chr Comparator (a character vector of length one), Default: 'Comparator'
#' @param drop_chr Drop (a character vector), Default: make_project_2_vars("drop")
#' @param keep_chr Keep (a character vector), Default: make_project_2_vars("keep")
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param modifiable_chr Modifiable (a character vector), Default: make_project_2_vars("modify")
#' @param type_1L_chr Type (a character vector of length one), Default: c("D", "AB", "C")
#' @param uid_tfmn_fn Unique identifier transformation (a function), Default: as.numeric
#' @return X (A dataset and data dictionary pair.)
#' @rdname make_project_2_results_synthesis
#' @export 
#' @importFrom purrr map reduce
#' @importFrom dplyr mutate across where filter select arrange case_when
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @keywords internal
make_project_2_results_synthesis <- function (inputs_ls, results_ls, comparator_1L_chr = "Comparator", 
    drop_chr = make_project_2_vars("drop"), keep_chr = make_project_2_vars("keep"), 
    intervention_1L_chr = "Intervention", modifiable_chr = make_project_2_vars("modify"), 
    type_1L_chr = c("D", "AB", "C"), uid_tfmn_fn = as.numeric) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    tfd_results_ls <- purrr::map(results_ls, ~renewSlot(.x, "ds_tb", 
        .x@ds_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~as.numeric(.x))) %>% dplyr::filter(!is.na(!!rlang::sym(modifiable_chr[1])))))
    groups_chr <- c(comparator_1L_chr, "Difference", intervention_1L_chr) %>% 
        sort()
    comparator_1L_int <- which(groups_chr == comparator_1L_chr)
    intervention_1L_int <- which(groups_chr == intervention_1L_chr)
    X_Ready4useDyad <- make_results_synthesis(inputs_ls$Synthetic_r4, 
        results_ls = tfd_results_ls, add_severity_1L_lgl = T, 
        exclude_chr = character(0), exclude_suffixes_chr = c("_change", 
            "_date", "_previous"), keep_chr = keep_chr, modifiable_chr = modifiable_chr, 
        severity_var_1L_chr = "K10_start", type_1L_chr = type_1L_chr)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::select(-tidyselect::any_of(drop_chr)))
    numerics_chr <- X_Ready4useDyad@ds_tb %>% dplyr::select(dplyr::where(is.numeric)) %>% 
        names() %>% sort()
    numerics_chr <- numerics_chr[!endsWith(numerics_chr, paste0("_", 
        comparator_1L_chr))]
    numerics_chr <- numerics_chr[!endsWith(numerics_chr, paste0("_", 
        intervention_1L_chr))]
    numerics_chr <- numerics_chr %>% setdiff(c(keep_chr, "Iteration"))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", numerics_chr %>% 
        purrr::reduce(.init = X_Ready4useDyad@ds_tb %>% dplyr::mutate(UID = UID %>% 
            uid_tfmn_fn()) %>% dplyr::arrange(Iteration, UID, 
            Data), ~{
            var_1L_chr <- .y
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                dplyr::case_when(Data == "Difference" ~ !!rlang::sym(paste0(var_1L_chr, 
                  "_", intervention_1L_chr)) - !!rlang::sym(paste0(var_1L_chr, 
                  "_", comparator_1L_chr)), T ~ !!rlang::sym(var_1L_chr))))
        }))
    return(X_Ready4useDyad)
}
#' Make project 2 results tibble
#' @description make_project_2_results_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 results tibble. The function returns Results (a tibble).
#' @param sim_results_ls Sim results (a list)
#' @param comparator_1L_chr Comparator (a character vector of length one)
#' @param intervention_1L_chr Intervention (a character vector of length one)
#' @param params_tb Parameters (a tibble)
#' @param platform_1L_chr Platform (a character vector of length one)
#' @param digits_1L_int Digits (an integer vector of length one), Default: 2
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines()
#' @param drop_chr Drop (a character vector), Default: character(0)
#' @param filter_1L_lgl Filter (a logical vector of length one), Default: TRUE
#' @param format_1L_lgl Format (a logical vector of length one), Default: TRUE
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "cost", "outcomes", "use")
#' @return Results (a tibble)
#' @rdname make_project_2_results_tb
#' @export 
#' @importFrom dplyr mutate arrange rename filter bind_rows select across where case_when everything
#' @importFrom stringr word str_replace_all str_replace str_detect
#' @importFrom rlang sym
#' @importFrom purrr map_dfr map_chr pmap_chr reduce
#' @importFrom scales dollar
#' @keywords internal
make_project_2_results_tb <- function (sim_results_ls, comparator_1L_chr, intervention_1L_chr, 
    params_tb, platform_1L_chr, digits_1L_int = 2, disciplines_chr = make_disciplines(), 
    drop_chr = character(0), filter_1L_lgl = TRUE, format_1L_lgl = TRUE, 
    type_1L_chr = c("main", "cost", "outcomes", "use")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    results_tb <- make_project_2_report(arms_chr = c(intervention_1L_chr, 
        comparator_1L_chr), params_tb = params_tb, platform_1L_chr = platform_1L_chr, 
        sim_results_ls = sim_results_ls, what_1L_chr = "resultsoutcome")
    results_tb <- results_tb %>% dplyr::mutate(Group = Outcome %>% 
        stringr::word(1)) %>% dplyr::mutate(Outcome = stringr::str_replace_all(Outcome, 
        " S1", " (Outcome sensitivity 1)") %>% stringr::str_replace_all(" S2", 
        " (Outcome sensitivity 2)") %>% stringr::str_replace_all("\\(Utility sensitivity ", 
        "(Outcome sensitivity ")) %>% dplyr::arrange(Group, Outcome)
    results_tb <- results_tb %>% dplyr::rename(Comparator = !!rlang::sym(comparator_1L_chr)) %>% 
        dplyr::filter(!Outcome %>% endsWith(paste0(" ", intervention_1L_chr))) %>% 
        dplyr::filter(!Outcome %>% endsWith(paste0(" ", comparator_1L_chr))) %>% 
        dplyr::mutate(Difference = !!rlang::sym(intervention_1L_chr) - 
            Comparator) %>% dplyr::filter(!is.na(Comparator))
    results_tb <- results_tb$Group %>% unique() %>% purrr::map_dfr(~{
        filtered_tb <- results_tb %>% dplyr::filter(Group == 
            .x)
        dplyr::bind_rows(filtered_tb %>% dplyr::filter(endsWith(Outcome, 
            "at model entry")), filtered_tb %>% dplyr::filter(!endsWith(Outcome, 
            "at model entry")))
    })
    labels_ls <- make_project_2_labels(type_1L_chr = "simulation")
    results_tb <- results_tb %>% dplyr::mutate(Replacement = Group %>% 
        purrr::map_chr(~ifelse(.x %in% names(labels_ls), labels_ls[[which(.x == 
            names(labels_ls))]], .x)))
    results_tb <- results_tb %>% dplyr::mutate(Outcome = purrr::pmap_chr(results_tb %>% 
        dplyr::select(Outcome, Group, Replacement), ~..1 %>% 
        stringr::str_replace(..2, ..3))) %>% dplyr::mutate(Group = Replacement) %>% 
        dplyr::select(-Replacement)
    if (format_1L_lgl) {
        results_tb <- results_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~round(.x, digits_1L_int)))
    }
    if (filter_1L_lgl) {
        results_tb <- results_tb %>% dplyr::filter(!endsWith(Outcome, 
            "Update 1") & !startsWith(Outcome, "Contact minutes"))
    }
    if (!identical(drop_chr, character(0))) {
        results_tb <- purrr::reduce(drop_chr, .init = results_tb, 
            ~{
                .x %>% dplyr::filter(!Outcome %>% stringr::str_detect(.y))
            })
    }
    if (type_1L_chr == "cost") {
        results_tb <- results_tb %>% dplyr::filter(Outcome %>% 
            stringr::str_detect("Cost") | Outcome %>% stringr::str_detect("cost")) %>% 
            dplyr::mutate(Outcome = dplyr::case_when(Outcome %>% 
                startsWith("Cost") ~ stringr::str_replace_all(Outcome, 
                "Cost", "Total cost"), T ~ Outcome)) %>% dplyr::arrange(Outcome)
        results_tb <- results_tb
        results_tb <- results_tb %>% dplyr::mutate(Group = dplyr::case_when(Outcome %>% 
            stringr::str_detect("Cost sensitivity 1") ~ "Sensitivity 1", 
            Outcome %>% stringr::str_detect("cost sensitivity 1") ~ 
                "Sensitivity 1", Outcome %>% stringr::str_detect("Cost sensitivity 2") ~ 
                "Sensitivity 2", Outcome %>% stringr::str_detect("cost sensitivity 2") ~ 
                "Sensitivity 2", T ~ "Base")) %>% dplyr::arrange(Group, 
            Outcome) %>% dplyr::rename(Analysis = Group) %>% 
            dplyr::select(Analysis, dplyr::everything()) %>% 
            dplyr::mutate(Outcome = stringr::str_replace(Outcome, 
                " \\s*\\([^\\)]+\\)", ""))
        if (format_1L_lgl) {
            results_tb <- results_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~scales::dollar(.x)))
        }
    }
    if (type_1L_chr == "outcomes") {
        results_tb <- results_tb %>% dplyr::filter(!endsWith(Group, 
            "Contact") & !endsWith(Group, "Cost") & !endsWith(Group, 
            "cost") & !endsWith(Group, "minutes") & !endsWith(Group, 
            "Episodes of care")) %>% dplyr::filter(!Group %in% 
            c("Ambulance", "Fixed", "IAR-DST", "Wait time, days", 
                disciplines_chr)) %>% dplyr::select(-Group)
    }
    if (type_1L_chr == "use") {
        results_tb <- results_tb %>% dplyr::filter(endsWith(Group, 
            "Contact") | endsWith(Group, "minutes") | endsWith(Group, 
            "Episodes of care") | Group %in% c("Ambulance", "IAR-DST", 
            "Wait time, days")) %>% dplyr::select(-Group) %>% 
            dplyr::filter(!endsWith(Outcome, " cost"))
        results_tb <- results_tb %>% dplyr::filter(Outcome == 
            "Episodes of care") %>% dplyr::bind_rows(results_tb %>% 
            dplyr::filter(Outcome != "Episodes of care"))
        results_tb <- results_tb %>% dplyr::mutate(Outcome = Outcome %>% 
            stringr::str_replace_all("General Practitioner", 
                "General practitioner"))
    }
    return(results_tb)
}
#' Make project 2 sensitivities list
#' @description make_project_2_sensitivities_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 sensitivities list. The function returns Sensitivities (a list).

#' @return Sensitivities (a list)
#' @rdname make_project_2_sensitivities_ls
#' @export 
#' @keywords internal
make_project_2_sensitivities_ls <- function () 
{
    sensitivities_ls <- make_sensitivities_ls()
    sensitivities_ls$costs_ls <- list(S1 = add_project_2_cost_sa_1, 
        S2 = add_project_2_cost_sa_2)
    return(sensitivities_ls)
}
#' Make project 2 sim summary
#' @description make_project_2_sim_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 sim summary. The function returns Summary (a tibble).
#' @param sim_results_ls Sim results (a list)
#' @param arms_chr Arms (a character vector)
#' @param element_1L_chr Element (a character vector of length one), Default: 'Z'
#' @param groupings_chr Groupings (a character vector), Default: c("Diagnosis", "Distress")
#' @param order_1L_lgl Order (a logical vector of length one), Default: TRUE
#' @param convert_1L_lgl Convert (a logical vector of length one), Default: TRUE
#' @param platform_1L_chr Platform (a character vector of length one), Default: 'Intervention'
#' @param select_1L_chr Select (a character vector of length one), Default: 'all'
#' @param type_1L_chr Type (a character vector of length one), Default: c("outcomes", "economic")
#' @param what_1L_chr What (a character vector of length one), Default: c("total", "diagnosis", "iar", "full_combos")
#' @return Summary (a tibble)
#' @rdname make_project_2_sim_summary
#' @export 
#' @importFrom purrr pluck reduce map_chr map_lgl
#' @importFrom dplyr mutate select everything group_by where ungroup rename distinct filter case_when left_join arrange desc across
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer pivot_wider all_of
#' @importFrom tidyselect all_of
#' @importFrom stringr str_replace_all str_sub word str_replace
#' @importFrom tibble tibble
#' @importFrom scales dollar
#' @keywords internal
make_project_2_sim_summary <- function (sim_results_ls, arms_chr, element_1L_chr = "Z", groupings_chr = c("Diagnosis", 
    "Distress"), order_1L_lgl = TRUE, convert_1L_lgl = TRUE, 
    platform_1L_chr = "Intervention", select_1L_chr = "all", 
    type_1L_chr = c("outcomes", "economic"), what_1L_chr = c("total", 
        "diagnosis", "iar", "full_combos")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    pick_1L_chr <- paste0(what_1L_chr, "_ls")
    X_Ready4useDyad <- sim_results_ls %>% purrr::pluck(pick_1L_chr) %>% 
        purrr::pluck(element_1L_chr)
    groupings_chr <- intersect(groupings_chr, names(X_Ready4useDyad@ds_tb))
    if (!identical(groupings_chr, character(0))) {
        X_Ready4useDyad@ds_tb <- purrr::reduce(groupings_chr, 
            .init = X_Ready4useDyad@ds_tb %>% dplyr::mutate(Group = ""), 
            ~dplyr::mutate(.x, Group = paste0(Group, !!rlang::sym(.y), 
                " - "))) %>% dplyr::select(Group, dplyr::everything()) %>% 
            dplyr::group_by(Group)
    }
    if (!identical(groupings_chr, character(0))) {
        x_tb <- X_Ready4useDyad@ds_tb %>% dplyr::group_by(Group, 
            Data)
    }
    else {
        x_tb <- X_Ready4useDyad@ds_tb %>% dplyr::group_by(Data)
    }
    x_tb <- x_tb %>% dplyr::select(dplyr::where(is.numeric)) %>% 
        tidyr::pivot_longer(dplyr::where(is.numeric), names_to = "Parameter", 
            values_to = "Value") %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = Data, 
        values_from = Value)
    if (!identical(groupings_chr, character(0))) {
        x_tb <- x_tb %>% dplyr::select(tidyselect::all_of(c("Group", 
            "Parameter", arms_chr, "Difference"))) %>% dplyr::group_by(Group)
    }
    else {
        x_tb <- x_tb %>% dplyr::select(tidyselect::all_of(c("Parameter", 
            arms_chr, "Difference")))
    }
    x_tb <- x_tb %>% dplyr::rename(`:=`(!!rlang::sym(platform_1L_chr), 
        !!rlang::sym(arms_chr[1]))) %>% dplyr::mutate(Parameter = Parameter %>% 
        stringr::str_replace_all("_YR1", "") %>% purrr::map_chr(~ifelse(startsWith(.x, 
        "QALYs_S"), paste0("QALYs (Utility sensitivity ", stringr::str_sub(.x, 
        start = 8), ")"), ifelse(startsWith(.x, "Cost_S"), paste0("Cost (Cost sensitivity ", 
        stringr::str_sub(.x, start = 7), ")"), .x))) %>% stringr::str_replace_all("AmbulanceOffsetCost", 
        "Ambulance attendance cost") %>% stringr::str_replace_all("UnmeasuredCosts", 
        "Fixed cost") %>% stringr::str_replace_all("cost_S1", 
        "cost (Cost sensitivity 1)") %>% stringr::str_replace_all("Cost_S2", 
        " cost (Cost sensitivity 2)")) %>% dplyr::distinct()
    if (type_1L_chr == "outcomes") {
        summary_tb <- x_tb %>% dplyr::filter(!startsWith(Parameter, 
            "CE_") & !startsWith(Parameter, "ICER_") & !startsWith(Parameter, 
            "Param") & !startsWith(Parameter, "PROB ")) %>% dplyr::mutate(Parameter = dplyr::case_when(Parameter == 
            "OffsetAmbulance" ~ "Ambulance attendances averted", 
            Parameter == "ExternalIARCost" ~ "IAR-DST (externally provided) cost", 
            T ~ Parameter %>% stringr::str_replace_all("_start", 
                " at model entry") %>% stringr::str_replace_all("ExternalIAR", 
                "IAR-DST (externally provided)") %>% stringr::str_replace_all("_", 
                " ") %>% stringr::str_replace_all("Minutes 12 Weeks", 
                "Contact minutes at 14 weeks") %>% stringr::str_replace_all("Minutes", 
                "Contact minutes at 1 year") %>% stringr::str_replace_all("12 Weeks", 
                "at last K10 change") %>% stringr::str_replace_all("treatment count", 
                "New clinic treatment episodes"))) %>% dplyr::rename(Outcome = Parameter) %>% 
            dplyr::filter(Outcome != "N")
    }
    if (type_1L_chr == "economic") {
        a_tb <- x_tb %>% dplyr::select(Parameter, Difference) %>% 
            dplyr::filter(startsWith(Parameter, "ICER_")) %>% 
            dplyr::rename(Scenario = Parameter, ICER = Difference) %>% 
            dplyr::mutate(Scenario = Scenario %>% stringr::str_replace_all("ICER_", 
                ""))
        b_tb <- x_tb %>% dplyr::select(Parameter, Difference) %>% 
            dplyr::filter(startsWith(Parameter, "PROB ")) %>% 
            dplyr::rename(Scenario = Parameter, `P (Cost-Effective)` = Difference) %>% 
            dplyr::mutate(`P (Cost-Effective)` = 100 * `P (Cost-Effective)`) %>% 
            dplyr::mutate(Scenario = Scenario %>% stringr::str_replace_all("PROB CE_", 
                ""))
        c_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
            "Difference") %>% dplyr::select(N) %>% dplyr::mutate(Share = 100 * 
            (N/sim_results_ls$size_1L_int)) %>% dplyr::select(-N)
        if (length(names(c_tb)) == 1) {
            c_tb <- tibble::tibble(Scenario = a_tb$Scenario, 
                Share = c_tb$Share)
        }
        summary_tb <- dplyr::left_join(a_tb, b_tb) %>% dplyr::left_join(c_tb)
        labels_ls <- make_project_2_labels(type_1L_chr = "simulation")
        summary_tb <- summary_tb %>% dplyr::mutate(Scenario = Scenario %>% 
            purrr::map_chr(~ifelse(stringr::word(.x, 1) %in% 
                names(labels_ls), paste0(labels_ls[[which(stringr::word(.x, 
                1) == names(labels_ls))]], " - Reference"), .x)))
        summary_tb <- summary_tb %>% dplyr::mutate(Scenario = Scenario %>% 
            stringr::str_replace_all("_S10", " - Cost 1") %>% 
            stringr::str_replace_all("_S01", " - Utility 1") %>% 
            stringr::str_replace_all("_S02", " - Utility 2") %>% 
            stringr::str_replace_all("_S11", " - Cost 1 & Utility 1") %>% 
            stringr::str_replace_all("_S12", " - Cost 1 & Utility 2"))
        if (order_1L_lgl) {
            summary_tb <- summary_tb %>% dplyr::arrange(Scenario) %>% 
                dplyr::group_by(Scenario) %>% dplyr::arrange(Scenario, 
                dplyr::desc(`P (Cost-Effective)`)) %>% dplyr::select(Scenario, 
                dplyr::everything()) %>% dplyr::ungroup()
            if ("Group" %in% names(summary_tb)) {
                summary_tb <- summary_tb %>% dplyr::select(Scenario, 
                  Group, Share, dplyr::everything())
            }
        }
        if ((summary_tb$Share %>% unique() %>% length()) == 1 & 
            unique(summary_tb$Share)[1] == 100) {
            summary_tb <- summary_tb %>% dplyr::select(-Share)
        }
        if (convert_1L_lgl) {
            summary_tb <- summary_tb %>% dplyr::mutate(dplyr::across(tidyr::all_of(intersect(c("Share"), 
                names(summary_tb))), ~round(.x, 2) %>% paste0("%")), 
                ICER = scales::dollar(ICER))
            if ("P (Cost-Effective)" %in% names(summary_tb)) {
                summary_tb$`P (Cost-Effective)` <- summary_tb$`P (Cost-Effective)` %>% 
                  round(2) %>% paste0("%")
            }
        }
        summary_tb <- summary_tb %>% dplyr::mutate(Scenario = Scenario %>% 
            purrr::map_chr(~ifelse(stringr::word(.x, 1) %in% 
                names(labels_ls), stringr::str_replace(.x, stringr::word(.x, 
                1), labels_ls[[which(stringr::word(.x, 1) == 
                names(labels_ls))]]), .x)))
        summary_tb <- summary_tb %>% dplyr::mutate(QALYs = sub("\\ - .*", 
            "", Scenario)) %>% dplyr::mutate(Scenario = sub(".* \\- ", 
            "", Scenario) %>% stringr::str_replace_all("Utility", 
            "Outcomes")) %>% dplyr::select(QALYs, dplyr::everything())
        if ("Group" %in% names(summary_tb)) {
            summary_tb <- summary_tb %>% dplyr::mutate(Group = dplyr::case_when(Group %>% 
                purrr::map_lgl(~stringr::str_sub(.x, start = -3) == 
                  " - ") ~ stringr::str_sub(Group, end = -4), 
                T ~ Group)) %>% dplyr::rename(`Probability (Cost-Effective)` = `P (Cost-Effective)`) %>% 
                dplyr::mutate(Diagnosis = sub("\\ - .*", "", 
                  Group), Distress = sub(".* \\- ", "", Group) %>% 
                  stringr::str_replace_all("VeryHigh", "Very high")) %>% 
                dplyr::select(Scenario, Diagnosis, Distress, 
                  dplyr::everything()) %>% dplyr::select(-Group)
        }
    }
    return(summary_tb)
}
#' Make project 2 synthetic pop
#' @description make_project_2_synthetic_pop() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 synthetic pop. The function returns Population (a list).
#' @param model_data_ls Model data (a list)
#' @param size_1L_int Size (an integer vector of length one), Default: 1000
#' @return Population (a list)
#' @rdname make_project_2_synthetic_pop
#' @export 
#' @importFrom dplyr filter select mutate row_number case_when everything as_tibble
#' @importFrom synthpop syn
#' @importFrom ready4use Ready4useDyad
#' @keywords internal
make_project_2_synthetic_pop <- function (model_data_ls, size_1L_int = 1000L) 
{
    X_Ready4useDyad <- renewSlot(model_data_ls$imputed_ls$Modelling_r4, 
        "ds_tb", model_data_ls$imputed_ls$Modelling_r4@ds_tb %>% 
            dplyr::filter(InScope) %>% dplyr::filter(Episode == 
            1) %>% dplyr::select(c("UID", "Age", "Employment", 
            "Gender", "IRSADQuintile", "Jurisdiction", "Diagnosis", 
            "SubthresholdDisorder", "SuicideRisk", "HasIAR", 
            "TreatmentPlan", "Medication", "K10")))
    synthetic_ls <- synthpop::syn(X_Ready4useDyad@ds_tb %>% dplyr::select(-c(UID)), 
        k = size_1L_int)
    Synthetic_r4 <- ready4use::Ready4useDyad(ds_tb = synthetic_ls$syn %>% 
        dplyr::mutate(UID = dplyr::row_number() %>% as.character(), 
            Sex = dplyr::case_when(Gender %in% c("Female", "Male") ~ 
                Gender, T ~ sample(c("Female", "Male"), size = size_1L_int, 
                replace = T)), Treatment_status = "Waitlist") %>% 
        dplyr::select(UID, dplyr::everything()) %>% dplyr::as_tibble(), 
        dictionary_r3 = X_Ready4useDyad@dictionary_r3)
    population_ls <- list(real_imputed_ls = list(imputed_ls = model_data_ls$imputed_ls$imputed_xx, 
        Imputed_r4 = X_Ready4useDyad), fully_synthetic_ls = list(synthetic_ls = synthetic_ls, 
        Synthetic_r4 = Synthetic_r4))
    return(population_ls)
}
#' Make project 2 transformation list
#' @description make_project_2_tfmn_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 transformation list. The function returns Transformation (a list).
#' @param type_1L_chr Type (a character vector of length one), Default: 'outcome'
#' @return Transformation (a list)
#' @rdname make_project_2_tfmn_ls
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
make_project_2_tfmn_ls <- function (type_1L_chr = "outcome") 
{
    tfmn_ls <- 1:6 %>% purrr::map(~as.numeric) %>% stats::setNames(make_project_2_vars(type_1L_chr))
    return(tfmn_ls)
}
#' Make project 2 theme function
#' @description make_project_2_theme_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 theme function. The function returns Plot (a function).
#' @param output_1L_chr Output (a character vector of length one), Default: c("Word", "PDF", "HTML")
#' @return Plot (a function)
#' @rdname make_project_2_theme_fn
#' @export 
#' @importFrom ggplot2 theme element_text
#' @keywords internal
make_project_2_theme_fn <- function (output_1L_chr = c("Word", "PDF", "HTML")) 
{
    output_1L_chr <- match.arg(output_1L_chr)
    if (output_1L_chr == "Word") {
        plot_fn <- function(plot_plt, size_main_1L_int = 9, size_title_1L_int = 10) {
            plot_plt + ggplot2::theme(axis.title = ggplot2::element_text(size = size_title_1L_int, 
                family = "Calibri"), axis.text.x.bottom = ggplot2::element_text(size = size_main_1L_int, 
                family = "Calibri"), axis.text.y.left = ggplot2::element_text(size = size_main_1L_int, 
                family = "Calibri"), axis.text = ggplot2::element_text(size = size_main_1L_int, 
                family = "Calibri"))
        }
    }
    else {
        plot_fn <- function(plot_plt, size_main_1L_int = 9, size_title_1L_int = 10) {
            plot_plt + ggplot2::theme(axis.title = ggplot2::element_text(size = size_title_1L_int), 
                axis.text.x.bottom = ggplot2::element_text(size = size_main_1L_int), 
                axis.text.y.left = ggplot2::element_text(size = size_main_1L_int), 
                axis.text = ggplot2::element_text(size = size_main_1L_int))
        }
    }
    return(plot_fn)
}
#' Make project 2 variables
#' @description make_project_2_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project 2 variables. The function returns Variables (a character vector).
#' @param type_1L_chr Type (a character vector of length one), Default: c("drop", "clinical", "keep", "modify", "outcome", "utility")
#' @param disciplines_chr Disciplines (a character vector), Default: make_disciplines("semi")
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @return Variables (a character vector)
#' @rdname make_project_2_vars
#' @export 
#' @keywords internal
make_project_2_vars <- function (type_1L_chr = c("drop", "clinical", "keep", "modify", 
    "outcome", "utility"), disciplines_chr = make_disciplines("semi"), 
    exclude_chr = character(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "clinical") {
        vars_chr <- "K10"
    }
    if (type_1L_chr == "drop") {
        vars_chr <- c("Treatment_change", "Treatment_count", 
            "Treatment_fraction", "Treatment_measurement", "Treatment_start", 
            "Treatment_status_previous", "HasIAR")
    }
    if (type_1L_chr == "keep") {
        vars_chr <- c("Age", "Employment", "Gender", "IRSADQuintile", 
            "Jurisdiction", "Diagnosis", "SubthresholdDisorder", 
            "SuicideRisk", "Medication", "TreatmentPlan", "Episode", 
            "EpisodeDurationDays")
    }
    if (type_1L_chr == "modify") {
        vars_chr <- c("K10", "AQoL8D", "EQ5D", "EQ5DM2", "SF6D", 
            "SF6DM2", paste0(c(disciplines_chr, "Total"), "UseMins"))
    }
    if (type_1L_chr == "outcome") {
        vars_chr <- c(make_project_2_vars("clinical"), make_project_2_vars("utility"))
    }
    if (type_1L_chr == "utility") {
        vars_chr <- c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2")
    }
    if (!identical(exclude_chr, character(0))) {
        vars_chr <- setdiff(vars_chr, exclude_chr)
    }
    return(vars_chr)
}
#' Make project activity dataset
#' @description make_project_activity_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project activity dataset. The function returns Activity (a tibble).
#' @param raw_data_ls Raw data (a list)
#' @param type_1L_chr Type (a character vector of length one), Default: c("initial", "wip")
#' @return Activity (a tibble)
#' @rdname make_project_activity_ds
#' @export 
#' @importFrom janitor row_to_names
#' @importFrom tidyr fill
#' @importFrom dplyr filter select rename
#' @keywords internal
make_project_activity_ds <- function (raw_data_ls, type_1L_chr = c("initial", "wip")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    wip_tb <- raw_data_ls$costing %>% janitor::row_to_names(1) %>% 
        tidyr::fill(Category, .direction = "down")
    activity_tb <- wip_tb %>% dplyr::filter(is.na(`Items in Catergory`)) %>% 
        dplyr::filter(!Category %in% c("Overall", "Service information")) %>% 
        dplyr::select(-`Items in Catergory`)
    if (type_1L_chr != "wip") {
        activity_tb <- activity_tb %>% dplyr::filter(!is.na(Total)) %>% 
            dplyr::select(-Description) %>% dplyr::rename(Metric = Category)
    }
    return(activity_tb)
}
#' Make project Assessment of Quality of Life Six Dimension models
#' @description make_project_aqol6d_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project assessment of quality of life six dimension models. The function returns Assessment of Quality of Life Six Dimension (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return Assessment of Quality of Life Six Dimension (a list)
#' @rdname make_project_aqol6d_mdls
#' @export 
#' @importFrom betareg betareg
#' @keywords internal
make_project_aqol6d_mdls <- function (X_Ready4useDyad) 
{
    Y_Ready4useDyad <- transform_to_min_and_max(X_Ready4useDyad, 
        vars_chr = c("AQoL6D_12_Weeks"))
    aqol6d_ls <- list(OLS_1_mdl = lm(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb))
    aqol6d_ls$GLM_GSN_2_mdl <- glm(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = gaussian())
    aqol6d_ls$GLM_GSN_LOG_3_mdl <- glm(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = gaussian(link = "log"))
    aqol6d_ls$GLM_GMA_4_mdl <- glm(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = Gamma(link = "inverse"))
    aqol6d_ls$GLM_CLL_5_mdl <- glm(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = binomial(link = "cloglog"))
    aqol6d_ls$BET_CLL_6_mdl <- betareg::betareg(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, link = "cloglog")
    aqol6d_ls$BET_LGT_7_mdl <- betareg::betareg(formula = AQoL6D_12_Weeks ~ 
        AQoL6D + k10 + k10_change + CHU9D_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, link = "logit")
    return(aqol6d_ls)
}
#' Make project CHU-9D models
#' @description make_project_chu9d_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project chu-9d models. The function returns a CHU-9D (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return a CHU-9D (a list)
#' @rdname make_project_chu9d_mdls
#' @export 
#' @importFrom betareg betareg
#' @keywords internal
make_project_chu9d_mdls <- function (X_Ready4useDyad) 
{
    Y_Ready4useDyad <- transform_to_min_and_max(X_Ready4useDyad, 
        vars_chr = c("CHU9D_12_Weeks"))
    chu9d_ls <- list(OLS_1_mdl = lm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb))
    chu9d_ls$GLM_GSN_2_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = gaussian())
    chu9d_ls$GLM_GSN_LOG_3_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = gaussian(link = "log"))
    chu9d_ls$GLM_GSN_INV_4_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = gaussian(link = "inverse"))
    chu9d_ls$GLM_GMA_6_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = Gamma())
    chu9d_ls$GLM_GMA_LOG_7_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = Gamma(link = "log"))
    chu9d_ls$GLM_GMA_INV_8_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = Gamma(link = "inverse"))
    chu9d_ls$GLM_BNL_LGT_9_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = binomial(link = "logit"))
    chu9d_ls$GLM_BNL_PBT_10_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = binomial(link = "probit"))
    chu9d_ls$GLM_BNL_CAU_11_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = binomial(link = "cauchit"))
    chu9d_ls$GLM_QSB_LGT_12_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = quasibinomial(link = "logit"))
    chu9d_ls$GLM_QSB_CLL_13_mdl <- glm(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, family = quasibinomial(link = "cloglog"))
    chu9d_ls$BET_CLL_14_mdl <- betareg::betareg(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, link = "cloglog")
    chu9d_ls$BET_LGT_15_mdl <- betareg::betareg(formula = CHU9D_12_Weeks ~ 
        Age + gender + CHU9D + k10 + k10_change + Minutes_12_Weeks, 
        data = Y_Ready4useDyad@ds_tb, link = "logit")
    return(chu9d_ls)
}
#' Make project consolidated dataset
#' @description make_project_consolidated_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project consolidated dataset. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param Z_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad
#' @param type_1L_chr Type (a character vector of length one), Default: c("outcomes", "tx_status")
#' @return A (A dataset and data dictionary pair.)
#' @rdname make_project_consolidated_ds
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom dplyr filter group_by mutate n ungroup arrange
#' @keywords internal
make_project_consolidated_ds <- function (X_Ready4useDyad, Y_Ready4useDyad, Z_Ready4useDyad = ready4use::Ready4useDyad, 
    type_1L_chr = c("outcomes", "tx_status")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "outcomes") {
        A_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            rbind(X_Ready4useDyad@ds_tb %>% dplyr::filter(MeasurementWeek == 
                "Week0"), X_Ready4useDyad@ds_tb %>% dplyr::filter(MeasurementWeek == 
                "Week12") %>% dplyr::filter(!UID %in% unique(Y_Ready4useDyad@ds_tb$UID)), 
                Y_Ready4useDyad@ds_tb) %>% dplyr::group_by(UID) %>% 
                dplyr::mutate(`Data Collection Rounds` = as.character(dplyr::n())) %>% 
                dplyr::ungroup() %>% dplyr::arrange(UID))
    }
    if (type_1L_chr == "tx_status") {
        A_Ready4useDyad <- make_project_tx_mdlng_ds(X_Ready4useDyad = X_Ready4useDyad, 
            Y_Ready4useDyad = Y_Ready4useDyad, Z_Ready4useDyad = Z_Ready4useDyad)
    }
    return(A_Ready4useDyad)
}
#' Make project contacts dataset
#' @description make_project_contacts_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project contacts dataset. The function returns Contacts (a tibble).
#' @param raw_data_ls Raw data (a list)
#' @param demographics_tb Demographics (a tibble)
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: make_project_recode_lup()
#' @return Contacts (a tibble)
#' @rdname make_project_contacts_ds
#' @export 
#' @importFrom dplyr inner_join mutate select
#' @importFrom ready4show manufacture.ready4show_correspondences
#' @keywords internal
make_project_contacts_ds <- function (raw_data_ls, demographics_tb, recode_lup_r3 = make_project_recode_lup()) 
{
    contacts_tb <- demographics_tb %>% dplyr::inner_join(raw_data_ls$contacts)
    contacts_tb <- contacts_tb %>% dplyr::mutate(primary_purpose = recode_lup_r3 %>% 
        ready4show::manufacture.ready4show_correspondences(contacts_tb %>% 
            dplyr::select(primary_purpose), flatten_1L_lgl = TRUE))
    contacts_tb <- contacts_tb %>% dplyr::mutate(Minutes = direct_mins + 
        indirect_mins)
    return(contacts_tb)
}
#' Make project cost modelling dataset
#' @description make_project_cost_mdlng_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project cost modelling dataset. The function is called for its side effects and does not return a value.
#' @param W_Ready4useDyad PARAM_DESCRIPTION
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param transform_gender_1L_lgl Transform gender (a logical vector of length one), Default: T
#' @return Z (A dataset and data dictionary pair.)
#' @rdname make_project_cost_mdlng_ds
#' @export 
#' @importFrom dplyr filter select starts_with arrange mutate case_when
#' @importFrom tidyr all_of any_of
#' @importFrom lubridate years
#' @keywords internal
make_project_cost_mdlng_ds <- function (W_Ready4useDyad, X_Ready4useDyad, transform_gender_1L_lgl = T) 
{
    Z_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::filter(IncludedDays > 0) %>% dplyr::select(-tidyr::all_of(c(dplyr::starts_with("Year")))) %>% 
        dplyr::select(-tidyr::any_of(c("Active", "Episodes", 
            "Separations", "role_type", "primary_mode", "primary_participant", 
            "primary_purpose", "Activity", "Enroll", "EpisodeEnd", 
            "Presentation", "onboarding_date", "first_contact", 
            "last_contact", "days_to_start", "days_to_end", "days_to_first", 
            "days_to_last", "IntersectingYears"))))
    duplicates_chr <- W_Ready4useDyad@ds_tb$UID[W_Ready4useDyad@ds_tb$UID %>% 
        duplicated()] %>% unique()
    duplicates_tb <- W_Ready4useDyad@ds_tb %>% dplyr::filter(UID %in% 
        duplicates_chr) %>% dplyr::arrange(UID) %>% dplyr::filter(Onboarded == 
        1)
    W_Ready4useDyad@ds_tb <- W_Ready4useDyad@ds_tb %>% dplyr::filter(!UID %in% 
        duplicates_chr) %>% rbind(duplicates_tb) %>% dplyr::arrange(Date, 
        UID)
    onboarded_tb <- W_Ready4useDyad@ds_tb %>% dplyr::filter(Date >= 
        Z_Ready4useDyad@ds_tb$Date %>% min() & Date < (Z_Ready4useDyad@ds_tb$Date %>% 
        min() + lubridate::years(1))) %>% dplyr::filter(!UID %in% 
        unique(Z_Ready4useDyad@ds_tb$UID))
    onboarded_tb <- onboarded_tb %>% add_activity()
    onboarded_tb <- onboarded_tb %>% dplyr::select(intersect(names(onboarded_tb), 
        names(Z_Ready4useDyad@ds_tb))) %>% dplyr::mutate(direct_mins = 0, 
        indirect_mins = 0, Minutes = 0, Tenure = 0, Cost = 0, 
        Cost_S1 = 0, Psychosocial = 0, Coordination = 0, Psychological = 0, 
        Suicideprevention = 0, Assessment = 0) %>% dplyr::mutate(IncludedDays = as.numeric(((Z_Ready4useDyad@ds_tb$Date %>% 
        min() + lubridate::years(1)) - Date))) %>% dplyr::mutate(IncludedDays = dplyr::case_when(IncludedDays > 
        366 ~ 0, IncludedDays < 0 ~ 0, TRUE ~ IncludedDays))
    Z_Ready4useDyad@ds_tb <- rbind(Z_Ready4useDyad@ds_tb, onboarded_tb) %>% 
        dplyr::arrange(Date)
    if (transform_gender_1L_lgl) {
        Z_Ready4useDyad@ds_tb <- Z_Ready4useDyad@ds_tb %>% update_gender()
    }
    Z_Ready4useDyad <- Z_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    return(Z_Ready4useDyad)
}
#' Make project costs dataset
#' @description make_project_costs_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project costs dataset. The function returns Costs (a tibble).
#' @param dss_ls Datasets (a list)
#' @param end_dtm End (a date vector), Default: NULL
#' @param financial_yrs_chr Financial years (a character vector), Default: c("FY 2021", "FY 2022", "FY 2023", "FY 2024")
#' @param price_indices_dbl Price indices (a double vector), Default: c(97.2, 100, 100.7796, 102.5514)
#' @param ref_1L_chr Reference (a character vector of length one), Default: 'FY 2024'
#' @param start_dtm Start (a date vector), Default: NULL
#' @param sunk_ls Sunk (a list), Default: list(Base = make_project_sunk_tb(), S1 = make_project_sunk_tb(0))
#' @param type_1L_chr Type (a character vector of length one), Default: c("constant", "current")
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "fixed", "variable", "unit", "initial")
#' @return Costs (a tibble)
#' @rdname make_project_costs_ds
#' @export 
#' @importFrom janitor row_to_names
#' @importFrom tidyr fill
#' @importFrom dplyr filter mutate across rename case_when select everything left_join where bind_rows arrange group_by summarise ungroup
#' @importFrom tidyselect any_of
#' @importFrom serious update_for_price_year
#' @importFrom stats setNames
#' @importFrom purrr pluck map_dfr
#' @importFrom stringr str_sub
#' @importFrom lubridate ymd years days
#' @importFrom rlang sym
#' @keywords internal
make_project_costs_ds <- function (dss_ls, end_dtm = NULL, financial_yrs_chr = c("FY 2021", 
    "FY 2022", "FY 2023", "FY 2024"), price_indices_dbl = c(97.2, 
    100, 100.7796, 102.5514), ref_1L_chr = "FY 2024", start_dtm = NULL, 
    sunk_ls = list(Base = make_project_sunk_tb(), S1 = make_project_sunk_tb(0)), 
    type_1L_chr = c("constant", "current"), what_1L_chr = c("all", 
        "fixed", "variable", "unit", "initial")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "initial") {
        wip_tb <- dss_ls$costing %>% janitor::row_to_names(1) %>% 
            tidyr::fill(Category, .direction = "down")
        costs_tb <- wip_tb %>% dplyr::filter(`Items in Catergory` != 
            "Total") %>% dplyr::mutate(dplyr::across(tidyselect::any_of(c(financial_yrs_chr, 
            "Total")), as.numeric)) %>% dplyr::rename(Item = `Items in Catergory`) %>% 
            dplyr::mutate(Type = dplyr::case_when(Item %in% c("Platform development and maintenance", 
                "Platform infrastructure", "Content development", 
                "Data collection, analysis & reporting", "Marketing & communications", 
                "Project management and leadership") ~ "Fixed", 
                T ~ "Variable")) %>% dplyr::select(Category, 
            Item, Description, Type, dplyr::everything())
        if (type_1L_chr == "constant") {
            costs_tb <- serious::update_for_price_year(costs_tb, 
                price_indices_dbl = price_indices_dbl %>% stats::setNames(financial_yrs_chr), 
                price_ref_1L_int = which(financial_yrs_chr == 
                  ref_1L_chr), total_1L_chr = "Total", years_are_cols_1L_lgl = T)
        }
    }
    else {
        costs_tb <- dss_ls %>% purrr::pluck(paste0("costs_", 
            type_1L_chr))
        if (what_1L_chr %in% c("all", "fixed", "unit")) {
            fixed_tb <- 1:length(sunk_ls) %>% purrr::map_dfr(~{
                index_1L_int <- .x
                costs_tb %>% dplyr::filter(Type == "Fixed") %>% 
                  dplyr::left_join(sunk_ls %>% purrr::pluck(index_1L_int)) %>% 
                  dplyr::mutate(Kept = dplyr::case_when(is.na(Kept) ~ 
                    1, T ~ Kept)) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                  ~. * Kept)) %>% dplyr::select(-c("Sunk", "Kept")) %>% 
                  dplyr::mutate(Scenario = names(sunk_ls)[index_1L_int]) %>% 
                  dplyr::select(Scenario, dplyr::everything())
            })
        }
        if (what_1L_chr %in% c("all", "variable", "unit")) {
            variable_tb <- 1:length(sunk_ls) %>% purrr::map_dfr(~{
                index_1L_int <- .x
                costs_tb %>% dplyr::filter(Type == "Variable") %>% 
                  dplyr::left_join(sunk_ls %>% purrr::pluck(index_1L_int)) %>% 
                  dplyr::mutate(Kept = dplyr::case_when(is.na(Kept) ~ 
                    1, T ~ Kept)) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                  ~. * Kept)) %>% dplyr::select(-c("Sunk", "Kept")) %>% 
                  dplyr::mutate(Scenario = names(sunk_ls)[index_1L_int]) %>% 
                  dplyr::select(Scenario, dplyr::everything())
            })
        }
        if (what_1L_chr %in% c("all", "unit")) {
            costs_tb <- dplyr::bind_rows(fixed_tb, variable_tb)
        }
        else {
            if (what_1L_chr == "fixed") {
                costs_tb <- fixed_tb
            }
            if (what_1L_chr == "variable") {
                costs_tb <- variable_tb
            }
        }
        costs_tb <- dplyr::select(costs_tb, Scenario, Type, dplyr::everything()) %>% 
            dplyr::arrange(Scenario, Type, Category)
        if (what_1L_chr == "unit") {
            contacts_tb <- dss_ls$contacts %>% dplyr::mutate(Date = date_contacted %>% 
                format() %>% stringr::str_sub(end = 10) %>% lubridate::ymd())
            if (is.null(start_dtm)) {
                start_dtm <- min(contacts_tb$Date)
            }
            if (is.null(end_dtm)) {
                end_dtm <- start_dtm + lubridate::years(1) - 
                  lubridate::days(1)
            }
            contacts_tb <- contacts_tb %>% dplyr::filter(Date >= 
                start_dtm & Date <= end_dtm)
            onboarded_tb <- dss_ls$overview %>% dplyr::filter(onboarding_date >= 
                start_dtm & onboarding_date < end_dtm) %>% dplyr::filter(!UID %in% 
                unique(contacts_tb$UID))
            onboarded_tb <- onboarded_tb %>% dplyr::select(intersect(names(onboarded_tb), 
                names(contacts_tb))) %>% dplyr::mutate(Date = onboarding_date, 
                direct_mins = 0, indirect_mins = 0, Minutes = 0, 
                role_type = "Enroll", primary_mode = "Enroll", 
                primary_participant = "Enroll", primary_purpose = "Enroll")
            active_clients_1L_dbl <- length(c(unique(contacts_tb$UID), 
                onboarded_tb$UID))
            unit_costs_tb <- dplyr::group_by(costs_tb, Scenario, 
                Type) %>% dplyr::summarise(TotalCost = sum(!!rlang::sym(ref_1L_chr))) %>% 
                dplyr::mutate(Quantity = c(active_clients_1L_dbl, 
                  sum(contacts_tb$Minutes)), Unit = c("Clients", 
                  "Contact Minutes")) %>% dplyr::mutate(UnitCost = TotalCost/Quantity) %>% 
                dplyr::select(Type, Unit, Quantity, TotalCost, 
                  UnitCost) %>% dplyr::ungroup()
            costs_tb <- unit_costs_tb
        }
    }
    return(costs_tb)
}
#' Make project demographics dataset
#' @description make_project_demographics_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project demographics dataset. The function returns Demographics (a tibble).
#' @param raw_data_ls Raw data (a list)
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @return Demographics (a tibble)
#' @rdname make_project_demographics_ds
#' @export 
#' @importFrom dplyr mutate
#' @importFrom serious add_new_uid
#' @keywords internal
make_project_demographics_ds <- function (raw_data_ls, uid_1L_chr = "UID") 
{
    demographics_tb <- raw_data_ls$demographics %>% dplyr::mutate(clinic_postcode = as.character(clinic_postcode)) %>% 
        serious::add_new_uid(uid_vars_chr = "case_number", recode_1L_lgl = T, 
            new_uid_var_1L_chr = uid_1L_chr)
    return(demographics_tb)
}
#' Make project dictionary
#' @description make_project_dictionary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project dictionary. The function returns Dictionary (an output object of multiple potential types).
#' @param raw_data_ls Raw data (a list)
#' @param platform_1L_chr Platform (a character vector of length one)
#' @param dss_ls Datasets (a list), Default: NULL
#' @param financial_yrs_chr Financial years (a character vector), Default: c("FY 2021", "FY 2022", "FY 2023", "FY 2024")
#' @param gender_1L_chr Gender (a character vector of length one), Default: 'gender'
#' @param index_date_1L_chr Index date (a character vector of length one), Default: 'onboarding_date'
#' @param outcomes_chr Outcomes (a character vector), Default: c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_utl")
#' @param price_indices_dbl Price indices (a double vector), Default: c(97.2, 100, 100.7796, 102.5514)
#' @param price_ref_1L_int Price reference (an integer vector of length one), Default: 4
#' @param recode_1L_lgl Recode (a logical vector of length one), Default: T
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: make_project_recode_lup()
#' @param ref_1L_chr Reference (a character vector of length one), Default: 'FY 2024'
#' @param type_1L_chr Type (a character vector of length one), Default: c("core", "all", "values")
#' @param what_chr What (a character vector), Default: c("activity", "contacts", "costs_constant", "costs_current", 
#'    "costs_adjusted", "costs_unit", "outcomes", "overview", "notes")
#' @return Dictionary (an output object of multiple potential types)
#' @rdname make_project_dictionary
#' @export 
#' @importFrom purrr map keep_at map_chr pluck
#' @importFrom janitor row_to_names
#' @importFrom dplyr filter mutate case_when bind_rows arrange select pull
#' @importFrom tidyr fill
#' @importFrom stringr str_replace_all
#' @importFrom Hmisc capitalize
#' @importFrom tibble add_case
#' @importFrom ready4use ready4use_dictionary renew.ready4use_dictionary
#' @importFrom rlang sym
#' @keywords internal
make_project_dictionary <- function (raw_data_ls, platform_1L_chr, dss_ls = NULL, financial_yrs_chr = c("FY 2021", 
    "FY 2022", "FY 2023", "FY 2024"), gender_1L_chr = "gender", 
    index_date_1L_chr = "onboarding_date", outcomes_chr = c("gad2", 
        "phq2", "gad7", "phq9", "k10", "chu9d_utl"), price_indices_dbl = c(97.2, 
        100, 100.7796, 102.5514), price_ref_1L_int = 4L, recode_1L_lgl = T, 
    recode_lup_r3 = make_project_recode_lup(), ref_1L_chr = "FY 2024", 
    type_1L_chr = c("core", "all", "values"), what_chr = c("activity", 
        "contacts", "costs_constant", "costs_current", "costs_adjusted", 
        "costs_unit", "outcomes", "overview", "notes")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (is.null(dss_ls)) {
        dss_ls <- make_project_ds(raw_data_ls, platform_1L_chr = platform_1L_chr, 
            financial_yrs_chr = financial_yrs_chr, gender_1L_chr = gender_1L_chr, 
            index_date_1L_chr = index_date_1L_chr, outcomes_chr = outcomes_chr, 
            price_indices_dbl = price_indices_dbl, recode_1L_lgl = recode_1L_lgl, 
            recode_lup_r3 = recode_lup_r3, ref_1L_chr = ref_1L_chr, 
            type_1L_chr = "tibble", what_1L_chr = "all")
    }
    else {
        dss_ls <- dss_ls %>% purrr::map(~{
            if (inherits(.x, "Ready4useDyad")) {
                .x@ds_tb
            }
            else {
                .x
            }
        })
    }
    dss_ls <- dss_ls %>% purrr::keep_at(what_chr)
    dictionary_tb <- raw_data_ls$dictionary %>% janitor::row_to_names(1)
    names(dictionary_tb) <- c(names(dictionary_tb)[1:3], "Value")
    dictionary_tb <- dictionary_tb %>% dplyr::filter(is.na(Section) | 
        Section != "Section") %>% dplyr::filter((!is.na(Variable))) %>% 
        tidyr::fill(Section, .direction = "down") %>% dplyr::mutate(Section = dplyr::case_when(startsWith(Section, 
        "Patient Health Questionnaire 2 (PHQ-2)") ~ "Patient Health Questionnaire 2 (PHQ-2)", 
        startsWith(Section, "Patient Health Questionnaire 2 (PHQ-9)") ~ 
            "Patient Health Questionnaire 9 (PHQ-9)", startsWith(Section, 
            "Kessler Psychological Distress Scale (K10)") ~ "Kessler Psychological Distress Scale (K10)", 
        startsWith(Section, "Quality of life (Child Health Utility 9D)") ~ 
            "Quality of life (Child Health Utility 9D)", startsWith(Section, 
            "Generalized Anxiety Disorder 2-item (GAD-2)") ~ 
            "Generalized Anxiety Disorder 2-item (GAD-2)", startsWith(Section, 
            "Generalized Anxiety Disorder 7-item (GAD-7)") ~ 
            "Generalized Anxiety Disorder 7-item (GAD-7)", startsWith(Section, 
            "Customer Satisfaction Survey") ~ "Customer Satisfaction Survey", 
        Section == "Metadata" ~ "Service", T ~ Section), Description = dplyr::case_when(endsWith(Description, 
        "Do you identify as Aboriginal and/or Torres Strait Islander?") ~ 
        "Aboriginal and/or Torres Strait Islander", startsWith(Description, 
        "Case number") ~ "Unique identifier", startsWith(Description, 
        "12-14 platform") ~ "Platform assignment (age-based)", 
        startsWith(Description, "Date of birth for YP") ~ "Date of birth", 
        startsWith(Description, "Sign-up date") ~ "Sign-up date", 
        startsWith(Description, "Date of onboarding") ~ "Date of onboarding", 
        startsWith(Description, "How do you describe your gender identity?") ~ 
            "Gender identity", startsWith(Description, "Employment status") ~ 
            "Employment status", startsWith(Description, "Classification of referring clinic") ~ 
            "Clinic type", startsWith(Description, "Number of sessions") ~ 
            "Number of sessions", startsWith(Description, "Cumulative total of session durations") ~ 
            "Cumulative sessions duration", startsWith(Description, 
            "YP are encouraged to re-visit therapy items") ~ 
            "Number of therapy items viewed", Variable == "therapy_views" ~ 
            "Number of views of therapy items", startsWith(Description, 
            "Number of posts/comment/reactions made") ~ "Number of posts made", 
        Variable == "comments_made" ~ "Number of comments made", 
        Variable == "reactions_made" ~ "Number of reactions made", 
        startsWith(Description, "YP's current stage of treatment") ~ 
            "Current stage of treatment", startsWith(Description, 
            "Role of the internal team member") ~ "Role", startsWith(Description, 
            "Moving or speaking") ~ "Moving or speaking (slow / more than usual)", 
        startsWith(Description, "Feeling bad about yourself") ~ 
            "Feeling bad about yourself", startsWith(Description, 
            "Trouble concentrating") ~ "Trouble concentrating", 
        startsWith(Description, paste0("Would you recommend ", 
            platform_1L_chr)) ~ "Satisfaction - would recommend", 
        T ~ Description)) %>% dplyr::mutate(Description = stringr::str_replace_all(Description, 
        "Number of DM", "Number of direct messages") %>% stringr::str_replace_all("About how often did you feel ", 
        "") %>% stringr::str_replace_all("How satisfied are you with ", 
        "Satistfaction - ") %>% stringr::str_replace_all(paste0(platform_1L_chr, 
        " overall"), "overall") %>% stringr::str_replace_all(paste0("the therapy content on ", 
        platform_1L_chr, " (e.g., Journey and Explore)?"), "therapy content") %>% 
        stringr::str_replace_all(paste0("the peer community on ", 
            platform_1L_chr, " (e.g., Newsfeed and Talk it Out)?"), 
            "peer community") %>% stringr::str_replace_all(paste0("the human support you received on ", 
        platform_1L_chr, " (e.g., a Clinician, Peer Worker, or Career Consultant)?"), 
        "human support") %>% stringr::str_replace_all(paste0("Has ", 
        platform_1L_chr, " helped you feel better?"), "Satisfaction - feel better") %>% 
        stringr::str_replace_all(paste0("Have you felt safe using ", 
            platform_1L_chr, "?"), "Satisfaction - felt safe") %>% 
        stringr::str_replace_all(" (things like eating, having a bath/shower, getting dressed)", 
            "") %>% stringr::str_replace_all(" (things like going out with your friends, doing sports, joining in things)", 
        "") %>% stringr::str_replace_all("\\?", "")) %>% dplyr::mutate(Description = dplyr::case_when(startsWith(Description, 
        "Satistfaction - therapy content") ~ "Satistfaction - therapy content", 
        startsWith(Description, "Satistfaction - peer community") ~ 
            "Satistfaction - peer community", startsWith(Description, 
            "Satistfaction - human support") ~ "Satistfaction - human support", 
        T ~ Description)) %>% dplyr::mutate(Variable = dplyr::case_when(startsWith(Variable, 
        "GAD7-") ~ Variable %>% stringr::str_replace_all("GAD7-", 
        "gad7_"), startsWith(Variable, "GAD2-") ~ Variable %>% 
        stringr::str_replace_all("GAD2-", "gad2_"), T ~ Variable)) %>% 
        dplyr::filter(!duplicated(Variable))
    dictionary_tb <- dplyr::bind_rows(dictionary_tb, dictionary_tb %>% 
        dplyr::filter(Variable == "phq9_item8") %>% dplyr::mutate(Variable = "phq9_item9", 
        Description = "Thoughts - better off dead / hurting self")) %>% 
        dplyr::mutate(Description = Hmisc::capitalize(Description))
    dictionary_tb <- dictionary_tb %>% tibble::add_case(Section = c("Reporting", 
        rep("Temporal", 7)), Variable = c("Metric", "FY 2021", 
        "FY 2022", "FY 2023", "FY 2024", "Total", "sign_up_date", 
        "MeasurementWeek"), Description = c("Reporting metric", 
        "Financial Year 2020-2021", "Financial Year 2021-2022", 
        "Financial Year 2022-2023", "Financial Year 2023-2024", 
        "Financial Year 2020-2021 through to Financial Year 2023-2024", 
        "Date of sign-up", "Week (baseline = 0) of data collection"), 
        Value = NA_character_) %>% tibble::add_case(Section = c(rep("Temporal", 
        4)), Variable = c("Category", "Item", "Description", 
        "Note"), Description = c("Reporting category", "Reporting item", 
        "Description of reporting item", "Note on reporting item"), 
        Value = NA_character_) %>% tibble::add_case(Section = "Identifier", 
        Variable = c("UID"), Description = c("Unique person identifier"), 
        Value = NA_character_) %>% tibble::add_case(Section = c("Generalized Anxiety Disorder 2-item (GAD-2)", 
        "Patient Health Questionnaire 2 (PHQ-2)", "Generalized Anxiety Disorder 7-item (GAD-7)", 
        "Patient Health Questionnaire 9 (PHQ-9)", "Kessler Psychological Distress Scale (K10)", 
        "Quality of life (Child Health Utility 9D)", "Quality of life (Child Health Utility 9D)"), 
        Variable = c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_cml", 
            "chu9d_utl"), Description = c("Generalized Anxiety Disorder 2-item (GAD-2) total score", 
            "Patient Health Questionnaire 2 (PHQ-2) total score", 
            "Generalized Anxiety Disorder 7-item (GAD-7) total score", 
            "Patient Health Questionnaire 9 (PHQ-9) total score", 
            "Kessler Psychological Distress Scale (K10) total score", 
            "Quality of life (Child Health Utility 9D) unweighted total score", 
            "Quality of life (Child Health Utility 9D) health utility"), 
        Value = NA_character_) %>% tibble::add_case(Section = "Cost", 
        Variable = "Type", Description = "Type of cost - fixed or variable", 
        Value = NA_character_) %>% tibble::add_case(Section = "Reporting", 
        Variable = "Quantity", Description = "Quantity as measured by a specified unit", 
        Value = NA_character_) %>% tibble::add_case(Section = "Reporting", 
        Variable = "Unit", Description = "Unit of measurement", 
        Value = NA_character_) %>% tibble::add_case(Section = "Cost", 
        Variable = "TotalCost", Description = "Total cost", Value = NA_character_) %>% 
        tibble::add_case(Section = "Cost", Variable = "UnitCost", 
            Description = "Unit cost", Value = NA_character_) %>% 
        dplyr::arrange(Section, Variable)
    dictionary_tb <- dplyr::mutate(dictionary_tb, Section = dplyr::case_when(Section == 
        "case_number" ~ "Identifier", Section %in% c("sign_up_date", 
        "onboarding_date", "MeasurementWeek", "date_contacted", 
        "date_of_birth") ~ "Temporal", Section == "Demographics" ~ 
        "Demographic", Section %in% c("clinic_type", "platform", 
        "treatment_stage") ~ "Service", Section %in% c(" clinic_state", 
        " clinic_postcode") ~ "Spatial", T ~ Section))
    if (type_1L_chr == "all") {
        dictionary_xx <- dictionary_tb
    }
    if (type_1L_chr == "values") {
        dictionary_xx <- dictionary_tb %>% dplyr::select(Variable, 
            Value) %>% dplyr::filter(!is.na(Value))
    }
    if (type_1L_chr == "core") {
        dictionary_xx <- dss_ls %>% purrr::map(~{
            ds_tb <- .x
            new_dict_tb <- dictionary_tb %>% dplyr::filter(Variable %in% 
                names(ds_tb))
            ready4use::ready4use_dictionary() %>% ready4use::renew.ready4use_dictionary(var_nm_chr = new_dict_tb$Variable, 
                var_ctg_chr = new_dict_tb$Section, var_desc_chr = new_dict_tb$Description, 
                var_type_chr = new_dict_tb$Variable %>% purrr::map_chr(~ds_tb %>% 
                  dplyr::pull(!!rlang::sym(.x)) %>% class() %>% 
                  purrr::pluck(1)))
        })
        if (length(dictionary_xx) == 1) {
            dictionary_xx <- dictionary_xx %>% purrr::pluck(1)
        }
    }
    return(dictionary_xx)
}
#' Make project dataset
#' @description make_project_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project dataset. The function returns Project (an output object of multiple potential types).
#' @param raw_data_ls Raw data (a list)
#' @param platform_1L_chr Platform (a character vector of length one)
#' @param age_1L_chr Age (a character vector of length one), Default: 'Age'
#' @param cleanse_1L_chr Cleanse (a character vector of length one), Default: c("itemdims", "all", "none", "dims", "items")
#' @param drop_1L_lgl Drop (a logical vector of length one), Default: TRUE
#' @param date_of_birth_1L_chr Date of birth (a character vector of length one), Default: 'date_of_birth'
#' @param employment_1L_chr Employment (a character vector of length one), Default: 'employment_status'
#' @param financial_yrs_chr Financial years (a character vector), Default: c("FY 2021", "FY 2022", "FY 2023", "FY 2024")
#' @param gender_1L_chr Gender (a character vector of length one), Default: 'gender'
#' @param index_date_1L_chr Index date (a character vector of length one), Default: 'onboarding_date'
#' @param outcomes_chr Outcomes (a character vector), Default: c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_utl")
#' @param price_indices_dbl Price indices (a double vector), Default: c(97.2, 100, 100.7796, 102.5514)
#' @param recode_1L_lgl Recode (a logical vector of length one), Default: T
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: make_project_recode_lup()
#' @param ref_1L_chr Reference (a character vector of length one), Default: 'FY 2024'
#' @param type_1L_chr Type (a character vector of length one), Default: c("dyad", "tibble")
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "activity", "contacts", "costs_constant", "costs_current", 
#'    "costs_adjusted", "costs_unit", "outcomes", "overview", "notes")
#' @return Project (an output object of multiple potential types)
#' @rdname make_project_ds
#' @export 
#' @importFrom tibble tibble
#' @importFrom purrr keep_at map reduce pluck
#' @importFrom stats setNames
#' @importFrom ready4use Ready4useDyad
#' @importFrom dplyr pull mutate select case_when everything
#' @importFrom rlang sym
#' @importFrom ready4show manufacture.ready4show_correspondences
#' @importFrom tidyr any_of
#' @importFrom tidyselect all_of
#' @keywords internal
make_project_ds <- function (raw_data_ls, platform_1L_chr, age_1L_chr = "Age", cleanse_1L_chr = c("itemdims", 
    "all", "none", "dims", "items"), drop_1L_lgl = TRUE, date_of_birth_1L_chr = "date_of_birth", 
    employment_1L_chr = "employment_status", financial_yrs_chr = c("FY 2021", 
        "FY 2022", "FY 2023", "FY 2024"), gender_1L_chr = "gender", 
    index_date_1L_chr = "onboarding_date", outcomes_chr = c("gad2", 
        "phq2", "gad7", "phq9", "k10", "chu9d_utl"), price_indices_dbl = c(97.2, 
        100, 100.7796, 102.5514), recode_1L_lgl = T, recode_lup_r3 = make_project_recode_lup(), 
    ref_1L_chr = "FY 2024", type_1L_chr = c("dyad", "tibble"), 
    uid_1L_chr = "UID", what_1L_chr = c("all", "activity", "contacts", 
        "costs_constant", "costs_current", "costs_adjusted", 
        "costs_unit", "outcomes", "overview", "notes")) 
{
    cleanse_1L_chr <- match.arg(cleanse_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    demographics_tb <- make_project_demographics_ds(raw_data_ls, 
        uid_1L_chr = uid_1L_chr)
    outcomes_tb <- make_project_outcomes_ds(raw_data_ls, demographics_tb = demographics_tb, 
        cleanse_1L_chr = cleanse_1L_chr, drop_1L_lgl = drop_1L_lgl, 
        outcomes_chr = outcomes_chr, type_1L_chr = "initial", 
        uid_1L_chr = uid_1L_chr)
    current_costs_tb <- make_project_costs_ds(raw_data_ls, type_1L_chr = "current", 
        what_1L_chr = "initial")
    constant_costs_tb <- make_project_costs_ds(raw_data_ls, financial_yrs_chr = financial_yrs_chr, 
        price_indices_dbl = price_indices_dbl, ref_1L_chr = ref_1L_chr, 
        type_1L_chr = "constant", what_1L_chr = "initial")
    contacts_tb <- make_project_contacts_ds(raw_data_ls, demographics_tb = demographics_tb, 
        recode_lup_r3 = recode_lup_r3)
    overview_tb <- make_project_overview_ds(raw_data_ls, demographics_tb = demographics_tb)
    activity_tb <- make_project_activity_ds(raw_data_ls)
    notes_tb <- make_project_notes_ds(raw_data_ls, financial_yrs_chr = financial_yrs_chr)
    dss_ls <- list(activity = activity_tb, contacts = contacts_tb, 
        costs_adjusted = tibble::tibble(), costs_constant = constant_costs_tb, 
        costs_current = current_costs_tb, costs_unit = tibble::tibble(), 
        outcomes = outcomes_tb, overview = overview_tb, notes = notes_tb)
    dss_ls$costs_adjusted <- make_project_costs_ds(dss_ls, what_1L_chr = "all")
    dss_ls$costs_unit <- make_project_costs_ds(dss_ls, what_1L_chr = "unit")
    if (what_1L_chr != "all") {
        dss_ls <- dss_ls %>% purrr::keep_at(what_1L_chr)
    }
    if (type_1L_chr == "dyad") {
        dictionaries_xx <- make_project_dictionary(raw_data_ls, 
            dss_ls = dss_ls, financial_yrs_chr = financial_yrs_chr, 
            gender_1L_chr = gender_1L_chr, index_date_1L_chr = index_date_1L_chr, 
            outcomes_chr = outcomes_chr, platform_1L_chr = platform_1L_chr, 
            price_indices_dbl = price_indices_dbl, recode_1L_lgl = recode_1L_lgl, 
            recode_lup_r3 = recode_lup_r3, ref_1L_chr = ref_1L_chr, 
            what_chr = names(dss_ls))
        if (length(names(dss_ls)) == 1) {
            dictionaries_ls <- list(dictionaries_xx) %>% stats::setNames(names(dss_ls))
        }
        else {
            dictionaries_ls <- dictionaries_xx
        }
        dss_ls <- 1:length(dss_ls) %>% purrr::map(~ready4use::Ready4useDyad(ds_tb = dss_ls[[.x]], 
            dictionary_r3 = dictionaries_ls[[.x]])) %>% stats::setNames(names(dss_ls))
    }
    dss_ls <- dss_ls %>% add_age_to_project_dss(age_1L_chr = age_1L_chr, 
        drop_1L_lgl = drop_1L_lgl, date_of_birth_1L_chr = date_of_birth_1L_chr, 
        index_date_1L_chr = index_date_1L_chr, what_chr = c("contacts", 
            "outcomes", "overview"))
    dss_ls <- dss_ls %>% purrr::map(~{
        if (inherits(.x, "Ready4useDyad")) {
            ds_tb <- .x@ds_tb
        }
        else {
            ds_tb <- .x
        }
        if (recode_1L_lgl) {
            if (length(intersect(names(ds_tb), c(employment_1L_chr, 
                "atsi_status"))) == 2) 
                ds_tb <- c(employment_1L_chr, "atsi_status") %>% 
                  purrr::reduce(.init = ds_tb, ~{
                    if (!is.numeric(.x %>% dplyr::pull(!!rlang::sym(.y)))) {
                      .x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                        recode_lup_r3 %>% ready4show::manufacture.ready4show_correspondences(.x %>% 
                          dplyr::select(!!rlang::sym(.y)), flatten_1L_lgl = TRUE)))
                    }
                  })
            if (gender_1L_chr %in% names(ds_tb)) {
                ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(gender_1L_chr), 
                  dplyr::case_when(!(!!rlang::sym(gender_1L_chr) %in% 
                    c("Female", "Male", "Prefer not to say")) & 
                    !is.na(!!rlang::sym(gender_1L_chr)) ~ "Other", 
                    T ~ !!rlang::sym(gender_1L_chr))))
            }
        }
        ds_tb <- ds_tb %>% dplyr::select(tidyr::any_of(c(uid_1L_chr, 
            "case_number", "sign_up_date", "onboarding_date", 
            "MeasurementWeek", "date_contacted", "platform", 
            "clinic_type", date_of_birth_1L_chr, age_1L_chr, 
            gender_1L_chr, "atsi_status", employment_1L_chr, 
            "clinic_state", "clinic_postcode", "treatment_stage")), 
            dplyr::everything())
        if (inherits(.x, "Ready4useDyad")) {
            X <- .x %>% renewSlot("ds_tb", ds_tb)
            if (drop_1L_lgl & "case_number" %in% names(X@ds_tb)) {
                X <- X %>% renew(type_1L_chr = "drop", names_chr = "case_number")
            }
            X
        }
        else {
            if (drop_1L_lgl & "case_number" %in% names(ds_tb)) {
                ds_tb <- ds_tb %>% dplyr::select(-tidyselect::all_of("case_number"))
            }
            ds_tb
        }
    })
    if (length(dss_ls) != 1) {
        project_xx <- dss_ls
    }
    else {
        project_xx <- dss_ls %>% purrr::pluck(what_1L_chr)
    }
    return(project_xx)
}
#' Make project joiners dataset
#' @description make_project_joiners_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project joiners dataset. The function returns Data (an output object of multiple potential types).
#' @param processed_ls Processed (a list)
#' @param as_dyad_1L_lgl As dyad (a logical vector of length one), Default: T
#' @return Data (an output object of multiple potential types)
#' @rdname make_project_joiners_ds
#' @export 
#' @importFrom dplyr mutate across select filter bind_rows case_when arrange inner_join
#' @importFrom stringr str_sub
#' @importFrom lubridate ymd
#' @importFrom tidyr all_of pivot_longer
#' @importFrom ready4use Ready4useDyad add_dictionary renew.ready4use_dictionary
#' @keywords internal
make_project_joiners_ds <- function (processed_ls, as_dyad_1L_lgl = T) 
{
    X_Ready4useDyad <- processed_ls$overview
    joiners_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c("sign_up_date", 
        "onboarding_date"), ~.x %>% format() %>% stringr::str_sub(end = 10) %>% 
        lubridate::ymd())) %>% dplyr::select(tidyr::all_of(c("UID", 
        "sign_up_date", "onboarding_date"))) %>% dplyr::mutate(simultaneous_lgl = (sign_up_date == 
        onboarding_date))
    joiners_tb <- joiners_tb %>% dplyr::filter(simultaneous_lgl) %>% 
        dplyr::mutate(SignedUp = 1, Onboarded = 1, Date = sign_up_date) %>% 
        dplyr::select(UID, Date, SignedUp, Onboarded) %>% dplyr::bind_rows(joiners_tb %>% 
        dplyr::filter(!simultaneous_lgl) %>% tidyr::pivot_longer(cols = c("sign_up_date", 
        "onboarding_date"), values_to = "Date") %>% dplyr::mutate(SignedUp = dplyr::case_when(name == 
        "sign_up_date" ~ 1, T ~ 0), Onboarded = dplyr::case_when(name == 
        "onboarding_date" ~ 1, T ~ 0)) %>% dplyr::select(UID, 
        Date, SignedUp, Onboarded)) %>% dplyr::arrange(Date)
    joiners_tb <- joiners_tb %>% dplyr::inner_join(X_Ready4useDyad@ds_tb %>% 
        dplyr::select(tidyr::all_of(c("UID", "platform", "clinic_type", 
            "Age", "gender", "atsi_status", "employment_status", 
            "clinic_state", "clinic_postcode", "treatment_stage"))))
    if (as_dyad_1L_lgl) {
        data_xx <- ready4use::Ready4useDyad(ds_tb = joiners_tb) %>% 
            ready4use::add_dictionary(new_cases_r3 = X_Ready4useDyad@dictionary_r3 %>% 
                ready4use::renew.ready4use_dictionary(var_nm_chr = c("Date", 
                  "SignedUp", "Onboarded"), var_ctg_chr = c("Temporal", 
                  rep("Service", 2)), var_desc_chr = c("Date", 
                  "Signed up to platform", "Onboarded to platform"), 
                  var_type_chr = c("POSIXct", rep("numeric", 
                    2))))
    }
    else {
        data_xx <- joiners_tb
    }
    data_xx <- data_xx %>% add_treatment_status(type_1L_int = 1L)
    return(data_xx)
}
#' Make project K10 models
#' @description make_project_k10_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project k10 models. The function returns K10 (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @return K10 (a list)
#' @rdname make_project_k10_mdls
#' @export 
#' @keywords internal
make_project_k10_mdls <- function (X_Ready4useDyad) 
{
    k10_ls <- list(OLS_1_mdl = lm(formula = k10_12_Weeks ~ AQoL6D + 
        CHU9D + k10 + treatment_change + Minutes_12_Weeks, data = X_Ready4useDyad@ds_tb))
    k10_ls$GLM_GSN_2_mdl <- glm(formula = k10_12_Weeks ~ AQoL6D + 
        CHU9D + k10 + treatment_change + Minutes_12_Weeks, data = X_Ready4useDyad@ds_tb, 
        family = gaussian())
    k10_ls$GLM_GSN_LOG_3_mdl <- glm(formula = k10_12_Weeks ~ 
        CHU9D + k10 + treatment_change + Minutes_12_Weeks, data = X_Ready4useDyad@ds_tb, 
        family = gaussian(link = "log"))
    k10_ls$GLM_GSN_INV_4_mdl <- glm(formula = k10_12_Weeks ~ 
        AQoL6D + CHU9D + k10 + treatment_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = gaussian(link = "inverse"))
    k10_ls$GLM_GMA_5_mdl <- glm(formula = k10_12_Weeks ~ AQoL6D + 
        CHU9D + k10 + treatment_change + Minutes_12_Weeks, data = X_Ready4useDyad@ds_tb, 
        family = Gamma())
    k10_ls$GLM_GMA_LOG_6_mdl <- glm(formula = k10_12_Weeks ~ 
        AQoL6D + CHU9D + k10 + treatment_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = Gamma(link = "log"))
    k10_ls$GLM_GMA_INV_7_mdl <- glm(formula = k10_12_Weeks ~ 
        AQoL6D + CHU9D + k10 + treatment_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = Gamma(link = "inverse"))
    k10_ls$GLM_ING_8_mdl <- glm(formula = k10_12_Weeks ~ AQoL6D + 
        CHU9D + k10 + treatment_change + Minutes_12_Weeks, data = X_Ready4useDyad@ds_tb, 
        family = inverse.gaussian())
    k10_ls$GLM_ING_INV_9_mdl <- glm(formula = k10_12_Weeks ~ 
        AQoL6D + CHU9D + k10 + treatment_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = inverse.gaussian(link = "inverse"))
    k10_ls$GLM_ING_SQT_10_mdl <- glm(formula = k10_12_Weeks ~ 
        AQoL6D + CHU9D + k10 + treatment_change + Minutes_12_Weeks, 
        data = X_Ready4useDyad@ds_tb, family = inverse.gaussian(link = "1/mu^2"))
    return(k10_ls)
}
#' Make project keys
#' @description make_project_keys() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project keys. The function returns Keys (a character vector).
#' @param type_1L_chr Type (a character vector of length one), Default: c("micro", "ts")
#' @return Keys (a character vector)
#' @rdname make_project_keys
#' @export 
#' @keywords internal
make_project_keys <- function (type_1L_chr = c("micro", "ts")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    keys_chr <- c("IncludedDays", "platform", "clinic_type", 
        "treatment_stage", "treatment_status", "Age", "gender", 
        "employment_status", "clinic_state", "clinic_postcode", 
        "role_type", "primary_mode", "primary_participant", "primary_purpose", 
        "Activity")
    if (type_1L_chr == "ts") {
        keys_chr <- setdiff(keys_chr, c("IncludedDays", "treatment_stage", 
            "clinic_postcode", "Activity"))
    }
    return(keys_chr)
}
#' Make project metrics
#' @description make_project_metrics() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project metrics. The function returns Metrics (a character vector).
#' @param unit_costs_tb Unit costs (a tibble)
#' @return Metrics (a character vector)
#' @rdname make_project_metrics
#' @export 
#' @keywords internal
make_project_metrics <- function (unit_costs_tb) 
{
    cost_names_chr <- get_unit_cost_detail(unit_costs_tb, what_1L_chr = "names")
    metrics_chr <- c("Active", "Episodes", "Separations", "direct_mins", 
        "indirect_mins", "Minutes", cost_names_chr)
    return(metrics_chr)
}
#' Make project minutes comparison
#' @description make_project_minutes_cmprsn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project minutes comparison. The function returns Comparison (a tibble).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param names_chr Names (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("dataset", "prediction")
#' @param var_1L_chr Variable (a character vector of length one), Default: character(0)
#' @param weeks_chr Weeks (a character vector), Default: c("Week14", "Week53")
#' @return Comparison (a tibble)
#' @rdname make_project_minutes_cmprsn
#' @export 
#' @importFrom dplyr filter summarise mutate select everything across where
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @keywords internal
make_project_minutes_cmprsn <- function (X_Ready4useDyad, Y_Ready4useDyad, names_chr = character(0), 
    type_1L_chr = c("dataset", "prediction"), var_1L_chr = character(0), 
    weeks_chr = c("Week14", "Week53")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "dataset") {
        Z_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(UID %in% 
                unique(Y_Ready4useDyad@ds_tb$UID)))
        if (identical(names_chr, character(0))) {
            names_chr <- c("All Contacts", "Outcomes Dataset")
        }
        if (identical(var_1L_chr, character(0))) {
            var_1L_chr <- "Minutes"
        }
    }
    else {
        Z_Ready4useDyad <- Y_Ready4useDyad
        if (identical(names_chr, character(0))) {
            names_chr <- c("Observed", "Simulated")
        }
        if (identical(var_1L_chr, character(0))) {
            var_1L_chr <- "Minutes_change"
        }
    }
    comparison_tb <- rbind(weeks_chr %>% purrr::map_dfr(~{
        cbind(X_Ready4useDyad@ds_tb %>% dplyr::filter(MeasurementWeek == 
            .x) %>% dplyr::summarise(`:=`(!!rlang::sym(names_chr[1]), 
            mean(!!rlang::sym(var_1L_chr)))), Z_Ready4useDyad@ds_tb %>% 
            dplyr::filter(MeasurementWeek == .x) %>% dplyr::summarise(`:=`(!!rlang::sym(names_chr[2]), 
            mean(!!rlang::sym(var_1L_chr))))) %>% dplyr::mutate(Timepoint = .x) %>% 
            dplyr::mutate(Measure = "Average minutes") %>% dplyr::select(Measure, 
            Timepoint, dplyr::everything())
    }) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
        ~round(.x, 2) %>% as.character())), weeks_chr %>% purrr::map_dfr(~{
        cbind(X_Ready4useDyad@ds_tb %>% dplyr::filter(MeasurementWeek == 
            .x, !!rlang::sym(var_1L_chr) > 0) %>% dplyr::summarise(`:=`(!!rlang::sym(names_chr[1]), 
            mean(!!rlang::sym(var_1L_chr)))), Z_Ready4useDyad@ds_tb %>% 
            dplyr::filter(MeasurementWeek == .x, !!rlang::sym(var_1L_chr) > 
                0) %>% dplyr::summarise(`:=`(!!rlang::sym(names_chr[2]), 
            mean(!!rlang::sym(var_1L_chr))))) %>% dplyr::mutate(Timepoint = .x) %>% 
            dplyr::mutate(Measure = "Average minutes, clients with >0 minutes") %>% 
            dplyr::select(Measure, Timepoint, dplyr::everything())
    }) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
        ~round(.x, 2) %>% as.character())), weeks_chr %>% purrr::map_dfr(~{
        cbind(X_Ready4useDyad@ds_tb %>% dplyr::filter(MeasurementWeek == 
            .x) %>% dplyr::mutate(NonZero = !!rlang::sym(var_1L_chr) > 
            0) %>% dplyr::summarise(`:=`(!!rlang::sym(names_chr[1]), 
            paste0(round(mean(NonZero) * 100, 2), " %"))), Z_Ready4useDyad@ds_tb %>% 
            dplyr::filter(MeasurementWeek == .x) %>% dplyr::mutate(NonZero = !!rlang::sym(var_1L_chr) > 
            0) %>% dplyr::summarise(`:=`(!!rlang::sym(names_chr[2]), 
            paste0(round(mean(NonZero) * 100, 2), " %")))) %>% 
            dplyr::mutate(Timepoint = .x) %>% dplyr::mutate(Measure = "% with non-zero minutes") %>% 
            dplyr::select(Measure, Timepoint, dplyr::everything())
    }))
    return(comparison_tb)
}
#' Make project minutes dataset
#' @description make_project_minutes_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project minutes dataset. The function is called for its side effects and does not return a value.
#' @param processed_ls Processed (a list)
#' @param cut_weeks_int Cut weeks (an integer vector), Default: c(14, 53)
#' @param drop_1L_lgl Drop (a logical vector of length one), Default: TRUE
#' @param model_data_ls Model data (a list), Default: NULL
#' @param period_dtm Period (a date vector), Default: lubridate::years(1) - lubridate::days(1)
#' @param type_1L_chr Type (a character vector of length one), Default: c("imputed", "unimputed")
#' @param what_1L_chr What (a character vector of length one), Default: c("wide", "long")
#' @return X (A dataset and data dictionary pair.)
#' @rdname make_project_minutes_ds
#' @export 
#' @importFrom lubridate years days interval weeks NA_Date_
#' @importFrom dplyr pull select filter group_by summarise first mutate bind_rows arrange relocate case_when left_join across everything
#' @importFrom purrr map_dfr
#' @importFrom tidyselect any_of
#' @importFrom ready4use add_dictionary
#' @importFrom stringr str_replace_all
#' @importFrom Hmisc capitalize
#' @keywords internal
make_project_minutes_ds <- function (processed_ls, cut_weeks_int = c(14, 53), drop_1L_lgl = TRUE, 
    model_data_ls = NULL, period_dtm = lubridate::years(1) - 
        lubridate::days(1), type_1L_chr = c("imputed", "unimputed"), 
    what_1L_chr = c("wide", "long")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    start_dtm <- processed_ls$contacts@ds_tb %>% dplyr::pull(date_contacted) %>% 
        min()
    end_dtm <- processed_ls$contacts@ds_tb %>% dplyr::pull(date_contacted) %>% 
        max()
    if (!is.null(period_dtm)) {
        end_dtm <- min((start_dtm + period_dtm), end_dtm)
    }
    overview_tb <- processed_ls$overview@ds_tb %>% dplyr::select(UID, 
        onboarding_date) %>% dplyr::filter(onboarding_date <= 
        end_dtm)
    minutes_tb <- processed_ls$contacts@ds_tb %>% dplyr::filter(date_contacted <= 
        end_dtm) %>% dplyr::group_by(UID) %>% dplyr::summarise(direct_mins = sum(direct_mins, 
        na.rm = T), indirect_mins = sum(indirect_mins, na.rm = T), 
        Minutes = sum(Minutes, na.rm = T), onboarding_date = dplyr::first(onboarding_date), 
        first_contact = min(date_contacted), last_contact = max(date_contacted)) %>% 
        dplyr::mutate(days_to_start = lubridate::interval(onboarding_date, 
            start_dtm)/lubridate::days(1), days_to_first = lubridate::interval(onboarding_date, 
            first_contact)/lubridate::days(1), days_to_last = lubridate::interval(onboarding_date, 
            last_contact)/lubridate::days(1))
    new_tb <- minutes_tb %>% dplyr::filter(days_to_start <= 0)
    new_chr <- unique(new_tb$UID)
    legacy_tb <- minutes_tb %>% dplyr::filter(days_to_start > 
        0)
    legacy_chr <- unique(legacy_tb$UID)
    zeros_tb <- overview_tb %>% dplyr::filter(!UID %in% c(new_chr, 
        legacy_chr)) %>% dplyr::mutate(days_to_start = lubridate::interval(onboarding_date, 
        start_dtm)/lubridate::days(1), direct_mins = 0, indirect_mins = 0, 
        Minutes = 0)
    new_tb <- dplyr::bind_rows(new_tb, zeros_tb %>% dplyr::filter(days_to_start <= 
        0)) %>% dplyr::arrange(onboarding_date)
    legacy_tb <- dplyr::bind_rows(legacy_tb, zeros_tb %>% dplyr::filter(days_to_start > 
        0)) %>% dplyr::arrange(onboarding_date)
    minutes_tb <- dplyr::bind_rows(new_tb %>% dplyr::mutate(Presentation = "New"), 
        legacy_tb %>% dplyr::mutate(Presentation = "Prior")) %>% 
        dplyr::mutate(days_to_end = lubridate::interval(onboarding_date, 
            end_dtm)/lubridate::days(1)) %>% dplyr::relocate(days_to_end, 
        .after = days_to_start) %>% dplyr::relocate(direct_mins, 
        indirect_mins, Minutes, .after = days_to_last) %>% dplyr::relocate(Presentation, 
        .after = UID)
    if (what_1L_chr == "long") {
        minutes_tb <- 1:length(cut_weeks_int) %>% purrr::map_dfr(~{
            period_int <- c(0, cut_weeks_int)[c(.x, .x + 1)]
            minutes_tb %>% dplyr::mutate(MeasurementWeek = paste0("Week", 
                period_int[2]), CutStart = onboarding_date + 
                lubridate::weeks(period_int[1]), CutEnd = onboarding_date + 
                lubridate::weeks(period_int[2])) %>% dplyr::mutate(CutStart = dplyr::case_when(CutStart < 
                start_dtm ~ lubridate::NA_Date_, T ~ CutStart)) %>% 
                dplyr::mutate(CutEnd = dplyr::case_when(CutEnd > 
                  end_dtm ~ lubridate::NA_Date_, T ~ CutEnd))
        }) %>% dplyr::arrange(UID, MeasurementWeek) %>% dplyr::select(-c(direct_mins, 
            indirect_mins, Minutes))
        if (drop_1L_lgl) {
            minutes_tb <- minutes_tb %>% dplyr::filter(!is.na(CutStart) & 
                !is.na(CutEnd))
        }
        Y_Ready4useDyad <- get_project_model_data(model_data_ls, 
            what_1L_chr = "MicroLong", type_1L_chr = type_1L_chr)
        minutes_tb <- minutes_tb$MeasurementWeek %>% unique() %>% 
            purrr::map_dfr(~{
                filtered_tb <- minutes_tb %>% dplyr::filter(MeasurementWeek == 
                  .x) %>% dplyr::select(UID, Presentation, MeasurementWeek, 
                  CutStart, CutEnd, days_to_start, days_to_end, 
                  days_to_first, days_to_last)
                Y_Ready4useDyad@ds_tb %>% dplyr::filter(UID %in% 
                  filtered_tb$UID) %>% dplyr::left_join(filtered_tb) %>% 
                  dplyr::group_by(UID) %>% dplyr::filter(Date >= 
                  CutStart) %>% dplyr::filter(Date < CutEnd) %>% 
                  dplyr::summarise(dplyr::across(c("direct_mins", 
                    "indirect_mins", "Minutes"), ~sum(.))) %>% 
                  dplyr::mutate(MeasurementWeek = .x) %>% dplyr::left_join(filtered_tb)
            }) %>% dplyr::arrange(UID, MeasurementWeek) %>% dplyr::left_join(Y_Ready4useDyad@ds_tb %>% 
            dplyr::group_by(UID) %>% dplyr::summarise(dplyr::across(c("platform", 
            "clinic_type", "clinic_state", "Age", "gender", "employment_status"), 
            ~dplyr::first(.))))
        minutes_tb <- minutes_tb %>% dplyr::select(tidyselect::any_of(c("UID", 
            "MeasurementWeek", "CutStart", "CutEnd", "days_to_start", 
            "days_to_end", "days_to_first", "days_to_last", "Presentation")), 
            dplyr::everything())
        minutes_tb <- minutes_tb %>% dplyr::mutate(dplyr::across(c(direct_mins, 
            indirect_mins, Minutes), .names = "{.col}_change", 
            ~.x))
    }
    X_Ready4useDyad <- renewSlot(processed_ls$overview, "ds_tb", 
        minutes_tb)
    X_Ready4useDyad <- X_Ready4useDyad %>% renewSlot("dictionary_r3", 
        .@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb))) %>% 
        ready4use::add_dictionary()
    X_Ready4useDyad <- X_Ready4useDyad %>% renewSlot("dictionary_r3", 
        .@dictionary_r3 %>% dplyr::filter(!duplicated(var_nm_chr)) %>% 
            dplyr::mutate(var_desc_chr = stringr::str_replace_all(var_desc_chr, 
                "_", " ") %>% Hmisc::capitalize()) %>% dplyr::mutate(var_ctg_chr = dplyr::case_when(var_nm_chr %in% 
            c("Presentation", "Minutes") ~ "Engagement", var_ctg_chr == 
            "Uncategorised" ~ "Temporal", T ~ var_ctg_chr)))
    return(X_Ready4useDyad)
}
#' Make project minutes models
#' @description make_project_minutes_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project minutes models. The function returns Tpm models (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param family_2_1L_chr Family 2 (a character vector of length one), Default: 'Gamma(link = 'inverse')'
#' @param link_1_1L_chr Link 1 (a character vector of length one), Default: 'logit'
#' @param x_part_1_ls X part 1 (a list), Default: NULL
#' @param x_part_2_ls X part 2 (a list), Default: NULL
#' @param y_1L_chr Y (a character vector of length one), Default: 'Minutes_change'
#' @param ... Additional arguments
#' @return Tpm models (a list)
#' @rdname make_project_minutes_mdls
#' @export 
#' @importFrom purrr map2
#' @importFrom stats setNames
#' @keywords internal
make_project_minutes_mdls <- function (X_Ready4useDyad, family_2_1L_chr = "Gamma(link = 'inverse')", 
    link_1_1L_chr = "logit", x_part_1_ls = NULL, x_part_2_ls = NULL, 
    y_1L_chr = "Minutes_change", ...) 
{
    data_tb <- X_Ready4useDyad@ds_tb
    if (is.null(x_part_1_ls)) {
        x_part_1_ls <- list(c("Age", "employment_status", "gender", 
            "clinic_type", "MeasurementWeek"), c("Age", "employment_status", 
            "gender", "clinic_type", "Minutes", "MeasurementWeek"), 
            c("Age", "employment_status", "gender", "Minutes", 
                "MeasurementWeek"), c("Age", "employment_status", 
                "gender", "clinic_type", "Minutes", "MeasurementWeek"), 
            c("Age", "employment_status", "clinic_type", "Minutes", 
                "MeasurementWeek"), c("Age", "gender", "clinic_type", 
                "Minutes", "MeasurementWeek"), c("employment_status", 
                "gender", "clinic_type", "Minutes", "MeasurementWeek"), 
            c("Age", "Minutes", "MeasurementWeek"), c("Age", 
                "employment_status", "gender", "MeasurementWeek"), 
            c("Age", "employment_status", "Minutes", "MeasurementWeek"), 
            c("Age", "gender", "Minutes", "MeasurementWeek"), 
            c("Age", "Minutes", "MeasurementWeek"))
    }
    if (is.null(x_part_2_ls)) {
        x_part_2_ls <- x_part_1_ls
    }
    tpm_mdls_ls <- purrr::map2(x_part_1_ls, x_part_2_ls, ~make_two_part_mdl(data_tb = data_tb, 
        family_2_1L_chr = family_2_1L_chr, link_1_1L_chr = link_1_1L_chr, 
        x_part_1_chr = .x, x_part_2_chr = .y, y_1L_chr = y_1L_chr)) %>% 
        stats::setNames(paste0("TPM_", 1:length(x_part_1_ls), 
            "_mdl"))
    return(tpm_mdls_ls)
}
#' Make project notes dataset
#' @description make_project_notes_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project notes dataset. The function returns Notes (a tibble).
#' @param raw_data_ls Raw data (a list)
#' @param financial_yrs_chr Financial years (a character vector)
#' @return Notes (a tibble)
#' @rdname make_project_notes_ds
#' @export 
#' @importFrom dplyr filter select rename
#' @keywords internal
make_project_notes_ds <- function (raw_data_ls, financial_yrs_chr) 
{
    notes_tb <- make_project_activity_ds(raw_data_ls, type_1L_chr = "wip") %>% 
        dplyr::filter(Category %in% financial_yrs_chr) %>% dplyr::filter(!is.na(Description)) %>% 
        dplyr::select(Category, Description) %>% dplyr::rename(Item = Category, 
        Note = Description)
    return(notes_tb)
}
#' Make project outcomes dataset
#' @description make_project_outcomes_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project outcomes dataset. The function returns Outcomes (an output object of multiple potential types).
#' @param data_ls Data (a list)
#' @param as_dyad_1L_lgl As dyad (a logical vector of length one), Default: T
#' @param cleanse_1L_chr Cleanse (a character vector of length one), Default: c("all", "none", "dims", "items", "itemdims")
#' @param demographics_tb Demographics (a tibble), Default: tibble::tibble()
#' @param complete_1L_lgl Complete (a logical vector of length one), Default: TRUE
#' @param drop_1L_lgl Drop (a logical vector of length one), Default: TRUE
#' @param drop_chr Drop (a character vector), Default: character(0)
#' @param mdls_lup Models (a lookup table), Default: NULL
#' @param mdl_meta_data_ls Model meta data (a list), Default: NULL
#' @param outcomes_chr Outcomes (a character vector), Default: c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_utl")
#' @param start_at_1L_int Start at (an integer vector of length one), Default: -2L
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param weeks_dbl Weeks (a double vector), Default: c(0, 12)
#' @param Y_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param type_1L_chr Type (a character vector of length one), Default: c("final", "initial")
#' @return Outcomes (an output object of multiple potential types)
#' @rdname make_project_outcomes_ds
#' @export 
#' @importFrom tibble tibble
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom tidyr pivot_longer any_of all_of
#' @importFrom dplyr rename_with inner_join select rowwise mutate c_across ungroup group_by summarise left_join filter arrange n everything case_when lag rename
#' @importFrom scorz add_adol_chu9d
#' @importFrom rlang sym
#' @importFrom lubridate weeks ymd
#' @importFrom stringr str_sub
#' @importFrom youthvars youthvars_gad7 youthvars_k10_aus youthvars_phq9 youthvars_aqol6d_adol youthvars_chu9d_adolaus
#' @importFrom youthu get_mdl_metadata make_predn_metadata_ls add_utl_predn add_qalys_to_ds add_change_in_ds_var
#' @importFrom purrr reduce
#' @keywords internal
make_project_outcomes_ds <- function (data_ls, as_dyad_1L_lgl = T, cleanse_1L_chr = c("all", 
    "none", "dims", "items", "itemdims"), demographics_tb = tibble::tibble(), 
    complete_1L_lgl = TRUE, drop_1L_lgl = TRUE, drop_chr = character(0), 
    mdls_lup = NULL, mdl_meta_data_ls = NULL, outcomes_chr = c("gad2", 
        "phq2", "gad7", "phq9", "k10", "chu9d_utl"), start_at_1L_int = -2L, 
    uid_1L_chr = "UID", weeks_dbl = c(0, 12), Y_Ready4useDyad = ready4use::Ready4useDyad(), 
    type_1L_chr = c("final", "initial")) 
{
    cleanse_1L_chr <- match.arg(cleanse_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "initial") {
        outcomes_tb <- data_ls$survey2 %>% tidyr::pivot_longer(cols = setdiff(names(data_ls$survey2), 
            "case_number"), cols_vary = "slowest", names_to = c(".value", 
            "MeasurementWeek"), names_pattern = "(.*(?<=_week))((?<=_week).*)") %>% 
            dplyr::rename_with(~gsub("_week", "", .x))
        outcomes_tb <- demographics_tb %>% dplyr::inner_join(outcomes_tb)
        outcomes_tb <- outcomes_tb %>% dplyr::select(-chu9d) %>% 
            scorz::add_adol_chu9d(cleanse_1L_chr = cleanse_1L_chr)
        if (drop_1L_lgl) {
            outcomes_tb <- outcomes_tb %>% dplyr::select(-tidyr::any_of(names(outcomes_tb)[grepl("_item", 
                names(outcomes_tb), fixed = TRUE)]))
        }
        if (!identical(outcomes_chr, character(0))) {
            outcomes_tb <- outcomes_tb %>% dplyr::rowwise() %>% 
                dplyr::mutate(missing_outcome_totals = sum(is.na(dplyr::c_across(tidyr::all_of(outcomes_chr))))) %>% 
                dplyr::ungroup()
            drop_tb <- outcomes_tb %>% dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% 
                dplyr::summarise(drop_these_records = !any(missing_outcome_totals == 
                  0))
            outcomes_tb <- outcomes_tb %>% dplyr::left_join(drop_tb) %>% 
                dplyr::filter(!drop_these_records) %>% dplyr::select(-c(drop_these_records, 
                missing_outcome_totals))
        }
        as_dyad_1L_lgl <- F
    }
    else {
        outcomes_tb <- data_ls$outcomes@ds_tb %>% dplyr::select(UID, 
            MeasurementWeek, stage_of_treatment, gad7, phq9, 
            k10, chu9d_utl) %>% dplyr::filter(MeasurementWeek %in% 
            weeks_dbl)
        if (weeks_dbl[1] < 0) {
            outcomes_tb <- rbind(outcomes_tb %>% dplyr::mutate(MeasurementWeek = weeks_dbl[1], 
                stage_of_treatment = 0), outcomes_tb) %>% dplyr::arrange(UID)
        }
        sample_tb <- data_ls$overview@ds_tb %>% dplyr::select(UID, 
            onboarding_date, platform, treatment_stage, clinic_type, 
            clinic_state, Age, gender, employment_status)
        if (!identical(drop_chr, character(0))) {
            sample_tb <- sample_tb %>% dplyr::select(-tidyr::all_of(drop_chr))
        }
        if (complete_1L_lgl) {
            outcomes_tb <- outcomes_tb %>% na.omit() %>% dplyr::group_by(UID) %>% 
                dplyr::filter(dplyr::n() == length(weeks_dbl))
            sample_tb <- sample_tb %>% na.omit()
        }
        outcomes_tb <- dplyr::inner_join(sample_tb, outcomes_tb) %>% 
            dplyr::mutate(Date = onboarding_date + lubridate::weeks(MeasurementWeek)) %>% 
            dplyr::mutate(Date = Date %>% format() %>% stringr::str_sub(end = 10) %>% 
                lubridate::ymd()) %>% dplyr::select(UID, Date, 
            MeasurementWeek, dplyr::everything()) %>% dplyr::mutate(MeasurementWeek = paste0("Week", 
            MeasurementWeek) %>% as.factor()) %>% dplyr::mutate(gad7 = youthvars::youthvars_gad7(as.integer(gad7)), 
            k10 = youthvars::youthvars_k10_aus(as.integer(k10)), 
            phq9 = youthvars::youthvars_phq9(as.integer(phq9)), 
            stage_of_treatment = as.factor(stage_of_treatment))
        if ("gender" %in% names(outcomes_tb)) {
            outcomes_tb <- outcomes_tb %>% dplyr::mutate(gender = as.factor(gender))
        }
        if ("employment_status" %in% names(outcomes_tb)) {
            outcomes_tb <- outcomes_tb %>% dplyr::mutate(employment_status = as.factor(employment_status))
        }
        outcomes_tb <- outcomes_tb %>% dplyr::select(-c(onboarding_date)) %>% 
            dplyr::arrange(UID, Date)
        if (!is.null(mdls_lup)) {
            if (is.null(mdl_meta_data_ls)) {
                mdl_meta_data_ls <- youthu::get_mdl_metadata(mdls_lup, 
                  mdl_nm_1L_chr = "GAD7_PHQ9_1_OLS_CLL")
            }
            predn_ds_ls <- youthu::make_predn_metadata_ls(outcomes_tb, 
                id_var_nm_1L_chr = "UID", mdl_meta_data_ls = mdl_meta_data_ls, 
                mdl_nm_1L_chr = "GAD7_PHQ9_1_OLS_CLL", mdls_lup = mdls_lup, 
                msrmnt_date_var_nm_1L_chr = "Date", predr_vars_nms_chr = c(PHQ9 = "phq9", 
                  GAD7 = "gad7"), round_var_nm_1L_chr = "MeasurementWeek", 
                round_bl_val_1L_chr = paste0("Week", min(weeks_dbl)), 
                utl_var_nm_1L_chr = "AQoL6D_HU")
            outcomes_tb <- youthu::add_utl_predn(outcomes_tb, 
                predn_ds_ls = predn_ds_ls, new_data_is_1L_chr = "Predicted")
            if (weeks_dbl[1] < 0) {
                outcomes_tb <- outcomes_tb %>% dplyr::mutate(AQoL6D_HU = dplyr::case_when(MeasurementWeek == 
                  paste0("Week", weeks_dbl[1]) ~ AQoL6D_HU, MeasurementWeek == 
                  paste0("Week", weeks_dbl[2]) ~ dplyr::lag(AQoL6D_HU)))
            }
            outcomes_tb <- outcomes_tb %>% youthu::add_qalys_to_ds(predn_ds_ls = predn_ds_ls, 
                include_predrs_1L_lgl = F, reshape_1L_lgl = F) %>% 
                dplyr::rename(AQoL6D = AQoL6D_HU, AQoL6D_change = AQoL6D_HU_change_dbl, 
                  AQoL6D_QALYs = qalys_dbl)
            predn_ds_ls$ds_ls$utl_var_nm_1L_chr <- "chu9d_utl"
            outcomes_tb <- outcomes_tb %>% youthu::add_qalys_to_ds(predn_ds_ls = predn_ds_ls, 
                include_predrs_1L_lgl = F, reshape_1L_lgl = F) %>% 
                dplyr::rename(CHU9D = chu9d_utl, CHU9D_change = chu9d_utl_change_dbl, 
                  CHU9D_QALYs = qalys_dbl)
            outcomes_tb <- outcomes_tb %>% dplyr::select(-c(duration_prd)) %>% 
                dplyr::mutate(AQoL6D = youthvars::youthvars_aqol6d_adol(AQoL6D), 
                  CHU9D = youthvars::youthvars_chu9d_adolaus(CHU9D))
        }
        outcomes_tb <- purrr::reduce(c("gad7", "phq9", "k10"), 
            .init = outcomes_tb, ~{
                youthu::add_change_in_ds_var(.x, id_var_nm_1L_chr = "UID", 
                  round_var_nm_1L_chr = "MeasurementWeek", round_bl_val_1L_chr = paste0("Week", 
                    min(weeks_dbl)), change_var_nm_1L_chr = paste0(.y, 
                    "_change"), var_nm_1L_chr = .y)
            })
        if (weeks_dbl[1] < 0) {
            outcomes_tb <- rbind(outcomes_tb %>% dplyr::filter(MeasurementWeek == 
                paste0("Week", weeks_dbl[1])) %>% add_treatment_status(three_levels_1L_lgl = T, 
                type_1L_int = 1L), outcomes_tb %>% dplyr::filter(MeasurementWeek == 
                paste0("Week", weeks_dbl[2])) %>% dplyr::mutate(stage_of_treatment = as.factor(as.integer(as.character(stage_of_treatment)))) %>% 
                add_treatment_status(three_levels_1L_lgl = T, 
                  type_1L_int = 2L)) %>% dplyr::arrange(UID)
        }
        else {
            outcomes_tb <- outcomes_tb %>% add_treatment_status(group_by_1L_chr = "MeasurementWeek", 
                three_levels_1L_lgl = T, type_1L_int = 2L)
        }
        outcomes_tb <- outcomes_tb %>% add_project_treatment_change(timepoint_bl_1L_chr = paste0("Week", 
            min(weeks_dbl)))
        outcomes_tb <- outcomes_tb %>% dplyr::select(UID, Date, 
            MeasurementWeek, platform, treatment_stage, stage_of_treatment, 
            treatment_status, treatment_change, dplyr::everything())
    }
    if (as_dyad_1L_lgl) {
        dictionary_r3 <- data_ls$outcomes@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% 
            names(outcomes_tb))
        dictionary_r3 <- ready4use::renew.ready4use_dictionary(dictionary_r3, 
            var_nm_chr = c("Date", "AQoL6D", "AQoL6D_change", 
                "AQoL6D_QALYs", "CHU9D", "CHU9D_change", "CHU9D_QALYs", 
                "treatment_status", "treatment_change"), var_ctg_chr = c("Temporal", 
                rep("Quality of life (Assessment of Quality of Life 6D)", 
                  3), rep("Quality of life (Child Health Utility 9D)", 
                  3), rep("Service", 2)), var_desc_chr = c("Date", 
                "Assessment of Quality of Life 6 Dimension health utility", 
                "Change in Assessment of Quality of Life 6 Dimension health utility", 
                "QALYs (derived from Assessment of Quality of Life 6 Dimension)", 
                "Child Health Utility 9 Dimension health utility", 
                "Change in Child Health Utility 9 Dimension health utility", 
                "QALYs (derived from Child Health Utility 9 Dimension)", 
                "Treatment status", "Change in treatment status"), 
            var_type_chr = c("POSIXct", rep("numeric", 6), rep("factor", 
                2))) %>% dplyr::arrange(var_nm_chr)
        dictionary_r3 <- ready4use::renew.ready4use_dictionary(dictionary_r3, 
            new_cases_r3 = dictionary_r3 %>% dplyr::filter(var_nm_chr %in% 
                c("gad7", "phq9", "k10")) %>% dplyr::mutate(var_nm_chr = paste0(var_nm_chr, 
                "_change"), var_desc_chr = paste0("Change in ", 
                var_desc_chr)))
        dictionary_r3 <- dplyr::mutate(dictionary_r3, var_type_chr = dplyr::case_when(var_nm_chr == 
            "stage_of_treatment" ~ "factor", var_nm_chr == "gender" ~ 
            "factor", var_nm_chr == "employment_status" ~ "factor", 
            var_nm_chr %in% paste0("gad7", c("", "_change")) ~ 
                "integer", var_nm_chr %in% paste0("k10", c("", 
                "_change")) ~ "integer", var_nm_chr %in% paste0("phq9", 
                c("", "_change")) ~ "integer", T ~ var_type_chr))
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = outcomes_tb, 
            dictionary_r3 = dictionary_r3)
        if (!identical(Y_Ready4useDyad, ready4use::Ready4useDyad())) {
            X_Ready4useDyad <- add_minutes(X_Ready4useDyad, Y_Ready4useDyad = Y_Ready4useDyad, 
                start_at_1L_int = start_at_1L_int)
        }
        outcomes_xx <- X_Ready4useDyad
    }
    else {
        outcomes_xx <- outcomes_tb
    }
    return(outcomes_xx)
}
#' Make project overview dataset
#' @description make_project_overview_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project overview dataset. The function returns Overview (a tibble).
#' @param raw_data_ls Raw data (a list)
#' @param demographics_tb Demographics (a tibble)
#' @return Overview (a tibble)
#' @rdname make_project_overview_ds
#' @export 
#' @importFrom dplyr inner_join
#' @keywords internal
make_project_overview_ds <- function (raw_data_ls, demographics_tb) 
{
    overview_tb <- demographics_tb %>% dplyr::inner_join(raw_data_ls$chats) %>% 
        dplyr::inner_join(raw_data_ls$engagement)
    return(overview_tb)
}
#' Make project parameters tibble
#' @description make_project_params_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project parameters tibble. The function returns Parameters (a tibble).

#' @return Parameters (a tibble)
#' @rdname make_project_params_tb
#' @export 
#' @importFrom tibble tribble tibble
#' @importFrom serious update_for_price_year
#' @importFrom stats setNames
#' @importFrom dplyr pull
#' @keywords internal
make_project_params_tb <- function () 
{
    params_tb <- tibble::tribble(~Parameter, ~Mean, ~SE, ~SD, 
        "K10ChangeHeadspaceLow", -0.8, 0.4/1.96, sqrt(round(27298 * 
            0.1, 0)) * 0.4/1.96, "K10ChangeHeadspaceModerate", 
        1.1, 0.2/1.96, sqrt(round(27298 * 0.2, 0)) * 0.2/1.96, 
        "K10ChangeHeadspaceHigh", 1.3, 0.1/1.96, sqrt(round(27298 * 
            0.3, 0)) * 0.1/1.96, "K10ChangeHeadspaceVeryHigh", 
        2, 0.1/1.96, sqrt(round(27298 * 0.4, 0)) * 0.1/1.96, 
        "K10ChangeSpecialistFemale", 3.5, 7.8/sqrt(385), 7.8, 
        "K10ChangeSpecialistMale", 2.9, 8.2/sqrt(233), 8.2, "K10ChangeSpecialistAll", 
        3.3, 8/sqrt(620), 8, "RTM_Q1", 0.8, 0.02, sqrt(round(215578/5, 
            0)) * 0.02, "RTM_Q2", -0.3, 0.02, sqrt(round(215578/5, 
            0)) * 0.02, "RTM_Q3", -1.1, 0.02, sqrt(round(215578/5, 
            0)) * 0.02, "RTM_Q4", -1.8, 0.02, sqrt(round(215578/5, 
            0)) * 0.02, "RTM_Q5", -2.9, 0.03, sqrt(round(215578/5, 
            0)) * 0.03, "EDOOSCost", 848, 0, 0, "HeadspaceOOSCost", 
        serious::update_for_price_year(tibble::tibble(FiscalYear = "FY 2020", 
            Cost = 230), price_indices_dbl = c(91.4, 93.3, 96, 
            100, 100 * 102.5514/100.7796) %>% stats::setNames(paste0("FY ", 
            2020:2024)), price_ref_1L_int = 5) %>% dplyr::pull(Cost) %>% 
            as.vector(), 0, 0, "SpecialistOOSCost", serious::update_for_price_year(tibble::tibble(FiscalYear = "FY 2020", 
            Cost = 439), price_indices_dbl = c(91.4, 93.3, 96, 
            100, 100 * 102.5514/100.7796) %>% stats::setNames(paste0("FY ", 
            2020:2024)), price_ref_1L_int = 5) %>% dplyr::pull(Cost) %>% 
            as.vector(), 0, 0)
    return(params_tb)
}
#' Make project recode lookup table
#' @description make_project_recode_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project recode lookup table. The function returns Recode (a ready4 submodule extension of lookup table).

#' @return Recode (a ready4 submodule extension of lookup table)
#' @rdname make_project_recode_lup
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @keywords internal
make_project_recode_lup <- function () 
{
    recode_lup_r3 <- ready4show::ready4show_correspondences() %>% 
        ready4show::renew.ready4show_correspondences(old_nms_chr = c("Employed casually in paid work", 
            "Self-employed", "Employed full-time in paid work", 
            "Employed part-time in paid work", "Casual work", 
            "Part-time work", "Full-time work", "Volunteering", 
            "Volunteer/unpaid work", "Not in the labour force (e.g. still at school, home duties)", 
            "Looking for part-time work", "Looking for casual work", 
            "Looking for full-time work", "Not working", "Aboriginal", 
            "Aboriginal And Torres Strait Islander", "Torres Strait Islander", 
            c("Psychosocial support", "Clinical care coordination / liaison", 
                "Structured psychological intervention", "Suicide prevention specific assistance")), 
            new_nms_chr = c(rep("Employed", 7), rep("Volunteer", 
                2), rep("Not in the workforce", 1), rep("Unemployed", 
                4), rep("Aboriginal and Torres Strait Islander", 
                3), c("Psychosocial", "Coordination", "Psychological", 
                "Suicide prevention")))
    return(recode_lup_r3)
}
#' Make project results
#' @description make_project_results() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project results. The function returns Sim results (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param inputs_ls Inputs (a list)
#' @param min_cell_size_1L_int Minimum cell size (an integer vector of length one), Default: 30
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param outcomes_chr Outcomes (a character vector), Default: character(0)
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @return Sim results (a list)
#' @rdname make_project_results
#' @export 
#' @importFrom dplyr filter
#' @keywords internal
make_project_results <- function (X_Ready4useDyad, inputs_ls, min_cell_size_1L_int = 30L, 
    modifiable_chr = character(0), outcomes_chr = character(0), 
    threshold_1L_dbl = 96000) 
{
    if (identical(outcomes_chr, character(0))) {
        outcomes_chr <- make_outcomes_vars(inputs_ls$Synthetic_r4, 
            Y_Ready4useDyad = renewSlot(X_Ready4useDyad, "ds_tb", 
                X_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
                  "Intervention")), Z_Ready4useDyad = renewSlot(X_Ready4useDyad, 
                "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
                  "Comparator")), exclude_chr = c("Adult", "Period", 
                "MeasurementWeek", "treatment_fraction", "treatment_measurement", 
                "treatment_start"), exclude_suffixes_chr = c("_change", 
                "_date", "_previous", "52_Weeks"), modifiable_chr = modifiable_chr, 
            numeric_only_1L_lgl = T)
    }
    full_combos_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("clinic_type", 
        "treatment_status_start", "Distress"), min_cell_size_1L_int = min_cell_size_1L_int, 
        outcomes_chr = outcomes_chr)
    clinic_stage_ls <- make_results_summary(X_Ready4useDyad, 
        group_by_chr = c("clinic_type", "treatment_status_start"), 
        min_cell_size_1L_int = min_cell_size_1L_int, outcomes_chr = outcomes_chr)
    stage_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("treatment_status_start"), 
        min_cell_size_1L_int = min_cell_size_1L_int, outcomes_chr = outcomes_chr)
    clinic_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("clinic_type"), 
        min_cell_size_1L_int = min_cell_size_1L_int, outcomes_chr = outcomes_chr)
    distress_ls <- make_results_summary(X_Ready4useDyad, group_by_chr = c("Distress"), 
        min_cell_size_1L_int = min_cell_size_1L_int, outcomes_chr = outcomes_chr)
    total_ls <- make_results_summary(X_Ready4useDyad, min_cell_size_1L_int = min_cell_size_1L_int, 
        outcomes_chr = outcomes_chr)
    sim_results_ls <- list(D_Ready4useDyad = X_Ready4useDyad, 
        clinic_ls = clinic_ls, clinic_stage_ls = clinic_stage_ls, 
        distress_ls = distress_ls, full_combos_ls = full_combos_ls, 
        stage_ls = stage_ls, total_ls = total_ls)
    return(sim_results_ls)
}
#' Make project results synthesis
#' @description make_project_results_synthesis() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project results synthesis. The function is called for its side effects and does not return a value.
#' @param inputs_ls Inputs (a list)
#' @param results_ls Results (a list)
#' @param modifiable_chr Modifiable (a character vector), Default: c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("D", "AB", "C")
#' @return X (A dataset and data dictionary pair.)
#' @rdname make_project_results_synthesis
#' @export 
#' @keywords internal
make_project_results_synthesis <- function (inputs_ls, results_ls, modifiable_chr = c("treatment_status", 
    "Minutes", "k10", "AQoL6D", "CHU9D"), type_1L_chr = c("D", 
    "AB", "C")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    X_Ready4useDyad <- make_results_synthesis(inputs_ls$Synthetic_r4, 
        results_ls = results_ls, add_severity_1L_lgl = T, exclude_chr = c("Adult", 
            "Period", "MeasurementWeek", "treatment_fraction", 
            "treatment_measurement", "treatment_start"), exclude_suffixes_chr = c("_change", 
            "_date", "_previous", "52_Weeks"), keep_chr = c("platform", 
            "clinic_state", "clinic_type", "Age", "gender", "employment_status"), 
        modifiable_chr = modifiable_chr, type_1L_chr = type_1L_chr)
    return(X_Ready4useDyad)
}
#' Make project service use dataset
#' @description make_project_service_use_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project service use dataset. The function is called for its side effects and does not return a value.
#' @param processed_ls Processed (a list)
#' @param data_extract_dtm Data extract (a date vector), Default: as.POSIXct("2024-10-25")
#' @return Y (A dataset and data dictionary pair.)
#' @rdname make_project_service_use_ds
#' @export 
#' @importFrom dplyr rename mutate case_when select filter full_join across left_join distinct bind_rows arrange everything pull group_by ungroup
#' @importFrom stringr str_sub
#' @importFrom lubridate ymd years
#' @importFrom serious add_temporal_vars make_temporal_vars add_tenure add_cumulatives add_episodes
#' @importFrom purrr map_chr
#' @importFrom rlang sym
#' @keywords internal
make_project_service_use_ds <- function (processed_ls, data_extract_dtm = as.POSIXct("2024-10-25")) 
{
    X_Ready4useDyad <- make_project_joiners_ds(processed_ls)
    Y_Ready4useDyad <- processed_ls$contacts
    data_tb <- Y_Ready4useDyad@ds_tb
    data_tb <- data_tb %>% dplyr::rename(Date = date_contacted)
    data_tb <- data_tb %>% dplyr::mutate(Date = Date %>% format() %>% 
        stringr::str_sub(end = 10) %>% lubridate::ymd())
    data_tb <- data_tb %>% dplyr::mutate(Date = dplyr::case_when(Date > 
        data_extract_dtm ~ Date - lubridate::years(1), T ~ Date))
    scenarios_chr <- get_unit_cost_detail(processed_ls$costs_unit@ds_tb, 
        what_1L_chr = "scenarios")
    cost_names_chr <- get_unit_cost_detail(processed_ls$costs_unit@ds_tb, 
        what_1L_chr = "names")
    data_tb <- add_cost_calculations(data_tb, inputs_ls = list(unit_costs_tb = processed_ls$costs_unit@ds_tb))
    data_tb <- data_tb %>% dplyr::select(-c(sign_up_date, onboarding_date))
    joiners_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(UID %in% 
        data_tb$UID) %>% add_activity()
    joiners_tb <- data_tb %>% dplyr::filter(F) %>% dplyr::full_join(joiners_tb) %>% 
        dplyr::mutate(dplyr::across(c("direct_mins", "indirect_mins", 
            "Minutes", cost_names_chr), ~0), role_type = "Enroll", 
            primary_mode = "Enroll", primary_participant = "Enroll", 
            primary_purpose = "Enroll")
    data_tb <- data_tb %>% dplyr::left_join(joiners_tb %>% dplyr::select(UID, 
        treatment_status) %>% dplyr::distinct()) %>% dplyr::mutate(SignedUp = 0, 
        Onboarded = 0, Activity = "Contact")
    data_tb <- dplyr::bind_rows(joiners_tb, data_tb) %>% dplyr::arrange(UID, 
        Date) %>% dplyr::select(intersect(names(X_Ready4useDyad@ds_tb), 
        names(data_tb)), dplyr::everything())
    data_tb <- data_tb %>% serious::add_temporal_vars()
    Y_Ready4useDyad@ds_tb <- data_tb
    Y_Ready4useDyad@dictionary_r3 <- Y_Ready4useDyad@dictionary_r3 %>% 
        dplyr::filter(var_nm_chr != "Date")
    Y_Ready4useDyad@dictionary_r3 <- Y_Ready4useDyad@dictionary_r3 %>% 
        renew.ready4use_dictionary(new_cases_r3 = X_Ready4useDyad@dictionary_r3 %>% 
            dplyr::filter(!var_nm_chr %in% Y_Ready4useDyad@dictionary_r3$var_nm_chr)) %>% 
        dplyr::filter(var_nm_chr %in% names(data_tb))
    Y_Ready4useDyad@dictionary_r3 <- Y_Ready4useDyad@dictionary_r3 %>% 
        renew.ready4use_dictionary(var_nm_chr = c("Minutes", 
            cost_names_chr, "treatment_stage", "Activity", serious::make_temporal_vars()), 
            var_ctg_chr = c("Contact", cost_names_chr, rep("Service", 
                2), rep("Temporal", length(serious::make_temporal_vars()))), 
            var_desc_chr = c("Total number of direct and indirect contact minutes", 
                paste0("Contact related costs", c("", paste0(" (", 
                  scenarios_chr[-1], ")"))), "Treatment stage", 
                "Type of service activity", serious::make_temporal_vars()), 
            var_type_chr = c(rep("numeric", length(c("Minutes", 
                cost_names_chr))), c("treatment_stage", "Activity", 
                serious::make_temporal_vars()) %>% purrr::map_chr(~(Y_Ready4useDyad@ds_tb %>% 
                dplyr::pull(!!rlang::sym(.x)) %>% class())[1])))
    Y_Ready4useDyad <- Y_Ready4useDyad %>% serious::add_tenure(date_var_1L_chr = "Date", 
        uid_var_1L_chr = "UID", unit_1L_chr = "year") %>% serious::add_cumulatives(metrics_chr = c("SignedUp", 
        "Onboarded", "direct_mins", "indirect_mins", "Minutes", 
        cost_names_chr), arrange_by_1L_chr = "Date", group_by_1L_chr = "UID")
    Y_Ready4useDyad@dictionary_r3 <- Y_Ready4useDyad@dictionary_r3 %>% 
        renew.ready4use_dictionary(var_nm_chr = "Tenure", var_ctg_chr = "Temporal", 
            var_desc_chr = "Length of service tenure in years", 
            var_type_chr = "numeric")
    Y_Ready4useDyad <- Y_Ready4useDyad %>% serious::add_episodes(separation_after_dbl = 3, 
        active_var_1L_chr = "Active", activity_var_1L_chr = "Activity", 
        exclude_chr = "Duration", fiscal_start_1L_int = 7L, metrics_chr = c("SignedUp", 
            "Onboarded", "direct_mins", "indirect_mins", "Minutes", 
            cost_names_chr), prefix_1L_chr = "Cumulative", separations_var_1L_chr = "Separations", 
        temporal_vars_chr = make_temporal_vars(), uid_1L_chr = "UID", 
        unit_1L_chr = "month")
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::mutate(role_type = dplyr::case_when(Activity == 
        "Separation" ~ "EpisodeEnd", T ~ role_type), primary_mode = dplyr::case_when(Activity == 
        "Separation" ~ "EpisodeEnd", T ~ primary_mode), primary_participant = dplyr::case_when(Activity == 
        "Separation" ~ "EpisodeEnd", T ~ primary_participant), 
        primary_purpose = dplyr::case_when(Activity == "Separation" ~ 
            "EpisodeEnd", T ~ primary_purpose))
    year_end_dtm <- (Y_Ready4useDyad@ds_tb %>% dplyr::filter(Cost > 
        0) %>% dplyr::pull(Date) %>% min()) + lubridate::years(1)
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::group_by(UID) %>% 
        dplyr::mutate(IncludedDays = as.numeric((year_end_dtm - 
            Date))) %>% dplyr::mutate(IncludedDays = dplyr::case_when(IncludedDays > 
        366 ~ 0, IncludedDays < 0 ~ 0, TRUE ~ IncludedDays)) %>% 
        dplyr::mutate(IncludedDays = dplyr::case_when(IncludedDays > 
            0 ~ max(IncludedDays), TRUE ~ IncludedDays)) %>% 
        dplyr::ungroup()
    Y_Ready4useDyad <- Y_Ready4useDyad %>% add_treatment_status(type_1L_int = 1)
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% 
        dplyr::select(UID, Date, IncludedDays, Tenure, treatment_stage, 
            treatment_status, dplyr::everything()))
    Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    return(Y_Ready4useDyad)
}
#' Make project sim summary
#' @description make_project_sim_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project sim summary. The function returns Summary (a tibble).
#' @param sim_results_ls Sim results (a list)
#' @param element_1L_chr Element (a character vector of length one), Default: 'Z'
#' @param groupings_chr Groupings (a character vector), Default: c("clinic_type", "treatment_status_start", "Distress")
#' @param order_1L_lgl Order (a logical vector of length one), Default: TRUE
#' @param convert_1L_lgl Convert (a logical vector of length one), Default: TRUE
#' @param platform_1L_chr Platform (a character vector of length one), Default: 'Intervention'
#' @param select_1L_chr Select (a character vector of length one), Default: c("both", "AQoL-6D", "CHU-9D")
#' @param type_1L_chr Type (a character vector of length one), Default: c("outcomes", "economic")
#' @param what_1L_chr What (a character vector of length one), Default: c("total", "clinic", "clinic_stage", "distress", "full_combos", 
#'    "stage")
#' @return Summary (a tibble)
#' @rdname make_project_sim_summary
#' @export 
#' @importFrom purrr pluck reduce
#' @importFrom dplyr mutate across case_when select everything group_by where ungroup rename distinct filter left_join arrange desc
#' @importFrom rlang sym
#' @importFrom stringr str_sub str_replace_all str_squish str_replace
#' @importFrom tidyr pivot_longer pivot_wider all_of
#' @importFrom tibble tibble
#' @importFrom scales dollar
#' @keywords internal
make_project_sim_summary <- function (sim_results_ls, element_1L_chr = "Z", groupings_chr = c("clinic_type", 
    "treatment_status_start", "Distress"), order_1L_lgl = TRUE, 
    convert_1L_lgl = TRUE, platform_1L_chr = "Intervention", 
    select_1L_chr = c("both", "AQoL-6D", "CHU-9D"), type_1L_chr = c("outcomes", 
        "economic"), what_1L_chr = c("total", "clinic", "clinic_stage", 
        "distress", "full_combos", "stage")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    pick_1L_chr <- paste0(what_1L_chr, "_ls")
    select_1L_chr <- match.arg(select_1L_chr)
    X_Ready4useDyad <- sim_results_ls %>% purrr::pluck(pick_1L_chr) %>% 
        purrr::pluck(element_1L_chr)
    groupings_chr <- intersect(groupings_chr, names(X_Ready4useDyad@ds_tb))
    if (!identical(groupings_chr, character(0))) {
        X_Ready4useDyad@ds_tb <- purrr::reduce(groupings_chr, 
            .init = X_Ready4useDyad@ds_tb %>% dplyr::mutate(Group = "") %>% 
                dplyr::mutate(dplyr::across(groupings_chr, ~dplyr::case_when(.x %in% 
                  c("Low", "Moderate", "High") ~ paste0(.x, " Distress"), 
                  .x == "VeryHigh" ~ "Very High Distress", TRUE ~ 
                    .x))), ~dplyr::mutate(.x, Group = paste0(Group, 
                !!rlang::sym(.y), " - "))) %>% dplyr::mutate(Group = stringr::str_sub(Group, 
            end = -4)) %>% dplyr::select(Group, dplyr::everything()) %>% 
            dplyr::group_by(Group)
    }
    if (!identical(groupings_chr, character(0))) {
        x_tb <- X_Ready4useDyad@ds_tb %>% dplyr::group_by(Group, 
            Data)
    }
    else {
        x_tb <- X_Ready4useDyad@ds_tb %>% dplyr::group_by(Data)
    }
    x_tb <- x_tb %>% dplyr::select(dplyr::where(is.numeric)) %>% 
        tidyr::pivot_longer(dplyr::where(is.numeric), names_to = "Parameter", 
            values_to = "Value") %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = Data, 
        values_from = Value)
    if (!identical(groupings_chr, character(0))) {
        x_tb <- x_tb %>% dplyr::select(Group, Parameter, Intervention, 
            Comparator, Difference) %>% dplyr::group_by(Group)
    }
    else {
        x_tb <- x_tb %>% dplyr::select(Parameter, Intervention, 
            Comparator, Difference)
    }
    x_tb <- x_tb %>% dplyr::rename(`:=`(!!rlang::sym(platform_1L_chr), 
        Intervention)) %>% dplyr::mutate(Parameter = Parameter %>% 
        stringr::str_replace_all("_YR1", "") %>% stringr::str_replace_all("QALYs_S1", 
        "QALYs (Utility sensitivity 1)") %>% stringr::str_replace_all("QALYs_S2", 
        "QALYs (Utility sensitivity 2)") %>% stringr::str_replace_all("Cost_S1", 
        "Cost (Cost sensitivity 1)")) %>% dplyr::distinct()
    if (type_1L_chr == "outcomes") {
        summary_tb <- x_tb %>% dplyr::filter(!startsWith(Parameter, 
            "CE_") & !startsWith(Parameter, "ICER_") & !startsWith(Parameter, 
            "Param") & !startsWith(Parameter, "PROB ")) %>% dplyr::mutate(Parameter = dplyr::case_when(startsWith(Parameter, 
            "Offset") ~ gsub("([A-Z]){1}", " \\1", Parameter) %>% 
            stringr::str_replace_all("E D", "Emergency Department ") %>% 
            stringr::str_squish() %>% stringr::str_replace("Offset ", 
            "Change in ") %>% stringr::str_replace_all("Count ", 
            "OOS - "), T ~ Parameter %>% stringr::str_replace_all("_start", 
            " at model entry") %>% stringr::str_replace_all("_", 
            " ") %>% stringr::str_replace_all("Minutes 12 Weeks", 
            "Contact minutes at 14 weeks") %>% stringr::str_replace_all("Minutes", 
            "Contact minutes at 1 year") %>% stringr::str_replace_all("12 Weeks", 
            "at last K10 change") %>% stringr::str_replace_all("treatment count", 
            "New clinic treatment episodes"))) %>% dplyr::rename(Outcome = Parameter) %>% 
            dplyr::filter(Outcome != "N")
    }
    if (type_1L_chr == "economic") {
        a_tb <- x_tb %>% dplyr::select(Parameter, Difference) %>% 
            dplyr::filter(startsWith(Parameter, "ICER_")) %>% 
            dplyr::rename(Scenario = Parameter, ICER = Difference) %>% 
            dplyr::mutate(Scenario = Scenario %>% stringr::str_replace_all("ICER_", 
                ""))
        b_tb <- x_tb %>% dplyr::select(Parameter, Difference) %>% 
            dplyr::filter(startsWith(Parameter, "PROB ")) %>% 
            dplyr::rename(Scenario = Parameter, `P (Cost-Effective)` = Difference) %>% 
            dplyr::mutate(`P (Cost-Effective)` = 100 * `P (Cost-Effective)`) %>% 
            dplyr::mutate(Scenario = Scenario %>% stringr::str_replace_all("PROB CE_", 
                ""))
        size_1L_int <- sim_results_ls$D_Ready4useDyad@ds_tb$UID %>% 
            unique() %>% length()
        c_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
            "Difference") %>% dplyr::select(N) %>% dplyr::mutate(Share = 100 * 
            (N/size_1L_int)) %>% dplyr::select(-N)
        if (length(names(c_tb)) == 1) {
            c_tb <- tibble::tibble(Scenario = a_tb$Scenario, 
                Share = c_tb$Share)
        }
        summary_tb <- dplyr::left_join(a_tb, b_tb) %>% dplyr::left_join(c_tb) %>% 
            dplyr::mutate(Scenario = Scenario %>% stringr::str_replace_all("AQoL6D", 
                "AQoL-6D QALYS") %>% stringr::str_replace_all("CHU9D", 
                "CHU-9D QALYS") %>% stringr::str_replace_all("_S10", 
                " - Cost 1") %>% stringr::str_replace_all("_S01", 
                " - Utility 1") %>% stringr::str_replace_all("_S02", 
                " - Utility 2") %>% stringr::str_replace_all("_S11", 
                " - Cost 1 & Utility 1") %>% stringr::str_replace_all("_S12", 
                " - Cost 1 & Utility 2"))
        if (order_1L_lgl) {
            summary_tb <- summary_tb %>% dplyr::arrange(Scenario) %>% 
                dplyr::group_by(Scenario) %>% dplyr::arrange(Scenario, 
                dplyr::desc(`P (Cost-Effective)`)) %>% dplyr::select(Scenario, 
                dplyr::everything()) %>% dplyr::ungroup()
            if ("Group" %in% names(summary_tb)) {
                summary_tb <- summary_tb %>% dplyr::select(Scenario, 
                  Group, Share, dplyr::everything())
            }
        }
        if ((summary_tb$Share %>% unique() %>% length()) == 1 & 
            unique(summary_tb$Share)[1] == 100) {
            summary_tb <- summary_tb %>% dplyr::select(-Share)
        }
        if (convert_1L_lgl) {
            summary_tb <- summary_tb %>% dplyr::mutate(dplyr::across(tidyr::all_of(intersect(c("Share"), 
                names(summary_tb))), ~round(.x, 2) %>% paste0("%")), 
                ICER = scales::dollar(ICER))
            if ("P (Cost-Effective)" %in% names(summary_tb)) {
                summary_tb$`P (Cost-Effective)` <- summary_tb$`P (Cost-Effective)` %>% 
                  round(2) %>% paste0("%")
            }
        }
        if (select_1L_chr != "both") {
            summary_tb <- summary_tb %>% dplyr::filter(startsWith(Scenario, 
                select_1L_chr)) %>% dplyr::mutate(Scenario = Scenario %>% 
                stringr::str_replace_all(paste0(select_1L_chr, 
                  " QALYS - "), "Sensitivity - ") %>% stringr::str_replace_all(paste0(select_1L_chr, 
                " QALYS"), "Base Case"))
        }
    }
    return(summary_tb)
}
#' Make project sunk tibble
#' @description make_project_sunk_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project sunk tibble. The function returns Sunk (a tibble).
#' @param global_1L_dbl Global (a double vector of length one), Default: numeric(0)
#' @param content_dbl Content (a double vector), Default: 0.9
#' @param development_dbl Development (a double vector), Default: 0.75
#' @param implementation_dbl Implementation (a double vector), Default: 0.25
#' @param infrastructure_dbl Infrastructure (a double vector), Default: 0
#' @param management_dbl Management (a double vector), Default: 0
#' @param marketing_dbl Marketing (a double vector), Default: 0
#' @param onboarding_dbl Onboarding (a double vector), Default: 0.1
#' @param reporting_dbl Reporting (a double vector), Default: 0.9
#' @return Sunk (a tibble)
#' @rdname make_project_sunk_tb
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_project_sunk_tb <- function (global_1L_dbl = numeric(0), content_dbl = 0.9, development_dbl = 0.75, 
    implementation_dbl = 0.25, infrastructure_dbl = 0, management_dbl = 0, 
    marketing_dbl = 0, onboarding_dbl = 0.1, reporting_dbl = 0.9) 
{
    if (identical(global_1L_dbl, numeric(0))) {
        sunk_dbl <- c(development_dbl, infrastructure_dbl, content_dbl, 
            reporting_dbl, marketing_dbl, management_dbl, implementation_dbl, 
            onboarding_dbl)
    }
    else {
        sunk_dbl <- global_1L_dbl
    }
    sunk_tb <- tibble::tibble(Type = c(rep("Fixed", 6), rep("Variable", 
        2)), Item = c("Platform development and maintenance", 
        "Platform infrastructure", "Content development", "Data collection, analysis & reporting", 
        "Marketing & communications", "Project management and leadership", 
        "Service implementation support", "Youth onboarding support"), 
        Sunk = sunk_dbl, Kept = 1 - Sunk)
    return(sunk_tb)
}
#' Make project test dataset
#' @description make_project_test_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project test dataset. The function is called for its side effects and does not return a value.
#' @param model_data_ls Model data (a list)
#' @param update_qalys_fn Update Quality Adjusted Life Years (a function), Default: identity
#' @return X (A dataset and data dictionary pair.)
#' @rdname make_project_test_ds
#' @export 
#' @importFrom dplyr group_by arrange mutate lag ungroup filter across
#' @importFrom serious add_new_uid
#' @importFrom tidyselect all_of
#' @keywords internal
make_project_test_ds <- function (model_data_ls, update_qalys_fn = identity) 
{
    X_Ready4useDyad <- renewSlot(model_data_ls$unimputed_ls$OutcomesAllWide_r4, 
        "ds_tb", model_data_ls$unimputed_ls$OutcomesAllWide_r4@ds_tb %>% 
            dplyr::group_by(UID) %>% dplyr::arrange(Period) %>% 
            dplyr::mutate(Minutes = dplyr::lag(Minutes_change, 
                default = 0) + Minutes_change) %>% dplyr::ungroup() %>% 
            dplyr::arrange(UID, Period) %>% dplyr::filter(as.character(Period) == 
            "0 to 12 Weeks") %>% serious::add_new_uid("UID", 
            recode_1L_lgl = T, recode_pfx_1L_chr = "") %>% dplyr::mutate(gender = as.factor(gender), 
            dplyr::across(tidyselect::all_of(c("Age", "gad7", 
                "k10", "phq9", "AQoL6D", "CHU9D")), ~as.double(.x))) %>% 
            dplyr::mutate(dplyr::across(c("AQoL6D_QALYs", "CHU9D_QALYs"), 
                ~update_qalys_fn(.x))) %>% dplyr::group_by(Period) %>% 
            dplyr::ungroup() %>% dplyr::arrange(UID) %>% dplyr::mutate(UID = as.character(as.numeric(UID)))) %>% 
        update_previous(modifiable_chr = c("treatment_status", 
            "Minutes", "k10", "AQoL6D", "CHU9D", "AQoL6D_QALYs", 
            "CHU9D_QALYs")) %>% update_previous(modifiable_chr = c("treatment_status", 
        "Minutes", "k10", "AQoL6D", "CHU9D", "AQoL6D_QALYs", 
        "CHU9D_QALYs"), pattern_1L_chr = "{col}_start")
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(dplyr::across(c("Minutes_previous", "Minutes_start"), 
            ~0)))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(dplyr::across(c("AQoL6D_QALYs", "CHU9D_QALYs"), 
            ~update_qalys_fn(.x))))
    return(X_Ready4useDyad)
}
#' Make project time series dataset
#' @description make_project_ts_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project time series dataset. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param processed_ls Processed (a list)
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param key_vars_chr Key variables (a character vector), Default: make_project_keys(type_1L_chr = "ts")
#' @return Y (A dataset and data dictionary pair.)
#' @rdname make_project_ts_ds
#' @export 
#' @importFrom serious transform_to_temporal add_temporal_vars
#' @importFrom dplyr mutate
#' @importFrom purrr map_int
#' @importFrom tsibble fill_gaps
#' @keywords internal
make_project_ts_ds <- function (X_Ready4useDyad, processed_ls, index_1L_chr = "Date", 
    key_vars_chr = make_project_keys(type_1L_chr = "ts")) 
{
    Y_Ready4useDyad <- serious::transform_to_temporal(renewSlot(X_Ready4useDyad, 
        "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(Tenure = purrr::map_int(Tenure, 
            ~max(ceiling(.), 1)))), index_1L_chr = index_1L_chr, 
        key_vars_chr = make_project_keys(type_1L_chr = "ts"), 
        metrics_chr = c("SignedUp", "Onboarded", make_project_metrics(processed_ls$costs_unit@ds_tb)))
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% 
        tsibble::fill_gaps(SignedUp = 0, Onboarded = 0, Active = 0, 
            Episodes = 0, Separations = 0, direct_mins = 0, indirect_mins = 0, 
            Minutes = 0, Cost = 0, Cost_S1 = 0) %>% serious::add_temporal_vars())
    return(Y_Ready4useDyad)
}
#' Make project treatment modelling dataset
#' @description make_project_tx_mdlng_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project treatment modelling dataset. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @param Z_Ready4useDyad PARAM_DESCRIPTION
#' @param periods_chr Periods (a character vector), Default: c(x = "-2 to 0 Weeks", y = "0 to 12 Weeks", z = "12 to 24 Weeks")
#' @param periods_ls Periods (a list), Default: list(x = c(-2, 0), y = c(0, 12), z = c(12, 24))
#' @param treatment_vals_chr Treatment values (a character vector), Default: c("Waitlist", "Treatment", "Discharged")
#' @param treatment_vars_chr Treatment variables (a character vector), Default: c("treatment_status", "treatment_status_t2")
#' @return A (A dataset and data dictionary pair.)
#' @rdname make_project_tx_mdlng_ds
#' @export 
#' @importFrom dplyr select ends_with mutate everything arrange case_when lag group_by ungroup
#' @importFrom rlang sym
#' @keywords internal
make_project_tx_mdlng_ds <- function (X_Ready4useDyad, Y_Ready4useDyad, Z_Ready4useDyad, 
    periods_chr = c(x = "-2 to 0 Weeks", y = "0 to 12 Weeks", 
        z = "12 to 24 Weeks"), periods_ls = list(x = c(-2, 0), 
        y = c(0, 12), z = c(12, 24)), treatment_vals_chr = c("Waitlist", 
        "Treatment", "Discharged"), treatment_vars_chr = c("treatment_status", 
        "treatment_status_t2")) 
{
    A_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", rbind(X_Ready4useDyad@ds_tb %>% 
        dplyr::select(-dplyr::ends_with("_Weeks")) %>% dplyr::mutate(Period = periods_chr[["x"]], 
        Duration = abs(periods_ls$x[1] - periods_ls$x[2]), Start = periods_ls$x[1], 
        FirstTwoWeeks = T), Y_Ready4useDyad@ds_tb %>% dplyr::select(-dplyr::ends_with("_Weeks")) %>% 
        dplyr::mutate(Period = periods_chr[["y"]], Duration = abs(periods_ls$y[1] - 
            periods_ls$y[2]), Start = periods_ls$y[1], FirstTwoWeeks = F), 
        Z_Ready4useDyad@ds_tb %>% dplyr::select(-dplyr::ends_with("_Weeks")) %>% 
            dplyr::mutate(Period = periods_chr[["z"]], Duration = abs(periods_ls$z[1] - 
                periods_ls$z[2]), Start = periods_ls$z[1], FirstTwoWeeks = F)) %>% 
        dplyr::mutate(Period = as.factor(Period)) %>% dplyr::select(UID, 
        Date, Period, Duration, Start, FirstTwoWeeks, dplyr::everything()) %>% 
        dplyr::arrange(UID, Date) %>% dplyr::mutate(Episodes_t2 = dplyr::case_when((!!rlang::sym(treatment_vars_chr[1]) != 
        treatment_vals_chr[2] & !!rlang::sym(treatment_vars_chr[2]) == 
        treatment_vals_chr[2]) ~ 1, (!!rlang::sym(treatment_vars_chr[1]) == 
        treatment_vals_chr[2] & Period == periods_chr[["x"]]) ~ 
        1, (!!rlang::sym(treatment_vars_chr[1]) == treatment_vals_chr[1] & 
        !!rlang::sym(treatment_vars_chr[2]) == treatment_vals_chr[3]) ~ 
        1, T ~ 0)) %>% dplyr::mutate(Episodes = dplyr::case_when(Period != 
        periods_chr[["x"]] ~ dplyr::lag(Episodes_t2), (!!rlang::sym(treatment_vars_chr[1]) == 
        treatment_vals_chr[2] & Period == periods_chr[["x"]]) ~ 
        1, T ~ 0)) %>% dplyr::group_by(UID) %>% dplyr::mutate(Episodes_t2 = cumsum(Episodes)) %>% 
        dplyr::ungroup() %>% dplyr::mutate(Adult = dplyr::case_when(Age < 
        18 ~ FALSE, Age >= 1 ~ TRUE, T ~ NA_real_)))
    return(A_Ready4useDyad)
}
#' Make project treatment models
#' @description make_project_tx_mdls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project treatment models. The function returns Treatment models (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param append_to_ls Append to (a list), Default: list()
#' @param predictors_ls Predictors (a list), Default: NULL
#' @param treatment_vars_chr Treatment variables (a character vector), Default: c("treatment_status", "treatment_status_t2")
#' @param what_1L_chr What (a character vector of length one), Default: c("All", "Waitlist", "Treatment", "Discharged")
#' @return Treatment models (a list)
#' @rdname make_project_tx_mdls
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
make_project_tx_mdls <- function (X_Ready4useDyad, append_to_ls = list(), predictors_ls = NULL, 
    treatment_vars_chr = c("treatment_status", "treatment_status_t2"), 
    what_1L_chr = c("All", "Waitlist", "Treatment", "Discharged")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "All") {
        tx_mdls_ls <- make_project_tx_mdls(X_Ready4useDyad, predictors_ls = predictors_ls, 
            treatment_vars_chr = treatment_vars_chr, what_1L_chr = "Waitlist") %>% 
            make_project_tx_mdls(X_Ready4useDyad = X_Ready4useDyad, 
                predictors_ls = predictors_ls, treatment_vars_chr = treatment_vars_chr, 
                what_1L_chr = "Treatment") %>% make_project_tx_mdls(X_Ready4useDyad = X_Ready4useDyad, 
            predictors_ls = predictors_ls, treatment_vars_chr = treatment_vars_chr, 
            what_1L_chr = "Discharged")
    }
    else {
        data_tb <- X_Ready4useDyad@ds_tb %>% transform_tx_mdlng_ds(treatment_vars_chr = treatment_vars_chr, 
            what_1L_chr = what_1L_chr)
        if (is.null(predictors_ls)) {
            predictors_ls <- list(c("Adult", "Episodes", "FirstTwoWeeks", 
                "clinic_state:clinic_type"), c("Adult", "Episodes", 
                "FirstTwoWeeks", "clinic_state"), c("Adult", 
                "Episodes", "FirstTwoWeeks", "clinic_type"), 
                c("Adult", "FirstTwoWeeks", "clinic_state:clinic_type"), 
                c("FirstTwoWeeks", "clinic_state:clinic_type"), 
                c("Adult", "clinic_state:clinic_type"), c("Adult", 
                  "FirstTwoWeeks"), c("clinic_state:clinic_type", 
                  "Period"), "Period", "FirstTwoWeeks", c("Adult", 
                  "Period", "clinic_state:clinic_type"))
        }
        tx_mdls_ls <- append(append_to_ls, list(purrr::map(predictors_ls, 
            ~make_parsnip_mdl(data_tb = data_tb, x_chr = .x, 
                y_1L_chr = treatment_vars_chr[2])) %>% stats::setNames(paste0("MNL_", 
            1:length(predictors_ls), "_mdl"))) %>% stats::setNames(paste0(what_1L_chr, 
            "_ls")))
    }
    return(tx_mdls_ls)
}
#' Make regression report
#' @description make_regression_report() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make regression report. The function returns Report (an output object of multiple potential types).
#' @param regressions_ls Regressions (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param colours_chr Colours (a character vector), Default: ready4use::get_colour_codes(9, style_1L_chr = "monash_2", type_1L_chr = "unicol")[c(9, 
#'    1, 5)]
#' @param digits_1L_int Digits (an integer vector of length one), Default: integer(0)
#' @param drop_chr Drop (a character vector), Default: character(0)
#' @param exclude_int Exclude (an integer vector), Default: integer(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param part_1L_int Part (an integer vector of length one), Default: integer(0)
#' @param report_1L_chr Report (a character vector of length one), Default: c("all", "main", "check", "compare", "confusion", "estimates", 
#'    "test")
#' @param rank_1L_lgl Rank (a logical vector of length one), Default: TRUE
#' @param residual_1L_chr Residual (a character vector of length one), Default: 'normal'
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "tests", "models")
#' @param var_1L_chr Variable (a character vector of length one), Default: character(0)
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @return Report (an output object of multiple potential types)
#' @rdname make_regression_report
#' @export 
#' @importFrom ready4use get_colour_codes Ready4useDyad
#' @importFrom purrr map
#' @importFrom stats setNames
#' @importFrom performance check_model compare_performance test_performance
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename select mutate across where
#' @importFrom tidyselect any_of
#' @importFrom stringr str_remove
#' @importFrom broom tidy
#' @keywords internal
make_regression_report <- function (regressions_ls, what_1L_chr, colours_chr = ready4use::get_colour_codes(9, 
    style_1L_chr = "monash_2", type_1L_chr = "unicol")[c(9, 1, 
    5)], digits_1L_int = integer(0), drop_chr = character(0), 
    exclude_int = integer(0), model_1L_int = integer(0), part_1L_int = integer(0), 
    report_1L_chr = c("all", "main", "check", "compare", "confusion", 
        "estimates", "test"), rank_1L_lgl = TRUE, residual_1L_chr = "normal", 
    type_1L_chr = c("candidates", "tests", "models"), var_1L_chr = character(0), 
    X_Ready4useDyad = ready4use::Ready4useDyad()) 
{
    report_1L_chr <- match.arg(report_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (report_1L_chr %in% c("all", "main")) {
        if (report_1L_chr == "all") {
            use_int <- 1:5
        }
        else {
            use_int <- c(1:2, 4:5)
        }
        report_xx <- purrr::map(c("check", "compare", "confusion", 
            "estimates", "test")[use_int], ~make_regression_report(regressions_ls, 
            colours_chr = colours_chr, exclude_int = exclude_int, 
            model_1L_int = model_1L_int, part_1L_int = part_1L_int, 
            report_1L_chr = .x, rank_1L_lgl = rank_1L_lgl, type_1L_chr = type_1L_chr, 
            what_1L_chr = what_1L_chr, var_1L_chr = var_1L_chr, 
            X_Ready4useDyad = X_Ready4useDyad)) %>% stats::setNames(c("check_plt", 
            "compare_df", "confusion_ls", "estimates_df", "test_df")[use_int])
    }
    else {
        if (report_1L_chr == "check") {
            report_xx <- performance::check_model(get_regression(regressions_ls, 
                model_1L_int = model_1L_int, part_1L_int = part_1L_int, 
                type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr), 
                colors = colours_chr, residual_type = residual_1L_chr)
        }
        if (report_1L_chr == "compare") {
            if (!identical(part_1L_int, integer(0)) && part_1L_int == 
                1) {
                report_xx <- NULL
            }
            else {
                report_xx <- performance::compare_performance(get_regression(regressions_ls, 
                  part_1L_int = part_1L_int, type_1L_chr = type_1L_chr, 
                  what_1L_chr = what_1L_chr), rank = rank_1L_lgl) %>% 
                  tibble::as_tibble()
                if ("R2_Nagelkerke" %in% names(report_xx)) {
                  report_xx <- report_xx %>% dplyr::rename(`R2 Nagelkerke` = R2_Nagelkerke)
                }
                report_xx <- report_xx %>% dplyr::rename(`Performance Score` = Performance_Score)
                if (!identical(drop_chr, character(0))) {
                  report_xx <- report_xx %>% dplyr::select(-tidyselect::any_of(drop_chr))
                }
                if (!identical(digits_1L_int, integer(0))) {
                  report_xx <- report_xx %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                    ~round(.x, digits = digits_1L_int)))
                }
            }
        }
        if (report_1L_chr == "confusion") {
            if (what_1L_chr %in% c("Tx_Waitlist", "Tx_Treatment", 
                "Tx_Discharged")) {
                tfmn_fn <- transform_tx_factor
                tfmn_args_ls <- list(treatment_vars_chr = c("treatment_status", 
                  "treatment_status_t2"), what_1L_chr = stringr::str_remove(what_1L_chr, 
                  "Tx_"))
            }
            else {
                tfmn_fn <- identity
                tfmn_args_ls <- NULL
            }
            report_xx <- make_confusion_ls(regressions_ls, X_Ready4useDyad = X_Ready4useDyad, 
                model_1L_int = model_1L_int, part_1L_int = part_1L_int, 
                tfmn_fn = tfmn_fn, type_1L_chr = type_1L_chr, 
                tfmn_args_ls = tfmn_args_ls, what_1L_chr = what_1L_chr, 
                var_1L_chr = var_1L_chr)
        }
        if (report_1L_chr == "estimates") {
            report_xx <- broom::tidy(get_regression(regressions_ls, 
                model_1L_int = model_1L_int, part_1L_int = part_1L_int, 
                type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr))
        }
        if (report_1L_chr == "test") {
            model_ls <- get_regression(regressions_ls, part_1L_int = part_1L_int, 
                type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)
            if (!identical(part_1L_int, integer(0)) && part_1L_int == 
                1) {
                report_xx <- NULL
            }
            else {
                report_xx <- performance::test_performance(append(model_ls[model_1L_int], 
                  model_ls[-c(model_1L_int, exclude_int)]))
            }
        }
    }
    return(report_xx)
}
#' Make regressions list
#' @description make_regressions_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make regressions list. The function returns Regressions (a list).

#' @return Regressions (a list)
#' @rdname make_regressions_ls
#' @export 
#' @keywords internal
make_regressions_ls <- function () 
{
    assessments_ls <- candidates_ls <- tests_ls <- list(AQoL6D_ls = list(), 
        CHU9D_ls = list(), k10_ls = list(), Minutes_ls = list(), 
        Treatments_ls = list(Waitlist_ls = list(), Treatment_ls = list(), 
            Discharged_ls = list()))
    models_ls <- list(AQoL6D_mdl = NULL, CHU9D_mdl = NULL, k10_mdl = NULL, 
        Minutes_mdl = NULL, Treatments_ls = list(Waitlist_mdl = NULL, 
            Treatment_mdl = NULL, Discharged_mdl = NULL))
    regressions_ls <- list(candidates_ls = candidates_ls, assessments_ls = assessments_ls, 
        models_ls = models_ls, tests_ls = tests_ls)
    return(regressions_ls)
}
#' Make report data
#' @description make_report_data() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make report data. The function returns Data (an output object of multiple potential types).
#' @param model_data_ls Model data (a list), Default: NULL
#' @param date_end_dtm Date end (a date vector), Default: as.POSIXct("2023-07-01")
#' @param date_start_dtm Date start (a date vector), Default: as.POSIXct("2023-06-01")
#' @param period_dtm Period (a date vector), Default: lubridate::years(1)
#' @param platform_1L_chr Platform (a character vector of length one), Default: 'Intervention'
#' @param processed_ls Processed (a list), Default: NULL
#' @param regressions_ls Regressions (a list), Default: NULL
#' @param sim_results_ls Sim results (a list), Default: NULL
#' @param timepoint_1L_chr Timepoint (a character vector of length one), Default: character(0)
#' @param transformations_chr Transformations (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'full_combos'
#' @param ungroup_1L_lgl Ungroup (a logical vector of length one), Default: FALSE
#' @param weeks_int Weeks (an integer vector), Default: integer(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("descriptives", "composite", "costadj", "costitem", "costsum", 
#'    "costunit", "minutes", "mnl-wait", "mnl-tx", "mnl-disc", 
#'    "outcomes", "outcomeslong", "paramscost", "paramsk10", "resultsaqol", 
#'    "resultschu", "resultsoutcomes", "resultseconomic", "serviceuse", 
#'    "serviceusecost")
#' @return Data (an output object of multiple potential types)
#' @rdname make_report_data
#' @export 
#' @importFrom lubridate years weeks
#' @importFrom dplyr bind_rows filter select mutate across where group_by summarise first arrange rename everything inner_join case_when n ungroup lag left_join
#' @importFrom tidyselect all_of any_of
#' @importFrom scales dollar
#' @importFrom tidyr pivot_wider
#' @importFrom youthvars YouthvarsProfile
#' @importFrom stringr str_replace_all
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @keywords internal
make_report_data <- function (model_data_ls = NULL, date_end_dtm = as.POSIXct("2023-07-01"), 
    date_start_dtm = as.POSIXct("2023-06-01"), period_dtm = lubridate::years(1), 
    platform_1L_chr = "Intervention", processed_ls = NULL, regressions_ls = NULL, 
    sim_results_ls = NULL, timepoint_1L_chr = character(0), transformations_chr = character(0), 
    type_1L_chr = "full_combos", ungroup_1L_lgl = FALSE, weeks_int = integer(0), 
    what_1L_chr = c("descriptives", "composite", "costadj", "costitem", 
        "costsum", "costunit", "minutes", "mnl-wait", "mnl-tx", 
        "mnl-disc", "outcomes", "outcomeslong", "paramscost", 
        "paramsk10", "resultsaqol", "resultschu", "resultsoutcomes", 
        "resultseconomic", "serviceuse", "serviceusecost")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "composite") {
        OUTCOMES <- make_report_data(model_data_ls = model_data_ls, 
            platform_1L_chr = platform_1L_chr, what_1L_chr = "outcomes")
        vars_chr <- c("UID", "platform", "clinic_type", "clinic_state", 
            "treatment_status", "Age", "gender", "employment_status")
        data_xx <- renewSlot(OUTCOMES, "ds_tb", dplyr::bind_rows(OUTCOMES@ds_tb %>% 
            dplyr::filter(MeasurementWeek == "Week0") %>% dplyr::select(tidyselect::all_of(vars_chr)) %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.factor), 
                ~as.character(.x)), Dataset = "Outcomes"), processed_ls$contacts@ds_tb %>% 
            add_treatment_status(three_levels_1L_lgl = T, type_1L_int = 1) %>% 
            dplyr::group_by(UID) %>% dplyr::summarise(dplyr::across(tidyselect::all_of(setdiff(vars_chr, 
            "UID")), ~dplyr::first(.x))) %>% dplyr::mutate(Dataset = "Contacts"), 
            processed_ls$overview@ds_tb %>% add_treatment_status(three_levels_1L_lgl = T, 
                type_1L_int = 1) %>% dplyr::select(tidyselect::all_of(vars_chr)) %>% 
                dplyr::mutate(Dataset = "Complete")))
    }
    if (what_1L_chr == "costadj") {
        data_xx <- processed_ls$costs_adjusted@ds_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~scales::dollar(.x))) %>% dplyr::select(Scenario, 
            Type, Item, `FY 2024`) %>% dplyr::arrange(Type, Item) %>% 
            tidyr::pivot_wider(names_from = "Scenario", values_from = "FY 2024") %>% 
            dplyr::rename(Adjusted = Base, Unadjusted = S1)
    }
    if (what_1L_chr == "costitem") {
        data_xx <- processed_ls$costs_constant@ds_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~scales::dollar(.x))) %>% dplyr::select(-c(Description, 
            Category)) %>% dplyr::select(Type, dplyr::everything()) %>% 
            dplyr::arrange(Type, Item)
    }
    if (what_1L_chr == "costsum") {
        data_tb <- processed_ls$costs_constant@ds_tb %>% dplyr::select(-c(Description, 
            Category)) %>% dplyr::select(Type, dplyr::everything()) %>% 
            dplyr::arrange(Type, Item) %>% dplyr::group_by(Type) %>% 
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
                ~sum(.x)))
        data_xx <- data_tb %>% rbind(data_tb %>% dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
            ~sum(.x))) %>% dplyr::mutate(Type = "Total")) %>% 
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
                ~scales::dollar(.x)))
    }
    if (what_1L_chr == "costunit") {
        data_xx <- processed_ls$costs_unit@ds_tb %>% dplyr::mutate(Quantity = formatC(Quantity, 
            big.mark = ",", format = "fg"), dplyr::across(c("TotalCost", 
            "UnitCost"), ~scales::dollar(.x))) %>% dplyr::rename(`Total Cost` = TotalCost, 
            `Unit Cost` = UnitCost)
    }
    if (what_1L_chr == "descriptives") {
        data_xx <- youthvars::YouthvarsProfile(a_Ready4useDyad = model_data_ls$unimputed_ls$Outcomes0To12Wide_r4)
    }
    if (what_1L_chr == "minutes") {
        data_xx <- renewSlot(model_data_ls$unimputed_ls$Outcomes0To12Wide_r4, 
            "ds_tb", model_data_ls$unimputed_ls$Outcomes0To12Wide_r4@ds_tb %>% 
                dplyr::inner_join(model_data_ls$imputed_ls$MinutesLong_r4@ds_tb %>% 
                  dplyr::filter(MeasurementWeek == "Week14") %>% 
                  dplyr::select(UID, Minutes, direct_mins, indirect_mins)) %>% 
                dplyr::mutate(`Contact Minutes` = dplyr::case_when(Minutes == 
                  0 ~ FALSE, is.na(Minutes) ~ NA, TRUE ~ TRUE)))
    }
    if (what_1L_chr %in% c("mnl-wait", "mnl-tx", "mnl-disc")) {
        data_xx <- get_regression(regressions_ls, report_1L_chr = "estimates", 
            type_1L_chr = "assessments", what_1L_chr = ifelse(what_1L_chr == 
                "mnl-wait", "Tx_Waitlist", ifelse(what_1L_chr == 
                "mnl-tx", "Tx_Treatment", "Tx_Discharged"))) %>% 
            dplyr::rename(Level = y.level, Characteristic = term, 
                Beta = estimate, SE = std.error, `p-value` = p.value) %>% 
            dplyr::select(-statistic)
    }
    if (what_1L_chr == "outcomes") {
        data_xx <- model_data_ls$unimputed_ls$Outcomes0To12Long_r4
        if ("age" %in% transformations_chr) {
            data_xx <- renewSlot(data_xx, "ds_tb", data_xx@ds_tb %>% 
                dplyr::mutate(Age = dplyr::case_when(Age < 18 ~ 
                  "Under 18", Age >= 18 ~ "18 or over")))
        }
        if ("clinics" %in% transformations_chr) {
            data_xx <- renewSlot(data_xx, "ds_tb", data_xx@ds_tb %>% 
                dplyr::mutate(clinic_state = dplyr::case_when(clinic_state == 
                  "VIC" ~ "Victoria", T ~ "Other"), platform = dplyr::case_when(platform == 
                  "over_15" ~ "Platform 15+", T ~ "Platform < 15")))
        }
    }
    if (what_1L_chr == "outcomeslong") {
        data_xx <- renewSlot(model_data_ls$unimputed_ls$OutcomesAllLong_r4, 
            "ds_tb", model_data_ls$unimputed_ls$OutcomesAllLong_r4@ds_tb %>% 
                dplyr::group_by(UID) %>% dplyr::filter(dplyr::n() == 
                3) %>% dplyr::ungroup())
    }
    if (what_1L_chr == "paramscost") {
        data_xx <- make_project_params_tb() %>% dplyr::filter(!startsWith(Parameter, 
            "K10") & !startsWith(Parameter, "RTM")) %>% dplyr::mutate(Parameter = gsub("([A-Z]){1}", 
            " \\1", Parameter) %>% stringr::str_replace_all("E D ", 
            "Emergency Department ") %>% stringr::str_replace_all("O O S ", 
            "Occasion of Service ")) %>% dplyr::mutate(Parameter = dplyr::case_when(endsWith(Parameter, 
            "Low") | endsWith(Parameter, "Moderate") | endsWith(Parameter, 
            "High") ~ paste0(Parameter, "Baseline Distress"), 
            T ~ Parameter))
    }
    if (what_1L_chr == "paramsk10") {
        data_xx <- make_project_params_tb() %>% dplyr::filter(startsWith(Parameter, 
            "K10") | startsWith(Parameter, "RTM")) %>% dplyr::mutate(Parameter = gsub("([A-Z]){1}", 
            " \\1", Parameter) %>% stringr::str_replace_all("Change", 
            "Improvement") %>% stringr::str_replace_all("R T M_ Q", 
            "Regression To Mean - Baseline Distress Quintile ") %>% 
            stringr::str_replace_all("E D ", "Emergency Department") %>% 
            stringr::str_replace_all("O O S  ", "Occasion of Service ")) %>% 
            dplyr::mutate(Parameter = dplyr::case_when(endsWith(Parameter, 
                "Low") | endsWith(Parameter, "Moderate") | endsWith(Parameter, 
                "High") ~ paste0(Parameter, " Baseline Distress"), 
                T ~ Parameter))
    }
    if (what_1L_chr == "resultsaqol") {
        data_xx <- sim_results_ls %>% make_project_sim_summary(type_1L_chr = "economic", 
            platform_1L_chr = platform_1L_chr, what_1L_chr = type_1L_chr, 
            select_1L_chr = "AQoL-6D")
    }
    if (what_1L_chr == "resultschu") {
        data_xx <- sim_results_ls %>% make_project_sim_summary(type_1L_chr = "economic", 
            platform_1L_chr = platform_1L_chr, what_1L_chr = type_1L_chr, 
            select_1L_chr = "CHU-9D")
    }
    if (what_1L_chr == "resultsoutcomes") {
        data_xx <- sim_results_ls %>% make_project_sim_summary(platform_1L_chr = platform_1L_chr) %>% 
            dplyr::mutate(Outcome = Outcome %>% stringr::str_replace_all("k10", 
                "K10") %>% stringr::str_replace_all("_change", 
                " change from baseline") %>% stringr::str_replace_all("_", 
                " ")) %>% dplyr::arrange(Outcome)
    }
    if (what_1L_chr == "resultseconomic") {
        data_xx <- sim_results_ls %>% make_project_sim_summary(platform_1L_chr = platform_1L_chr, 
            type_1L_chr = "economic")
    }
    if (what_1L_chr %in% c("serviceuse", "serviceusecost")) {
        if (!is.null(model_data_ls)) {
            demographics_tb <- model_data_ls$imputed_ls$Joiners_r4@ds_tb %>% 
                dplyr::filter(Onboarded == 1) %>% dplyr::rename(onboarding_date = Date)
        }
        else {
            demographics_tb <- processed_ls$overview@ds_tb
        }
        if ("gender" %in% transformations_chr) {
            demographics_tb <- update_gender(demographics_tb)
        }
        if (!identical(weeks_int, integer(0))) {
            data_xx <- purrr::reduce(weeks_int, .init = make_report_data(model_data_ls = model_data_ls, 
                processed_ls = processed_ls, date_end_dtm = date_end_dtm, 
                date_start_dtm = date_start_dtm, period_dtm = lubridate::weeks(0), 
                platform_1L_chr = platform_1L_chr, timepoint_1L_chr = "Week0", 
                transformations_chr = transformations_chr, what_1L_chr = what_1L_chr) %>% 
                renewSlot("ds_tb", .@ds_tb %>% dplyr::filter(F)), 
                ~renewSlot(.x, "ds_tb", dplyr::bind_rows(.x@ds_tb, 
                  make_report_data(model_data_ls = model_data_ls, 
                    processed_ls = processed_ls, date_end_dtm = date_end_dtm, 
                    date_start_dtm = date_start_dtm, period_dtm = lubridate::weeks(.y), 
                    platform_1L_chr = platform_1L_chr, timepoint_1L_chr = paste0("Week", 
                      .y), transformations_chr = transformations_chr, 
                    what_1L_chr = what_1L_chr) %>% procureSlot("ds_tb"))))
            data_xx <- renewSlot(data_xx, "ds_tb", setdiff(data_xx@ds_tb %>% 
                dplyr::ungroup() %>% dplyr::select(dplyr::where(is.numeric)) %>% 
                names(), "Age") %>% purrr::reduce(.init = data_xx@ds_tb %>% 
                dplyr::arrange(UID, onboarding_date) %>% dplyr::group_by(UID), 
                ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, 
                  "_change")), !!rlang::sym(.y) - dplyr::lag(!!rlang::sym(.y), 
                  default = 0)), `:=`(!!rlang::sym(.y), !!rlang::sym(.y) - 
                  !!rlang::sym(paste0(.y, "_change"))))) %>% 
                dplyr::ungroup())
        }
        else {
            data_xx <- renewSlot(processed_ls$contacts, "ds_tb", 
                demographics_tb %>% dplyr::filter(onboarding_date >= 
                  date_start_dtm & onboarding_date < date_end_dtm) %>% 
                  dplyr::select(tidyselect::any_of(c("UID", c("onboarding_date", 
                    "platform", "clinic_type", "Age", "gender", 
                    "employment_status", "clinic_state")))) %>% 
                  dplyr::group_by(UID) %>% dplyr::summarise(dplyr::across(dplyr::everything(), 
                  ~dplyr::first(.))) %>% dplyr::left_join(processed_ls$contacts@ds_tb %>% 
                  dplyr::filter(onboarding_date >= date_start_dtm & 
                    onboarding_date < date_end_dtm) %>% dplyr::group_by(UID) %>% 
                  dplyr::mutate(cutoffdate = dplyr::first(onboarding_date) + 
                    period_dtm) %>% dplyr::filter(date_contacted <= 
                  cutoffdate) %>% dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
                  ~sum(.))) %>% dplyr::select(-Age)) %>% dplyr::mutate(dplyr::across(c("direct_mins", 
                  "indirect_mins", "Minutes"), ~dplyr::case_when(is.na(.) ~ 
                  0, T ~ .))) %>% dplyr::ungroup())
        }
        if (what_1L_chr == "serviceusecost") {
            data_xx <- renewSlot(data_xx, "ds_tb", add_cost_calculations(data_xx@ds_tb, 
                inputs_ls = list(unit_costs_tb = processed_ls$costs_unit@ds_tb), 
                add_fixed_1L_lgl = T) %>% dplyr::mutate(Contact = dplyr::case_when(Minutes > 
                0 ~ TRUE, TRUE ~ FALSE)))
        }
        if (!identical(timepoint_1L_chr, character(0))) {
            data_xx <- renewSlot(data_xx, "ds_tb", data_xx@ds_tb %>% 
                dplyr::mutate(MeasurementWeek = timepoint_1L_chr))
        }
    }
    if (ungroup_1L_lgl) {
        data_xx <- renewSlot(data_xx, "ds_tb", data_xx@ds_tb %>% 
            dplyr::ungroup())
    }
    return(data_xx)
}
#' Make results matrix
#' @description make_results_matrix() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make results matrix. The function returns Results (a matrix).
#' @param data_tb Data (a tibble)
#' @param names_chr Names (a character vector)
#' @param arms_1L_chr Arms (a character vector of length one), Default: 'Data'
#' @param var_1L_chr Variable (a character vector of length one), Default: 'Cost'
#' @return Results (a matrix)
#' @rdname make_results_matrix
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr filter pull
#' @importFrom rlang sym
#' @keywords internal
make_results_matrix <- function (data_tb, names_chr, arms_1L_chr = "Data", var_1L_chr = "Cost") 
{
    results_mat <- 1:length(names_chr) %>% purrr::reduce(.init = matrix(rep(NA_real_, 
        nrow(data_tb)), ncol = 2), ~{
        .x[, .y] <- data_tb %>% dplyr::filter(!!rlang::sym(arms_1L_chr) == 
            names_chr[.y]) %>% dplyr::pull(!!rlang::sym(var_1L_chr))
        .x
    })
    return(results_mat)
}
#' Make results summary
#' @description make_results_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make results summary. The function returns Results summary (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param outcomes_chr Outcomes (a character vector)
#' @param group_by_chr Group by (a character vector), Default: character(0)
#' @param min_cell_size_1L_int Minimum cell size (an integer vector of length one), Default: 1
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @return Results summary (a list)
#' @rdname make_results_summary
#' @export 
#' @importFrom dplyr group_by across summarise_at left_join summarise n ungroup filter starts_with rename_with bind_rows mutate
#' @importFrom tidyr all_of
#' @importFrom purrr map_dfr
#' @keywords internal
make_results_summary <- function (X_Ready4useDyad, outcomes_chr, group_by_chr = character(0), 
    min_cell_size_1L_int = 1L, threshold_1L_dbl = 96000) 
{
    D <- X_Ready4useDyad
    E <- renewSlot(D, "ds_tb", D@ds_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(c("Iteration", 
        "Data", group_by_chr)))) %>% dplyr::summarise_at(outcomes_chr, 
        mean) %>% dplyr::left_join(X_Ready4useDyad@ds_tb %>% 
        dplyr::group_by(dplyr::across(tidyr::all_of(c("Iteration", 
            "Data", group_by_chr)))) %>% dplyr::summarise(N = dplyr::n())) %>% 
        dplyr::ungroup())
    AB <- renewSlot(E, "ds_tb", E@ds_tb %>% dplyr::filter(Data != 
        "Difference"))
    E <- renewSlot(E, "ds_tb", E@ds_tb %>% dplyr::filter(Data == 
        "Difference"))
    ab_tb <- AB@ds_tb
    x_tb <- E@ds_tb
    y_tb <- D@ds_tb %>% dplyr::filter(Data == "Difference")
    z_tb <- E@ds_tb %>% add_cost_effectiveness_stats(threshold_1L_dbl = threshold_1L_dbl)
    E <- renewSlot(E, "ds_tb", z_tb)
    ab_tb <- ab_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(c("Data", 
        group_by_chr))))
    x_tb <- x_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(c("Data", 
        group_by_chr))))
    y_tb <- y_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(c("Data", 
        group_by_chr))))
    z_tb <- z_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(c("Data", 
        group_by_chr))))
    ab_tb <- ab_tb %>% dplyr::summarise_at(outcomes_chr, mean, 
        .groups = "drop")
    x_tb <- x_tb %>% dplyr::summarise_at(outcomes_chr, mean, 
        .groups = "drop")
    y_tb <- y_tb %>% dplyr::summarise(N = dplyr::n()/max(X_Ready4useDyad@ds_tb$Iteration), 
        .groups = "drop")
    z_tb <- z_tb %>% dplyr::summarise(dplyr::across(dplyr::starts_with("CE_"), 
        ~mean(as.numeric(.x))), .groups = "drop")
    z_tb <- z_tb %>% dplyr::rename_with(~paste("PROB", .x), .cols = names(z_tb)[names(z_tb) %>% 
        startsWith("CE_")])
    x_tb <- x_tb %>% dplyr::left_join(y_tb) %>% dplyr::ungroup() %>% 
        dplyr::left_join(z_tb)
    x_tb <- x_tb %>% add_cost_effectiveness_stats(threshold_1L_dbl = threshold_1L_dbl)
    x_tb <- dplyr::bind_rows(x_tb, ab_tb %>% dplyr::left_join(ab_tb$Data %>% 
        unique() %>% purrr::map_dfr(~y_tb %>% dplyr::mutate(Data = .x))))
    E <- renewSlot(E, "ds_tb", E@ds_tb %>% dplyr::bind_rows(AB@ds_tb))
    E1 <- renewSlot(E, "ds_tb", x_tb)
    E2 <- renewSlot(E1, "ds_tb", E1@ds_tb %>% dplyr::filter(N >= 
        min_cell_size_1L_int))
    results_summary_ls <- list(X = E, Y = E1, Z = E2)
    return(results_summary_ls)
}
#' Make results synthesis
#' @description make_results_synthesis() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make results synthesis. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param add_severity_1L_lgl Add severity (a logical vector of length one), Default: TRUE
#' @param exclude_chr Exclude (a character vector), Default: c("Adult", "Period", "MeasurementWeek", "treatment_fraction", 
#'    "treatment_measurement", "treatment_start")
#' @param exclude_suffixes_chr Exclude suffixes (a character vector), Default: c("_change", "_date", "_previous", "52_Weeks")
#' @param keep_chr Keep (a character vector), Default: c("platform", "clinic_state", "clinic_type", "Age", "gender", 
#'    "employment_status")
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param results_ls Results (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("D", "AB", "C")
#' @param Y_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param Z_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @return A (A dataset and data dictionary pair.)
#' @rdname make_results_synthesis
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom dplyr mutate case_when
#' @keywords internal
make_results_synthesis <- function (X_Ready4useDyad, add_severity_1L_lgl = TRUE, exclude_chr = c("Adult", 
    "Period", "MeasurementWeek", "treatment_fraction", "treatment_measurement", 
    "treatment_start"), exclude_suffixes_chr = c("_change", "_date", 
    "_previous", "52_Weeks"), keep_chr = c("platform", "clinic_state", 
    "clinic_type", "Age", "gender", "employment_status"), modifiable_chr = character(0), 
    results_ls = NULL, type_1L_chr = c("D", "AB", "C"), Y_Ready4useDyad = ready4use::Ready4useDyad(), 
    Z_Ready4useDyad = ready4use::Ready4useDyad()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (!is.null(results_ls)) {
        Y_Ready4useDyad = results_ls$Y_Ready4useDyad
        Z_Ready4useDyad = results_ls$Z_Ready4useDyad
    }
    A_Ready4useDyad <- make_composite_results(X_Ready4useDyad, 
        Y_Ready4useDyad = Y_Ready4useDyad, Z_Ready4useDyad = Z_Ready4useDyad, 
        exclude_chr = exclude_chr, exclude_suffixes_chr = exclude_suffixes_chr, 
        keep_chr = keep_chr, modifiable_chr = modifiable_chr, 
        type_1L_chr = type_1L_chr)
    if (add_severity_1L_lgl) {
        severity_ls <- make_k10_severity_cuts()
        A_Ready4useDyad <- renewSlot(A_Ready4useDyad, "ds_tb", 
            A_Ready4useDyad@ds_tb %>% dplyr::mutate(Distress = dplyr::case_when(as.numeric(k10_start) >= 
                severity_ls$Low[1] & as.numeric(k10_start) <= 
                severity_ls$Low[2] ~ "Low", as.numeric(k10_start) >= 
                severity_ls$Moderate[1] & as.numeric(k10_start) <= 
                severity_ls$Moderate[2] ~ "Moderate", as.numeric(k10_start) >= 
                severity_ls$High[1] & as.numeric(k10_start) <= 
                severity_ls$High[2] ~ "High", as.numeric(k10_start) >= 
                severity_ls$VeryHigh[1] & as.numeric(k10_start) <= 
                severity_ls$VeryHigh[2] ~ "VeryHigh", T ~ NA_character_)))
    }
    return(A_Ready4useDyad)
}
#' Make sensitivities list
#' @description make_sensitivities_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sensitivities list. The function returns Sensitivities (a list).

#' @return Sensitivities (a list)
#' @rdname make_sensitivities_ls
#' @export 
#' @keywords internal
make_sensitivities_ls <- function () 
{
    sensitivities_ls <- list(costs_ls = list(), outcomes_ls = list(YR1 = add_projected_maintenance, 
        YR1_S1 = add_projected_decay, YR1_S2 = add_projected_growth))
    return(sensitivities_ls)
}
#' Make simulated draws
#' @description make_simulated_draws() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make simulated draws. The function returns Simulations (a data.frame).
#' @param model_mdl Model (a model)
#' @param new_data_tb New data (a tibble)
#' @param sample_fn Sample (a function), Default: rnorm
#' @param iterations_int Iterations (an integer vector), Default: 1:100
#' @return Simulations (a data.frame)
#' @rdname make_simulated_draws
#' @export 
#' @importFrom stats setNames
#' @keywords internal
make_simulated_draws <- function (model_mdl, new_data_tb, sample_fn = rnorm, iterations_int = 1:100) 
{
    iterations_1L_int <- length(unique(iterations_int))
    predictions_num <- predict(model_mdl, newdata = new_data_tb, 
        type = "response")
    simulations_df <- replicate(iterations_1L_int, sample_fn(rep(1, 
        length(predictions_num)), predictions_num)) %>% as.data.frame() %>% 
        stats::setNames(paste0("sim_", iterations_int))
    return(simulations_df)
}
#' Make structural variables
#' @description make_structural_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make structural variables. The function returns Structural (a character vector).

#' @return Structural (a character vector)
#' @rdname make_structural_vars
#' @export 
#' @keywords internal
make_structural_vars <- function () 
{
    structural_chr <- c("Iteration", "InModel", "Arm", "Data", 
        "StartDate", "CurrentDate", "EndDate", "CurrentEvent", 
        "NextEvent", "ScheduledFor")
    return(structural_chr)
}
#' Make synthetic data
#' @description make_synthetic_data() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make synthetic data. The function returns Output (a list).
#' @param model_data_ls Model data (a list)
#' @param imputations_1L_int Imputations (an integer vector of length one), Default: 5
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param size_1L_int Size (an integer vector of length one), Default: 1000
#' @param transform_gender_1L_lgl Transform gender (a logical vector of length one), Default: TRUE
#' @return Output (a list)
#' @rdname make_synthetic_data
#' @export 
#' @importFrom mice mice complete
#' @importFrom dplyr group_by summarise across first mutate where rename select everything row_number case_when as_tibble
#' @importFrom synthpop syn
#' @importFrom ready4use Ready4useDyad
#' @keywords internal
make_synthetic_data <- function (model_data_ls, imputations_1L_int = 5L, seed_1L_int = 2001, 
    size_1L_int = 1000, transform_gender_1L_lgl = TRUE) 
{
    X_Ready4useDyad <- model_data_ls$imputed_ls$OutcomesJoiners_r4
    if (transform_gender_1L_lgl) {
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% update_gender()
    }
    imputed_ls <- mice::mice(X_Ready4useDyad@ds_tb, m = 1, print = F)
    imputed_ls$method[["treatment_status"]] <- "rf"
    imputed_ls$predictorMatrix <- imputed_ls$predictorMatrix * 
        0
    imputed_ls$predictorMatrix["k10", c("Age", "gender", "employment_status", 
        "clinic_type", "treatment_status")] <- 1
    imputed_ls$predictorMatrix["gad7", c("Age", "gender", "employment_status", 
        "clinic_type", "treatment_status", "k10")] <- 1
    imputed_ls$predictorMatrix["phq9", c("Age", "gender", "employment_status", 
        "clinic_type", "treatment_status", "k10", "gad7")] <- 1
    imputed_ls$predictorMatrix["AQoL6D", c("Age", "gender", "k10", 
        "gad7", "phq9")] <- 1
    imputed_ls$predictorMatrix["CHU9D", c("Age", "gender", "k10", 
        "gad7", "phq9")] <- 1
    imputed_ls <- mice::mice(X_Ready4useDyad@ds_tb, method = imputed_ls$method, 
        predictorMatrix = imputed_ls$predictorMatrix, m = imputations_1L_int, 
        print = F, seed = seed_1L_int)
    X_Ready4useDyad@ds_tb <- mice::complete(imputed_ls, action = "long") %>% 
        dplyr::group_by(.id) %>% dplyr::summarise(dplyr::across(c("platform", 
        "clinic_type", "Age", "gender", "employment_status", 
        "clinic_state", "treatment_stage"), ~dplyr::first(.x)), 
        dplyr::across("treatment_status", ~sample(.x, size = 1)), 
        dplyr::across(c("gad7", "k10", "phq9", "AQoL6D", "CHU9D"), 
            ~mean(.x))) %>% dplyr::mutate(dplyr::across(c("gad7", 
        "k10", "phq9"), ~as.integer(.x)), dplyr::across(dplyr::where(is.character), 
        ~as.factor(.x))) %>% dplyr::rename(UID = .id)
    synthetic_ls <- synthpop::syn(X_Ready4useDyad@ds_tb %>% dplyr::select(-c(UID, 
        platform)) %>% dplyr::select(clinic_state, clinic_type, 
        treatment_stage, treatment_status, dplyr::everything()), 
        k = size_1L_int)
    Synthetic_r4 <- ready4use::Ready4useDyad(ds_tb = synthetic_ls$syn %>% 
        dplyr::mutate(UID = dplyr::row_number() %>% as.character(), 
            platform = dplyr::case_when(Age >= 15 ~ "over_15", 
                T ~ "under_15") %>% as.factor()) %>% dplyr::select(UID, 
        platform, dplyr::everything()) %>% dplyr::as_tibble(), 
        dictionary_r3 = X_Ready4useDyad@dictionary_r3)
    output_ls <- list(real_imputed_ls = list(imputed_ls = imputed_ls, 
        Imputed_r4 = X_Ready4useDyad), fully_synthetic_ls = list(synthetic_ls = synthetic_ls, 
        Synthetic_r4 = Synthetic_r4))
    return(output_ls)
}
#' Make synthetic tests
#' @description make_synthetic_tests() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make synthetic tests. The function returns Synthetic tests (a list).
#' @param population_ls Population (a list)
#' @param model_data_ls Model data (a list), Default: NULL
#' @param comparison_1L_chr Comparison (a character vector of length one), Default: c("OutcomesJoinersImputed", "Joiners", "OutcomesJoiners", "Outcomes")
#' @param ... Additional arguments
#' @return Synthetic tests (a list)
#' @rdname make_synthetic_tests
#' @export 
#' @importFrom dplyr mutate case_when select across slice cross_join
#' @importFrom tidyselect all_of
#' @importFrom synthpop compare
#' @keywords internal
make_synthetic_tests <- function (population_ls, model_data_ls = NULL, comparison_1L_chr = c("OutcomesJoinersImputed", 
    "Joiners", "OutcomesJoiners", "Outcomes"), ...) 
{
    comparison_1L_chr <- match.arg(comparison_1L_chr)
    if (comparison_1L_chr == "Joiners") {
        original_tb <- model_data_ls$unimputed_ls$Joiners_r4@ds_tb %>% 
            dplyr::mutate(treatment_status = dplyr::case_when(treatment_status == 
                "Other" ~ NA_character_, T ~ treatment_status) %>% 
                as.factor())
    }
    if (comparison_1L_chr == "OutcomesJoinersImputed") {
        original_tb <- population_ls$real_imputed_ls$Imputed_r4@ds_tb
    }
    if (comparison_1L_chr %in% c("OutcomesJoiners", "Outcomes")) {
        original_tb <- model_data_ls$imputed_ls$OutcomesJoiners_r4@ds_tb
    }
    original_tb <- dplyr::select(original_tb, intersect(names(original_tb), 
        names(population_ls$fully_synthetic_ls$Synthetic_r4@ds_tb)))
    if (comparison_1L_chr == "Outcomes") {
        original_tb <- original_tb[complete.cases(original_tb), 
            ]
    }
    extras_chr <- setdiff(names(population_ls$fully_synthetic_ls$synthetic_ls$syn), 
        names(original_tb))
    if (length(extras_chr) > 0) {
        extras_tb <- population_ls$fully_synthetic_ls$synthetic_ls$syn %>% 
            dplyr::select(tidyselect::all_of(extras_chr))
        extras_tb <- extras_tb %>% dplyr::mutate(dplyr::across(names(extras_tb), 
            ~ifelse(.x == Inf, Inf, NA_real_))) %>% dplyr::slice(1)
        original_tb <- dplyr::cross_join(original_tb, extras_tb)
    }
    synthetic_tests_ls <- synthpop::compare(population_ls$fully_synthetic_ls$synthetic_ls, 
        original_tb, print.flag = F, ...)
    return(synthetic_tests_ls)
}
#' Make test comparisons
#' @description make_test_comparisons() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make test comparisons. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param type_1L_chr Type (a character vector of length one), Default: c("full", "wsummary", "lsummary")
#' @return Z (A dataset and data dictionary pair.)
#' @rdname make_test_comparisons
#' @export 
#' @importFrom ready4use Ready4useDyad add_dictionary
#' @importFrom dplyr select mutate select_if group_by summarise across where filter left_join
#' @importFrom tidyr any_of pivot_longer
#' @keywords internal
make_test_comparisons <- function (X_Ready4useDyad, Y_Ready4useDyad = ready4use::Ready4useDyad(), 
    type_1L_chr = c("full", "wsummary", "lsummary")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (!identical(Y_Ready4useDyad, ready4use::Ready4useDyad())) {
        Z_Ready4useDyad <- make_predd_observed_ds(renewSlot(X_Ready4useDyad, 
            "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::select(tidyr::any_of(names(Y_Ready4useDyad@ds_tb))) %>% 
                dplyr::mutate(UID = as.integer(UID))), Y_Ready4useDyad = Y_Ready4useDyad)
        Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
            Z_Ready4useDyad@ds_tb %>% dplyr::select_if(~!any(is.na(.))))
    }
    else {
        Z_Ready4useDyad <- X_Ready4useDyad
    }
    if (type_1L_chr %in% c("wsummary", "lsummary")) {
        Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
            Z_Ready4useDyad@ds_tb %>% dplyr::group_by(Data) %>% 
                dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
                  ~mean(.x))))
        if (type_1L_chr == "lsummary") {
            Z_Ready4useDyad <- renewSlot(Z_Ready4useDyad, "ds_tb", 
                Z_Ready4useDyad@ds_tb %>% dplyr::filter(Data == 
                  "Observed") %>% tidyr::pivot_longer(cols = dplyr::where(is.numeric), 
                  names_to = "Variable", values_to = "Observed") %>% 
                  dplyr::select(-Data) %>% dplyr::left_join(Z_Ready4useDyad@ds_tb %>% 
                  dplyr::filter(Data == "Simulated") %>% tidyr::pivot_longer(cols = dplyr::where(is.numeric), 
                  names_to = "Variable", values_to = "Simulated") %>% 
                  dplyr::select(-Data)))
            Z_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = Z_Ready4useDyad@ds_tb) %>% 
                ready4use::add_dictionary()
        }
    }
    return(Z_Ready4useDyad)
}
#' Make transformed Minimum Dataset outcomes tibble
#' @description make_tfd_mds_outcomes_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make transformed minimum dataset outcomes tibble. The function returns Data (a tibble).
#' @param processed_ls Processed (a list)
#' @param outcomes_ls Outcomes (a list)
#' @param jurisdiction_1L_chr Jurisdiction (a character vector of length one), Default: 'Jurisdiction'
#' @param missing_after_dtm Missing after (a date vector), Default: Sys.Date()
#' @param postcode_lup Postcode (a lookup table), Default: NULL
#' @param referrals_1L_lgl Referrals (a logical vector of length one), Default: FALSE
#' @param reviews_1L_lgl Reviews (a logical vector of length one), Default: FALSE
#' @return Data (a tibble)
#' @rdname make_tfd_mds_outcomes_tb
#' @export 
#' @importFrom dplyr select mutate across where left_join group_by arrange row_number ungroup rename case_when rename_with
#' @importFrom purrr flatten_chr map map_vec reduce map2_dbl map_chr
#' @importFrom lubridate NA_Date_ interval years
#' @importFrom stringr str_remove
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @keywords internal
make_tfd_mds_outcomes_tb <- function (processed_ls, outcomes_ls, jurisdiction_1L_chr = "Jurisdiction", 
    missing_after_dtm = Sys.Date(), postcode_lup = NULL, referrals_1L_lgl = FALSE, 
    reviews_1L_lgl = FALSE) 
{
    if (is.null(postcode_lup)) {
        postcode_lup <- make_postcode_lup()
    }
    postcode_lup <- dplyr::select(postcode_lup, -"Decile")
    data_tb <- processed_ls$outcomes_tb
    outcomes_chr <- outcomes_ls %>% purrr::flatten_chr() %>% 
        purrr::map(~names(data_tb)[names(data_tb) %>% startsWith(.x)]) %>% 
        purrr::flatten_chr() %>% sort()
    collection_dates_chr <- names(data_tb)[names(data_tb) %>% 
        startsWith("collection_occasion_date")]
    data_tb <- data_tb %>% dplyr::mutate(dplyr::across(dplyr::where(function(x) inherits(x, 
        "Date")), ~purrr::map_vec(., ~if (is.na(.x)) {
        .x
    }
    else {
        if (.x > missing_after_dtm) {
            lubridate::NA_Date_
        }
        else {
            .x
        }
    })))
    data_tb <- dplyr::left_join(data_tb, make_phn_lup(jurisdiction_1L_chr = jurisdiction_1L_chr))
    data_tb <- data_tb %>% dplyr::group_by(client_key) %>% dplyr::arrange(client_key, 
        first_service_date) %>% dplyr::mutate(Episode = dplyr::row_number()) %>% 
        dplyr::ungroup()
    data_tb <- dplyr::left_join(data_tb, processed_ls$clients_tb %>% 
        dplyr::select(-c(organisation_path, slk)))
    data_tb <- data_tb %>% dplyr::mutate(Age = round(lubridate::interval(date_of_birth, 
        first_service_date)/lubridate::years(1), 0))
    data_tb <- data_tb %>% dplyr::rename(Gender = client_gender, 
        ServiceStartDate = start_date, MatureService = Mature)
    data_tb <- data_tb %>% dplyr::mutate(iar_dst_practitioner_level_of_care = dplyr::case_when(iar_dst_practitioner_level_of_care == 
        9 ~ NA_integer_, T ~ iar_dst_practitioner_level_of_care))
    outcomes_chr <- outcomes_chr %>% sort()
    roots_chr <- outcomes_chr[endsWith(outcomes_chr, "_Referral")] %>% 
        stringr::str_remove("_Referral")
    roots_chr <- roots_chr[!startsWith(roots_chr, "iar_")]
    roots_chr <- roots_chr[!startsWith(roots_chr, "k10p_item14")]
    data_tb <- roots_chr %>% purrr::reduce(.init = data_tb, ~{
        .x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, "_change_Referral_Start")), 
            !!rlang::sym(paste0(.y, "_Referral")) %>% purrr::map2_dbl(!!rlang::sym(paste0(.y, 
                "_Start")), ~{
                if (is.na(.x) | is.na(.y)) {
                  NA_real_
                }
                else {
                  .y - .x
                }
            })), `:=`(!!rlang::sym(paste0(.y, "_change_Start_Review")), 
            !!rlang::sym(paste0(.y, "_Start")) %>% purrr::map2_dbl(!!rlang::sym(paste0(.y, 
                "_Review")), ~{
                if (is.na(.x) | is.na(.y)) {
                  NA_real_
                }
                else {
                  .y - .x
                }
            })), `:=`(!!rlang::sym(paste0(.y, "_change_Review_End")), 
            !!rlang::sym(paste0(.y, "_Review")) %>% purrr::map2_dbl(!!rlang::sym(paste0(.y, 
                "_End")), ~{
                if (is.na(.x) | is.na(.y)) {
                  NA_real_
                }
                else {
                  .y - .x
                }
            })), `:=`(!!rlang::sym(paste0(.y, "_change_Start_End")), 
            !!rlang::sym(paste0(.y, "_Start")) %>% purrr::map2_dbl(!!rlang::sym(paste0(.y, 
                "_End")), ~{
                if (is.na(.x) | is.na(.y)) {
                  NA_real_
                }
                else {
                  .y - .x
                }
            })))
    })
    data_tb <- data_tb %>% dplyr::mutate(EpisodeDurationDays = as.numeric(last_service_date - 
        first_service_date))
    data_tb <- data_tb %>% dplyr::left_join(processed_ls$episodes_tb %>% 
        dplyr::select(!dplyr::where(function(x) inherits(x, "Date"))))
    data_tb <- data_tb %>% dplyr::mutate(client_postcode = client_postcode %>% 
        purrr::map_chr(~ifelse(.x == 9999, NA_character_, ifelse(.x < 
            1000, paste0("0", .x), as.character(.x)))))
    data_tb <- data_tb %>% dplyr::mutate(Diagnosis = dplyr::case_when(principal_diagnosis >= 
        100 & principal_diagnosis < 300 ~ "Anxiety and Mood", 
        principal_diagnosis >= 300 & principal_diagnosis < 400 ~ 
            "Substance", principal_diagnosis >= 400 & principal_diagnosis < 
            500 ~ "Psychotic", principal_diagnosis >= 500 & principal_diagnosis < 
            700 ~ "Other", principal_diagnosis >= 900 & principal_diagnosis < 
            999 ~ "Sub-syndromal", T ~ NA_character_))
    data_tb <- data_tb %>% dplyr::mutate(Medication = dplyr::case_when((medication_antipsychotics == 
        1 | medication_anxiolytics == 1 | medication_hypnotics == 
        1 | medication_antidepressants == 1 | medication_psychostimulants == 
        1) ~ TRUE, (medication_antipsychotics == 2 | medication_anxiolytics == 
        2 | medication_hypnotics == 2 | medication_antidepressants == 
        2 | medication_psychostimulants == 2) ~ FALSE, T ~ NA))
    data_tb <- data_tb %>% dplyr::mutate(TreatmentPlan = dplyr::case_when(mental_health_treatment_plan == 
        1 ~ T, mental_health_treatment_plan == 2 ~ F, T ~ NA))
    data_tb <- data_tb %>% dplyr::mutate(Employment = dplyr::case_when(labour_force_status == 
        1 ~ "Employed", labour_force_status == 2 ~ "Unemployed", 
        labour_force_status == 3 ~ "NILF", T ~ NA_character_))
    data_tb <- data_tb %>% dplyr::mutate(SuicideRisk = dplyr::case_when(suicide_referral_flag == 
        1 ~ T, suicide_referral_flag == 2 ~ F, T ~ NA))
    data_tb <- data_tb %>% dplyr::left_join(postcode_lup %>% 
        dplyr::rename(client_postcode = Postcode))
    outcomes_chr <- outcomes_ls %>% purrr::flatten_chr() %>% 
        purrr::map(~names(data_tb)[names(data_tb) %>% startsWith(.x)]) %>% 
        purrr::flatten_chr() %>% sort()
    if (!referrals_1L_lgl) {
        outcomes_chr <- outcomes_chr[!endsWith(outcomes_chr, 
            "_Referral")]
        outcomes_chr <- outcomes_chr[!endsWith(outcomes_chr, 
            "Referral_Start")]
        collection_dates_chr <- collection_dates_chr[!endsWith(collection_dates_chr, 
            "_Referral")]
    }
    if (!reviews_1L_lgl) {
        outcomes_chr <- outcomes_chr[!endsWith(outcomes_chr, 
            "_Review")]
        outcomes_chr <- outcomes_chr[!endsWith(outcomes_chr, 
            "Review_End")]
        collection_dates_chr <- collection_dates_chr[!endsWith(collection_dates_chr, 
            "_Review")]
    }
    if (!reviews_1L_lgl & !referrals_1L_lgl) {
        data_tb <- data_tb %>% dplyr::rename_with(~gsub("_change_Start_End", 
            "_change", .x))
        outcomes_chr <- gsub("_change_Start_End", "_change", 
            outcomes_chr)
    }
    data_tb <- data_tb %>% dplyr::select(tidyselect::all_of(c("InterventionGroup", 
        "client_key", "episode_key", "Episode", "EpisodeDurationDays", 
        "referral_date", "first_service_date", "last_service_date", 
        collection_dates_chr, jurisdiction_1L_chr, "PHN_code", 
        "PHN_area_name", "InScope", "service_centre", "ServiceStartDate", 
        "MatureService", "Medication", "Diagnosis", "SuicideRisk", 
        "TreatmentPlan", "Age", "Gender", "Employment", "IRSADQuintile", 
        outcomes_chr)))
    return(data_tb)
}
#' Make two part model
#' @description make_two_part_mdl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make two part model. The function returns Model (a model).
#' @param data_tb Data (a tibble)
#' @param family_2_1L_chr Family 2 (a character vector of length one), Default: 'Gamma(link = 'inverse')'
#' @param link_1_1L_chr Link 1 (a character vector of length one), Default: 'logit'
#' @param x_part_1_chr X part 1 (a character vector)
#' @param x_part_2_chr X part 2 (a character vector)
#' @param y_1L_chr Y (a character vector of length one)
#' @param ... Additional arguments
#' @return Model (a model)
#' @rdname make_two_part_mdl
#' @export 
#' @keywords internal
make_two_part_mdl <- function (data_tb, family_2_1L_chr = "Gamma(link = 'inverse')", 
    link_1_1L_chr = "logit", x_part_1_chr, x_part_2_chr, y_1L_chr, 
    ...) 
{
    model_mdl <- eval(parse(text = paste0("twopartm::tpm(formula_part1 = ", 
        y_1L_chr, " ~ ", paste0(x_part_1_chr, collapse = " + "), 
        ",", "formula_part2 = ", y_1L_chr, " ~ ", paste0(x_part_2_chr, 
            collapse = " + "), ",", "link_part1 = '", link_1_1L_chr, 
        "'", ",", "family_part2 = ", family_2_1L_chr, ",", "data = data_tb, ...)")))
    return(model_mdl)
}
#' Make treatment model confusion
#' @description make_tx_mdl_confusion() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make treatment model confusion. The function returns Confusion (a list).
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param tx_mdls_ls Treatment models (a list)
#' @param model_1L_int Model (an integer vector of length one)
#' @param treatment_vars_chr Treatment variables (a character vector), Default: c("treatment_status", "treatment_status_t2")
#' @param what_1L_chr What (a character vector of length one), Default: c("Waitlist", "Treatment", "Discharged")
#' @return Confusion (a list)
#' @rdname make_tx_mdl_confusion
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom purrr pluck
#' @importFrom caret confusionMatrix
#' @importFrom stats predict
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
make_tx_mdl_confusion <- function (X_Ready4useDyad = ready4use::Ready4useDyad(), tx_mdls_ls, 
    model_1L_int, treatment_vars_chr = c("treatment_status", 
        "treatment_status_t2"), what_1L_chr = c("Waitlist", "Treatment", 
        "Discharged")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    model_mdl <- tx_mdls_ls[[paste0(what_1L_chr, "_ls")]] %>% 
        purrr::pluck(model_1L_int)
    data_tb <- X_Ready4useDyad@ds_tb %>% transform_tx_factor(treatment_vars_chr = treatment_vars_chr, 
        what_1L_chr = what_1L_chr)
    confusion_ls <- caret::confusionMatrix(stats::predict(model_mdl, 
        data_tb) %>% dplyr::pull(.pred_class), data_tb %>% dplyr::pull(!!rlang::sym(treatment_vars_chr[2])))
    return(confusion_ls)
}
#' Make unit cost parameters tibble
#' @description make_unit_cost_params_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make unit cost parameters tibble. The function returns Unit cost parameters (a tibble).
#' @param cost_per_mins_tb Cost per minimums (a tibble)
#' @param new_ls New (a list), Default: NULL
#' @return Unit cost parameters (a tibble)
#' @rdname make_unit_cost_params_tb
#' @export 
#' @importFrom dplyr mutate select filter bind_rows pull arrange
#' @importFrom stringr str_replace
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @keywords internal
make_unit_cost_params_tb <- function (cost_per_mins_tb, new_ls = NULL) 
{
    unit_cost_params_tb <- cost_per_mins_tb %>% dplyr::mutate(Parameter = Provider %>% 
        stringr::str_replace("Clinical psychologists", "ClinicalPsychologist") %>% 
        stringr::str_replace("General practitioners", "GP") %>% 
        stringr::str_replace("Other allied health", "Other") %>% 
        stringr::str_replace("Psychiatrists", "Psychiatrist"), 
        Mean = round(Total, 2)) %>% dplyr::select(Parameter, 
        Mean)
    unit_cost_params_tb <- 1:length(new_ls) %>% purrr::reduce(.init = unit_cost_params_tb, 
        ~.x %>% dplyr::filter(Parameter != names(new_ls)[.y]) %>% 
            dplyr::bind_rows(tibble::tibble(Parameter = names(new_ls)[.y], 
                Mean = new_ls[[.y]])))
    if (!"OtherMedical" %in% unit_cost_params_tb$Parameter) {
        unit_cost_params_tb <- unit_cost_params_tb %>% dplyr::bind_rows(tibble::tibble(Parameter = "OtherMedical", 
            Mean = unit_cost_params_tb %>% dplyr::filter(Parameter %in% 
                c("GP", "Psychiatrist")) %>% dplyr::pull(Mean) %>% 
                mean()))
    }
    unit_cost_params_tb <- unit_cost_params_tb %>% dplyr::arrange(Parameter)
    unit_cost_params_tb <- unit_cost_params_tb %>% dplyr::mutate(Parameter = paste0(Parameter, 
        "CostPerMin"))
    return(unit_cost_params_tb)
}
#' Make utility predictions dataset
#' @description make_utility_predictions_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make utility predictions dataset. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param Y_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param Z_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param model_mdl Model (a model)
#' @param utility_1L_chr Utility (a character vector of length one), Default: c("AQoL6D", "CHU9D")
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: 12
#' @param iterations_1L_int Iterations (an integer vector of length one), Default: 100
#' @param join_with_chr Join with (a character vector), Default: character(0)
#' @param maintain_for_1L_int Maintain for (an integer vector of length one), Default: 12
#' @param tfmn_1L_chr Transformation (a character vector of length one), Default: 'NTF'
#' @param type_1L_chr Type (a character vector of length one), Default: c("predict", "simulate")
#' @param with_1L_chr With (a character vector of length one), Default: '_sim_mean'
#' @param what_1L_chr What (a character vector of length one), Default: c("old", "new")
#' @return Y (A dataset and data dictionary pair.)
#' @rdname make_utility_predictions_ds
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom youthvars youthvars_chu9d_adolaus youthvars_aqol6d_adol
#' @importFrom dplyr mutate select inner_join starts_with
#' @importFrom rlang sym
#' @importFrom Hmisc capitalize
#' @importFrom tidyr any_of
#' @keywords internal
make_utility_predictions_ds <- function (X_Ready4useDyad = ready4use::Ready4useDyad(), Y_Ready4useDyad = ready4use::Ready4useDyad(), 
    Z_Ready4useDyad = ready4use::Ready4useDyad(), model_mdl, 
    utility_1L_chr = c("AQoL6D", "CHU9D"), follow_up_1L_int = 12, 
    iterations_1L_int = 100L, join_with_chr = character(0), maintain_for_1L_int = 12, 
    tfmn_1L_chr = "NTF", type_1L_chr = c("predict", "simulate"), 
    with_1L_chr = "_sim_mean", what_1L_chr = c("old", "new")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    utility_1L_chr <- match.arg(utility_1L_chr)
    var_1L_chr <- paste0(utility_1L_chr, "_", follow_up_1L_int, 
        "_Weeks")
    qaly_vars_chr <- paste0(paste0(utility_1L_chr, "_QALYs"), 
        c("", "_YR1", "_YR1_S1", "_YR1_S2"))
    if (utility_1L_chr == "CHU9D") {
        class_fn <- youthvars::youthvars_chu9d_adolaus
        min_1L_dbl <- -0.2118
    }
    if (utility_1L_chr == "AQoL6D") {
        class_fn <- youthvars::youthvars_aqol6d_adol
        min_1L_dbl <- 0.03
    }
    if (type_1L_chr == "predict") {
        Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            predict(model_mdl, newdata = X_Ready4useDyad@ds_tb, 
                type = "response")))
    }
    else {
        Y_Ready4useDyad <- add_simulated_data(model_mdl = model_mdl, 
            var_1L_chr = var_1L_chr, Y_Ready4useDyad = Y_Ready4useDyad, 
            iterations_int = iterations_int, tfmn_1L_chr = tfmn_1L_chr, 
            what_1L_chr = what_1L_chr)
        Y_Ready4useDyad <- Y_Ready4useDyad %>% update_predictions_ds(do_int = 1, 
            follow_up_1L_int = follow_up_1L_int, tfmn_1L_chr = tfmn_1L_chr, 
            utility_1L_chr = utility_1L_chr, with_1L_chr = with_1L_chr)
    }
    Y_Ready4useDyad <- Y_Ready4useDyad %>% update_predictions_ds(do_int = 2, 
        follow_up_1L_int = follow_up_1L_int, tfmn_1L_chr = tfmn_1L_chr, 
        utility_1L_chr = utility_1L_chr, with_1L_chr = with_1L_chr)
    Y_Ready4useDyad <- Y_Ready4useDyad %>% update_predictions_ds(do_int = 3, 
        follow_up_1L_int = follow_up_1L_int, tfmn_1L_chr = tfmn_1L_chr, 
        utility_1L_chr = utility_1L_chr, with_1L_chr = with_1L_chr)
    if (type_1L_chr == "predict") {
        consolidate_1L_chr <- character(0)
    }
    else {
        consolidate_1L_chr <- var_1L_chr
    }
    Y_Ready4useDyad <- make_predd_observed_ds(X_Ready4useDyad, 
        Y_Ready4useDyad = Y_Ready4useDyad, consolidate_1L_chr = consolidate_1L_chr, 
        new_1L_chr = paste0(Hmisc::capitalize(type_1L_chr), ifelse(type_1L_chr == 
            "simulate", "d", "ed")), join_with_chr = join_with_chr)
    Y_Ready4useDyad <- Y_Ready4useDyad %>% update_predictions_ds(do_int = 4:5, 
        follow_up_1L_int = follow_up_1L_int, maintain_for_1L_int = maintain_for_1L_int, 
        tfmn_1L_chr = tfmn_1L_chr, utility_1L_chr = utility_1L_chr, 
        with_1L_chr = with_1L_chr)
    Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    if (!identical(Z_Ready4useDyad, ready4use::Ready4useDyad())) {
        Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::select(tidyr::any_of(c("UID", 
            "Date", "Data", "Iteration", names(Y_Ready4useDyad@ds_tb)[startsWith(names(Y_Ready4useDyad@ds_tb), 
                utility_1L_chr)]))) %>% dplyr::inner_join(Z_Ready4useDyad@ds_tb %>% 
            dplyr::select(-dplyr::starts_with(utility_1L_chr)))
        Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
            type_1L_chr = "update")
    }
    return(Y_Ready4useDyad)
}
#' Make weeks suffix
#' @description make_weeks_suffix() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make weeks suffix. The function returns Suffix (a character vector of length one).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param adjustment_1L_dbl Adjustment (a double vector of length one), Default: 0
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: integer(0)
#' @return Suffix (a character vector of length one)
#' @rdname make_weeks_suffix
#' @export 
#' @keywords internal
make_weeks_suffix <- function (X_Ready4useDyad, adjustment_1L_dbl = 0, follow_up_1L_int = integer(0)) 
{
    if (!identical(follow_up_1L_int, integer(0))) {
        suffix_1L_chr <- paste0("_", follow_up_1L_int, "_Weeks")
    }
    else {
        suffix_1L_chr <- paste0("_", adjustment_1L_dbl + round(as.numeric((X_Ready4useDyad@ds_tb$CurrentDate[1] - 
            X_Ready4useDyad@ds_tb$StartDate[1]))/7, 0), "_Weeks")
    }
    return(suffix_1L_chr)
}
