#' Transform dates
#' @description transform_dates() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dates. The function returns Dates (a date vector).
#' @param dates_chr Dates (a character vector)
#' @return Dates (a date vector)
#' @rdname transform_dates
#' @export 
#' @importFrom purrr map_vec
#' @importFrom lubridate NA_Date_
#' @importFrom stringr str_sub
#' @keywords internal
transform_dates <- function (dates_chr) 
{
    dates_dtm <- dates_chr %>% purrr::map_vec(~{
        if (is.na(.x)) {
            lubridate::NA_Date_
        }
        else {
            as.Date(paste0(stringr::str_sub(.x, start = -4), 
                "/", stringr::str_sub(.x, start = -6, end = -5), 
                "/", stringr::str_sub(.x, end = -7)))
        }
    })
    return(dates_dtm)
}
#' Transform dataset to wide
#' @description transform_ds_to_wide() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dataset to wide. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param processed_ls Processed (a list)
#' @param join_before_dtm Join before (a date vector), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: make_project_keys()
#' @param max_periods_1L_int Maximum periods (an integer vector of length one), Default: integer(0)
#' @param max_tenure_1L_dbl Maximum tenure (a double vector of length one), Default: numeric(0)
#' @param metric_var_1L_chr Metric variable (a character vector of length one), Default: 'Minutes'
#' @param patterns_ls Patterns (a list), Default: list(c("[[:space:]]", ""), c("NA", "MISSING"))
#' @param period_var_1L_chr Period variable (a character vector of length one), Default: 'Year'
#' @param separation_after_dbl Separation after (a double vector), Default: 3
#' @param service_var_1L_chr Service variable (a character vector of length one), Default: 'primary_purpose'
#' @return Y (A dataset and data dictionary pair.)
#' @rdname transform_ds_to_wide
#' @export 
#' @importFrom serious make_service_summary make_summary_ds
#' @importFrom dplyr left_join select ends_with where mutate across case_when
#' @importFrom tidyselect any_of all_of
#' @importFrom stringr str_sub
#' @importFrom purrr map2 reduce map_lgl
#' @keywords internal
transform_ds_to_wide <- function (X_Ready4useDyad, processed_ls, join_before_dtm = NULL, 
    key_vars_chr = make_project_keys(), max_periods_1L_int = integer(0), 
    max_tenure_1L_dbl = numeric(0), metric_var_1L_chr = "Minutes", 
    patterns_ls = list(c("[[:space:]]", ""), c("NA", "MISSING")), 
    period_var_1L_chr = "Year", separation_after_dbl = 3, service_var_1L_chr = "primary_purpose") 
{
    Y_Ready4useDyad <- serious::make_service_summary(X_Ready4useDyad, 
        max_periods_1L_int = max_periods_1L_int, service_var_1L_chr = service_var_1L_chr, 
        metrics_chr = make_project_metrics(processed_ls$costs_unit@ds_tb), 
        metric_var_1L_chr = metric_var_1L_chr, patterns_ls = patterns_ls, 
        period_var_1L_chr = period_var_1L_chr)
    Y_Ready4useDyad <- serious::make_summary_ds(X_Ready4useDyad, 
        add_with_join_xx = Y_Ready4useDyad, join_before_dtm = join_before_dtm, 
        key_vars_chr = key_vars_chr, max_tenure_1L_dbl = max_tenure_1L_dbl, 
        patterns_ls = patterns_ls, separation_after_dbl = separation_after_dbl)
    Z_Ready4useDyad <- make_project_minutes_ds(processed_ls)
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", dplyr::left_join(Y_Ready4useDyad@ds_tb %>% 
        dplyr::select(-tidyselect::any_of(dplyr::ends_with("EpisodeEnd"))), 
        Z_Ready4useDyad@ds_tb %>% dplyr::select(-Minutes)))
    numerics_chr <- setdiff(Y_Ready4useDyad@ds_tb %>% dplyr::select(dplyr::where(is.numeric)) %>% 
        names(), c("Age", "days_to_start", "days_to_end", "days_to_first", 
        "days_to_last"))
    numerics_chr <- numerics_chr[!numerics_chr %>% endsWith("MISSING")]
    episode_separations_chr <- numerics_chr[numerics_chr %>% 
        endsWith("Episodes") | numerics_chr %>% endsWith("Separations")]
    others_chr <- setdiff(numerics_chr, episode_separations_chr)
    years_chr <- others_chr[others_chr %>% startsWith("Year")]
    main_chr <- setdiff(others_chr, years_chr)
    years_int <- years_chr %>% stringr::str_sub(start = 5, end = 5) %>% 
        as.numeric() %>% unique()
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(IntersectingYears = days_to_start %>% purrr::map2(days_to_end, 
            ~unique(c(ceiling(.x/365.25), ceiling(.y/365.25))))) %>% 
        dplyr::mutate(dplyr::across(tidyselect::all_of(episode_separations_chr), 
            ~dplyr::case_when(is.na(days_to_first) ~ NA_real_, 
                TRUE ~ .x))))
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", purrr::reduce(years_int, 
        .init = Y_Ready4useDyad@ds_tb, ~{
            year_1L_int <- .y
            .x %>% dplyr::mutate(dplyr::across(tidyselect::all_of(years_chr[years_chr %>% 
                startsWith(paste0("Year", year_1L_int))]), ~dplyr::case_when(IntersectingYears %>% 
                purrr::map_lgl(~{
                  year_1L_int %in% .x
                }) ~ .x, T ~ NA_real_)))
        }) %>% dplyr::select(-IntersectingYears))
    return(Y_Ready4useDyad)
}
#' Transform integer dates
#' @description transform_integer_dates() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform integer dates. The function returns Dates (a date vector).
#' @param dates_int Dates (an integer vector)
#' @return Dates (a date vector)
#' @rdname transform_integer_dates
#' @export 
#' @importFrom ready4use transform_dates
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#' @importFrom lubridate as_date
#' @keywords internal
transform_integer_dates <- function (dates_int) 
{
    if (!is.integer(dates_int)) {
        dates_dtm <- dates_int %>% ready4use::transform_dates()
    }
    else {
        dates_dtm <- dates_int %>% purrr::map_chr(~{
            date_1L_chr <- ifelse(.x < 10000000, paste0("0", 
                as.integer(.x)), as.integer(.x))
            paste0(stringr::str_sub(date_1L_chr, start = 5, end = 8), 
                "-", stringr::str_sub(date_1L_chr, start = 3, 
                  end = 4), "-", stringr::str_sub(date_1L_chr, 
                  start = 1, end = 2))
        }) %>% lubridate::as_date()
    }
    return(dates_dtm)
}
#' Transform project 2 model dataset
#' @description transform_project_2_model_ds() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform project 2 model dataset. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param cut_off_date_1L_chr Cut off date (a character vector of length one)
#' @param intervention_1L_chr Intervention (a character vector of length one), Default: 'Intervention'
#' @param type_1L_chr Type (a character vector of length one), Default: c("has_iar", "representation")
#' @return Data (a tibble)
#' @rdname transform_project_2_model_ds
#' @export 
#' @importFrom dplyr filter mutate case_when
#' @importFrom lubridate years
#' @keywords internal
transform_project_2_model_ds <- function (data_tb, cut_off_date_1L_chr, intervention_1L_chr = "Intervention", 
    type_1L_chr = c("has_iar", "representation")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "has_iar") {
        data_tb <- data_tb %>% dplyr::filter(Intervention == 
            intervention_1L_chr) %>% dplyr::filter(MatureService) %>% 
            dplyr::mutate(HasIAR = dplyr::case_when(HasIAR ~ 
                "Has an IAR", !HasIAR ~ "Does not have an IAR", 
                T ~ NA_character_))
    }
    if (type_1L_chr == "representation") {
        data_tb <- data_tb %>% dplyr::mutate(DaysToYearOneRepresentation = dplyr::case_when(DaysToYearOneRepresentation == 
            0 ~ 1, OneYearCutOffDate >= as.Date(cut_off_date_1L_chr) ~ 
            NA_real_, is.na(DaysToYearOneRepresentation) ~ 0, 
            T ~ DaysToYearOneRepresentation)) %>% dplyr::mutate(DaysSinceIndexService = as.numeric(last_service_date - 
            (OneYearCutOffDate - lubridate::years(1)))) %>% dplyr::filter(DaysSinceIndexService < 
            366) %>% dplyr::filter(!is.na(DaysToYearOneRepresentation)) %>% 
            dplyr::filter(YearOne)
    }
    return(data_tb)
}
#' Transform project outcomes dataset
#' @description transform_project_outcomes_ds() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform project outcomes dataset. The function returns Outcomes (an output object of multiple potential types).
#' @param outcomes_xx Outcomes (an output object of multiple potential types)
#' @param transform_gender_1L_lgl Transform gender (a logical vector of length one), Default: F
#' @param follow_up_1L_int Follow up (an integer vector of length one), Default: 12
#' @param minutes_chr Minutes (a character vector), Default: c("direct_mins", "indirect_mins", "Minutes")
#' @return Outcomes (an output object of multiple potential types)
#' @rdname transform_project_outcomes_ds
#' @export 
#' @importFrom serious transform_data_fmt
#' @importFrom dplyr select rename mutate rename_with filter inner_join across where left_join case_when
#' @importFrom tidyselect any_of
#' @importFrom tidyr pivot_wider any_of
#' @importFrom stringr str_detect str_remove_all str_extract_all str_replace str_remove
#' @importFrom purrr map2_int map2_chr reduce map2_dfr map_dfr
#' @importFrom rlang sym
#' @importFrom ready4use renew.ready4use_dictionary
#' @keywords internal
transform_project_outcomes_ds <- function (outcomes_xx, transform_gender_1L_lgl = F, follow_up_1L_int = 12, 
    minutes_chr = c("direct_mins", "indirect_mins", "Minutes")) 
{
    X_Ready4useDyad <- outcomes_xx %>% serious::transform_data_fmt(type_1L_chr = "input")
    if (!identical(intersect(names(X_Ready4useDyad@ds_tb), minutes_chr), 
        character(0))) {
        minutes_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(tidyselect::any_of(c("UID", 
            "MeasurementWeek", minutes_chr))) %>% tidyr::pivot_wider(names_from = "MeasurementWeek", 
            values_from = minutes_chr)
        weeks_chr <- names(minutes_tb)[names(minutes_tb) %>% 
            stringr::str_detect("\\_Week-+\\d+") | names(minutes_tb) %>% 
            stringr::str_detect("\\_Week+\\d+")]
        starts_chr <- weeks_chr %>% stringr::str_remove_all("\\_Week-+\\d+") %>% 
            stringr::str_remove_all("\\_Week+\\d+")
        weeks_int <- weeks_chr %>% stringr::str_extract_all("\\-+\\d+") %>% 
            purrr::map2_int(weeks_chr %>% stringr::str_remove_all("\\-+\\d+") %>% 
                stringr::str_extract_all("\\d+"), ~c(.x, .y) %>% 
                as.integer)
        ref_1L_int <- min(weeks_int)
        replacements_chr <- starts_chr %>% purrr::map2_chr(weeks_int, 
            ~ifelse(.y == ref_1L_int, .x, paste0(.x, "_", .y, 
                "_Weeks")))
        unchanged_chr <- intersect(starts_chr, replacements_chr) %>% 
            sort()
        changed_chr <- paste0(unchanged_chr, "_change")
        renamed_chr <- setdiff(replacements_chr, starts_chr) %>% 
            sort()
        changed_int <- which(replacements_chr %in% renamed_chr)
        minutes_tb <- purrr::reduce(1:length(replacements_chr), 
            .init = minutes_tb, ~{
                change_1L_lgl <- .y %in% changed_int
                ds_tb <- .x
                if (change_1L_lgl) {
                  ds_tb <- dplyr::rename(ds_tb, `:=`(!!rlang::sym(paste0(starts_chr[.y], 
                    "_change")), !!rlang::sym(weeks_chr[.y])))
                }
                else {
                  ds_tb <- dplyr::rename(ds_tb, `:=`(!!rlang::sym(paste0(starts_chr[.y], 
                    "_", weeks_int[.y], "_Weeks")), !!rlang::sym(weeks_chr[.y])))
                }
                ds_tb
            })
        minutes_tb <- starts_chr %>% unique() %>% sort() %>% 
            purrr::reduce(.init = minutes_tb, ~dplyr::mutate(.x, 
                `:=`(!!rlang::sym(paste0(.y, "_", max(weeks_int), 
                  "_Weeks")), !!rlang::sym(paste0(.y, "_", ref_1L_int, 
                  "_Weeks")) + !!rlang::sym(paste0(.y, "_change")))))
    }
    else {
        minutes_tb <- NULL
    }
    outcomes_tb <- X_Ready4useDyad@ds_tb
    additions_tb <- outcomes_tb %>% dplyr::select(c("UID", "MeasurementWeek", 
        "AQoL6D", "AQoL6D_change", "AQoL6D_QALYs", "CHU9D_change", 
        "CHU9D_QALYs", "k10_change", "gad7_change", "phq9_change", 
        "treatment_status", "treatment_change")) %>% tidyr::pivot_wider(names_from = "MeasurementWeek", 
        values_from = c("AQoL6D", "AQoL6D_change", "AQoL6D_QALYs", 
            "CHU9D_change", "CHU9D_QALYs", "k10_change", "gad7_change", 
            "phq9_change", "treatment_status", "treatment_change")) %>% 
        dplyr::select(c("UID", paste0(c("AQoL6D_change_Week", 
            "AQoL6D_QALYs_Week", "CHU9D_change_Week", "CHU9D_QALYs_Week", 
            "k10_change_Week", "gad7_change_Week", "phq9_change_Week", 
            "treatment_status_Week", "treatment_change_Week"), 
            follow_up_1L_int)))
    additions_tb <- additions_tb %>% dplyr::rename_with(~stringr::str_replace(., 
        paste0("_Week", follow_up_1L_int), ""), setdiff(names(additions_tb), 
        "UID")) %>% dplyr::rename(treatment_status_t2 = treatment_status)
    tfd_outcomes_tb <- outcomes_tb %>% dplyr::select(-c(AQoL6D_change, 
        AQoL6D_QALYs, CHU9D_change, CHU9D_QALYs, k10_change, 
        gad7_change, phq9_change, treatment_change)) %>% dplyr::filter(MeasurementWeek != 
        paste0("Week", follow_up_1L_int)) %>% dplyr::select(-MeasurementWeek) %>% 
        dplyr::inner_join(additions_tb) %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), 
        as.factor)) %>% dplyr::select(tidyr::any_of(c("UID", 
        "Date", "treatment_status", "treatment_status_t2", "treatment_change", 
        "platform", "clinic_type", "clinic_state", "Age", "employment_status", 
        "gender", "gad7", "gad7_change", "k10", "k10_change", 
        "phq9", "phq9_change", "AQoL6D", "AQoL6D_change", "AQoL6D_QALYs", 
        "CHU9D", "CHU9D_change", "CHU9D_QALYs")))
    if (transform_gender_1L_lgl) {
        tfd_outcomes_tb <- tfd_outcomes_tb %>% update_gender()
    }
    X_Ready4useDyad@ds_tb <- tfd_outcomes_tb
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0("AQoL6D_", follow_up_1L_int, 
            "_Weeks")), (AQoL6D + AQoL6D_change) %>% as.double()), 
            `:=`(!!rlang::sym(paste0("CHU9D_", follow_up_1L_int, 
                "_Weeks")), (CHU9D + CHU9D_change) %>% as.double()), 
            `:=`(!!rlang::sym(paste0("gad7_", follow_up_1L_int, 
                "_Weeks")), (gad7 + gad7_change) %>% as.integer()), 
            `:=`(!!rlang::sym(paste0("k10_", follow_up_1L_int, 
                "_Weeks")), (k10 + k10_change) %>% as.integer()), 
            `:=`(!!rlang::sym(paste0("phq9_", follow_up_1L_int, 
                "_Weeks")), (phq9 + phq9_change) %>% as.integer())))
    if (!is.null(minutes_tb)) {
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::left_join(minutes_tb)
        new_r3 <- unchanged_chr %>% purrr::map2_dfr(renamed_chr, 
            ~{
                week_1L_chr <- stringr::str_remove(.y, paste0(.x, 
                  "_"))
                X_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr == 
                  .x) %>% dplyr::mutate(var_nm_chr = paste0(var_nm_chr, 
                  "_", week_1L_chr), var_desc_chr = paste0(var_desc_chr, 
                  " (Cumulative up to end of most recent period - ending ", 
                  week_1L_chr %>% stringr::str_replace("_", " "), 
                  ")"))
            }) %>% rbind(unchanged_chr %>% purrr::map_dfr(~X_Ready4useDyad@dictionary_r3 %>% 
            dplyr::filter(var_nm_chr == .x) %>% dplyr::mutate(var_nm_chr = paste0(var_nm_chr, 
            "_change"), var_desc_chr = paste0(var_desc_chr, " (During most recent period)"))))
        X_Ready4useDyad@dictionary_r3 <- ready4use::renew.ready4use_dictionary(X_Ready4useDyad@dictionary_r3 %>% 
            dplyr::filter(!var_nm_chr %in% minutes_chr) %>% dplyr::mutate(var_desc_chr = dplyr::case_when(var_nm_chr %in% 
            unchanged_chr ~ paste0(var_desc_chr, " (Cumulative up to start of most recent period)"), 
            T ~ var_nm_chr)), new_cases_r3 = new_r3)
    }
    X_Ready4useDyad <- X_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(treatment_change = as.factor(as.character(treatment_change))))
    outcomes_xx <- outcomes_xx %>% serious::transform_data_fmt(X_Ready4useDyad = X_Ready4useDyad, 
        type_1L_chr = "output")
    return(outcomes_xx)
}
#' Transform to long results
#' @description transform_to_long_results() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to long results. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param var_1L_chr Variable (a character vector of length one)
#' @param add_means_1L_lgl Add means (a logical vector of length one), Default: TRUE
#' @param tidy_1L_lgl Tidy (a logical vector of length one), Default: TRUE
#' @return X (A dataset and data dictionary pair.)
#' @rdname transform_to_long_results
#' @export 
#' @importFrom purrr map reduce
#' @importFrom stringr str_remove
#' @importFrom dplyr filter mutate select everything
#' @importFrom rlang sym
#' @importFrom tidyr all_of
#' @keywords internal
transform_to_long_results <- function (X_Ready4useDyad, var_1L_chr, add_means_1L_lgl = TRUE, 
    tidy_1L_lgl = TRUE) 
{
    names_chr <- setdiff(names(X_Ready4useDyad@ds_tb)[names(X_Ready4useDyad@ds_tb) %>% 
        startsWith(paste0(var_1L_chr, "_sim_"))], paste0(var_1L_chr, 
        "_sim_mean"))
    if (add_means_1L_lgl) {
        names_chr <- c(paste0(var_1L_chr, "_sim_mean"), names_chr)
    }
    bind_ls <- names_chr %>% purrr::map(~{
        iteration_1L_dbl <- ifelse(.x == paste0(var_1L_chr, "_sim_mean"), 
            0, stringr::str_remove(.x, paste0(var_1L_chr, "_sim_")) %>% 
                as.numeric())
        ds_tb <- X_Ready4useDyad@ds_tb
        if ("Iteration" %in% names(ds_tb)) {
            ds_tb <- ds_tb %>% dplyr::filter(Iteration == iteration_1L_dbl)
        }
        ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            !!rlang::sym(.x)), Iteration = iteration_1L_dbl)
    })
    if (length(bind_ls) > 1) {
        X_Ready4useDyad@ds_tb <- purrr::reduce(bind_ls[-1], .init = bind_ls[[1]], 
            ~rbind(.x, .y))
    }
    else {
        X_Ready4useDyad@ds_tb <- bind_ls[[1]]
    }
    if (tidy_1L_lgl) {
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(-tidyr::all_of(names_chr))
    }
    X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(Iteration, 
        dplyr::everything())
    X_Ready4useDyad <- X_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
        type_1L_chr = "update")
    return(X_Ready4useDyad)
}
#' Transform to minimum and maximum
#' @description transform_to_min_and_max() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to minimum and maximum. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param vars_chr Variables (a character vector)
#' @param max_1L_dbl Maximum (a double vector of length one), Default: 0.999999
#' @param min_1L_dbl Minimum (a double vector of length one), Default: 0.000001
#' @return Y (A dataset and data dictionary pair.)
#' @rdname transform_to_min_and_max
#' @export 
#' @importFrom dplyr mutate across
#' @importFrom purrr map_dbl
#' @keywords internal
transform_to_min_and_max <- function (X_Ready4useDyad, vars_chr, max_1L_dbl = 0.999999, min_1L_dbl = 0.000001) 
{
    Y_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::mutate(dplyr::across(vars_chr, ~.x %>% purrr::map_dbl(~min(max(.x, 
            0.000001), 0.999999)))))
    return(Y_Ready4useDyad)
}
#' Transform to remove duplicates
#' @description transform_to_remove_duplicates() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to remove duplicates. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param group_by_chr Group by (a character vector), Default: c("episode_key", "collection_occasion_date", "reason_for_collection")
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'episode_key'
#' @return Data (a tibble)
#' @rdname transform_to_remove_duplicates
#' @export 
#' @importFrom dplyr sample_n ungroup pull group_by across mutate n case_when filter select bind_rows
#' @importFrom rlang sym
#' @importFrom tidyr all_of
#' @keywords internal
transform_to_remove_duplicates <- function (data_tb, group_by_chr = c("episode_key", "collection_occasion_date", 
    "reason_for_collection"), uid_1L_chr = "episode_key") 
{
    duplicates_tb <- get_duplicated_measures(data_tb, group_by_chr = group_by_chr, 
        uid_1L_chr = uid_1L_chr)
    duplicates_tb <- duplicates_tb %>% dplyr::sample_n(size = 1) %>% 
        dplyr::ungroup()
    keys_chr <- duplicates_tb %>% dplyr::pull(!!rlang::sym(uid_1L_chr))
    data_tb <- data_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(group_by_chr))) %>% 
        dplyr::mutate(N = dplyr::n()) %>% dplyr::ungroup() %>% 
        dplyr::mutate(Duplicate = !!rlang::sym(uid_1L_chr) %in% 
            keys_chr) %>% dplyr::mutate(Duplicate = dplyr::case_when(N == 
        1 ~ FALSE, T ~ Duplicate)) %>% dplyr::filter(!Duplicate) %>% 
        dplyr::select(-c(Duplicate, N)) %>% dplyr::bind_rows(duplicates_tb %>% 
        dplyr::select(-N))
    return(data_tb)
}
#' Transform treatment factor
#' @description transform_tx_factor() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform treatment factor. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param treatment_vars_chr Treatment variables (a character vector), Default: c("treatment_status", "treatment_status_t2")
#' @param what_1L_chr What (a character vector of length one), Default: c("Waitlist", "Treatment", "Discharged")
#' @return Data (a tibble)
#' @rdname transform_tx_factor
#' @export 
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @keywords internal
transform_tx_factor <- function (data_tb, treatment_vars_chr = c("treatment_status", 
    "treatment_status_t2"), what_1L_chr = c("Waitlist", "Treatment", 
    "Discharged")) 
{
    data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(treatment_vars_chr[1]) == 
        what_1L_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(treatment_vars_chr[2]), 
        as.factor(as.character(!!rlang::sym(treatment_vars_chr[2])))))
    return(data_tb)
}
#' Transform treatment modelling dataset
#' @description transform_tx_mdlng_ds() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform treatment modelling dataset. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param treatment_vars_chr Treatment variables (a character vector), Default: c("treatment_status", "treatment_status_t2")
#' @param what_1L_chr What (a character vector of length one), Default: c("Waitlist", "Treatment", "Discharged")
#' @return Data (a tibble)
#' @rdname transform_tx_mdlng_ds
#' @export 
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @keywords internal
transform_tx_mdlng_ds <- function (data_tb, treatment_vars_chr = c("treatment_status", 
    "treatment_status_t2"), what_1L_chr = c("Waitlist", "Treatment", 
    "Discharged")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(treatment_vars_chr[1]) == 
        what_1L_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(treatment_vars_chr[2]), 
        as.factor(as.character(!!rlang::sym(treatment_vars_chr[2])))))
    return(data_tb)
}
