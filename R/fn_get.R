#' Get duplicated measures
#' @description get_duplicated_measures() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get duplicated measures. The function returns Duplicates (an output object of multiple potential types).
#' @param data_tb Data (a tibble)
#' @param group_by_chr Group by (a character vector)
#' @param type_1L_chr Type (a character vector of length one), Default: c("table", "uid")
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'episode_key'
#' @param ungroup_1L_lgl Ungroup (a logical vector of length one), Default: FALSE
#' @return Duplicates (an output object of multiple potential types)
#' @rdname get_duplicated_measures
#' @export 
#' @importFrom dplyr group_by across mutate n filter ungroup pull
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
#' @keywords internal
get_duplicated_measures <- function (data_tb, group_by_chr, type_1L_chr = c("table", "uid"), 
    uid_1L_chr = "episode_key", ungroup_1L_lgl = FALSE) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    duplicates_xx <- data_tb %>% dplyr::group_by(dplyr::across(tidyselect::all_of(group_by_chr))) %>% 
        dplyr::mutate(N = dplyr::n()) %>% dplyr::filter(N >= 
        2)
    if (ungroup_1L_lgl) {
        duplicates_xx <- dplyr::ungroup(duplicates_xx)
    }
    if (type_1L_chr == "uid") {
        duplicates_xx <- duplicates_xx %>% dplyr::pull(!!rlang::sym(uid_1L_chr)) %>% 
            unique()
    }
    return(duplicates_xx)
}
#' Get pooled
#' @description get_pooled() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get pooled. The function returns Predictions (an output object of multiple potential types).
#' @param pooled_ls Pooled (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param as_1L_chr As (a character vector of length one), Default: c("vector", "histogram", "summary")
#' @param n_1L_int N (an integer vector of length one), Default: 5000
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param resample_1L_lgl Resample (a logical vector of length one), Default: TRUE
#' @return Predictions (an output object of multiple potential types)
#' @rdname get_pooled
#' @export 
#' @keywords internal
get_pooled <- function (pooled_ls, what_1L_chr, as_1L_chr = c("vector", "histogram", 
    "summary"), n_1L_int = 5000, seed_1L_int = 2001L, resample_1L_lgl = TRUE) 
{
    as_1L_chr <- match.arg(as_1L_chr)
    pooled_mdl <- pooled_ls[[what_1L_chr]]$model_ls
    args_ls <- pooled_ls[[what_1L_chr]]$arguments_ls
    predictions_xx <- predict_from_pool(pooled_mdl, adjustment_1L_dbl = args_ls$adjustment_1L_dbl, 
        as_1L_chr = as_1L_chr, distributions_chr = args_ls$distributions_chr, 
        n_1L_int = n_1L_int, seed_1L_int = seed_1L_int, resample_1L_lgl = resample_1L_lgl, 
        what_1L_chr = what_1L_chr)
    return(predictions_xx)
}
#' Get private keys list
#' @description get_private_keys_ls() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get private keys list. The function returns Private keys (an output object of multiple potential types).
#' @param path_to_keys_1L_chr Path to keys (a character vector of length one)
#' @param divider_1L_chr Divider (a character vector of length one), Default: '//'
#' @param what_1L_chr What (a character vector of length one), Default: c("data", "names")
#' @return Private keys (an output object of multiple potential types)
#' @rdname get_private_keys_ls
#' @export 
#' @importFrom stringr str_remove_all
#' @keywords internal
get_private_keys_ls <- function (path_to_keys_1L_chr, divider_1L_chr = "//", what_1L_chr = c("data", 
    "names")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    files_chr <- list.files(path_to_keys_1L_chr)
    names_chr <- files_chr %>% stringr::str_remove_all(".RDS")
    if (what_1L_chr == "names") {
        private_keys_xx <- names_chr
    }
    else {
        private_keys_xx <- list(program_services_lup = readRDS(paste0(path_to_keys_1L_chr, 
            divider_1L_chr, "program_services_lup.RDS")), provider_lup_tb = readRDS(paste0(path_to_keys_1L_chr, 
            divider_1L_chr, "provider_lup.RDS")))
    }
    return(private_keys_xx)
}
#' Get project model data
#' @description get_project_model_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get project model data. The function is called for its side effects and does not return a value.
#' @param model_data_ls Model data (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param type_1L_chr Type (a character vector of length one), Default: c("imputed", "unimputed")
#' @return X (A dataset and data dictionary pair.)
#' @rdname get_project_model_data
#' @export 
#' @importFrom purrr pluck
#' @importFrom assertthat assert_that
#' @keywords internal
get_project_model_data <- function (model_data_ls, what_1L_chr, type_1L_chr = c("imputed", 
    "unimputed")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    data_ls <- model_data_ls %>% purrr::pluck(paste0(type_1L_chr, 
        "_ls"))
    assertthat::assert_that(paste0(what_1L_chr, "_r4") %in% names(data_ls))
    X_Ready4useDyad <- data_ls %>% purrr::pluck(paste0(what_1L_chr, 
        "_r4"))
    return(X_Ready4useDyad)
}
#' Get raw Minimum Dataset data
#' @description get_raw_mds_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get raw minimum dataset data. The function returns a Minimum Dataset data (an output object of multiple potential types).
#' @param path_to_raw_dir_1L_chr Path to raw directory (a character vector of length one)
#' @param divider_1L_chr Divider (a character vector of length one), Default: '\'
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param select_chr Select (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("csv", "rds")
#' @param what_1L_chr What (a character vector of length one), Default: c("data", "names")
#' @return a Minimum Dataset data (an output object of multiple potential types)
#' @rdname get_raw_mds_data
#' @export 
#' @importFrom stringr str_remove_all str_replace_all
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
get_raw_mds_data <- function (path_to_raw_dir_1L_chr, divider_1L_chr = "\\", r_dir_1L_chr = "R", 
    select_chr = character(0), type_1L_chr = c("csv", "rds"), 
    what_1L_chr = c("data", "names")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    path_to_dir_1L_chr <- paste0(path_to_raw_dir_1L_chr, divider_1L_chr, 
        r_dir_1L_chr)
    files_chr <- list.files(path_to_dir_1L_chr)
    if (type_1L_chr == "csv") {
        names_chr <- files_chr %>% stringr::str_remove_all(".csv")
    }
    else {
        names_chr <- files_chr %>% stringr::str_remove_all(".RDS")
    }
    if (what_1L_chr == "names") {
        mds_data_xx <- names_chr
    }
    else {
        if (!identical(select_chr, character(0))) {
            select_int <- which(names_chr %in% select_chr)
        }
        else {
            select_int <- 1:length(names_chr)
        }
        mds_data_xx <- files_chr[select_int] %>% purrr::map(~{
            path_1L_chr <- paste0(path_to_dir_1L_chr, "/", .x)
            if (type_1L_chr == "csv") {
                read.csv2(path_1L_chr, , header = T, sep = ",")
            }
            else {
                readRDS(path_1L_chr)
            }
        }) %>% stats::setNames(names_chr[select_int] %>% stringr::str_replace_all("-", 
            "_"))
    }
    return(mds_data_xx)
}
#' Get raw parameters data
#' @description get_raw_params_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get raw parameters data. The function returns Raw parameters data (a list).
#' @param path_to_param_data_1L_chr Path to parameter data (a character vector of length one)
#' @param program_fl_nm_1L_chr Program file name (a character vector of length one)
#' @param unit_cost_fl_nm_1L_chr Unit cost file name (a character vector of length one)
#' @param cost_types_chr Cost types (a character vector), Default: character(0)
#' @param divider_1L_chr Divider (a character vector of length one), Default: '\'
#' @param erp_fl_nm_1L_chr Estimatedesident Population file name (a character vector of length one), Default: character(0)
#' @param mbs_fl_nm_1L_chr Medicare Benefits Schedule file name (a character vector of length one), Default: character(0)
#' @param mbs_sheet_1L_chr Medicare Benefits Schedule sheet (a character vector of length one), Default: 'Table EXP.14'
#' @param mbs_skip_1L_int Medicare Benefits Schedule skip (an integer vector of length one), Default: 4
#' @param no_care_fl_nm_1L_chr No care file name (a character vector of length one), Default: 'HILDA_K10.RDS'
#' @param program_sheet_1L_chr Program sheet (a character vector of length one)
#' @param program_skip_1L_int Program skip (an integer vector of length one), Default: 0
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return Raw parameters data (a list)
#' @rdname get_raw_params_data
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
get_raw_params_data <- function (path_to_param_data_1L_chr, program_fl_nm_1L_chr, unit_cost_fl_nm_1L_chr, 
    cost_types_chr = character(0), divider_1L_chr = "\\", erp_fl_nm_1L_chr = character(0), 
    mbs_fl_nm_1L_chr = character(0), mbs_sheet_1L_chr = "Table EXP.14", 
    mbs_skip_1L_int = 4, no_care_fl_nm_1L_chr = "HILDA_K10.RDS", 
    program_sheet_1L_chr, program_skip_1L_int = 0, r_dir_1L_chr = "R") 
{
    if (identical(erp_fl_nm_1L_chr, character(0))) {
        erp_fl_nm_1L_chr <- "LGA_pop_ppl_DbR_Nov24_-8778602758636367154.csv"
    }
    raw_erp_tb <- read.csv(paste0(path_to_param_data_1L_chr, 
        divider_1L_chr, erp_fl_nm_1L_chr))
    program_expenditure_tb <- readxl::read_xlsx(paste0(path_to_param_data_1L_chr, 
        "/", program_fl_nm_1L_chr), sheet = program_sheet_1L_chr, 
        skip = program_skip_1L_int)
    if (identical(mbs_fl_nm_1L_chr, character(0))) {
        mbs_fl_nm_1L_chr <- "Expenditure on mental health services 2022-23.xlsx"
    }
    mbs_expenditure_tb <- readxl::read_xlsx(paste0(path_to_param_data_1L_chr, 
        "/", mbs_fl_nm_1L_chr), sheet = mbs_sheet_1L_chr, skip = mbs_skip_1L_int)
    if (identical(cost_types_chr, character(0))) {
        cost_types_chr <- c("Allied health costs", "Psychologist costs", 
            "psychaitrist costs", "GP costs")
    }
    unit_cost_tb_ls <- cost_types_chr %>% purrr::map(~readxl::read_xlsx(paste0(path_to_param_data_1L_chr, 
        "/", unit_cost_fl_nm_1L_chr), sheet = .x)) %>% stats::setNames(cost_types_chr)
    no_care_K10_params_tb <- readRDS(paste0(path_to_param_data_1L_chr, 
        divider_1L_chr, no_care_fl_nm_1L_chr))
    raw_params_data_ls <- list(mbs_expenditure_tb = mbs_expenditure_tb, 
        program_expenditure_tb = program_expenditure_tb, no_care_K10_params_tb = no_care_K10_params_tb, 
        raw_erp_tb = raw_erp_tb, unit_cost_tb_ls = unit_cost_tb_ls)
    return(raw_params_data_ls)
}
#' Get regression
#' @description get_regression() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get regression. The function returns Model (an output object of multiple potential types).
#' @param regressions_ls Regressions (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param constrained_1L_lgl Constrained (a logical vector of length one), Default: logical(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param named_1L_lgl Named (a logical vector of length one), Default: FALSE
#' @param part_1L_int Part (an integer vector of length one), Default: integer(0)
#' @param report_1L_chr Report (a character vector of length one), Default: c("all", "check", "compare", "confusion", "density", "estimates", 
#'    "histogram", "scatter", "test")
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "assessments", "models", "tests")
#' @return Model (an output object of multiple potential types)
#' @rdname get_regression
#' @export 
#' @importFrom purrr pluck map
#' @importFrom assertthat assert_that
#' @importFrom stringr str_remove
#' @keywords internal
get_regression <- function (regressions_ls, what_1L_chr, constrained_1L_lgl = logical(0), 
    model_1L_int = integer(0), named_1L_lgl = FALSE, part_1L_int = integer(0), 
    report_1L_chr = c("all", "check", "compare", "confusion", 
        "density", "estimates", "histogram", "scatter", "test"), 
    type_1L_chr = c("candidates", "assessments", "models", "tests")) 
{
    report_1L_chr <- match.arg(report_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_ls <- regressions_ls %>% purrr::pluck(paste0(type_1L_chr, 
        "_ls"))
    if (!what_1L_chr %in% c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged")) {
        pick_1L_chr <- paste0(what_1L_chr, ifelse(type_1L_chr == 
            "models", "_mdl", "_ls"))
    }
    else {
        pick_1L_chr <- "Treatments_ls"
    }
    models_ls <- what_ls %>% purrr::pluck(pick_1L_chr)
    if (!identical(part_1L_int, integer(0))) {
        assertthat::assert_that(part_1L_int %in% 1:2)
        if (type_1L_chr %in% c("candidates", "models")) {
            if (type_1L_chr == "models") {
                models_ls <- list(models_ls)
            }
            models_ls <- purrr::map(models_ls, ~{
                if (inherits(.x, "twopartm")) {
                  if (part_1L_int == 1) {
                    .x@model_part1
                  }
                  else {
                    .x@model_part2
                  }
                }
                else {
                  .x
                }
            })
            if (type_1L_chr == "models") {
                models_ls <- models_ls[[1]]
            }
        }
        else {
            models_ls <- models_ls[[part_1L_int]]
        }
    }
    if (what_1L_chr %in% c("Tx_Waitlist", "Tx_Treatment", "Tx_Discharged")) {
        pick_1L_chr <- paste0(stringr::str_remove(what_1L_chr, 
            "Tx_"), "_ls")
        models_ls <- models_ls %>% purrr::pluck(pick_1L_chr)
    }
    if (identical(model_1L_int, integer(0)) | !type_1L_chr %in% 
        c("candidates")) {
        model_xx <- models_ls
    }
    else {
        if (named_1L_lgl) {
            model_xx <- models_ls[model_1L_int]
        }
        else {
            model_xx <- models_ls[[model_1L_int]]
        }
    }
    if (type_1L_chr == "assessments" & report_1L_chr != "all") {
        model_xx <- model_xx %>% purrr::pluck(c("check_plt", 
            "compare_df", "confusion_ls", "estimates_df", "test_df")[which(report_1L_chr == 
            c("check", "compare", "confusion", "estimates", "test"))])
    }
    if (type_1L_chr == "tests" & report_1L_chr != "all") {
        model_xx <- model_xx %>% purrr::pluck(c("density_ls", 
            "histogram_ls", "scatter_ls", "comparison_tb")[which(report_1L_chr == 
            c("density", "histogram", "scatter", "compare"))])
        if (!identical(constrained_1L_lgl, logical(0))) {
            model_xx <- model_xx[[ifelse(constrained_1L_lgl, 
                2, 1)]]
        }
    }
    return(model_xx)
}
#' Get timestamp
#' @description get_timestamp() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get timestamp. The function returns Timestamp (a character vector of length one).
#' @param sensitivities_ls Sensitivities (a list), Default: make_sensitivities_ls()
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: '_'
#' @param what_1L_chr What (a character vector of length one), Default: c("outcomes", "costs")
#' @return Timestamp (a character vector of length one)
#' @rdname get_timestamp
#' @export 
#' @importFrom purrr keep_at pluck
#' @keywords internal
get_timestamp <- function (sensitivities_ls = make_sensitivities_ls(), prefix_1L_chr = "_", 
    what_1L_chr = c("outcomes", "costs")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    names_chr <- sensitivities_ls %>% purrr::keep_at(paste0(what_1L_chr, 
        "_ls")) %>% purrr::pluck(1) %>% names()
    timestamp_1L_chr <- paste0(prefix_1L_chr, names_chr[nchar(names_chr) == 
        min(nchar(names_chr))])
    return(timestamp_1L_chr)
}
#' Get unit cost detail
#' @description get_unit_cost_detail() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get unit cost detail. The function returns Detail (an output object of multiple potential types).
#' @param unit_costs_tb Unit costs (a tibble)
#' @param what_1L_chr What (a character vector of length one), Default: c("scenarios", "fixed", "names", "variable")
#' @return Detail (an output object of multiple potential types)
#' @rdname get_unit_cost_detail
#' @export 
#' @importFrom purrr map_dbl
#' @importFrom ready4 get_from_lup_obj
#' @importFrom dplyr filter
#' @keywords internal
get_unit_cost_detail <- function (unit_costs_tb, what_1L_chr = c("scenarios", "fixed", 
    "names", "variable")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    scenarios_chr <- unit_costs_tb$Scenario %>% unique()
    base_1L_int <- which(scenarios_chr == "Base")
    scenarios_chr <- c(scenarios_chr[base_1L_int], scenarios_chr[-base_1L_int])
    if (what_1L_chr == "scenarios") {
        detail_xx <- scenarios_chr
    }
    if (what_1L_chr == "fixed") {
        detail_xx <- scenarios_chr %>% purrr::map_dbl(~ready4::get_from_lup_obj(unit_costs_tb %>% 
            dplyr::filter(Scenario == .x), match_value_xx = "Fixed", 
            match_var_nm_1L_chr = "Type", target_var_nm_1L_chr = "UnitCost"))
    }
    if (what_1L_chr == "names") {
        detail_xx <- paste0("Cost", c("", paste0("_", scenarios_chr[-1])))
    }
    if (what_1L_chr == "variable") {
        detail_xx <- scenarios_chr %>% purrr::map_dbl(~ready4::get_from_lup_obj(unit_costs_tb %>% 
            dplyr::filter(Scenario == .x), match_value_xx = "Variable", 
            match_var_nm_1L_chr = "Type", target_var_nm_1L_chr = "UnitCost"))
    }
    return(detail_xx)
}
