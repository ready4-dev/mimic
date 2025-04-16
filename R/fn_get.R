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
#' Get regression
#' @description get_regression() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get regression. The function returns Model (an output object of multiple potential types).
#' @param regressions_ls Regressions (a list)
#' @param constrained_1L_lgl Constrained (a logical vector of length one), Default: logical(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param named_1L_lgl Named (a logical vector of length one), Default: FALSE
#' @param part_1L_int Part (an integer vector of length one), Default: integer(0)
#' @param report_1L_chr Report (a character vector of length one), Default: c("all", "check", "compare", "confusion", "density", "estimates", 
#'    "histogram", "scatter", "test")
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "assessments", "models", "tests")
#' @param what_1L_chr What (a character vector of length one), Default: c("AQoL6D", "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
#'    "Tx_Treatment", "Tx_Discharged")
#' @return Model (an output object of multiple potential types)
#' @rdname get_regression
#' @export 
#' @importFrom purrr pluck map
#' @importFrom assertthat assert_that
#' @importFrom stringr str_remove
#' @keywords internal
get_regression <- function (regressions_ls, constrained_1L_lgl = logical(0), model_1L_int = integer(0), 
    named_1L_lgl = FALSE, part_1L_int = integer(0), report_1L_chr = c("all", 
        "check", "compare", "confusion", "density", "estimates", 
        "histogram", "scatter", "test"), type_1L_chr = c("candidates", 
        "assessments", "models", "tests"), what_1L_chr = c("AQoL6D", 
        "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist", 
        "Tx_Treatment", "Tx_Discharged")) 
{
    report_1L_chr <- match.arg(report_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    what_ls <- regressions_ls %>% purrr::pluck(paste0(type_1L_chr, 
        "_ls"))
    if (what_1L_chr %in% c("AQoL6D", "CHU9D", "K10", "Minutes", 
        "Treatments")) {
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
