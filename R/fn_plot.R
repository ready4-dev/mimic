#' Plot pooled
#' @description plot_pooled() is a Plot function that plots data. Specifically, this function implements an algorithm to plot pooled. The function returns Plot (a plot).
#' @param pooled_fits_ls Pooled fits (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param distributions_chr Distributions (a character vector), Default: 'best'
#' @param pool_1L_lgl Pool (a logical vector of length one), Default: TRUE
#' @param ... Additional arguments
#' @return Plot (a plot)
#' @rdname plot_pooled
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom SHELF plotfit
#' @importFrom purrr pluck
#' @keywords internal
plot_pooled <- function (pooled_fits_ls, what_1L_chr, distributions_chr = "best", 
    pool_1L_lgl = TRUE, ...) 
{
    assertthat::assert_that(what_1L_chr %in% names(pooled_fits_ls))
    plt <- SHELF::plotfit(pooled_fits_ls %>% purrr::pluck(what_1L_chr), 
        lp = pool_1L_lgl, returnPlot = T, showPlot = F, d = distributions_chr, 
        ...)
    return(plt)
}
#' Plot test scatter
#' @description plot_test_scatter() is a Plot function that plots data. Specifically, this function implements an algorithm to plot test scatter. The function returns Scatter (a plot).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param collapse_1L_lgl Collapse (a logical vector of length one), Default: F
#' @param grouping_1L_chr Grouping (a character vector of length one), Default: character(0)
#' @param var_1L_chr Variable (a character vector of length one)
#' @param old_1L_chr Old (a character vector of length one), Default: 'Observed'
#' @param type_1L_chr Type (a character vector of length one), Default: c("Simulated", "Predicted")
#' @return Scatter (a plot)
#' @rdname plot_test_scatter
#' @export 
#' @importFrom dplyr select filter mutate distinct pull
#' @importFrom tidyr any_of pivot_wider
#' @importFrom rlang sym
#' @importFrom purrr map_dfr
#' @importFrom tune coord_obs_pred
#' @keywords internal
plot_test_scatter <- function (X_Ready4useDyad, collapse_1L_lgl = F, grouping_1L_chr = character(0), 
    var_1L_chr, old_1L_chr = "Observed", type_1L_chr = c("Simulated", 
        "Predicted")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::select(tidyr::any_of(c("UID", "Data", grouping_1L_chr, 
            var_1L_chr, paste0(var_1L_chr, "_sim_mean")))))
    if (collapse_1L_lgl) {
        X_Ready4useDyad@ds_tb <- rbind(X_Ready4useDyad@ds_tb %>% 
            dplyr::filter(Data != type_1L_chr), X_Ready4useDyad@ds_tb %>% 
            dplyr::filter(Data == type_1L_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
            !!rlang::sym(paste0(var_1L_chr, "_sim_mean"))))) %>% 
            dplyr::distinct()
    }
    if (identical(grouping_1L_chr, character(0))) {
        new_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(c("UID", 
            "Data", var_1L_chr)) %>% tidyr::pivot_wider(names_from = "Data", 
            values_from = var_1L_chr)
    }
    else {
        values_xx <- X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(grouping_1L_chr)) %>% 
            unique() %>% sort()
        if ("factor" %in% class(values_xx)) {
            values_xx <- as.character(values_xx)
            tfmn_fn <- as.character
        }
        else {
            tfmn_fn <- identity
        }
        new_tb <- values_xx %>% purrr::map_dfr(~X_Ready4useDyad@ds_tb %>% 
            dplyr::filter(tfmn_fn(!!rlang::sym(grouping_1L_chr)) == 
                .x) %>% dplyr::select(c("UID", "Data", grouping_1L_chr, 
            var_1L_chr)) %>% tidyr::pivot_wider(names_from = "Data", 
            values_from = var_1L_chr))
    }
    scatter_plt <- depict(renewSlot(X_Ready4useDyad, "ds_tb", 
        new_tb), x_vars_chr = type_1L_chr, y_vars_chr = old_1L_chr, 
        as_percent_1L_lgl = F, drop_missing_1L_lgl = T, what_1L_chr = "scatter") + 
        tune::coord_obs_pred()
    return(scatter_plt)
}
#' Plot treatment model confusion
#' @description plot_tx_mdl_confusion() is a Plot function that plots data. Specifically, this function implements an algorithm to plot treatment model confusion. The function returns Confusion (a plot).
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param tx_mdls_ls Treatment models (a list)
#' @param model_1L_int Model (an integer vector of length one)
#' @param high_1L_chr High (a character vector of length one), Default: '#2E86C1'
#' @param low_1L_chr Low (a character vector of length one), Default: '#D6EAF8'
#' @param treatment_vars_chr Treatment variables (a character vector), Default: c("treatment_status", "treatment_status_t2")
#' @param what_1L_chr What (a character vector of length one), Default: c("Waitlist", "Treatment", "Discharged")
#' @return Confusion (a plot)
#' @rdname plot_tx_mdl_confusion
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom purrr pluck
#' @importFrom yardstick conf_mat
#' @importFrom stats predict
#' @importFrom dplyr rename mutate pull
#' @importFrom rlang sym
#' @importFrom ggplot2 autoplot scale_fill_gradient
#' @keywords internal
plot_tx_mdl_confusion <- function (X_Ready4useDyad = ready4use::Ready4useDyad(), tx_mdls_ls, 
    model_1L_int, high_1L_chr = "#2E86C1", low_1L_chr = "#D6EAF8", 
    treatment_vars_chr = c("treatment_status", "treatment_status_t2"), 
    what_1L_chr = c("Waitlist", "Treatment", "Discharged")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    model_mdl <- tx_mdls_ls[[paste0(what_1L_chr, "_ls")]] %>% 
        purrr::pluck(model_1L_int)
    data_tb <- X_Ready4useDyad@ds_tb %>% transform_tx_factor(treatment_vars_chr = treatment_vars_chr, 
        what_1L_chr = what_1L_chr)
    confusion_plt <- yardstick::conf_mat(stats::predict(model_mdl, 
        data_tb) %>% dplyr::rename(Predicted = .pred_class) %>% 
        dplyr::mutate(Observed = data_tb %>% dplyr::pull(!!rlang::sym(treatment_vars_chr[2]))), 
        Observed, Predicted, dnn = c("Predicted", "Observed")) %>% 
        ggplot2::autoplot(type = "heatmap") + ggplot2::scale_fill_gradient(low = low_1L_chr, 
        high = high_1L_chr)
    return(confusion_plt)
}
