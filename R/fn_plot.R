#' Plot economic results
#' @description plot_economic_results() is a Plot function that plots data. Specifically, this function implements an algorithm to plot economic results. The function returns Plot (a plot).
#' @param economic_results_ls Economic results (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param alpha_1L_dbl Alpha (a double vector of length one), Default: 0.8
#' @param colour_1L_chr Colour (a character vector of length one), Default: ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr = "monash_2")
#' @param currency_1L_chr Currency (a character vector of length one), Default: '$'
#' @param plot_tfmn_fn Plot transformation (a function), Default: identity
#' @param size_1L_dbl Size (a double vector of length one), Default: 3
#' @param threshold_1L_dbl Threshold (a double vector of length one), Default: 96000
#' @param title_1L_chr Title (a character vector of length one), Default: ' '
#' @param type_1L_chr Type (a character vector of length one), Default: c("cep", "ceac", "evi")
#' @param x_limits_dbl X limits (a double vector), Default: numeric(0)
#' @param ... Additional arguments
#' @return Plot (a plot)
#' @rdname plot_economic_results
#' @export 
#' @importFrom ready4use get_colour_codes
#' @importFrom purrr pluck
#' @importFrom BCEA ceac.plot ceplane.plot evi.plot
#' @importFrom ggplot2 labs scale_x_continuous expand_limits
#' @importFrom scales dollar_format
plot_economic_results <- function (economic_results_ls, what_1L_chr, alpha_1L_dbl = 0.8, 
    colour_1L_chr = ready4use::get_colour_codes(type_1L_chr = "unicol", 
        style_1L_chr = "monash_2"), currency_1L_chr = "$", plot_tfmn_fn = identity, 
    size_1L_dbl = 3, threshold_1L_dbl = 96000, title_1L_chr = " ", 
    type_1L_chr = c("cep", "ceac", "evi"), x_limits_dbl = numeric(0), 
    ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    data_xx <- economic_results_ls %>% purrr::pluck(what_1L_chr)
    if (type_1L_chr == "ceac") {
        plot_plt <- BCEA::ceac.plot(data_xx, graph = "ggplot2", 
            currency = currency_1L_chr, line = list(color = colour_1L_chr), 
            title = title_1L_chr, ...)
    }
    if (type_1L_chr == "cep") {
        plot_plt <- BCEA::ceplane.plot(data_xx, graph = "ggplot2", 
            wtp = threshold_1L_dbl, currency = currency_1L_chr, 
            point = list(color = colour_1L_chr, size = size_1L_dbl, 
                alpha = alpha_1L_dbl), title = title_1L_chr, 
            ...)
    }
    if (type_1L_chr == "evi") {
        plot_plt <- BCEA::evi.plot(data_xx, graph = "ggplot2", 
            line_colors = colour_1L_chr, ...) + ggplot2::labs(title = title_1L_chr)
        if (!identical(currency_1L_chr, character(0))) {
            plot_plt <- plot_plt + ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = currency_1L_chr))
        }
    }
    if (!identical(x_limits_dbl, numeric(0))) {
        plot_plt <- plot_plt + ggplot2::expand_limits(x = x_limits_dbl)
    }
    plot_plt <- plot_tfmn_fn(plot_plt)
    return(plot_plt)
}
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
plot_pooled <- function (pooled_fits_ls, what_1L_chr, distributions_chr = "best", 
    pool_1L_lgl = TRUE, ...) 
{
    assertthat::assert_that(what_1L_chr %in% names(pooled_fits_ls))
    plt <- SHELF::plotfit(pooled_fits_ls %>% purrr::pluck(what_1L_chr), 
        lp = pool_1L_lgl, returnPlot = T, showPlot = F, d = distributions_chr, 
        ...)
    return(plt)
}
#' Plot regression
#' @description plot_regression() is a Plot function that plots data. Specifically, this function implements an algorithm to plot regression. The function returns Plot (a plot).
#' @param regressions_ls Regressions (a list)
#' @param what_1L_chr What (a character vector of length one)
#' @param close_1L_lgl Close (a logical vector of length one), Default: TRUE
#' @param colors_chr Colors (a character vector), Default: ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr = "monash_2")
#' @param constrained_1L_lgl Constrained (a logical vector of length one), Default: logical(0)
#' @param model_1L_int Model (an integer vector of length one), Default: integer(0)
#' @param named_1L_lgl Named (a logical vector of length one), Default: FALSE
#' @param part_1L_int Part (an integer vector of length one), Default: integer(0)
#' @param plot_tfmn_fn Plot transformation (a function), Default: identity
#' @param report_1L_chr Report (a character vector of length one), Default: c("all", "check", "compare", "confusion", "density", "estimates", 
#'    "histogram", "scatter", "test")
#' @param type_1L_chr Type (a character vector of length one), Default: c("candidates", "assessments", "models", "tests")
#' @param ... Additional arguments
#' @return Plot (a plot)
#' @rdname plot_regression
#' @export 
#' @importFrom ready4use get_colour_codes
#' @importFrom jtools plot_summs
plot_regression <- function (regressions_ls, what_1L_chr, close_1L_lgl = TRUE, colors_chr = ready4use::get_colour_codes(type_1L_chr = "unicol", 
    style_1L_chr = "monash_2"), constrained_1L_lgl = logical(0), 
    model_1L_int = integer(0), named_1L_lgl = FALSE, part_1L_int = integer(0), 
    plot_tfmn_fn = identity, report_1L_chr = c("all", "check", 
        "compare", "confusion", "density", "estimates", "histogram", 
        "scatter", "test"), type_1L_chr = c("candidates", "assessments", 
        "models", "tests"), ...) 
{
    report_1L_chr <- match.arg(report_1L_chr)
    type_1L_chr <- type_1L_chr
    plot_plt <- regressions_ls %>% get_regression(what_1L_chr = what_1L_chr, 
        constrained_1L_lgl = constrained_1L_lgl, model_1L_int = model_1L_int, 
        named_1L_lgl = named_1L_lgl, part_1L_int = part_1L_int, 
        report_1L_chr = report_1L_chr, type_1L_chr = type_1L_chr) %>% 
        jtools::plot_summs(colors = colors_chr) %>% plot_tfmn_fn()
    if (close_1L_lgl) {
        closeAllConnections()
    }
    return(plot_plt)
}
#' Plot test scatter
#' @description plot_test_scatter() is a Plot function that plots data. Specifically, this function implements an algorithm to plot test scatter. The function returns Scatter (a plot).
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param var_1L_chr Variable (a character vector of length one)
#' @param collapse_1L_lgl Collapse (a logical vector of length one), Default: F
#' @param colour_1L_chr Colour (a character vector of length one), Default: ready4use::get_colour_codes(style_1L_chr = "monash_2", type_1L_chr = "unicol")
#' @param grouping_1L_chr Grouping (a character vector of length one), Default: character(0)
#' @param new_1L_lgl New (a logical vector of length one), Default: F
#' @param plot_tfmn_fn Plot transformation (a function), Default: identity
#' @param old_1L_chr Old (a character vector of length one), Default: 'Observed'
#' @param type_1L_chr Type (a character vector of length one), Default: c("Simulated", "Predicted")
#' @return Scatter (a plot)
#' @rdname plot_test_scatter
#' @export 
#' @importFrom ready4use get_colour_codes
#' @importFrom dplyr select filter mutate distinct group_by summarise pull
#' @importFrom tidyr any_of pivot_wider
#' @importFrom rlang sym
#' @importFrom purrr map_dfr
#' @importFrom tune coord_obs_pred
plot_test_scatter <- function (X_Ready4useDyad, var_1L_chr, collapse_1L_lgl = F, colour_1L_chr = ready4use::get_colour_codes(style_1L_chr = "monash_2", 
    type_1L_chr = "unicol"), grouping_1L_chr = character(0), 
    new_1L_lgl = F, plot_tfmn_fn = identity, old_1L_chr = "Observed", 
    type_1L_chr = c("Simulated", "Predicted")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
        dplyr::select(tidyr::any_of(c("UID", "Data", grouping_1L_chr, 
            var_1L_chr, paste0(var_1L_chr, "_sim_mean")))))
    if (collapse_1L_lgl) {
        if (!new_1L_lgl) {
            X_Ready4useDyad@ds_tb <- rbind(X_Ready4useDyad@ds_tb %>% 
                dplyr::filter(Data != type_1L_chr), X_Ready4useDyad@ds_tb %>% 
                dplyr::filter(Data == type_1L_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                !!rlang::sym(paste0(var_1L_chr, "_sim_mean"))))) %>% 
                dplyr::select(-!!rlang::sym(paste0(var_1L_chr, 
                  "_sim_mean"))) %>% dplyr::distinct()
        }
        else {
            X_Ready4useDyad <- rbind(X_Ready4useDyad@ds_tb %>% 
                dplyr::filter(Data != type_1L_chr), X_Ready4useDyad@ds_tb %>% 
                dplyr::filter(Data == type_1L_chr) %>% dplyr::group_by(UID) %>% 
                dplyr::summarise(`:=`(!!rlang::sym(var_1L_chr), 
                  mean(!!rlang::sym(paste0(var_1L_chr, "_sim_mean"))))))
        }
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
        as_percent_1L_lgl = F, colours_chr = colour_1L_chr, drop_missing_1L_lgl = T, 
        type_1L_chr = "manual", what_1L_chr = "scatter") %>% 
        plot_tfmn_fn() + tune::coord_obs_pred()
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
