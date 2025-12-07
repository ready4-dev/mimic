plot_economic_results <- function (economic_results_ls, what_1L_chr, alpha_1L_dbl = 0.8, 
                                   colour_1L_chr = ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr = "monash_2"),
                                   # ready4use::get_colour_codes(2)[2], 
                                   currency_1L_chr = "$",
                                   plot_tfmn_fn = identity,
                                   size_1L_dbl = 3,
                                   threshold_1L_dbl = 96000, title_1L_chr = " ", type_1L_chr = c("cep", 
                                                                                                 "ceac", "evi"), x_limits_dbl = numeric(0), ...) 
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
                                   wtp = threshold_1L_dbl, currency = currency_1L_chr, point = list(color = colour_1L_chr, 
                                                                                                    size = size_1L_dbl, alpha = alpha_1L_dbl), title = title_1L_chr, 
                                   ...)
  }
  if (type_1L_chr == "evi") {
    plot_plt <- BCEA::evi.plot(data_xx, graph = "ggplot2", 
                               line_colors = colour_1L_chr,...) + ggplot2::labs(title = title_1L_chr)
    if(!identical(currency_1L_chr, character(0))){
      plot_plt <- plot_plt +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = currency_1L_chr))
    }
  }
  if (!identical(x_limits_dbl, numeric(0))) {
    plot_plt <- plot_plt + ggplot2::expand_limits(x = x_limits_dbl)
  }
  plot_plt <- plot_tfmn_fn(plot_plt)
  return(plot_plt)
}
plot_pooled <- function(pooled_fits_ls,
                        what_1L_chr,
                        distributions_chr = "best",
                        pool_1L_lgl = TRUE,
                        ...){
  assertthat::assert_that(what_1L_chr %in% names(pooled_fits_ls))
  plt <- SHELF::plotfit(pooled_fits_ls %>% purrr::pluck(what_1L_chr), lp = pool_1L_lgl, returnPlot = T, showPlot = F, d = distributions_chr, ...)
  return(plt)
}
plot_regression <- function (regressions_ls, what_1L_chr, close_1L_lgl = TRUE, 
                             colors_chr = ready4use::get_colour_codes(type_1L_chr = "unicol", style_1L_chr = "monash_2"), 
                             constrained_1L_lgl = logical(0), model_1L_int = integer(0), 
                             named_1L_lgl = FALSE, part_1L_int = integer(0), 
                             plot_tfmn_fn = identity,
                             report_1L_chr = c("all", 
                                               "check", "compare", "confusion", "density", "estimates", 
                                               "histogram", "scatter", "test"), type_1L_chr = c("candidates", 
                                                                                                "assessments", "models", "tests"), ...) {
  report_1L_chr <- match.arg(report_1L_chr)
  type_1L_chr <- type_1L_chr
  plot_plt <- regressions_ls %>% get_regression(what_1L_chr = what_1L_chr, 
                                                constrained_1L_lgl = constrained_1L_lgl, model_1L_int = model_1L_int, 
                                                named_1L_lgl = named_1L_lgl, part_1L_int = part_1L_int, 
                                                report_1L_chr = report_1L_chr, type_1L_chr = type_1L_chr) %>% 
    jtools::plot_summs(colors = colors_chr) %>%
    plot_tfmn_fn()
  if (close_1L_lgl) {
    closeAllConnections()
  }
  return(plot_plt)
}
plot_test_scatter <- function (X_Ready4useDyad, var_1L_chr, 
                               collapse_1L_lgl = F, 
                               colour_1L_chr = ready4use::get_colour_codes(style_1L_chr = "monash_2", type_1L_chr = "unicol"),
                               grouping_1L_chr = character(0), 
                               new_1L_lgl = F, 
                               plot_tfmn_fn = identity,
                               old_1L_chr = "Observed", type_1L_chr = c("Simulated", 
                                                                        "Predicted")) 
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
    }  else {
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
  }  else {
    values_xx <- X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(grouping_1L_chr)) %>% 
      unique() %>% sort()
    if ("factor" %in% class(values_xx)) {
      values_xx <- as.character(values_xx)
      tfmn_fn <- as.character
    }    else {
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
                        as_percent_1L_lgl = F, colours_chr = colour_1L_chr, drop_missing_1L_lgl = T, type_1L_chr = "manual", what_1L_chr = "scatter") %>%
    plot_tfmn_fn() + tune::coord_obs_pred()
  return(scatter_plt)
}
plot_tx_mdl_confusion <- function(X_Ready4useDyad = ready4use::Ready4useDyad(),
                                  tx_mdls_ls,
                                  model_1L_int,
                                  high_1L_chr = "#2E86C1",
                                  low_1L_chr = "#D6EAF8",
                                  treatment_vars_chr = c("treatment_status", "treatment_status_t2"),
                                  what_1L_chr = c("Waitlist", "Treatment", "Discharged")){
  what_1L_chr <- match.arg(what_1L_chr)
  model_mdl <- tx_mdls_ls[[paste0(what_1L_chr, "_ls")]] %>% purrr::pluck(model_1L_int)
  data_tb <- X_Ready4useDyad@ds_tb %>% transform_tx_factor(treatment_vars_chr = treatment_vars_chr, what_1L_chr = what_1L_chr)
  confusion_plt <- yardstick::conf_mat(stats::predict(model_mdl, data_tb) %>%
                                         dplyr::rename(Predicted = .pred_class) %>% 
                                         dplyr::mutate(Observed = data_tb %>% dplyr::pull(!!rlang::sym(treatment_vars_chr[2]))), 
                                       Observed, Predicted, dnn = c("Predicted", "Observed")) %>%
    ggplot2::autoplot(type = "heatmap") +
    ggplot2::scale_fill_gradient(low=low_1L_chr,high = high_1L_chr)
  return(confusion_plt)
}
