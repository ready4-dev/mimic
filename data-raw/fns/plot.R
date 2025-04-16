plot_pooled <- function(pooled_fits_ls,
                        what_1L_chr,
                        distributions_chr = "best",
                        pool_1L_lgl = TRUE,
                        ...){
  assertthat::assert_that(what_1L_chr %in% names(pooled_fits_ls))
  plt <- SHELF::plotfit(pooled_fits_ls %>% purrr::pluck(what_1L_chr), lp = pool_1L_lgl, returnPlot = T, showPlot = F, d = distributions_chr, ...)
  return(plt)
}
plot_test_scatter <- function(X_Ready4useDyad,
                              collapse_1L_lgl = F,
                              grouping_1L_chr = character(0),
                              var_1L_chr,
                              old_1L_chr = "Observed",
                              type_1L_chr = c("Simulated", "Predicted")){
  type_1L_chr <- match.arg(type_1L_chr)
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                               X_Ready4useDyad@ds_tb %>% dplyr::select(tidyr::any_of(c("UID", "Data", grouping_1L_chr, var_1L_chr, paste0(var_1L_chr,"_sim_mean")))))
  if(collapse_1L_lgl){
    X_Ready4useDyad@ds_tb <- rbind(X_Ready4useDyad@ds_tb %>% dplyr::filter(Data!=type_1L_chr),
                                   X_Ready4useDyad@ds_tb %>% dplyr::filter(Data==type_1L_chr) %>%
                                     dplyr::mutate(!!rlang::sym(var_1L_chr) := !!rlang::sym(paste0(var_1L_chr,"_sim_mean")))) %>%
      dplyr::distinct()
  }
  if(identical(grouping_1L_chr, character(0))){
    new_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(c("UID", "Data", var_1L_chr)) %>% tidyr::pivot_wider(names_from = "Data", values_from = var_1L_chr)
  }else{
    values_xx <- X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(grouping_1L_chr)) %>% unique() %>% sort() 
    if("factor" %in% class(values_xx)){
      values_xx <- as.character(values_xx)
      tfmn_fn <- as.character
    }else{
      tfmn_fn <- identity
    }
    new_tb <- values_xx %>% purrr::map_dfr(~X_Ready4useDyad@ds_tb %>% dplyr::filter(tfmn_fn(!!rlang::sym(grouping_1L_chr))==.x) %>% 
                                             dplyr::select(c("UID", "Data", grouping_1L_chr, var_1L_chr)) %>% tidyr::pivot_wider(names_from = "Data", values_from = var_1L_chr))
  }
  scatter_plt <- depict(renewSlot(X_Ready4useDyad, "ds_tb", 
                                  new_tb), 
                        x_vars_chr = type_1L_chr,  
                        y_vars_chr = old_1L_chr, 
                        as_percent_1L_lgl = F,
                        drop_missing_1L_lgl = T, 
                        what_1L_chr = "scatter") +
    tune::coord_obs_pred()
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
