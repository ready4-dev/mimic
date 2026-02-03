write_batch <- function (batch_1L_int, 
                         # add_logic_fn, 
                         arms_chr, 
                         # base_for_rates_int, 
                         comparator_fn, 
                         drop_missing_1L_lgl, 
                         drop_suffix_1L_chr, 
                         extra_draws_fn,
                         horizon_dtm, 
                         inputs_ls, intervention_fn, iterations_ls, modifiable_chr, 
                         prior_batches_1L_int, 
                         # scale_1L_int, 
                         seed_1L_int, sensitivities_ls, 
                         start_dtm, tfmn_ls, 
                         # tx_duration_dtm, 
                         utilities_chr, 
                         # variable_unit_1L_chr, 
                         write_to_1L_chr,
                         ...) 
{
  iterations_int <- iterations_ls[[batch_1L_int]]
  draws_tb <- make_draws_tb(inputs_ls, 
                            extra_draws_fn = extra_draws_fn,
                            iterations_int = iterations_int, 
                            drop_missing_1L_lgl = drop_missing_1L_lgl, drop_suffix_1L_chr = drop_suffix_1L_chr, 
                            # scale_1L_int = scale_1L_int,
                            seed_1L_int = seed_1L_int + batch_1L_int)
  extras_ls <- list(...)
  if (!is.null(intervention_fn)) {
    args_ls <- list(inputs_ls, 
                    # add_logic_fn = add_logic_fn, 
                    arm_1L_chr = arms_chr[1], 
                    # base_for_rates_int = base_for_rates_int, 
                    draws_tb = draws_tb, 
                    extra_draws_fn = extra_draws_fn,
                    iterations_int = iterations_int, 
                    horizon_dtm = horizon_dtm, 
                    modifiable_chr = modifiable_chr, 
                    sensitivities_ls = sensitivities_ls, 
                    tfmn_ls = tfmn_ls, 
                    # tx_duration_dtm = tx_duration_dtm, 
                    seed_1L_int = seed_1L_int + batch_1L_int, 
                    start_dtm = start_dtm, 
                    utilities_chr = utilities_chr
                    # , variable_unit_1L_chr = variable_unit_1L_chr
    ) %>%
      append(extras_ls)
    Y_Ready4useDyad <- rlang::exec(intervention_fn, !!!args_ls)
  }  else {
    Y_Ready4useDyad <- ready4use::Ready4useDyad()
  }
  if (!is.null(comparator_fn)) {
    args_ls <- list(inputs_ls, 
                    arm_1L_chr = arms_chr[2], 
                    # add_logic_fn = add_logic_fn, base_for_rates_int = base_for_rates_int, 
                    draws_tb = draws_tb, 
                    extra_draws_fn = extra_draws_fn,
                    iterations_int = iterations_int, 
                    horizon_dtm = horizon_dtm, 
                    modifiable_chr = modifiable_chr, 
                    sensitivities_ls = sensitivities_ls, 
                    tfmn_ls = tfmn_ls, 
                    # tx_duration_dtm = tx_duration_dtm, 
                    seed_1L_int = seed_1L_int + batch_1L_int, 
                    start_dtm = start_dtm, 
                    utilities_chr = utilities_chr
                    # , 
                    # variable_unit_1L_chr = variable_unit_1L_chr
    ) %>%
      append(extras_ls)
    Z_Ready4useDyad <- rlang::exec(comparator_fn, !!!args_ls)
  }  else {
    Z_Ready4useDyad <- ready4use::Ready4useDyad()
  }
  output_ls <- list(Y_Ready4useDyad = Y_Ready4useDyad, Z_Ready4useDyad = Z_Ready4useDyad)
  message(paste0("Batch ", batch_1L_int, " completed."))
  if (!dir.exists(write_to_1L_chr)) {
    dir.create(write_to_1L_chr)
  }
  saveRDS(output_ls, paste0(write_to_1L_chr, "/SimBatch", batch_1L_int + 
                              prior_batches_1L_int, ".RDS"))
  message(paste0("Output saved as SimBatch", batch_1L_int + 
                   prior_batches_1L_int, ".RDS"))
}
write_project_csvs <- function(model_data_ls,
                            path_to_private_1L_chr,
                            processed_dir_1L_chr,
                            divider_1L_chr = "\\"){
  c("unimputed", "imputed") %>%
    purrr::walk(~{
      type_1L_chr <- .x
      element_ls <- model_data_ls %>% purrr::pluck(paste0(.x,"_ls"))
      element_ls %>%
        purrr::walk2(names(element_ls),
                     ~{
                       write.csv(ready4::procureSlot(.x, "ds_tb"), paste0(path_to_private_1L_chr,
                                                                          "\\",
                                                                          processed_dir_1L_chr,
                                                                          "\\",
                                                                          "csv",
                                                                          "\\",
                                                                          type_1L_chr,
                                                                          "\\",
                                                                          stringr::str_remove(.y,"_r4"),
                                                                          ".csv"
                       ), row.names = FALSE)
                     })
      
    })
}

write_project_RDS <- function (data_ls, path_to_private_1L_chr, processed_dir_1L_chr, 
                               divider_1L_chr = "\\", r_dir_1L_chr = "R") 
{
  if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
                         processed_dir_1L_chr))) {
    dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                      processed_dir_1L_chr))
  }
  if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
                         processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr))) {
    dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                      processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr))
  }
  data_ls %>% names() %>% stringr::str_remove_all("_ls") %>% 
    purrr::walk(~{
      type_1L_chr <- .x
      if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
                             processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, divider_1L_chr, 
                             type_1L_chr))) {
        dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                          processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                          divider_1L_chr, type_1L_chr))
      }
      element_ls <- data_ls %>% purrr::pluck(paste0(.x, 
                                                    "_ls"))
      element_ls %>% purrr::walk2(names(element_ls), ~{
        saveRDS(.x, paste0(path_to_private_1L_chr, divider_1L_chr, 
                           processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                           divider_1L_chr, type_1L_chr, divider_1L_chr, 
                           .y, ".RDS"))
      })
    })
}
write_project_ws <- function(path_to_private_1L_chr,
                          processed_dir_1L_chr,
                          divider_1L_chr = "\\"){
  if(!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, processed_dir_1L_chr))){
    dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, processed_dir_1L_chr))
  }
  c("csv", "R") %>% purrr::walk(~{
    if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x))){
      dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x))
    }
    if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "unimputed"))){
      dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "unimputed"))
    }
    if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "imputed"))){
      dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "imputed"))
    }
  })
  if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,"R",divider_1L_chr, "regressions"))){
    dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,"R",divider_1L_chr, "regressions"))
  }
}
write_raw_mds_data<- function (raw_mds_data_ls,
                               path_to_raw_dir_1L_chr,
                               r_dir_1L_chr = "R"){
  raw_mds_data_ls %>% purrr::walk2(names(raw_mds_data_ls), 
                                   ~ saveRDS(.x, file = paste0(path_to_raw_dir_1L_chr,"/", r_dir_1L_chr,"/",.y,".RDS")))
}