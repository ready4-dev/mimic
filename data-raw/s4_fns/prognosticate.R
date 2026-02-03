prognosticate_MimicConfiguration <- function(x,
                                             Y_MimicRepos = MimicRepos(),
                                             # batch_dir_1L_chr = "BatchedSimResults",
                                             consent_1L_chr = "",
                                             consent_indcs_int = 1L,
                                             options_chr = c("Y", "N"),
                                             purge_1L_lgl = FALSE,
                                             suffix_1L_chr = "",
                                             type_1L_chr = c("NULL", "D", "AB", "C"),
                                             unlink_1L_lgl = TRUE,
                                             ...){
  type_1L_chr <- match.arg(type_1L_chr)
  
  author_MimicRepos(Y_MimicRepos, ## UPDATE METHOD NAME
                    consent_1L_chr = consent_1L_chr,
                    consent_indcs_int = consent_indcs_int,
                    options_chr = options_chr,
                    suffix_1L_chr = suffix_1L_chr, 
                    what_1L_chr = "sim_ws_dirs_chr")
  write_to_1L_chr <- manufacture_MimicRepos(Y, suffix_1L_chr = suffix_1L_chr, type_1L_chr = "batch_to", what_1L_chr = "sim_ws_dirs_chr") ## UPDATE METHOD NAME
  # write_to_1L_chr <- paste0(Y_MimicRepos@path_to_output_1L_chr,
  #                           Y_MimicRepos@divider_1L_chr,
  #                           Y_MimicRepos@processed_dir_1L_chr,
  #                           Y_MimicRepos@divider_1L_chr,
  #                           Y_MimicRepos@r_dir_1L_chr,
  #                           Y_MimicRepos@divider_1L_chr,
  #                           Y_MimicRepos@batch_to_1L_chr)
  # ready4::write_new_dirs(write_to_1L_chr,
  #                        consent_1L_chr = consent_1L_chr)
  extras_ls <- list(...)
  
  args_ls <- list(arms_chr = x@arms_chr,
                  comparator_fn = x@x_MimicAlgorithms@main_ls$comparator_fn,
                  drop_missing_1L_lgl = x@drop_missing_1L_lgl,
                  drop_suffix_1L_chr = if(is.na(x@drop_suffix_1L_chr)){
                    character(0)
                  }else{
                    x@drop_suffix_1L_chr 
                  },
                  horizon_dtm = x@horizon_dtm,
                  inputs_ls = list(models_ls = x@x_MimicInputs@models_ls,
                                   params_tb = x@x_MimicInputs@x_Ready4useDyad@ds_tb,
                                   Synthetic_r4 = x@x_MimicInputs@y_Ready4useDyad),
                  intervention_fn = x@x_MimicAlgorithms@main_ls$intervention_fn,
                  iterations_ls = x@iterations_ls,
                  modifiable_chr = if(is.na(x@modifiable_chr[1])){
                    character(0)
                  }else{
                    x@modifiable_chr 
                  },
                  purge_1L_lgl = purge_1L_lgl, 
                  seed_1L_int = x@seed_1L_int,
                  sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls,
                  tfmn_ls = x@x_MimicAlgorithms@transformations_ls,
                  utilities_chr = x@utilities_chr,
                  unlink_1L_lgl = unlink_1L_lgl,
                  write_to_1L_chr = write_to_1L_chr, 
                  type_1L_chr = type_1L_chr) %>%
      append(extras_ls)
  errors_ls <- rlang::exec(predict_with_sim, !!!args_ls)
  return(errors_ls)
}