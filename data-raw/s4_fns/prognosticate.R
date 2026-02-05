prognosticate_MimicConfiguration <- function(x,
                                             Y_MimicRepos = MimicRepos(),
                                             consent_1L_chr = "",
                                             consent_indcs_int = 1L,
                                             draws_1L_chr = c("make", "make_batch", "read", "read_batch"),
                                             options_chr = c("Y", "N"),
                                             purge_1L_lgl = FALSE,
                                             suffix_1L_chr = "",
                                             type_1L_chr = c("NULL", "D", "AB", "C"),
                                             unlink_1L_lgl = TRUE,
                                             ...){
  draws_1L_chr <- match.arg(draws_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  author(Y_MimicRepos, 
         consent_1L_chr = consent_1L_chr,
         consent_indcs_int = consent_indcs_int,
         options_chr = options_chr,
         suffix_1L_chr = suffix_1L_chr, 
         what_1L_chr = "sim_ws_dirs_chr")
  write_to_1L_chr <- manufacture(Y_MimicRepos, suffix_1L_chr = suffix_1L_chr, type_1L_chr = "batch_to", what_1L_chr = "sim_ws_dirs_chr") 
  # Make parameter value draws and save results in batches
  if(draws_1L_chr %in% c("make", "make_batch")){
    author(X,
           consent_1L_chr = consent_1L_chr,
           consent_indcs_int = consent_indcs_int,
           options_chr = options_chr,
           unlink_1L_lgl = T,
           what_1L_chr = "draws", 
           Y_MimicRepos = Y_MimicRepos)
  }
  # Ingest parameter value draws and check for concordence in iteration numbers
  if(draws_1L_chr %in% c("make", "read")){
    draws_tb <- ingest(Y_MimicRepos, type_1L_chr = "ParamDraws")
    test_1L_lgl <- assertthat::assert_that(identical(sort(draws_tb$Iteration), sort(x@iterations_ls %>% purrr::flatten_int())),
                                           msg = "Iterations in iteration list and composite parameter draws table do not match.")
  }else{
    draws_tb <- NULL
    batches_int <- manufacture(Y, return_1L_chr = "batches",type_1L_chr = "draw_to")
    test_1L_lgl <- assertthat::assert_that(identical(batches_int, 1:length(X@iterations_ls)),
                                           msg = "Batches of parameter draws tables do not match batches of iterations in iterations list.")
  }
  extras_ls <- list(...)
  args_ls <- list(arms_chr = x@arms_chr,
                  arms_tb = x@arms_tb,
                  comparator_fn = x@x_MimicAlgorithms@main_ls$comparator_fn,
                  draws_tb = draws_tb,
                  drop_missing_1L_lgl = x@drop_missing_1L_lgl,
                  drop_suffix_1L_chr = if(is.na(x@drop_suffix_1L_chr)){
                    character(0)
                  }else{
                    x@drop_suffix_1L_chr 
                  },
                  extra_draws_fn = x@x_MimicAlgorithms@processing_ls$extra_draws_fn,
                  horizon_dtm = x@horizon_dtm,
                  inputs_ls = manufacture(x@x_MimicInputs, what_1L_chr = "inputs_ls"), 
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
                  start_dtm = x@start_dtm,
                  synthesis_fn = x@x_MimicAlgorithms@processing_ls$synthesis_fn,
                  tfmn_ls = x@x_MimicAlgorithms@transformations_ls,
                  utilities_chr = x@utilities_chr,
                  unlink_1L_lgl = unlink_1L_lgl,
                  write_to_1L_chr = write_to_1L_chr, 
                  type_1L_chr = type_1L_chr,
                  X_MimicAlgorithms = x@x_MimicAlgorithms,
                  Y_MimicRepos = Y_MimicRepos) %>%
      append(extras_ls)
  errors_ls <- rlang::exec(predict_with_sim, !!!args_ls)
  return(errors_ls)
}