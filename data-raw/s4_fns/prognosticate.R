prognosticate_MimicConfiguration <- function(x,
                                             Y_MimicRepos = MimicRepos(),
                                             consent_1L_chr = "",
                                             consent_indcs_int = 1L,
                                             draws_1L_chr = c("make", "make_batch", "read", "read_batch"),
                                             options_chr = c("Y", "N"),
                                             purge_1L_lgl = FALSE,
                                             suffix_1L_chr = "",
                                             type_1L_chr = c("NULL", "D", "AB", "C"),
                                             what_1L_chr = c("all", "batch"),
                                             unlink_1L_lgl = TRUE, # default to FALSE
                                             ...){
  draws_1L_chr <- match.arg(draws_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "all"){
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
      batches_int <- manufacture(Y_MimicRepos, return_1L_chr = "batches",type_1L_chr = "draw_to") ####
      test_1L_lgl <- assertthat::assert_that(identical(batches_int, 1:length(X@iterations_ls)),
                                             msg = "Batches of parameter draws tables do not match batches of iterations in iterations list.")
    }
    extras_ls <- list(...)
    # Most of these arguments can be removed once appropriate defaults are added to predict_with_sim
    args_ls <- list(
      # inputs_ls = NULL,
      arms_chr = character(0),
      comparator_fn = NULL,
      draws_tb = draws_tb,
      drop_missing_1L_lgl = logical(0),
      # drop_suffix_1L_chr = character(0), 
      # extra_draws_fn = NULL,
      horizon_dtm = lubridate::period(),
      intervention_fn = NULL,
      iterations_ls = NULL,
      modifiable_chr = character(0),
      # prior_batches_1L_int = 0, 
      purge_1L_lgl = purge_1L_lgl, 
      seed_1L_int = integer(0),
      sensitivities_ls = NULL,
      start_dtm = lubridate::NA_Date_,
      synthesis_fn = NULL,
      tfmn_ls = NULL,
      type_1L_chr = type_1L_chr,
      unlink_1L_lgl = unlink_1L_lgl,
      utilities_chr = character(0),
      utility_fns_ls = NULL, # list(),
      write_to_1L_chr = write_to_1L_chr, 
      X_MimicConfiguration = x,
      Y_MimicRepos = Y_MimicRepos
    ) %>%
      append(extras_ls)
    
    # comparator_fn = predict_comparator_pathway, # make NULL
    # draws_tb = NULL,
    # drop_missing_1L_lgl = FALSE, 
    # drop_suffix_1L_chr = character(0), 
    # extra_draws_fn = NULL,
    # intervention_fn = predict_digital_pathway, # make NULL
    # iterations_ls = make_batches(5, of_1L_int = 20), 
    # horizon_dtm = lubridate::years(1), 
    # modifiable_chr = c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D"), # Remove default
    # prior_batches_1L_int = 0, 
    # purge_1L_lgl = TRUE, 
    # seed_1L_int = 2001L, 
    # sensitivities_ls = make_sensitivities_ls(), 
    # synthesis_fn = make_project_results_synthesis,
    # start_dtm = Sys.Date(), 
    # tfmn_ls = make_class_tfmns(),
    # type_1L_chr = c("D", "AB", "C", "NULL"), 
    # unlink_1L_lgl = FALSE, 
    # utilities_chr = c("AQoL6D", "CHU9D"), # Remove default
    # write_to_1L_chr = character(0),
    
    errors_ls <- rlang::exec(predict_with_sim, !!!args_ls)
  }

  return(errors_ls)
}