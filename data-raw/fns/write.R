write_batch <- function (batch_1L_int, 
                         # add_logic_fn, 
                         arms_chr = character(0), 
                         # arms_tb = make_arms_tb(),
                         # base_for_rates_int, 
                         comparator_fn = NULL, 
                         # draws_dir_1L_chr = character(0), 
                         draws_tb = NULL,
                         drop_missing_1L_lgl = FALSE, 
                         drop_suffix_1L_chr = FALSE, 
                         extra_draws_fn = NULL,
                         horizon_dtm = lubridate::years(1), 
                         inputs_ls = NULL, 
                         intervention_fn = NULL, 
                         iterations_ls = NULL, 
                         modifiable_chr = character(0), 
                         prior_batches_1L_int = integer(0), 
                         # scale_1L_int, 
                         seed_1L_int = 2001L, 
                         sensitivities_ls = NULL, 
                         start_dtm = Sys.Date(), 
                         tfmn_ls = NULL, 
                         # tx_duration_dtm, 
                         utilities_chr = character(0), 
                         # variable_unit_1L_chr, 
                         write_to_1L_chr,
                         X_MimicConfiguration = MimicConfiguration(),
                         # X_MimicAlgorithms = MimicAlgorithms(),
                         # X_MimicInputs = MimicInputs(),
                         # Y_MimicRepos = MimicRepos(),
                         ...) 
{
  old_algorithm_1L_lgl <- T
  if(!identical(X_MimicConfiguration, MimicConfiguration())){
    # drop_missing_1L_lgl = X_MimicConfiguration@drop_missing_1L_lgl
    # drop_suffix_1L_chr = if(is.na(X_MimicConfiguration@drop_suffix_1L_chr)){
    #   character(0)
    # }else{
    #   X_MimicConfiguration@drop_suffix_1L_chr 
    # }
    # extra_draws_fn = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$extra_draws_fn
    # horizon_dtm = X_MimicConfiguration@horizon_dtm
    # inputs_ls = manufacture(X_MimicConfiguration@x_MimicInputs, what_1L_chr = "inputs_ls")
    # iterations_ls = X_MimicConfiguration@iterations_ls
    # modifiable_chr = if(is.na(X_MimicConfiguration@modifiable_chr[1])){
    #   character(0)
    # }else{
    #   X_MimicConfiguration@modifiable_chr 
    # }
    # seed_1L_int = X_MimicConfiguration@seed_1L_int
    # sensitivities_ls = X_MimicConfiguration@x_MimicAlgorithms@sensitivities_ls
    # start_dtm = X_MimicConfiguration@start_dtm
    # synthesis_fn = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$synthesis_fn
    # tfmn_ls = X_MimicConfiguration@x_MimicAlgorithms@transformations_ls
    # utilities_chr = X_MimicConfiguration@utilities_chr
    old_algorithm_1L_lgl <- F
  }else{
    X_MimicConfiguration <- make_configuration(arms_chr = arms_chr,
                                               drop_missing_1L_lgl = drop_missing_1L_lgl,
                                               drop_suffix_1L_chr = drop_suffix_1L_chr,
                                               extra_draws_fn = extra_draws_fn,
                                               horizon_dtm = horizon_dtm,
                                               iterations_ls = iterations_ls,
                                               modifiable_chr = modifiable_chr,
                                               seed_1L_int = seed_1L_int,
                                               sensitivities_ls = sensitivities_ls,
                                               start_dtm = start_dtm,
                                               synthesis_fn = synthesis_fn,
                                               utilities_chr = utilities_chr)
    #   MimicConfiguration() %>%
    #   renewSlot(drop_missing_1L_lgl = drop_missing_1L_lgl,
    #             drop_suffix_1L_chr = drop_suffix_1L_chr,
    #             horizon_dtm = horizon_dtm,
    #             iterations_ls = iterations_ls,
    #             modifiable_chr = modifiable_chr,
    #             seed_1L_int = seed_1L_int,
    #             start_dtm = start_dtm,
    #             utilities_chr = utilities_chr)
    # X_MimicConfiguration <- renewSlot(X_MimicConfiguration,
    #             "x_MimicAlgorithms@processing_ls",
    #             list(extra_draws_fn = extra_draws_fn,
    #                  synthesis_fn = synthesis_fn)) %>%
    #   renewSlot("x_MimicAlgorithms@sensitivities_ls",
    #             sensitivities_ls) %>%
    #   renewSlot("x_MimicAlgorithms@transformations_ls",
    #             tfmn_ls) %>%
    #   renewSlot("arms_tb",
    #             make_arms_tb(arms_chr,
    #                          settings_ls = list(
    #                            # Treatment = rep("MMHC",2),
    #                            Algorithm = c("intervention_fn", "comparator_fn")))
    #             # "arms_chr", c("MMHC","FlexPsych")
    #   )
  }
  # if(!identical(X_MimicAlgorithms = MimicAlgorithms())){
  #   
  #   # comparator_fn = x@x_MimicAlgorithms@main_ls$comparator_fn,
  #   # intervention_fn = x@x_MimicAlgorithms@main_ls$intervention_fn,
  #   extra_draws_fn <- X_MimicAlgorithms@processing_ls$extra_draws_fn
  #   sensitivities_ls <- X_MimicAlgorithms@sensitivities_ls
  #   synthesis_fn <- X_MimicAlgorithms@processing_ls$synthesis_fn
  #   tfmn_ls <- X_MimicAlgorithms@transformations_ls
  # }
  # if(!identical(X_MimicInputs = MimicInputs())){
  #   inputs_ls <- manufacture(X_MimicInputs, what_1L_chr = "inputs_ls")
  # }
  # iterations_int <- X_MimicConfiguration@iterations_ls[[batch_1L_int]] # manufacture method
  iterations_int <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, what_1L_chr = "iterations")
  if(is.null(draws_tb)){
    if(!identical(Y_MimicRepos, MimicRepos())){
      draws_tb <- ingest(Y_MimicRepos, batches_int = batch_1L_int, type_1L_chr = "ParamDraws")
    }else{
      draws_tb <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, what_1L_chr = "draws_tb") 
      # draws_tb <- make_draws_tb(inputs_ls, 
      #                           extra_draws_fn = extra_draws_fn,
      #                           iterations_int = iterations_int, 
      #                           drop_missing_1L_lgl = drop_missing_1L_lgl, 
      #                           drop_suffix_1L_chr = drop_suffix_1L_chr, 
      #                           # scale_1L_int = scale_1L_int,
      #                           seed_1L_int = seed_1L_int + batch_1L_int)
    }
  }
  test_1L_lgl <- assertthat::assert_that(identical(sort(draws_tb$Iteration), sort(iterations_int)),
                                         msg = "Iterations in iteration vector and parameter draws table do not match.")
  # if(nrow(arms_tb>0)){
  #   arms_chr <- arms_tb$Arm
  # }
  extras_ls <- list(...)
  # make the next bit a manufacture method
  # args_ls <- list(
  #   # inputs_ls, 
  #                 # add_logic_fn = add_logic_fn, 
  #                 arm_1L_chr = NA_character_, 
  #                 batch_1L_int = batch_1L_int,
  #                 ### base_for_rates_int = base_for_rates_int, 
  #                 # draws_tb = draws_tb, 
  #                 # extra_draws_fn = extra_draws_fn,
  #                 # iterations_int = iterations_int, 
  #                 # horizon_dtm = horizon_dtm, 
  #                 # modifiable_chr = modifiable_chr, 
  #                 # sensitivities_ls = sensitivities_ls, 
  #                 # tfmn_ls = tfmn_ls, 
  #                 ### tx_duration_dtm = tx_duration_dtm, 
  #                 # seed_1L_int = X_MimicConfiguration@seed_1L_int + batch_1L_int, 
  #                 # start_dtm = start_dtm, 
  #                 # utilities_chr = utilities_chr,
  #                 X_MimicConfiguration = X_MimicConfiguration
  #                 #### , variable_unit_1L_chr = variable_unit_1L_chr
  # ) %>%
  #   append(extras_ls) # manufacture method
  if(!old_algorithm_1L_lgl){
    # This bit can be made part of the prognosticate method
    output_ls <- purrr::map(X_MimicConfiguration@arms_tb$Arm,
                            ~{
                              algorithm_1L_chr <- get_from_lup_obj(X_MimicConfiguration@arms_tb, target_var_nm_1L_chr = "Algorithm", match_var_nm_1L_chr = "Arm", match_value_xx = .x) # Make method
                              function_fn <- X_MimicConfiguration@x_MimicAlgorithms@main_ls %>% purrr::pluck(algorithm_1L_chr) # Make method
                              new_args_ls <- manufacture(X_MimicConfiguration, arm_1L_chr = .x, batch_1L_int = batch_1L_int, what_1L_chr = "args_all")
                              # new_args_ls$arm_1L_chr <- .x
                              new_args_ls <- update_arguments_ls(new_args_ls, function_fn = function_fn)
                              rlang::exec(function_fn, !!!new_args_ls)
                            }) %>% stats::setNames(X_MimicConfiguration@arms_tb$Arm)
  }else{
    if (!is.null(intervention_fn)) {
      args_ls <- manufacture(X_MimicConfiguration, arm_1L_chr = arms_chr[1], batch_1L_int = batch_1L_int, what_1L_chr = "args_all")
      # args_ls$arm_1L_chr <- arms_chr[1]
      # args_ls <- list(inputs_ls, 
      #                 # add_logic_fn = add_logic_fn, 
      #                 arm_1L_chr = arms_chr[1], 
      #                 # base_for_rates_int = base_for_rates_int, 
      #                 draws_tb = draws_tb, 
      #                 extra_draws_fn = extra_draws_fn,
      #                 iterations_int = iterations_int, 
      #                 horizon_dtm = horizon_dtm, 
      #                 modifiable_chr = modifiable_chr, 
      #                 sensitivities_ls = sensitivities_ls, 
      #                 tfmn_ls = tfmn_ls, 
      #                 # tx_duration_dtm = tx_duration_dtm, 
      #                 seed_1L_int = seed_1L_int + batch_1L_int, 
      #                 start_dtm = start_dtm, 
      #                 utilities_chr = utilities_chr
      #                 # , variable_unit_1L_chr = variable_unit_1L_chr
      # ) %>%
      #   append(extras_ls) 
      args_ls <- update_arguments_ls(new_args_ls, function_fn = intervention_fn)
      Y_Ready4useDyad <- rlang::exec(intervention_fn, !!!args_ls)
    }  else {
      Y_Ready4useDyad <- ready4use::Ready4useDyad()
    }
    if (!is.null(comparator_fn)) {
      args_ls <- manufacture(X_MimicConfiguration, arm_1L_chr = arms_chr[2], batch_1L_int = batch_1L_int, what_1L_chr = "args_all")
      # args_ls$arm_1L_chr <- arms_chr[2]
      # args_ls <- list(inputs_ls, 
      #                 arm_1L_chr = arms_chr[2], ## ONLY DIFFERENCE
      #                 # add_logic_fn = add_logic_fn, base_for_rates_int = base_for_rates_int, 
      #                 draws_tb = draws_tb, 
      #                 extra_draws_fn = extra_draws_fn,
      #                 iterations_int = iterations_int, 
      #                 horizon_dtm = horizon_dtm, 
      #                 modifiable_chr = modifiable_chr, 
      #                 sensitivities_ls = sensitivities_ls, 
      #                 tfmn_ls = tfmn_ls, 
      #                 # tx_duration_dtm = tx_duration_dtm, 
      #                 seed_1L_int = seed_1L_int + batch_1L_int, 
      #                 start_dtm = start_dtm, 
      #                 utilities_chr = utilities_chr
      #                 # , 
      #                 # variable_unit_1L_chr = variable_unit_1L_chr
      # ) %>%
      #   append(extras_ls)
      args_ls <- update_arguments_ls(args_ls, function_fn = comparator_fn)
      Z_Ready4useDyad <- rlang::exec(comparator_fn, !!!args_ls)
    }  else {
      Z_Ready4useDyad <- ready4use::Ready4useDyad()
    }
    output_ls <- list(Y_Ready4useDyad = Y_Ready4useDyad, # when generalising to more than two interventions, each element should be named by arm
                      Z_Ready4useDyad = Z_Ready4useDyad)
  }
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