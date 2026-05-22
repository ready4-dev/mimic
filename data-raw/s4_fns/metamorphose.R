metamorphose_MimicConfiguration <- function(x,
                                            arm_1L_chr = NA_character_,
                                            batch_1L_int = integer(0),
                                            draws_tb = NULL,
                                            env_ls = list(),
                                            # tx_prefix_1L_chr = character(0),
                                            Y_Ready4Module = Ready4Module(),
                                            ...){
  if(inherits(Y_Ready4Module, "MimicPopulation")){
    population_ls <- manufacture(x, arm_1L_chr = arm_1L_chr,
                                 batch_1L_int = batch_1L_int,
                                 draws_tb = draws_tb,
                                 # tx_prefix_1L_chr = tx_prefix_1L_chr,
                                 type_1L_chr = "entry",
                                 what_1L_chr = c("population_ls"))
    Y_Ready4Module <- renew(Y_Ready4Module, population_ls = population_ls, type_1L_chr = "transform")
    Y_Ready4Module <- renew(Y_Ready4Module, env_ls = env_ls, type_1L_chr = "customise", X_MimicConfiguration = x)
  }
  if(inherits(Y_Ready4Module, "MimicResults")){
    # This is temporary - needs edits and possibly to be moved
    inputs_ls <- manufactureSlot(x, "x_MimicInputs", what_1L_chr = c("inputs_ls"))
    comparator_1L_int = 2L
    intervention_1L_int = 1L
    drop_chr <- c(make_project_2_vars("drop"), c("Sex","Treatment_status"))
    keep_chr <- c("Episode", "EpisodeDurationDays") # Should Episode and EpisodeDurationDays be in modifiable?
    keep_chr <- sort(c(setdiff(names(results_xx$Y_Ready4useDyad@ds_tb) %>% 
                                 intersect(names(inputs_ls$Synthetic_r4@ds_tb)), 
                               c(drop_chr, manufacture(x, type_1L_chr = "measure", what_1L_chr = "modifiable"), "UID")), keep_chr))
    groups_ls <- list(diagnosis_ls = c("Diagnosis"), distress_ls = c("Distress"))
    min_cell_size_1L_int <- 30L
    threshold_1L_dbl <- 96000
    comparator_1L_chr = x@arms_tb$Arm[comparator_1L_int]
    intervention_1L_chr = x@arms_tb$Arm[intervention_1L_int]
    results_xx <- inputs_ls %>%
      make_project_results_synthesis(results_ls = results_xx,
                                     comparator_1L_chr = comparator_1L_chr,
                                     drop_chr = drop_chr, 
                                     exclude_chr = character(0), 
                                     keep_chr = keep_chr, 
                                     intervention_1L_chr = intervention_1L_chr, 
                                     modifiable_chr =  manufacture(x, type_1L_chr = "measure", what_1L_chr = "modifiable")) 
    intervention_vars_chr <- names(results_xx@ds_tb)[stringr::str_ends(names(results_xx@ds_tb),paste0("_",intervention_1L_chr))] %>% sort()
    comparator_vars_chr <- names(results_xx@ds_tb)[stringr::str_ends(names(results_xx@ds_tb),paste0("_",comparator_1L_chr))] %>% sort()
    core_chr <- setdiff(setdiff(names(results_xx@ds_tb),intervention_vars_chr), comparator_vars_chr)
    baseline_vars_chr <- setdiff(core_chr %>% intersect(names(inputs_ls$Synthetic_r4@ds_tb)), "UID")
    changed_vars_chr <- intersect(baseline_vars_chr, manufacture(X, type_1L_chr = "measure", what_1L_chr = "modifiable"))
    characteristics_chr <- c(setdiff(baseline_vars_chr, changed_vars_chr), paste0(changed_vars, "_start") %>% intersect(names(results_xx@ds_tb))) %>% sort()
    results_xx <- add_severity_cuts(results_xx, cut_var_1L_chr = "Distress", severity_fn = make_k10_severity_cuts, severity_var_1L_chr = "K10_start")
    results_xx <- results_xx %>%
      make_project_2_results(comparator_1L_chr = comparator_1L_chr, # needs to be read from data_raw
                             groups_ls = groups_ls,
                             inputs_ls = inputs_ls,
                             intervention_1L_chr = intervention_1L_chr,
                             min_cell_size_1L_int = min_cell_size_1L_int,
                             threshold_1L_dbl = threshold_1L_dbl,
                             utilities_chr = x@x_MimicAlgorithms@x_MimicUtility@names_chr)
  }
  
  
  return(Y_Ready4Module)
}