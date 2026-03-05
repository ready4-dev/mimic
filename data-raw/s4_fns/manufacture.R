manufacture_MimicArguments <- function(x,
                                       batch_1L_int = integer(0),
                                       env_ls = list(),
                                       what_1L_chr = c("args_ls"),
                                       X_MimicConfiguration = MimicConfiguration(),
                                       ...){
  what_1L_chr <- match.arg(what_1L_chr)
  object_xx <- list()
  if(what_1L_chr == "args_ls"){
       object_xx <- manufacture(x@x_MimicDerivations, env_ls = env_ls, flatten_1L_lgl = FALSE, what_1L_chr = c("args_ls"), X_MimicConfiguration = X_MimicConfiguration)
    if(!identical(x@derive_ls, list())){
      object_xx <- object_xx %>%
        append(x@derive_ls %>% purrr::map(~{
          manufacture(.x, env_ls = env_ls, flatten_1L_lgl = TRUE, X_MimicConfiguration = X_MimicConfiguration) 
        }))
    }
    if(!identical(x@models_ls, list())){
      object_xx <- object_xx %>%
        append(x@models_ls %>%
                 purrr::map(~procureSlot(X_MimicConfiguration@x_MimicInputs, "models_ls") %>% purrr::pluck(.x)))
    }
    if(!identical(batch_1L_int, integer(0)) & x@iterations_1L_lgl){
      object_xx <- object_xx %>%
        append(list(iterations_int = manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, what_1L_chr = "iterations")))
    }
       object_xx <- object_xx[!duplicated(names(object_xx))]
  }
  return(object_xx)
}
manufacture_MimicConfiguration <- function(x,
                                           arm_1L_chr = NA_character_,
                                           batch_1L_int = integer(0),
                                           draws_tb = NULL,
                                           extras_ls = list(),
                                           include_chr = "Modelled",
                                           subset_1L_chr = character(0),
                                           target_1L_chr = character(0),
                                           total_1L_lgl = TRUE,
                                           type_1L_chr = c("current","concept", "entry", "measure"),
                                           what_1L_chr = c("draws_tb", "append_ls", "args_all","daystonever", "iterations", "modifiable","outcomes", "population_ls", "resources", "utilities"),
                                           ...){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "args_all"){
    object_xx <- list(
      arm_1L_chr = arm_1L_chr,
      batch_1L_int = batch_1L_int,
      draws_tb = draws_tb,
      X_MimicConfiguration = x) %>%
      append(extras_ls) 
    object_xx <- object_xx[!duplicated(names(object_xx))]
  }
  if(what_1L_chr == "append_ls"){
    object_xx <-   manufacture(x@x_MimicArguments, batch_1L_int = batch_1L_int,
                               env_ls = make_sim_env_ls(manufacture(x, arm_1L_chr = arm_1L_chr, batch_1L_int = batch_1L_int, draws_tb = draws_tb, extras_ls = extras_ls, what_1L_chr = "args_all"),
                                                        discard_chr = "X_MimicConfiguration"),
                               what_1L_chr = "args_ls", X_MimicConfiguration = x)
  }
  if(what_1L_chr == "draws_tb"){
    iterations_int <- manufacture(x, batch_1L_int = batch_1L_int, what_1L_chr = "iterations")
    if(identical(batch_1L_int, integer(0))){
      batch_1L_int <- 0
    }
    inputs_ls <- manufacture(x@x_MimicInputs, what_1L_chr = "inputs_ls")
    object_xx <- make_draws_tb(inputs_ls, 
                               extra_draws_fn = x@x_MimicAlgorithms@processing_ls$extra_draws_fn,
                               iterations_int = iterations_int, 
                               drop_missing_1L_lgl = x@drop_missing_1L_lgl, 
                               drop_suffix_1L_chr = x@drop_suffix_1L_chr, 
                               seed_1L_int = x@seed_1L_int + batch_1L_int)
  }
  if(what_1L_chr == "daystonever"){
    object_xx <- floor(x@horizon_dtm/lubridate::days(1))+1
  }
  if(what_1L_chr == "iterations"){
    if(identical(batch_1L_int, integer(0))){
      object_xx <- x@iterations_ls %>% purrr::flatten_int()
    }else{
      object_xx <- x@iterations_ls[[batch_1L_int]]
    } 
  }
  if(what_1L_chr %in% c("modifiable","outcomes", "resources", "utilities")){
    object_xx <- c(character(0))
    if(what_1L_chr %in% c("modifiable", "outcomes")){
      object_xx <- c(object_xx,
                     manufacture(x@x_MimicVariables, include_chr = include_chr, subset_1L_chr = subset_1L_chr, target_1L_chr = target_1L_chr, total_1L_lgl = total_1L_lgl, type_1L_chr = type_1L_chr, what_1L_chr = "outcomes"))
    }
    if(what_1L_chr %in% c("modifiable", "utilities")){
      object_xx <- c(object_xx, x@x_MimicAlgorithms@x_MimicUtility@names_chr)
    }
    if(what_1L_chr %in% c("modifiable", "resources")){
      object_xx <- c(object_xx,manufacture(x@x_MimicVariables, include_chr = include_chr, subset_1L_chr = subset_1L_chr, target_1L_chr = target_1L_chr, total_1L_lgl = total_1L_lgl, type_1L_chr = type_1L_chr, what_1L_chr = "resources"))
    }
    object_xx <- object_xx %>% unique()
  }
  if(what_1L_chr == "population_ls"){
    if(type_1L_chr == "current"){
      object_xx <- manufacture(x@x_MimicPopulation, what_1L_chr = what_1L_chr)
    }
    if(type_1L_chr == "entry"){
      object_xx <- add_enter_model_event(X_Ready4useDyad = x@x_MimicInputs@y_Ready4useDyad, 
                                         arm_1L_chr = arm_1L_chr, 
                                         default_fn = x@x_MimicAlgorithms@processing_ls$initialise_ls$default_fn,
                                         default_args_ls = list(sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls,
                                                                X_MimicVariable = x@x_MimicVariables),
                                         derive_fn_ls = x@x_MimicAlgorithms@processing_ls$initialise_ls$derive_ls,
                                         draws_tb = draws_tb,
                                         horizon_dtm = x@horizon_dtm,
                                         iterations_int = manufacture(x, batch_1L_int = batch_1L_int, what_1L_chr = "iterations"), 
                                         modifiable_chr = c(manufacture(x, include_chr = include_chr, target_1L_chr = target_1L_chr, total_1L_lgl = total_1L_lgl, type_1L_chr = "measure", what_1L_chr = "outcomes"),
                                                            manufacture(x, what_1L_chr = "utilities")),
                                         start_dtm = x@start_dtm,  
                                         tfmn_ls = x@x_MimicAlgorithms@transformations_ls, 
                                         tx_duration_dtm = procure(x, match_value_xx = arm_1L_chr, empty_xx = NULL, target_1L_chr = "Treatment duration"),
                                         tidy_cols_1L_lgl = T,
                                         tx_prefix_1L_chr = x@tx_prefix_1L_chr) %>%
        update_population_ls(population_ls = NULL,  type_1L_chr = "form")
    }
  }
  return(object_xx)
}
manufacture_MimicDerivations <- function(x,
                                         env_ls = list(),
                                         flatten_1L_lgl = FALSE,
                                         name_1L_chr = character(0),
                                         what_1L_chr = c("args_ls"),
                                         X_MimicConfiguration = MimicConfiguration(),
                                         ...){
  if(what_1L_chr=="args_ls"){
    object_xx <- x@args_fixed_ls
    # if(!is.na(x@method_1L_chr[1])){
      # class_1L_chr <- class(X_MimicConfiguration) %>% as.character()
      # allowed_chr <- names(Rdpack::S4formals(x@method_1L_chr, class_1L_chr))
      if(!identical(env_ls, list())){
        object_xx <- x@args_env_ls %>% purrr::map(~ purrr::pluck(env_ls,.x)) %>%
          append(object_xx)
      }
      if(!is.na(x@method_1L_chr[1])){
        object_xx <- rlang::exec(x@method_1L_chr, X_MimicConfiguration, !!!object_xx)
        if(!flatten_1L_lgl){
          object_xx <- object_xx %>% list()
        }
        if(!identical(name_1L_chr, character(0))){
          object_xx <- object_xx %>% stats::setNames(name_1L_chr)
        }
      }
      # procure(X_MimicConfiguration, match_value_xx = arm_1L_chr, empty_xx = character(0), target_1L_chr = "Treatment")
    # }
  }
  return(object_xx)
}
manufacture_MimicEligible <- function(x,
                                      append_ls = list(),
                                      type_1L_chr = c("filter", "reset"),
                                      what_1L_chr = "args_ls",
                                      ...){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "args_ls"){
    object_xx <- list(ineligible_1L_chr = x@ineligible_1L_chr,
                      post_fn = x@functions_ls$post_fn,
                      pre_fn = x@functions_ls$pre_fn,
                      type_1L_chr = type_1L_chr)
    if(!is.null(names(append_ls))){
      object_xx <- object_xx %>% purrr::discard_at(names(append_ls)) %>% append(append_ls)
    }
  }
  return(object_xx)
}
manufacture_MimicInputs <- function(x,
                                    what_1L_chr = c("inputs_ls")){
  if(what_1L_chr == "inputs_ls"){
    object_xx <- list(models_ls = x@models_ls,
                      params_tb = x@x_Ready4useDyad@ds_tb,
                      Synthetic_r4 = x@y_Ready4useDyad)
  }
  return(object_xx)
}
manufacture_MimicPopulation <- function(x,
                                        what_1L_chr = c("population_ls"),
                                        ...){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "population_ls"){
    object_xx <- list(X_Ready4useDyad = x@x_MimicActive@x_Ready4useDyad,
                      Y_Ready4useDyad = x@y_Ready4useDyad,
                      Z_Ready4useDyad = x@z_Ready4useDyad)
  }
  return(object_xx)
}
manufacture_MimicRepos <- function(x,
                                   # draw_to_1L_chr = "BatchedParamDraws", # UPDATE / REMOVE
                                   prefix_1L_chr = character(0),
                                   return_1L_chr = c("default", "batches", "files"),
                                   suffix_1L_chr = "",
                                   type_1L_chr = c("all", "batch_to", "draw_to"),
                                   what_1L_chr = c("sim_ws_dirs_chr"),
                              ...){
  return_1L_chr <- match.arg(return_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if(what_1L_chr=="sim_ws_dirs_chr"){ 
    object_xx <- c(
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr),
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr,
             x@divider_1L_chr,
             x@r_dir_1L_chr),
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr,
             x@divider_1L_chr,
             x@r_dir_1L_chr,
             x@divider_1L_chr,
             x@batch_to_1L_chr,
             suffix_1L_chr),
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr,
             x@divider_1L_chr,
             x@r_dir_1L_chr,
             x@divider_1L_chr,
             x@draw_to_1L_chr,
             suffix_1L_chr) 
    )
    if(type_1L_chr == "batch_to"){
      object_xx <- object_xx[3]
      if(identical(prefix_1L_chr, character(0))){
        prefix_1L_chr <- "SimBatch"
      }
    }
    if(type_1L_chr == "draw_to"){
      object_xx <- object_xx[4]
      if(identical(prefix_1L_chr, character(0))){
        prefix_1L_chr <- "ParamDrawsBatch"
      }
    }
  }
  if(return_1L_chr %in% c("batches", "files")){
    object_xx <- list.files(object_xx, full.names = F)
    object_xx <- object_xx[endsWith(object_xx, ".RDS")] %>% sort()
    if(return_1L_chr == c("batches")){
      object_xx <- stringr::str_remove(object_xx, pattern = prefix_1L_chr) %>% stringr::str_sub(end=-5) %>% as.integer() %>% sort()
    }
  }
  return(object_xx)
}
manufacture_MimicVariables <- function(x,
                                       include_chr = character(0),#c("Modelled", "Total"),
                                       subset_1L_chr = character(0),
                                       target_1L_chr = character(0),
                                       total_1L_lgl = FALSE,
                                       type_1L_chr = c("measure","concept"),
                                       what_1L_chr = c("both", "outcomes", "resources"),
                                       ...){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "outcomes"){
    object_xx <- x@outcomes_tb
    }
  if(what_1L_chr == "resources"){
    object_xx <- x@resources_tb 
    }
  if(what_1L_chr %in% c("outcomes", "resources")){
    if(!identical(target_1L_chr, character(0))){
      object_xx <- object_xx %>% dplyr::filter(Category == target_1L_chr)
    }
    if(!identical(subset_1L_chr, character(0))){
      object_xx <- object_xx %>% dplyr::filter(Subcategory == subset_1L_chr)
    }
    if(!identical(include_chr, character(0))){
      object_xx <- object_xx %>% dplyr::filter(Derivation %in% include_chr)
    }
    if(what_1L_chr == "outcomes"){
      object_xx <- object_xx %>% dplyr::pull(Outcome)
    }
    if(what_1L_chr == "resources"){
      if(type_1L_chr == "concept"){
        object_xx <- object_xx %>%
          dplyr::mutate(Measure = "")
      }
      object_xx <- object_xx %>% 
        dplyr::mutate(ResourceUse = paste0(Resource, Measure)) %>%
        dplyr::group_by(Category) %>% 
        dplyr::reframe(ResourceUse = ResourceUse, Totals = purrr::map2_chr(Total, Measure, ~ifelse(is.na(.x) | (!total_1L_lgl), NA_character_, paste0(.x,.y)))) %>%  
        dplyr::select(-Category) %>% purrr::flatten_chr() %>% purrr::discard(is.na) %>% unique()
    }
  }
  if(what_1L_chr == "both"){
    object_xx <- c(character(0),
                   manufacture(x, include_chr =  include_chr, subset_1L_chr = subset_1L_chr, target_1L_chr = intersect(target_1L_chr, x@outcomes_tb$Category), total_1L_lgl = total_1L_lgl, type_1L_chr = type_1L_chr, what_1L_chr = "outcomes"),
                   manufacture(x,  include_chr =  include_chr, subset_1L_chr = subset_1L_chr, target_1L_chr = intersect(target_1L_chr, x@resources_tb$Category), total_1L_lgl = total_1L_lgl, type_1L_chr = type_1L_chr, what_1L_chr = "resources")
                   )
  }
  return(object_xx)
}