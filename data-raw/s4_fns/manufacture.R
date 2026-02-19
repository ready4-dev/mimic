manufacture_MimicArguments <- function(x,
                                       batch_1L_int = integer(0),
                                       env_ls = list(),
                                       what_1L_chr = c("args_ls"),
                                       X_MimicConfiguration = MimicConfiguration(),
                                       ...){
  what_1L_chr <- match.arg(what_1L_chr)
  # object_xx <- list()
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
    if(!identical(batch_1L_int, integer(0))){
      object_xx <- object_xx %>%
        append(list(iterations_int = manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, what_1L_chr = "iterations")))
    }
  }
  return(object_xx)
}
manufacture_MimicConfiguration <- function(x,
                                           arm_1L_chr = NA_character_,
                                           batch_1L_int = integer(0),
                                           extras_ls = list(),
                                           what_1L_chr = c("draws_tb", "args_all", "iterations", "population_ls")){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "args_all"){
    object_xx <- list(
      arm_1L_chr = arm_1L_chr,
      arms_chr = x@arms_tb$Arm,
      batch_1L_int = batch_1L_int,
      X_MimicConfiguration = x) %>%
      append(extras_ls) 
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
  if(what_1L_chr == "iterations"){
    if(identical(batch_1L_int, integer(0))){
      object_xx <- x@iterations_ls %>% purrr::flatten_int()
    }else{
      object_xx <- x@iterations_ls[[batch_1L_int]]
    } 
  }
  if(what_1L_chr == "population_ls"){
    object_xx <- manufacture(x@x_MimicPopulation, what_1L_chr = what_1L_chr)
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