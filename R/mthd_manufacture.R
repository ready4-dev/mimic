#' 
#' Manufacture a new object
#' @name manufacture-MimicArguments
#' @description manufacture method applied to MimicArguments
#' @param x An object of class MimicArguments
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param env_ls Environment list (a list of environments), Default: list()
#' @param what_1L_chr What (a character vector of length one), Default: c("args_ls")
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicArguments-method
#' @export 
#' @importFrom purrr map pluck
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicArguments", function(x,
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
  }
  return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-MimicConfiguration
#' @description manufacture method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param arm_1L_chr Arm (a character vector of length one), Default: 'NA'
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param extras_ls Extras (a list), Default: list()
#' @param type_1L_chr Type (a character vector of length one), Default: c("current", "entry")
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("draws_tb", "args_all", "iterations", "population_ls")
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicConfiguration-method
#' @export 
#' @importFrom purrr flatten_int
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicConfiguration", manufacture_MimicConfiguration <- function(x,
                                                                                                   arm_1L_chr = NA_character_,
                                                                                                   batch_1L_int = integer(0),
                                                                                                   draws_tb = NULL,
                                                                                                   extras_ls = list(),
                                                                                                   tx_prefix_1L_chr = character(0),
                                                                                                   type_1L_chr = c("current", "entry"),
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
    if(type_1L_chr == "current"){
      object_xx <- manufacture(x@x_MimicPopulation, what_1L_chr = what_1L_chr)
    }
    if(type_1L_chr == "entry"){
      object_xx <- add_enter_model_event(X_Ready4useDyad = x@x_MimicInputs@y_Ready4useDyad, 
                                         default_fn = x@x_MimicAlgorithms@processing_ls$initialise_ls$default_fn,
                                         derive_fn_ls = x@x_MimicAlgorithms@processing_ls$initialise_ls$derive_ls,
                                         horizon_dtm = x@horizon_dtm,
                                         modifiable_chr = x@x_MimicAlgorithms@processing_ls$initialise_ls$update_fn(x@modifiable_chr),
                                         start_dtm = x@start_dtm,  
                                         tfmn_ls = x@x_MimicAlgorithms@transformations_ls, 
                                         tx_duration_dtm = procure(x, match_value_xx = arm_1L_chr, empty_xx = NULL, target_1L_chr = "Treatment duration"),
                                         arm_1L_chr = arm_1L_chr, 
                                         default_args_ls = list(sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls),
                                         draws_tb = draws_tb,
                                         iterations_int = manufacture(x, batch_1L_int = batch_1L_int, what_1L_chr = "iterations"), 
                                         tidy_cols_1L_lgl = T,
                                         tx_prefix_1L_chr = tx_prefix_1L_chr) %>%
        update_population_ls(population_ls = NULL,  type_1L_chr = "form")
    }
  }
  return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-MimicRepos
#' @description manufacture method applied to MimicRepos
#' @param x An object of class MimicRepos
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: character(0)
#' @param return_1L_chr Return (a character vector of length one), Default: c("default", "batches", "files")
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param type_1L_chr Type (a character vector of length one), Default: c("all", "batch_to", "draw_to")
#' @param what_1L_chr What (a character vector of length one), Default: c("sim_ws_dirs_chr")
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicRepos-method
#' @export 
#' @importFrom stringr str_remove str_sub
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicRepos", function (x, prefix_1L_chr = character(0), return_1L_chr = c("default", 
    "batches", "files"), suffix_1L_chr = "", type_1L_chr = c("all", 
    "batch_to", "draw_to"), what_1L_chr = c("sim_ws_dirs_chr"), 
    ...) 
{
    return_1L_chr <- match.arg(return_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (what_1L_chr == "sim_ws_dirs_chr") {
        object_xx <- c(paste0(x@path_to_output_1L_chr, x@divider_1L_chr, 
            x@processed_dir_1L_chr), paste0(x@path_to_output_1L_chr, 
            x@divider_1L_chr, x@processed_dir_1L_chr, x@divider_1L_chr, 
            x@r_dir_1L_chr), paste0(x@path_to_output_1L_chr, 
            x@divider_1L_chr, x@processed_dir_1L_chr, x@divider_1L_chr, 
            x@r_dir_1L_chr, x@divider_1L_chr, x@batch_to_1L_chr, 
            suffix_1L_chr), paste0(x@path_to_output_1L_chr, x@divider_1L_chr, 
            x@processed_dir_1L_chr, x@divider_1L_chr, x@r_dir_1L_chr, 
            x@divider_1L_chr, x@draw_to_1L_chr, suffix_1L_chr))
        if (type_1L_chr == "batch_to") {
            object_xx <- object_xx[3]
            if (identical(prefix_1L_chr, character(0))) {
                prefix_1L_chr <- "SimBatch"
            }
        }
        if (type_1L_chr == "draw_to") {
            object_xx <- object_xx[4]
            if (identical(prefix_1L_chr, character(0))) {
                prefix_1L_chr <- "ParamDrawsBatch"
            }
        }
    }
    if (return_1L_chr %in% c("batches", "files")) {
        object_xx <- list.files(object_xx, full.names = F)
        object_xx <- object_xx[endsWith(object_xx, ".RDS")] %>% 
            sort()
        if (return_1L_chr == c("batches")) {
            object_xx <- stringr::str_remove(object_xx, pattern = prefix_1L_chr) %>% 
                stringr::str_sub(end = -5) %>% as.integer() %>% 
                sort()
        }
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-MimicPopulation
#' @description manufacture method applied to MimicPopulation
#' @param x An object of class MimicPopulation
#' @param what_1L_chr What (a character vector of length one), Default: c("population_ls")
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicPopulation-method
#' @export 
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicPopulation", function (x, what_1L_chr = c("population_ls"), ...) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "population_ls") {
        object_xx <- list(X_Ready4useDyad = x@x_MimicActive@x_Ready4useDyad, 
            Y_Ready4useDyad = x@y_Ready4useDyad, Z_Ready4useDyad = x@z_Ready4useDyad)
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-MimicInputs
#' @description manufacture method applied to MimicInputs
#' @param x An object of class MimicInputs
#' @param what_1L_chr What (a character vector of length one), Default: c("inputs_ls")
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicInputs-method
#' @export 
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicInputs", function (x, what_1L_chr = c("inputs_ls")) 
{
    if (what_1L_chr == "inputs_ls") {
        object_xx <- list(models_ls = x@models_ls, params_tb = x@x_Ready4useDyad@ds_tb, 
            Synthetic_r4 = x@y_Ready4useDyad)
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-MimicDerivations
#' @description manufacture method applied to MimicDerivations
#' @param x An object of class MimicDerivations
#' @param env_ls Environment list (a list of environments), Default: list()
#' @param flatten_1L_lgl Flatten (a logical vector of length one), Default: FALSE
#' @param name_1L_chr Name (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("args_ls")
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicDerivations-method
#' @export 
#' @importFrom purrr map pluck flatten
#' @importFrom rlang exec
#' @importFrom stats setNames
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicDerivations", function(x,
                                                               env_ls = list(),
                                                               flatten_1L_lgl = FALSE,
                                                               name_1L_chr = character(0),
                                                               what_1L_chr = c("args_ls"),
                                                               X_MimicConfiguration = MimicConfiguration(),
                                                               ...){
  if(what_1L_chr=="args_ls"){
    object_xx <- x@args_fixed_ls
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
  }
  return(object_xx)
})
