#' 
#' Manufacture a new object
#' @name manufacture-MimicConfiguration
#' @description manufacture method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param arm_1L_chr Arm (a character vector of length one), Default: 'NA'
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param extras_ls Extras (a list), Default: list()
#' @param what_1L_chr What (a character vector of length one), Default: c("draws_tb", "args_all", "iterations", "population_ls")
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicConfiguration-method
#' @export 
#' @importFrom purrr flatten_int
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicConfiguration", function (x, arm_1L_chr = NA_character_, batch_1L_int = integer(0), 
    extras_ls = list(), what_1L_chr = c("draws_tb", "args_all", 
        "iterations", "population_ls")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "args_all") {
        object_xx <- list(arm_1L_chr = arm_1L_chr, arms_chr = x@arms_tb$Arm, 
            batch_1L_int = batch_1L_int, X_MimicConfiguration = x) %>% 
            append(extras_ls)
    }
    if (what_1L_chr == "draws_tb") {
        iterations_int <- manufacture(x, batch_1L_int = batch_1L_int, 
            what_1L_chr = "iterations")
        if (identical(batch_1L_int, integer(0))) {
            batch_1L_int <- 0
        }
        inputs_ls <- manufacture(x@x_MimicInputs, what_1L_chr = "inputs_ls")
        object_xx <- make_draws_tb(inputs_ls, extra_draws_fn = x@x_MimicAlgorithms@processing_ls$extra_draws_fn, 
            iterations_int = iterations_int, drop_missing_1L_lgl = x@drop_missing_1L_lgl, 
            drop_suffix_1L_chr = x@drop_suffix_1L_chr, seed_1L_int = x@seed_1L_int + 
                batch_1L_int)
    }
    if (what_1L_chr == "iterations") {
        if (identical(batch_1L_int, integer(0))) {
            object_xx <- x@iterations_ls %>% purrr::flatten_int()
        }
        else {
            object_xx <- x@iterations_ls[[batch_1L_int]]
        }
    }
    if (what_1L_chr == "population_ls") {
        object_xx <- manufacture(x@x_MimicPopulation, what_1L_chr = what_1L_chr)
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
        object_xx <- list(X_Ready4useDyad = x@x_Ready4useDyad, 
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
