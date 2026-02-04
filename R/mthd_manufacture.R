#' 
#' Manufacture a new object
#' @name manufacture-MimicRepos
#' @description manufacture method applied to MimicRepos
#' @param x An object of class MimicRepos
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param type_1L_chr Type (a character vector of length one), Default: c("all", "batch_to", "draw_to")
#' @param what_1L_chr What (a character vector of length one), Default: c("sim_ws_dirs_chr")
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicRepos-method
#' @export 
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicRepos", function (x, suffix_1L_chr = "", type_1L_chr = c("all", "batch_to", 
    "draw_to"), what_1L_chr = c("sim_ws_dirs_chr"), ...) 
{
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
            x@draw_to_1L_chr))
        if (type_1L_chr == "batch_to") {
            object_xx <- object_xx[3]
        }
        if (type_1L_chr == "draw_to") {
            object_xx <- object_xx[4]
        }
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
