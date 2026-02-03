#' 
#' Manufacture a new object
#' @name manufacture-MimicRepos
#' @description manufacture method applied to MimicRepos
#' @param x An object of class MimicRepos
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param type_1L_chr Type (a character vector of length one), Default: c("all", "batch_to")
#' @param what_1L_chr What (a character vector of length one), Default: c("sim_ws_dirs_chr")
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,MimicRepos-method
#' @export 
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "MimicRepos", function (x, suffix_1L_chr = "", type_1L_chr = c("all", "batch_to"), 
    what_1L_chr = c("sim_ws_dirs_chr"), ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (what_1L_chr == "sim_ws_dirs_chr") {
        object_xx <- c(paste0(Y@path_to_output_1L_chr, Y@divider_1L_chr, 
            Y@processed_dir_1L_chr), paste0(Y@path_to_output_1L_chr, 
            Y@divider_1L_chr, Y@processed_dir_1L_chr, Y@divider_1L_chr, 
            Y@r_dir_1L_chr), paste0(Y@path_to_output_1L_chr, 
            Y@divider_1L_chr, Y@processed_dir_1L_chr, Y@divider_1L_chr, 
            Y@r_dir_1L_chr, Y@divider_1L_chr, Y@batch_to_1L_chr, 
            suffix_1L_chr))
        if (type_1L_chr == "batch_to") {
            object_xx <- object_xx[3]
        }
    }
    return(object_xx)
})
