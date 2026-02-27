#' MimicRepos
#' 
#' Local and remote repositories for model input and output data.
#' 
#' @include 
#' @slot batch_to_1L_chr Batch to (a character vector of length one)
#' @slot draw_to_1L_chr Draw to (a character vector of length one)
#' @slot divider_1L_chr Divider (a character vector of length one)
#' @slot outp_data_dir_1L_chr Output data directory (a character vector of length one)
#' @slot path_to_keys_1L_chr Path to keys (a character vector of length one)
#' @slot path_to_output_1L_chr Path to output (a character vector of length one)
#' @slot path_to_param_data_1L_chr Path to parameter data (a character vector of length one)
#' @slot path_to_private_1L_chr Path to private (a character vector of length one)
#' @slot processed_dir_1L_chr Processed directory (a character vector of length one)
#' @slot r_dir_1L_chr R directory (a character vector of length one)
#' @slot raw_dir_1L_chr Raw directory (a character vector of length one)
#' @slot reports_dir_1L_chr Reports directory (a character vector of length one)
#' @slot x_Ready4useRepos  (an instance of the Ready4useRepos class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicRepos-class
#' @rdname MimicRepos-class
#' @export MimicRepos
#' @exportClass MimicRepos
MimicRepos <- methods::setClass("MimicRepos",
contains = "Ready4Module",
slots = c(batch_to_1L_chr = "character",draw_to_1L_chr = "character",divider_1L_chr = "character",outp_data_dir_1L_chr = "character",path_to_keys_1L_chr = "character",path_to_output_1L_chr = "character",path_to_param_data_1L_chr = "character",path_to_private_1L_chr = "character",processed_dir_1L_chr = "character",r_dir_1L_chr = "character",raw_dir_1L_chr = "character",reports_dir_1L_chr = "character",x_Ready4useRepos = "Ready4useRepos",dissemination_1L_chr = "character"),
prototype =  list(batch_to_1L_chr = 'BatchedSimResults',draw_to_1L_chr = 'BatchedParamDraws',divider_1L_chr = '',outp_data_dir_1L_chr = 'Output',path_to_keys_1L_chr = NA_character_,path_to_output_1L_chr = NA_character_,path_to_param_data_1L_chr = NA_character_,path_to_private_1L_chr = NA_character_,processed_dir_1L_chr = 'Processed',r_dir_1L_chr = 'R',raw_dir_1L_chr = NA_character_,reports_dir_1L_chr = 'Reports',x_Ready4useRepos = ready4use::Ready4useRepos()))


methods::setValidity(methods::className("MimicRepos"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
