#' MimicArguments
#' 
#' Method argument data.
#' 
#' @slot iterations_1L_lgl Iterations (a logical vector of length one)
#' @slot derive_ls Derive (a list)
#' @slot models_ls Models (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicArguments-class
#' @rdname MimicArguments-class
#' @export MimicArguments
#' @exportClass MimicArguments
MimicArguments <- methods::setClass("MimicArguments",
contains = "Ready4Module",
slots = c(iterations_1L_lgl = "logical",derive_ls = "list",models_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(iterations_1L_lgl = NA,derive_ls = list(list()),models_ls = list(list())))


methods::setValidity(methods::className("MimicArguments"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
