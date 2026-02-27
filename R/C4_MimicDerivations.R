#' MimicDerivations
#' 
#' Data for deriving method arguments.
#' 
#' @include 
#' @slot method_1L_chr Method (a character vector of length one)
#' @slot args_env_ls Arguments (a list of environments)
#' @slot args_fixed_ls Arguments fixed (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicDerivations-class
#' @rdname MimicDerivations-class
#' @export MimicDerivations
#' @exportClass MimicDerivations
MimicDerivations <- methods::setClass("MimicDerivations",
contains = "Ready4Module",
slots = c(method_1L_chr = "character",args_env_ls = "list",args_fixed_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(method_1L_chr = NA_character_,args_env_ls = list(),args_fixed_ls = list()))


methods::setValidity(methods::className("MimicDerivations"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
