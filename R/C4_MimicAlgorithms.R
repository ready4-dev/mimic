#' MimicAlgorithms
#' 
#' The core set of functions that define simulation behaviour.
#' 
#' @include C4_MimicUtility.R fn_make.R
#' @slot main_ls Main (a list)
#' @slot processing_ls Processing (a list)
#' @slot sensitivities_ls Sensitivities (a list)
#' @slot transformations_ls Transformations (a list)
#' @slot x_MimicUtility  (an instance of the MimicUtility class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicAlgorithms-class
#' @rdname MimicAlgorithms-class
#' @export MimicAlgorithms
#' @exportClass MimicAlgorithms
MimicAlgorithms <- methods::setClass("MimicAlgorithms",
contains = "Ready4Module",
slots = c(main_ls = "list",processing_ls = "list",sensitivities_ls = "list",transformations_ls = "list",x_MimicUtility = "MimicUtility",dissemination_1L_chr = "character"),
prototype =  list(main_ls = make_simulation_fns_ls('main'),processing_ls = make_simulation_fns_ls('processing'),sensitivities_ls = make_simulation_fns_ls('sensitivity'),transformations_ls = make_simulation_fns_ls('transformation'),x_MimicUtility = MimicUtility()))


methods::setValidity(methods::className("MimicAlgorithms"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
