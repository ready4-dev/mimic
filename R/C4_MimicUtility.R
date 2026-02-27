#' MimicUtility
#' 
#' Utility algorithms and supporting data (instrument names, modules, mapping functions and transformation functions).
#' 
#' @include 
#' @slot names_chr Names (a character vector)
#' @slot data_ls Data (a list)
#' @slot mapping_ls Mapping (a list)
#' @slot transformations_ls Transformations (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicUtility-class
#' @rdname MimicUtility-class
#' @export MimicUtility
#' @exportClass MimicUtility
MimicUtility <- methods::setClass("MimicUtility",
contains = "Ready4Module",
slots = c(names_chr = "character",data_ls = "list",mapping_ls = "list",transformations_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(names_chr = NA_character_,data_ls = list(),mapping_ls = list(),transformations_ls = list()))


methods::setValidity(methods::className("MimicUtility"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
