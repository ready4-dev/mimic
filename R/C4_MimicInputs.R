#' MimicInputs
#' 
#' Model input data (lookup tables, regression models, population and parameters).
#' 
#' @slot lookups_ls Lookups (a list)
#' @slot models_ls Models (a list)
#' @slot x_Ready4useDyad  (an instance of the Ready4useDyad class)
#' @slot y_Ready4useDyad  (an instance of the Ready4useDyad class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicInputs-class
#' @rdname MimicInputs-class
#' @export MimicInputs
#' @exportClass MimicInputs
MimicInputs <- methods::setClass("MimicInputs",
contains = "Ready4Module",
slots = c(lookups_ls = "list",models_ls = "list",x_Ready4useDyad = "Ready4useDyad",y_Ready4useDyad = "Ready4useDyad",dissemination_1L_chr = "character"),
prototype =  list(lookups_ls = list(),models_ls = list(),x_Ready4useDyad = ready4use::Ready4useDyad(),y_Ready4useDyad = ready4use::Ready4useDyad()))


methods::setValidity(methods::className("MimicInputs"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
