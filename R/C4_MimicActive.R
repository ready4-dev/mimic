#' MimicActive
#' 
#' Model active population (currently eligible for events).
#' 
#' @slot x_Ready4useDyad  (an instance of the Ready4useDyad class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicActive-class
#' @rdname MimicActive-class
#' @export MimicActive
#' @exportClass MimicActive
MimicActive <- methods::setClass("MimicActive",
contains = "Ready4Module",
slots = c(x_Ready4useDyad = "Ready4useDyad",dissemination_1L_chr = "character"),
prototype =  list(x_Ready4useDyad = ready4use::Ready4useDyad()))


methods::setValidity(methods::className("MimicActive"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
