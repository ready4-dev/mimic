#' MimicPopulation
#' 
#' Model dynamic population data container (specifying population subgroups who are active and eligible for the next model event,  who are temporariliy inactive but are still in the model or who have exited the model).
#' 
#' @include C4_MimicActive.R
#' @slot x_MimicActive  (an instance of the MimicActive class)
#' @slot y_Ready4useDyad  (an instance of the Ready4useDyad class)
#' @slot z_Ready4useDyad  (an instance of the Ready4useDyad class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicPopulation-class
#' @rdname MimicPopulation-class
#' @export MimicPopulation
#' @exportClass MimicPopulation
MimicPopulation <- methods::setClass("MimicPopulation",
contains = "Ready4Module",
slots = c(x_MimicActive = "MimicActive",y_Ready4useDyad = "Ready4useDyad",z_Ready4useDyad = "Ready4useDyad",dissemination_1L_chr = "character"),
prototype =  list(x_MimicActive = MimicActive(),y_Ready4useDyad = ready4use::Ready4useDyad(),z_Ready4useDyad = ready4use::Ready4useDyad()))


methods::setValidity(methods::className("MimicPopulation"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
