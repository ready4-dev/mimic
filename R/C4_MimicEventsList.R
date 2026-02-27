#' MimicEventsList
#' 
#' Data on all events included in model.
#' 
#' @include 
#' @slot events_ls Events (a list)
#' @slot last_1L_chr Last (a character vector of length one)
#' @slot main_chr Main (a character vector)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicEventsList-class
#' @rdname MimicEventsList-class
#' @export MimicEventsList
#' @exportClass MimicEventsList
MimicEventsList <- methods::setClass("MimicEventsList",
contains = "Ready4Module",
slots = c(events_ls = "list",last_1L_chr = "character",main_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(events_ls = list(),last_1L_chr = 'WrapUp',main_chr = character(0)))


methods::setValidity(methods::className("MimicEventsList"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
