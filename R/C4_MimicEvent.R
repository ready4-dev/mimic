#' MimicEvent
#' 
#' Model event scheduling and event logic data.
#' 
#' @include C4_MimicSchedule.R C4_MimicTrigger.R
#' @slot MimicEligible (an instance of the MimicEligible class)
#' @slot x_MimicSchedule  (an instance of the MimicSchedule class)
#' @slot x_MimicTrigger  (an instance of the MimicTrigger class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicEvent-class
#' @rdname MimicEvent-class
#' @export MimicEvent
#' @exportClass MimicEvent
MimicEvent <- methods::setClass("MimicEvent",
contains = "Ready4Module",
slots = c(x_MimicEligible = "MimicEligible", x_MimicSchedule = "MimicSchedule",x_MimicTrigger = "MimicTrigger",dissemination_1L_chr = "character"),
prototype =  list(x_MimicEligible = MimicEligible(), x_MimicSchedule = MimicSchedule(),x_MimicTrigger = MimicTrigger()))


methods::setValidity(methods::className("MimicEvent"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
