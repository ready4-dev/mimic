#' MimicVariables
#' 
#' Model outcome and resource use variables metadata.
#' 
#' @include fn_make.R
#' @slot outcomes_tb Outcomes (a tibble)
#' @slot resources_tb Resources (a tibble)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicVariables-class
#' @rdname MimicVariables-class
#' @export MimicVariables
#' @exportClass MimicVariables
MimicVariables <- methods::setClass("MimicVariables",
contains = "Ready4Module",
slots = c(outcomes_tb = "tbl_df",resources_tb = "tbl_df", dissemination_1L_chr = "character"),
prototype =  list(outcomes_tb = make_outcomes_tb(),resources_tb = make_resources_tb()))


methods::setValidity(methods::className("MimicVariables"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
