#' MimicEligible
#' 
#' Model event eligibility logic data.
#' 
#' @include fn_make.R
#' @slot ineligible_1L_chr Ineligible (a character vector of length one)
#' @slot functions_ls Functions (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicEligible-class
#' @rdname MimicEligible-class
#' @export MimicEligible
#' @exportClass MimicEligible
MimicEligible <- methods::setClass("MimicEligible",
contains = "Ready4Module",
slots = c(ineligible_1L_chr = "character",functions_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(ineligible_1L_chr = character(0),functions_ls = make_ineligibility_fns_ls()))


methods::setValidity(methods::className("MimicEligible"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
