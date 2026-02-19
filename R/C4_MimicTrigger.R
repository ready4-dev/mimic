#' MimicTrigger
#' 
#' Model event logic data.
#' 
#' @include C4_MimicArguments.R
#' @slot assert_1L_lgl Assert (a logical vector of length one)
#' @slot event_1L_chr Event (a character vector of length one)
#' @slot functions_ls Functions (a list)
#' @slot use_1L_chr Use (a character vector of length one)
#' @slot validate_chr Validate (a character vector)
#' @slot x_MimicArguments  (an instance of the MimicArguments class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicTrigger-class
#' @rdname MimicTrigger-class
#' @export MimicTrigger
#' @exportClass MimicTrigger
MimicTrigger <- methods::setClass("MimicTrigger",
contains = "Ready4Module",
slots = c(assert_1L_lgl = "logical",event_1L_chr = "character",functions_ls = "list",use_1L_chr = "character",validate_chr = "character",x_MimicArguments = "MimicArguments",dissemination_1L_chr = "character"),
prototype =  list(assert_1L_lgl = FALSE,event_1L_chr = NA_character_,functions_ls = list(action_fn = identity, invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf)),use_1L_chr = 'Y',validate_chr = character(0),x_MimicArguments = MimicArguments()))


methods::setValidity(methods::className("MimicTrigger"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
