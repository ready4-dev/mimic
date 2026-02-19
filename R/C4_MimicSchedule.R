#' MimicSchedule
#' 
#' Model event scheduling data.
#' 
#' @include C4_MimicArguments.R
#' @slot assert_1L_lgl Assert (a logical vector of length one)
#' @slot event_1L_chr Event (a character vector of length one)
#' @slot functions_ls Functions (a list)
#' @slot step_dtm Step (a date vector)
#' @slot update_type_1L_chr Update type (a character vector of length one)
#' @slot use_1L_chr Use (a character vector of length one)
#' @slot validate_chr Validate (a character vector)
#' @slot x_MimicArguments  (an instance of the MimicArguments class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicSchedule-class
#' @rdname MimicSchedule-class
#' @export MimicSchedule
#' @exportClass MimicSchedule
MimicSchedule <- methods::setClass("MimicSchedule",
contains = "Ready4Module",
slots = c(assert_1L_lgl = "logical",event_1L_chr = "character",functions_ls = "list",step_dtm = "Date",update_type_1L_chr = "character",use_1L_chr = "character",validate_chr = "character",x_MimicArguments = "MimicArguments",dissemination_1L_chr = "character"),
prototype =  list(assert_1L_lgl = FALSE,event_1L_chr = NA_character_,functions_ls = list(invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf | x <0), schedule_fn = NULL),step_dtm = lubridate::days(0),update_type_1L_chr = 'split',use_1L_chr = 'Y',validate_chr = character(0),x_MimicArguments = MimicArguments()))


methods::setValidity(methods::className("MimicSchedule"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
