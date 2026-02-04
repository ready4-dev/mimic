#' MimicConfiguration
#' 
#' Configuration details for a simulation run.
#' 
#' @include C4_MimicInputs.R fn_make.R
#' @slot arms_chr Arms (a character vector)
#' @slot arms_tb Arms (a tibble)
#' @slot drop_missing_1L_lgl Drop missing (a logical vector of length one)
#' @slot drop_suffix_1L_chr Drop suffix (a character vector of length one)
#' @slot horizon_dtm Horizon (a date vector)
#' @slot iterations_ls Iterations (a list)
#' @slot modifiable_chr Modifiable (a character vector)
#' @slot prior_batches_1L_int Prior batches (an integer vector of length one)
#' @slot seed_1L_int Seed (an integer vector of length one)
#' @slot start_dtm Start (a date vector)
#' @slot utilities_chr Utilities (a character vector)
#' @slot x_MimicAlgorithms  (an instance of the MimicAlgorithms class)
#' @slot x_MimicInputs  (an instance of the MimicInputs class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicConfiguration-class
#' @rdname MimicConfiguration-class
#' @export MimicConfiguration
#' @exportClass MimicConfiguration
MimicConfiguration <- methods::setClass("MimicConfiguration",
contains = "Ready4Module",
slots = c(arms_chr = "character",arms_tb = "tbl_df",drop_missing_1L_lgl = "logical",drop_suffix_1L_chr = "character",horizon_dtm = "Period",iterations_ls = "list",modifiable_chr = "character",prior_batches_1L_int = "integer",seed_1L_int = "integer",start_dtm = "POSIXt",utilities_chr = "character",x_MimicAlgorithms = "MimicAlgorithms",x_MimicInputs = "MimicInputs",dissemination_1L_chr = "character"),
prototype =  list(arms_chr = c('Intervention', 'Comparator'),arms_tb = make_arms_tb(),drop_missing_1L_lgl = FALSE,drop_suffix_1L_chr = NA_character_,horizon_dtm = lubridate::years(1),iterations_ls = make_batches(5, of_1L_int = 20),modifiable_chr = NA_character_,prior_batches_1L_int = 0L,seed_1L_int = 2001L,start_dtm = Sys.Date(),utilities_chr = NA_character_,x_MimicAlgorithms = MimicAlgorithms(),x_MimicInputs = MimicInputs()))


methods::setValidity(methods::className("MimicConfiguration"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
