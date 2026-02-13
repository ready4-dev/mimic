#' MimicConfiguration
#' 
#' Configuration details for a simulation run.
#' 
#' @include C4_MimicAlgorithms.R C4_MimicInputs.R C4_MimicPopulation.R fn_make.R
#' @slot arms_tb Arms (a tibble)
#' @slot drop_missing_1L_lgl Drop missing (a logical vector of length one)
#' @slot drop_suffix_1L_chr Drop suffix (a character vector of length one)
#' @slot horizon_dtm Horizon (a date vector)
#' @slot iterations_ls Iterations (a list)
#' @slot modifiable_chr Modifiable (a character vector)
#' @slot prior_batches_1L_int Prior batches (an integer vector of length one)
#' @slot seed_1L_int Seed (an integer vector of length one)
#' @slot start_dtm Start (a date vector)
#' @slot x_MimicAlgorithms  (an instance of the MimicAlgorithms class)
#' @slot x_MimicInputs  (an instance of the MimicInputs class)
#' @slot x_MimicPopulation  (an instance of the MimicPopulation class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicConfiguration-class
#' @rdname MimicConfiguration-class
#' @export MimicConfiguration
#' @exportClass MimicConfiguration
MimicConfiguration <- methods::setClass("MimicConfiguration",
contains = "Ready4Module",
slots = c(arms_tb = "tbl_df",drop_missing_1L_lgl = "logical",drop_suffix_1L_chr = "character",horizon_dtm = "Period",iterations_ls = "list",modifiable_chr = "character",prior_batches_1L_int = "integer",seed_1L_int = "integer",start_dtm = "Date",x_MimicAlgorithms = "MimicAlgorithms",x_MimicInputs = "MimicInputs",x_MimicPopulation = "MimicPopulation",dissemination_1L_chr = "character"),
prototype =  list(arms_tb = make_arms_tb(),drop_missing_1L_lgl = FALSE,drop_suffix_1L_chr = character(0),horizon_dtm = lubridate::years(1),iterations_ls = make_batches(5, of_1L_int = 20),modifiable_chr = character(0),prior_batches_1L_int = 0L,seed_1L_int = 2001L,start_dtm = Sys.Date(),x_MimicAlgorithms = MimicAlgorithms(),x_MimicInputs = MimicInputs(),x_MimicPopulation = MimicPopulation()))


methods::setValidity(methods::className("MimicConfiguration"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
