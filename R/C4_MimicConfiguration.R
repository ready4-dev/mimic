#' MimicConfiguration
#' 
#' Configuration details for a simulation run.
#' 
#' @include C4_Ready4usePointer.R C4_Ready4useIngest.R
#' @slot arms_chr Arms (a character vector)
#' @slot drop_missing_1L_lgl Drop missing (a logical vector of length one)
#' @slot drop_suffix_1L_chr Drop suffix (a character vector of length one)
#' @slot functions_ls Functions (a list)
#' @slot horizon_dtm Horizon (a date vector)
#' @slot inputs_ls Inputs (a list)
#' @slot iterations_ls Iterations (a list)
#' @slot modifiable_chr Modifiable (a character vector)
#' @slot prior_batches_1L_int Prior batches (an integer vector of length one)
#' @slot seed_1L_int Seed (an integer vector of length one)
#' @slot sensitivities_ls Sensitivities (a list)
#' @slot start_dtm Start (a date vector)
#' @slot utilities_chr Utilities (a character vector)
#' @slot x_MimicInputs  (an instance of the MimicInputs class)
#' @slot x_MimicRepos  (an instance of the MimicRepos class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name MimicConfiguration-class
#' @rdname MimicConfiguration-class
#' @export MimicConfiguration
#' @exportClass MimicConfiguration
MimicConfiguration <- methods::setClass("MimicConfiguration",
contains = "Ready4Module",
slots = c(arms_chr = "character",drop_missing_1L_lgl = "logical",drop_suffix_1L_chr = "character",functions_ls = "list",horizon_dtm = "period",inputs_ls = "list",iterations_ls = "list",modifiable_chr = "character",prior_batches_1L_int = "integer",seed_1L_int = "integer",sensitivities_ls = "list",start_dtm = "POSIXt",utilities_chr = "character",x_MimicInputs = "MimicInputs",x_MimicRepos = "MimicRepos",dissemination_1L_chr = "character"),
prototype =  list(arms_chr = NA_character_,drop_missing_1L_lgl = NA,drop_suffix_1L_chr = NA_character_,functions_ls = list(list()),horizon_dtm = lubridate::period(),inputs_ls = list(list()),iterations_ls = list(list()),modifiable_chr = NA_character_,prior_batches_1L_int = NA_integer_,seed_1L_int = NA_integer_,sensitivities_ls = list(list()),start_dtm = .POSIXct(NA_character_),utilities_chr = NA_character_,x_MimicInputs = MimicInputs(),x_MimicRepos = MimicRepos()))

