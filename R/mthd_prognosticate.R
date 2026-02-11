#' 
#' Prognosticate (make predictions) by solving a forward problem
#' @name prognosticate-MimicConfiguration
#' @description prognosticate method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param Y_MimicRepos PARAM_DESCRIPTION, Default: MimicRepos()
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param draws_1L_chr Draws (a character vector of length one), Default: c("make", "make_batch", "read", "read_batch")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param purge_1L_lgl Purge (a logical vector of length one), Default: FALSE
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param type_1L_chr Type (a character vector of length one), Default: c("NULL", "D", "AB", "C")
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "batch")
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: TRUE
#' @param ... Additional arguments
#' @return Errors (a list)
#' @rdname prognosticate-methods
#' @aliases prognosticate,MimicConfiguration-method
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom purrr flatten_int
#' @importFrom lubridate period NA_Date_
#' @importFrom rlang exec
#' @importFrom ready4 prognosticate
methods::setMethod("prognosticate", "MimicConfiguration", function (x, Y_MimicRepos = MimicRepos(), consent_1L_chr = "", 
    consent_indcs_int = 1L, draws_1L_chr = c("make", "make_batch", 
        "read", "read_batch"), options_chr = c("Y", "N"), purge_1L_lgl = FALSE, 
    suffix_1L_chr = "", type_1L_chr = c("NULL", "D", "AB", "C"), 
    what_1L_chr = c("all", "batch"), unlink_1L_lgl = TRUE, ...) 
{
    draws_1L_chr <- match.arg(draws_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "all") {
        author(Y_MimicRepos, consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, options_chr = options_chr, 
            suffix_1L_chr = suffix_1L_chr, what_1L_chr = "sim_ws_dirs_chr")
        write_to_1L_chr <- manufacture(Y_MimicRepos, suffix_1L_chr = suffix_1L_chr, 
            type_1L_chr = "batch_to", what_1L_chr = "sim_ws_dirs_chr")
        if (draws_1L_chr %in% c("make", "make_batch")) {
            author(X, consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
                options_chr = options_chr, unlink_1L_lgl = T, 
                what_1L_chr = "draws", Y_MimicRepos = Y_MimicRepos)
        }
        if (draws_1L_chr %in% c("make", "read")) {
            draws_tb <- ingest(Y_MimicRepos, type_1L_chr = "ParamDraws")
            test_1L_lgl <- assertthat::assert_that(identical(sort(draws_tb$Iteration), 
                sort(x@iterations_ls %>% purrr::flatten_int())), 
                msg = "Iterations in iteration list and composite parameter draws table do not match.")
        }
        else {
            draws_tb <- NULL
            batches_int <- manufacture(Y, return_1L_chr = "batches", 
                type_1L_chr = "draw_to")
            test_1L_lgl <- assertthat::assert_that(identical(batches_int, 
                1:length(X@iterations_ls)), msg = "Batches of parameter draws tables do not match batches of iterations in iterations list.")
        }
        extras_ls <- list(...)
        args_ls <- list(arms_chr = character(0), comparator_fn = NULL, 
            draws_tb = draws_tb, drop_missing_1L_lgl = logical(0), 
            horizon_dtm = lubridate::period(), intervention_fn = NULL, 
            iterations_ls = NULL, modifiable_chr = character(0), 
            purge_1L_lgl = purge_1L_lgl, seed_1L_int = integer(0), 
            sensitivities_ls = NULL, start_dtm = lubridate::NA_Date_, 
            synthesis_fn = NULL, tfmn_ls = NULL, type_1L_chr = type_1L_chr, 
            unlink_1L_lgl = unlink_1L_lgl, utilities_chr = character(0), 
            utility_fns_ls = NULL, write_to_1L_chr = write_to_1L_chr, 
            X_MimicConfiguration = x, Y_MimicRepos = Y_MimicRepos) %>% 
            append(extras_ls)
        errors_ls <- rlang::exec(predict_with_sim, !!!args_ls)
    }
    return(errors_ls)
})
