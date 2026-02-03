#' 
#' Prognosticate (make predictions) by solving a forward problem
#' @name prognosticate-MimicConfiguration
#' @description prognosticate method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param Y_MimicRepos PARAM_DESCRIPTION, Default: MimicRepos()
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param purge_1L_lgl Purge (a logical vector of length one), Default: FALSE
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param type_1L_chr Type (a character vector of length one), Default: c("NULL", "D", "AB", "C")
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: TRUE
#' @param ... Additional arguments
#' @return Errors (a list)
#' @rdname prognosticate-methods
#' @aliases prognosticate,MimicConfiguration-method
#' @export 
#' @importFrom rlang exec
#' @importFrom ready4 prognosticate
methods::setMethod("prognosticate", "MimicConfiguration", function (x, Y_MimicRepos = MimicRepos(), consent_1L_chr = "", 
    consent_indcs_int = 1L, options_chr = c("Y", "N"), purge_1L_lgl = FALSE, 
    suffix_1L_chr = "", type_1L_chr = c("NULL", "D", "AB", "C"), 
    unlink_1L_lgl = TRUE, ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    author(Y_MimicRepos, consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
        options_chr = options_chr, suffix_1L_chr = suffix_1L_chr, 
        what_1L_chr = "sim_ws_dirs_chr")
    write_to_1L_chr <- manufacture(Y, suffix_1L_chr = suffix_1L_chr, 
        type_1L_chr = "batch_to", what_1L_chr = "sim_ws_dirs_chr")
    extras_ls <- list(...)
    args_ls <- list(arms_chr = x@arms_chr, comparator_fn = x@x_MimicAlgorithms@main_ls$comparator_fn, 
        drop_missing_1L_lgl = x@drop_missing_1L_lgl, drop_suffix_1L_chr = if (is.na(x@drop_suffix_1L_chr)) {
            character(0)
        } else {
            x@drop_suffix_1L_chr
        }, extra_draws_fn = x@x_MimicAlgorithms@processing_ls$extra_draws_fn, 
        horizon_dtm = x@horizon_dtm, inputs_ls = list(models_ls = x@x_MimicInputs@models_ls, 
            params_tb = x@x_MimicInputs@x_Ready4useDyad@ds_tb, 
            Synthetic_r4 = x@x_MimicInputs@y_Ready4useDyad), 
        intervention_fn = x@x_MimicAlgorithms@main_ls$intervention_fn, 
        iterations_ls = x@iterations_ls, modifiable_chr = if (is.na(x@modifiable_chr[1])) {
            character(0)
        } else {
            x@modifiable_chr
        }, purge_1L_lgl = purge_1L_lgl, seed_1L_int = x@seed_1L_int, 
        sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls, 
        start_dtm = x@start_dtm, synthesis_fn = x@x_MimicAlgorithms@processing_ls$synthesis_fn, 
        tfmn_ls = x@x_MimicAlgorithms@transformations_ls, utilities_chr = x@utilities_chr, 
        unlink_1L_lgl = unlink_1L_lgl, write_to_1L_chr = write_to_1L_chr, 
        type_1L_chr = type_1L_chr) %>% append(extras_ls)
    errors_ls <- rlang::exec(predict_with_sim, !!!args_ls)
    return(errors_ls)
})
