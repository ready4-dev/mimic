#' 
#' Prognosticate (make predictions) by solving a forward problem
#' @name prognosticate-MimicConfiguration
#' @description prognosticate method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param purge_1L_lgl Purge (a logical vector of length one), Default: FALSE
#' @param type_1L_chr Type (a character vector of length one), Default: c("NULL", "D", "AB", "C")
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: TRUE
#' @param ... Additional arguments
#' @return Errors (a list)
#' @rdname prognosticate-methods
#' @aliases prognosticate,MimicConfiguration-method
#' @export 
#' @importFrom ready4 write_new_dirs prognosticate
#' @importFrom rlang exec
methods::setMethod("prognosticate", "MimicConfiguration", function (x, consent_1L_chr = "", purge_1L_lgl = FALSE, type_1L_chr = c("NULL", 
    "D", "AB", "C"), unlink_1L_lgl = TRUE, ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    write_to_1L_chr <- paste0(x@x_MimicRepos@path_to_output_1L_chr, 
        x@x_MimicRepos@divider_1L_chr, x@x_MimicRepos@processed_dir_1L_chr, 
        x@x_MimicRepos@divider_1L_chr, x@x_MimicRepos@r_dir_1L_chr, 
        x@x_MimicRepos@divider_1L_chr, x@x_MimicRepos@batch_to_1L_chr)
    ready4::write_new_dirs(write_to_1L_chr, consent_1L_chr = consent_1L_chr)
    extras_ls <- list(...)
    args_ls <- list(arms_chr = x@arms_chr, comparator_fn = x@x_MimicAlgorithms@main_ls$comparator_fn, 
        drop_missing_1L_lgl = x@drop_missing_1L_lgl, drop_suffix_1L_chr = if (is.na(x@drop_suffix_1L_chr)) {
            character(0)
        } else {
            x@drop_suffix_1L_chr
        }, horizon_dtm = x@horizon_dtm, inputs_ls = list(models_ls = x@x_MimicInputs@models_ls, 
            params_tb = x@x_MimicInputs@x_Ready4useDyad@ds_tb, 
            Synthetic_r4 = x@x_MimicInputs@y_Ready4useDyad), 
        intervention_fn = x@x_MimicAlgorithms@main_ls$intervention_fn, 
        iterations_ls = x@iterations_ls, modifiable_chr = if (is.na(x@modifiable_chr)) {
            character(0)
        } else {
            x@modifiable_chr
        }, purge_1L_lgl = purge_1L_lgl, seed_1L_int = x@seed_1L_int, 
        sensitivities_ls = x@x_MimicAlgorithms@sensitivities_ls, 
        tfmn_ls = x@x_MimicAlgorithms@transformations_ls, utilities_chr = x@utilities_chr, 
        unlink_1L_lgl = unlink_1L_lgl, write_to_1L_chr = write_to_1L_chr, 
        type_1L_chr = type_1L_chr) %>% append(extras_ls)
    errors_ls <- rlang::exec(predict_with_sim, !!!args_ls)
    return(errors_ls)
})
