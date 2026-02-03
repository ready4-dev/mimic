#' 
#' Author and save files
#' @name author-MimicRepos
#' @description author method applied to MimicRepos
#' @param x An object of class MimicRepos
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @param what_1L_chr What (a character vector of length one), Default: c("sim_ws_dirs_chr")
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname author-methods
#' @aliases author,MimicRepos-method
#' @export 
#' @importFrom ready4 author
methods::setMethod("author", "MimicRepos", function (x, consent_1L_chr = "", consent_indcs_int = 1L, options_chr = c("Y", 
    "N"), suffix_1L_chr = "", what_1L_chr = c("sim_ws_dirs_chr"), 
    ...) 
{
    sim_ws_dirs_chr <- character(0)
    if (what_1L_chr == "sim_ws_dirs_chr") {
        sim_ws_dirs_chr <- manufacture(x, suffix_1L_chr = suffix_1L_chr)
    }
    if (!identical(sim_ws_dirs_chr, character(0))) {
        write_new_dirs(sim_ws_dirs_chr, consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, options_chr = options_chr)
    }
})
