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
#' 
#' Author and save files
#' @name author-MimicConfiguration
#' @description author method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: FALSE
#' @param what_1L_chr What (a character vector of length one), Default: c("draws")
#' @param Y_MimicRepos PARAM_DESCRIPTION, Default: MimicRepos()
#' @return No return value, called for side effects.
#' @rdname author-methods
#' @aliases author,MimicConfiguration-method
#' @export 
#' @importFrom purrr walk
#' @importFrom ready4 author
methods::setMethod("author", "MimicConfiguration", function (x, consent_1L_chr = "", consent_indcs_int = 1L, options_chr = c("Y", 
    "N"), unlink_1L_lgl = FALSE, what_1L_chr = c("draws"), Y_MimicRepos = MimicRepos()) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "draws") {
        author(Y_MimicRepos, what_1L_chr = c("sim_ws_dirs_chr"))
        draw_to_1L_chr <- manufacture(Y_MimicRepos, type_1L_chr = "draw_to", 
            what_1L_chr = "sim_ws_dirs_chr")
        if (unlink_1L_lgl) {
            list.files(draw_to_1L_chr, full.names = T) %>% write_to_delete_fls(consent_1L_chr = consent_1L_chr, 
                consent_indcs_int = consent_indcs_int, options_chr = options_chr, 
                return_1L_lgl = FALSE)
        }
        1:length(x@iterations_ls) %>% purrr::walk(~{
            draws_tb <- manufacture(x, batch_1L_int = .x, what_1L_chr = "draws_tb")
            write_obj_with_prompt(draws_tb, obj_nm_1L_chr = paste0("ParamDrawsBatch", 
                .x), outp_dir_1L_chr = draw_to_1L_chr, consent_1L_chr = consent_1L_chr, 
                consent_indcs_int = consent_indcs_int, options_chr = options_chr, 
                return_1L_lgl = FALSE)
        })
    }
})
