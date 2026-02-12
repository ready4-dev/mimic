#' 
#' Ingest data
#' @name ingest-MimicRepos
#' @description ingest method applied to MimicRepos
#' @param x An object of class MimicRepos
#' @param batches_int Batches (an integer vector), Default: integer(0)
#' @param gh_token_1L_chr Github token (a character vector of length one), Default: ''
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: character(0)
#' @param remote_fls_chr Remote files (a character vector), Default: 'NA'
#' @param type_1L_chr Type (a character vector of length one), Default: c("MimicInputs", "ParamDraws", "list", "element")
#' @param what_chr What (a character vector), Default: character(0)
#' @param ... Additional arguments
#' @return Ingest (an output object of multiple potential types)
#' @rdname ingest-methods
#' @aliases ingest,MimicRepos-method
#' @export 
#' @importFrom purrr pluck reduce
#' @importFrom dplyr bind_rows
#' @importFrom ready4 ingest
methods::setMethod("ingest", "MimicRepos", function (x, batches_int = integer(0), gh_token_1L_chr = "", 
    key_1L_chr = NULL, prefix_1L_chr = character(0), remote_fls_chr = NA_character_, 
    type_1L_chr = c("MimicInputs", "ParamDraws", "list", "element"), 
    what_chr = character(0), ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    ingest_xx <- list()
    if (type_1L_chr == "MimicInputs") {
        what_chr <- unique(c(what_chr, c("models_ls", "params_tb", 
            "Synthetic_r4")))
    }
    else {
        if (type_1L_chr == "element") {
            what_chr <- what_chr[1]
        }
    }
    if (type_1L_chr == "ParamDraws") {
        if (identical(prefix_1L_chr, character(0))) {
            prefix_1L_chr <- "ParamDrawsBatch"
        }
        dir_1L_chr <- manufacture(x, type_1L_chr = "draw_to", 
            what_1L_chr = "sim_ws_dirs_chr")
        files_chr <- manufacture(x, return_1L_chr = "files", 
            type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr")
        what_chr <- unique(c(what_chr, files_chr))
    }
    if (!is.na(remote_fls_chr)) {
        ingest_xx <- ingest(x@x_Ready4useRepos, fls_to_ingest_chr = remote_fls_chr, 
            metadata_1L_lgl = F)
        what_chr <- setdiff(what_chr, names(ingest_xx))
    }
    if (!type_1L_chr %in% "ParamDraws") {
        if ("models_ls" %in% what_chr) {
            ingest_xx <- ingest_xx %>% append(list(models_ls = x@path_to_private_1L_chr %>% 
                import_project_data(dir_1L_chr = x@processed_dir_1L_chr, 
                  r_dir_1L_chr = x@r_dir_1L_chr, custom_1L_chr = "regressions", 
                  type_1L_chr = "custom", names_ls = list("models_ls")) %>% 
                purrr::pluck("models_ls")))
        }
        if ("params_tb" %in% what_chr) {
            ingest_xx <- ingest_xx %>% append(list(params_tb = x@path_to_private_1L_chr %>% 
                import_project_data(dir_1L_chr = x@processed_dir_1L_chr, 
                  r_dir_1L_chr = x@r_dir_1L_chr, custom_1L_chr = "parameters", 
                  type_1L_chr = "custom", names_ls = list("params_tb")) %>% 
                purrr::pluck("params_tb")))
        }
        if ("Synthetic_r4" %in% what_chr) {
            ingest_xx <- ingest_xx %>% append(list(Synthetic_r4 = x@path_to_private_1L_chr %>% 
                import_project_data(dir_1L_chr = x@processed_dir_1L_chr, 
                  r_dir_1L_chr = x@r_dir_1L_chr, custom_1L_chr = "population", 
                  type_1L_chr = "custom", names_ls = list("fully_synthetic_ls")) %>% 
                purrr::pluck("fully_synthetic_ls") %>% purrr::pluck("Synthetic_r4")))
        }
    }
    if (type_1L_chr %in% "ParamDraws") {
        if (identical(batches_int, integer(0))) {
            batches_int <- manufacture(x, return_1L_chr = "batches", 
                type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr")
        }
        ingest_xx <- batches_int %>% purrr::reduce(.init = NULL, 
            ~{
                new_tb <- readRDS(paste0(dir_1L_chr, "/", files_chr[.y]))
                if (!is.null(.x)) {
                  new_tb <- dplyr::bind_rows(.x, new_tb)
                }
                new_tb
            })
    }
    if (type_1L_chr == "MimicInputs") {
        ingest_xx <- MimicInputs() %>% renewSlot("models_ls", 
            ingest_xx$models_ls) %>% renewSlot("x_Ready4useDyad@ds_tb", 
            ingest_xx$params_tb) %>% renewSlot("y_Ready4useDyad", 
            ingest_xx$Synthetic_r4)
        ingest_xx <- renewSlot(ingest_xx, "x_Ready4useDyad", 
            renew(ingest_xx@x_Ready4useDyad, what_1L_chr = "dictionary", 
                type_1L_chr = "new"))
    }
    else {
        if (type_1L_chr == "element") {
            ingest_xx <- ingest_xx %>% purrr::pluck(what_chr)
        }
    }
    return(ingest_xx)
})
