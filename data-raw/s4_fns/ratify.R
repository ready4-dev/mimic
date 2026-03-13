ratify_MimicRepos <- function(x,
                              batches_int = integer(0),
                              gh_token_1L_chr = "",
                              key_1L_chr = NULL,
                              match_xx = NULL,
                              prefix_1L_chr = character(0),
                              suffix_1L_chr = "",
                              remote_fls_chr = NA_character_,
                              type_1L_chr = c("MimicInputs", "ParamDraws", "list", "element"),
                              what_chr = character(0),
                              ...){
  type_1L_chr <- match.arg(type_1L_chr)
  ingest_xx <- ingest(Y, batches_int = batches_int, 
                    gh_token_1L_chr = gh_token_1L_chr,
                    key_1L_chr = key_1L_chr,
                    prefix_1L_chr = prefix_1L_chr,
                    remote_fls_chr = remote_fls_chr,
                    type_1L_chr = type_1L_chr,
                    what_chr = what_chr, 
                    ...)
  pass_1L_lgl <- assertthat::assert_that(!is.null(ingest_xx), msg = "Ingested data is NULL.")
  if(!pass_1L_lgl){
    if(is.null(matches_xx)){
      if(type_1L_chr == "ParamDraws"){
        # dir_1L_chr <- manufacture(x, prefix_1L_chr = prefix_1L_chr, suffix_1L_chr = suffix_1L_chr, type_1L_chr = "batch_to", what_1L_chr = "sim_ws_dirs_chr") 
        if(identical(batches_int, integer(0))){
          batches_1L_int = batches_int
        }else{
          batches_1L_int = max(batches_int)
        }
        match_xx  <- import_results_batches(batches_1L_int = batches_1L_int, drop_params_1L_lgl = FALSE, suffix_1L_chr = suffix_1L_chr, Y_MimicRepos = x) %>% # update when this is an ingest method
          purrr::map_dfr(~ .x@ds_tb %>% dplyr::select(tidyselect::all_of(names(ingest_xx))) %>% dplyr::filter(Iteration %in% unique(ingest_xx$Iteration)) %>% dplyr::group_by(Iteration) %>% dplyr::summarise(dplyr::across(tidyselect::everything(), unique)) %>% dplyr::mutate(Iteration = as.integer(Iteration))) %>%
          dplyr::distinct()
      }else{
        pass_1L_lgl <- assertthat::assert_that(FALSE, msg = "Test data is NULL.")
      }
    }
    pass_1L_lgl <- assertthat::assert_that(identical(ingest_xx, match_xx), msg = "Ingested data does not match test data.")
  }
  return(pass_1L_lgl)
}