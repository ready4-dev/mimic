# ingest_MimicConfiguration <- function(x,
#                                       batches_int = integer(0),
#                                       prefix_1L_chr = character(0),
#                                       what_1L_chr = c("ParamDraws"),
#                                       Y_MimicRepos = MimicRepos()){
#   what_1L_chr <- match.arg(what_1L_chr)
#   # if(what_1L_chr == "ParamDraws"){
#   #   dir_1L_chr <- manufacture(Y_MimicRepos, type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr") 
#   # }
#   # files_chr <- list.files(dir_1L_chr, full.names = F)
#   # files_chr <- files_chr[endsWith(files_chr, ".RDS")] %>% sort()
#   if(what_1L_chr == "ParamDraws"){
#     dir_1L_chr <- manufacture(Y_MimicRepos, type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr") 
#     if(identical(prefix_1L_chr, character(0))){
#       prefix_1L_chr <- "ParamDrawsBatch"
#     }
#   }
#   if(what_1L_chr %in% "ParamDraws"){
#     if(identical(batches_int, integer(0))){
#       batches_int <- stringr::str_remove(files_chr, pattern = prefix_1L_chr) %>% stringr::str_sub(end=-5) %>% as.integer() %>% sort()
#     }
#     object_xx <- batches_int %>%
#       purrr:reduce(.init = NULL,
#                    ~ {
#                      new_tb <- readRDS(paste0(dir_1L_chr, "/", files_chr[.y]))
#                      if(!is.null(.x)){
#                        new_tb <- dplyr::bind_rows(.x, new_tb)
#                      }
#                      new_tb
#                    })
#   }
#   return(object_xx)
# }
ingest_MimicRepos <- function(x,
                              batches_int = integer(0),
                              gh_token_1L_chr = "",
                              key_1L_chr = NULL,
                              # metadata_1L_lgl = T,
                              prefix_1L_chr = character(0),
                              remote_fls_chr = NA_character_,
                              type_1L_chr = c("MimicInputs", "ParamDraws", "list", "element"),
                              what_chr = character(0),
                              ...){
  type_1L_chr <- match.arg(type_1L_chr)
  ingest_xx <- list()
  if(type_1L_chr == "MimicInputs"){ # Order important - needs to be before what_chr is updated
    what_chr <- unique(c(what_chr, c("models_ls", "params_tb", "Synthetic_r4")))
  }else{
    if(type_1L_chr == "element"){
      what_chr <- what_chr[1]
    }
  }
  if(type_1L_chr == "ParamDraws"){
    if(identical(prefix_1L_chr, character(0))){
      prefix_1L_chr <- "ParamDrawsBatch"
    }
    dir_1L_chr <- manufacture(x, type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr") 
    ## Next two lines only valid for locally stored batches - alternative logic needed for bathches in remote repository
    ##
    files_chr <- list.files(dir_1L_chr, full.names = F)
    files_chr <- files_chr[endsWith(files_chr, ".RDS")] %>% sort()
    ##
    what_chr <- unique(c(what_chr, files_chr))
  }
  if(!is.na(remote_fls_chr)){
    ingest_xx <- ingest(x@x_Ready4useRepos,
                         fls_to_ingest_chr = remote_fls_chr,
                         metadata_1L_lgl = F)
    what_chr <- setdiff(what_chr, names(ingest_xx))
  }
  if("models_ls" %in% what_chr){
    ingest_xx <- ingest_xx %>% 
      append(list(models_ls = x@path_to_private_1L_chr %>%
                    import_project_data(dir_1L_chr = x@processed_dir_1L_chr,
                                        r_dir_1L_chr = x@r_dir_1L_chr,
                                        custom_1L_chr = "regressions",
                                        type_1L_chr = "custom",
                                        names_ls = list("models_ls")) %>%
                    purrr::pluck("models_ls")))
    
  }
  if("params_tb" %in% what_chr){
    ingest_xx <- ingest_xx %>% 
      append(list(params_tb = x@path_to_private_1L_chr %>%
                    import_project_data(dir_1L_chr = x@processed_dir_1L_chr,
                                        r_dir_1L_chr = x@r_dir_1L_chr,
                                        custom_1L_chr = "parameters",
                                        type_1L_chr = "custom",
                                        names_ls = list("params_tb")) %>%
                    purrr::pluck("params_tb")))
    
  }
  if("Synthetic_r4" %in% what_chr){
    ingest_xx <- ingest_xx %>% 
      append(list(Synthetic_r4 = x@path_to_private_1L_chr %>%
                    import_project_data(dir_1L_chr = x@processed_dir_1L_chr,
                                        r_dir_1L_chr = x@r_dir_1L_chr,
                                        custom_1L_chr = "population",
                                        type_1L_chr = "custom",
                                        names_ls = list("fully_synthetic_ls")) %>%
                    purrr::pluck("fully_synthetic_ls") %>%
                    purrr::pluck("Synthetic_r4")))
    
  }
  if(type_1L_chr == "ParamDraws"){
    dir_1L_chr <- manufacture(x, type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr") 

  }
  if(type_1L_chr %in% "ParamDraws"){
    if(identical(batches_int, integer(0))){
      batches_int <- stringr::str_remove(files_chr, pattern = prefix_1L_chr) %>% stringr::str_sub(end=-5) %>% as.integer() %>% sort()
    }
    ingest_xx <- batches_int %>%
      purrr::reduce(.init = NULL,
                   ~ {
                     new_tb <- readRDS(paste0(dir_1L_chr, "/", files_chr[.y]))
                     if(!is.null(.x)){
                       new_tb <- dplyr::bind_rows(.x, new_tb)
                     }
                     new_tb
                   })
  }
  if(type_1L_chr=="MimicInputs"){
    ingest_xx <- MimicInputs() %>%
      renewSlot("models_ls", ingest_xx$models_ls) %>%
      renewSlot("x_Ready4useDyad@ds_tb", ingest_xx$params_tb) %>%
      renewSlot("y_Ready4useDyad", ingest_xx$Synthetic_r4)
    ingest_xx <- renewSlot(ingest_xx, "x_Ready4useDyad", renew(ingest_xx@x_Ready4useDyad, what_1L_chr = "dictionary",type_1L_chr = "new"))
  }else{
    if(type_1L_chr == "element"){
      ingest_xx <- ingest_xx %>%
        purrr::pluck(what_chr)
    }
  }
  return(ingest_xx)
}