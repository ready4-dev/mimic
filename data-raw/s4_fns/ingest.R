ingest_MimicRepos <- function(x,
                              gh_token_1L_chr = "",
                              key_1L_chr = NULL,
                              # metadata_1L_lgl = T,
                              remote_fls_chr = NA_character_,
                              type_1L_chr = c("MimicInputs", "list", "element"),
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