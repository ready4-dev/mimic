manufacture_MimicInputs <- function(x,
                                    what_1L_chr = c("inputs_ls")){
  if(what_1L_chr == "inputs_ls"){
    object_xx <- list(models_ls = x@models_ls,
                      params_tb = x@x_Ready4useDyad@ds_tb,
                      Synthetic_r4 = x@y_Ready4useDyad)
  }
  return(object_xx)
}

manufacture_MimicRepos <- function(x,
                              suffix_1L_chr = "",
                              type_1L_chr = c("all", "batch_to"),
                              what_1L_chr = c("sim_ws_dirs_chr"),
                              ...){
  type_1L_chr <- match.arg(type_1L_chr)
  if(what_1L_chr=="sim_ws_dirs_chr"){ 
    object_xx <- c(
      paste0(Y@path_to_output_1L_chr,
             Y@divider_1L_chr,
             Y@processed_dir_1L_chr),
      paste0(Y@path_to_output_1L_chr,
             Y@divider_1L_chr,
             Y@processed_dir_1L_chr,
             Y@divider_1L_chr,
             Y@r_dir_1L_chr),
      paste0(Y@path_to_output_1L_chr,
             Y@divider_1L_chr,
             Y@processed_dir_1L_chr,
             Y@divider_1L_chr,
             Y@r_dir_1L_chr,
             Y@divider_1L_chr,
             Y@batch_to_1L_chr,
             suffix_1L_chr)
    )
    if(type_1L_chr == "batch_to"){
      object_xx <- object_xx[3]
    }
  }
  return(object_xx)
}