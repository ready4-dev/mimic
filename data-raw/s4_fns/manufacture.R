manufacture_MimicConfiguration <- function(x,
                                           batch_1L_int = integer(0),
                                           what_1L_chr = c("draws_tb")){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "draws_tb"){
    if(!identical(batch_1L_int, integer(0))){
      iterations_int <- x@iterations_ls %>% purrr::flatten_int()
      batch_1L_int <- 0
    }else{
      iterations_int <- x@iterations_ls[[batch_1L_int]]
    }
    inputs_ls <- manufacture(x@x_MimicInputs, what_1L_chr = "inputs_ls")
    object_xx <- make_draws_tb(inputs_ls, 
                              extra_draws_fn = x@x_MimicAlgorithms@processing_ls$extra_draws_fn,
                              iterations_int = iterations_int, 
                              drop_missing_1L_lgl = x@drop_missing_1L_lgl, 
                              drop_suffix_1L_chr = x@drop_suffix_1L_chr, 
                              seed_1L_int = x@seed_1L_int + batch_1L_int)
  }
  return(object_xx)
}
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
                                   # draw_to_1L_chr = "BatchedParamDraws", # UPDATE / REMOVE
                                   suffix_1L_chr = "",
                                   type_1L_chr = c("all", "batch_to", "draw_to"),
                                   what_1L_chr = c("sim_ws_dirs_chr"),
                              ...){
  type_1L_chr <- match.arg(type_1L_chr)
  if(what_1L_chr=="sim_ws_dirs_chr"){ 
    object_xx <- c(
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr),
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr,
             x@divider_1L_chr,
             x@r_dir_1L_chr),
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr,
             x@divider_1L_chr,
             x@r_dir_1L_chr,
             x@divider_1L_chr,
             x@batch_to_1L_chr,
             suffix_1L_chr),
      paste0(x@path_to_output_1L_chr,
             x@divider_1L_chr,
             x@processed_dir_1L_chr,
             x@divider_1L_chr,
             x@r_dir_1L_chr,
             x@divider_1L_chr,
             x@draw_to_1L_chr) # UPDATE
    )
    if(type_1L_chr == "batch_to"){
      object_xx <- object_xx[3]
    }
    if(type_1L_chr == "draw_to"){
      object_xx <- object_xx[4]
    }
  }
  return(object_xx)
}