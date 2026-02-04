author_MimicConfiguration <- function(x,
                                      consent_1L_chr = "",
                                      consent_indcs_int = 1L,
                                      options_chr = c("Y", "N"),
                                      unlink_1L_lgl = FALSE,
                                      what_1L_chr = c("draws"),
                                      Y_MimicRepos = MimicRepos()){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "draws"){
    author(Y_MimicRepos, what_1L_chr = c("sim_ws_dirs_chr"))
    draw_to_1L_chr <- manufacture(Y_MimicRepos, type_1L_chr = "draw_to", what_1L_chr = "sim_ws_dirs_chr") 
    if (unlink_1L_lgl) {
      list.files(draw_to_1L_chr, full.names = T) %>%
        write_to_delete_fls(consent_1L_chr = consent_1L_chr, 
                            consent_indcs_int = consent_indcs_int, 
                            options_chr = options_chr, 
                            return_1L_lgl = FALSE)
    }
    1:length(x@iterations_ls) %>%
      purrr::walk(~{
        draws_tb <- manufacture_MimicConfiguration(x, batch_1L_int = .x, what_1L_chr = "draws_tb")  ## UPDATE METHOD NAME 
        # Could also add option to write to dataverse
        write_obj_with_prompt(draws_tb,
                              obj_nm_1L_chr = paste0("ParamDrawsBatch",.x), 
                              outp_dir_1L_chr = draw_to_1L_chr, 
                              consent_1L_chr = consent_1L_chr, 
                              consent_indcs_int = consent_indcs_int, 
                              options_chr = options_chr, 
                              return_1L_lgl = FALSE)
      })
  }
}
author_MimicRepos <- function(x,
                              consent_1L_chr = "",
                              consent_indcs_int = 1L,
                              options_chr = c("Y", "N"),
                              suffix_1L_chr = "",
                              what_1L_chr = c("sim_ws_dirs_chr"),
                              ...){
  # paths_ls <- manufacture(x,
  #                         what_1L_chr = "paths_ls")
  sim_ws_dirs_chr <- character(0)
  if(what_1L_chr=="sim_ws_dirs_chr"){ 
    sim_ws_dirs_chr <- manufacture(x, suffix_1L_chr = suffix_1L_chr) 
  }
 if(!identical(sim_ws_dirs_chr, character(0))){
   write_new_dirs(sim_ws_dirs_chr,
                  consent_1L_chr = consent_1L_chr,
                  consent_indcs_int = consent_indcs_int,
                  options_chr = options_chr)
 }

}