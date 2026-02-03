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
  if(what_1L_chr=="sim_ws_dirs_chr"){ # This could be manufacture method
    sim_ws_dirs_chr <- manufacture(x, suffix_1L_chr = suffix_1L_chr) ## UPDATE METHOD NAME _MimicRepos
  }
 if(!identical(sim_ws_dirs_chr, character(0))){
   write_new_dirs(sim_ws_dirs_chr,
                  consent_1L_chr = consent_1L_chr,
                  consent_indcs_int = consent_indcs_int,
                  options_chr = options_chr)
 }

}