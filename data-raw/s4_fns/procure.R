procure_MimicConfiguration <- function(x,
                                       arm_1L_chr = NA_character_,
                                       target_1L_chr = character(0),
                                       what_1L_chr = c("arm")){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "arm"){
    object_xx <- x@arms_tb
    if(!identical(target_1L_chr, character(0))){
      if(target_1L_chr %in% names(object_xx)){
        object_xx <- get_from_lup_obj(object_xx, target_var_nm_1L_chr = target_1L_chr, match_var_nm_1L_chr = "Arm", match_value_xx = arm_1L_chr)[[1]] 
      }else{
        object_xx <- NULL
      }
    } 
  }
  return(object_xx)
}