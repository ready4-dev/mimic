#' 
#' Procure data from a model module
#' @name procure-MimicConfiguration
#' @description procure method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param empty_xx Empty (an output object of multiple potential types), Default: NULL
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param target_1L_chr Target (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'Arm'
#' @param what_1L_chr What (a character vector of length one), Default: c("arm")
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @aliases procure,MimicConfiguration-method
#' @export 
#' @importFrom ready4 procure
methods::setMethod("procure", "MimicConfiguration", function(x,
                                                             empty_xx = NULL,
                                                             match_value_xx = NULL,
                                                             target_1L_chr = character(0),
                                                             type_1L_chr = "Arm",
                                                             what_1L_chr = c("arm", "event")){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr == "arm"){
    object_xx <- x@arms_tb
    if(!type_1L_chr %in% names(object_xx)){
      object_xx <- empty_xx
    }else{
      type_1L_chr <- match.arg(type_1L_chr, names(object_xx))
      if(!identical(target_1L_chr, character(0))){
        if(target_1L_chr %in% names(object_xx)){
          object_xx <- get_from_lup_obj(object_xx, target_var_nm_1L_chr = target_1L_chr, match_var_nm_1L_chr = type_1L_chr, match_value_xx =  match_value_xx)[[1]] 
        }else{
          object_xx <- empty_xx
        }
      }
    }
  }
  if(what_1L_chr == "event"){
    object_xx <- x@x_MimicEventsList@events_ls
    if(type_1L_chr == "main"){
      object_xx <- object_xx %>% purrr::keep_at(x@x_MimicEventsList@main_chr)
    }
    if(!identical(object_xx, list()) & !identical(target_1L_chr, character(0))){
      if(target_1L_chr %in% names(object_xx)){
        object_xx <- object_xx %>% purrr::pluck(target_1L_chr)
      }else{
        object_xx <- empty_xx
      }
    }
  }
  return(object_xx)
})
