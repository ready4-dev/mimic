write_project_csvs <- function(model_data_ls,
                            path_to_private_1L_chr,
                            processed_dir_1L_chr,
                            divider_1L_chr = "\\"){
  c("unimputed", "imputed") %>%
    purrr::walk(~{
      type_1L_chr <- .x
      element_ls <- model_data_ls %>% purrr::pluck(paste0(.x,"_ls"))
      element_ls %>%
        purrr::walk2(names(element_ls),
                     ~{
                       write.csv(ready4::procureSlot(.x, "ds_tb"), paste0(path_to_private_1L_chr,
                                                                          "\\",
                                                                          processed_dir_1L_chr,
                                                                          "\\",
                                                                          "csv",
                                                                          "\\",
                                                                          type_1L_chr,
                                                                          "\\",
                                                                          stringr::str_remove(.y,"_r4"),
                                                                          ".csv"
                       ), row.names = FALSE)
                     })
      
    })
}
write_project_RDS <- function (data_ls, path_to_private_1L_chr, processed_dir_1L_chr, 
                               divider_1L_chr = "\\", r_dir_1L_chr = "R") 
{
  if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
                         processed_dir_1L_chr))) {
    dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                      processed_dir_1L_chr))
  }
  if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
                         processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr))) {
    dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                      processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr))
  }
  data_ls %>% names() %>% stringr::str_remove_all("_ls") %>% 
    purrr::walk(~{
      type_1L_chr <- .x
      if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
                             processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, divider_1L_chr, 
                             type_1L_chr))) {
        dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                          processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                          divider_1L_chr, type_1L_chr))
      }
      element_ls <- data_ls %>% purrr::pluck(paste0(.x, 
                                                    "_ls"))
      element_ls %>% purrr::walk2(names(element_ls), ~{
        saveRDS(.x, paste0(path_to_private_1L_chr, divider_1L_chr, 
                           processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                           divider_1L_chr, type_1L_chr, divider_1L_chr, 
                           .y, ".RDS"))
      })
    })
}
write_project_ws <- function(path_to_private_1L_chr,
                          processed_dir_1L_chr,
                          divider_1L_chr = "\\"){
  if(!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, processed_dir_1L_chr))){
    dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, processed_dir_1L_chr))
  }
  c("csv", "R") %>% purrr::walk(~{
    if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x))){
      dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x))
    }
    if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "unimputed"))){
      dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "unimputed"))
    }
    if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "imputed"))){
      dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,.x,divider_1L_chr, "imputed"))
    }
  })
  if(!dir.exists(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,"R",divider_1L_chr, "regressions"))){
    dir.create(paste0(path_to_private_1L_chr,divider_1L_chr, processed_dir_1L_chr,divider_1L_chr,"R",divider_1L_chr, "regressions"))
  }
}
write_raw_mds_data<- function (raw_mds_data_ls,
                               path_to_raw_dir_1L_chr,
                               r_dir_1L_chr = "R"){
  raw_mds_data_ls %>% purrr::walk2(names(raw_mds_data_ls), 
                                   ~ saveRDS(.x, file = paste0(path_to_raw_dir_1L_chr,"/", r_dir_1L_chr,"/",.y,".RDS")))
}