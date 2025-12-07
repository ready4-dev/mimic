import_population_k10 <- function(dir_1L_chr,
                                  fl_nm_1L_chr = "HILDA k10.xlsx",
                                  areas_chr = c("Intervention", "Matched"),
                                  divider_1L_chr = "\\"){
  population_k10_tb <- list(c("A9:F11","A15:F17","A39:F41","A45:F47"),
                            c("A9:F11","A15:F17","A40:F42","A46:F48")) %>%
    purrr::map2_dfr(c("MOST", "Matched"),
                    ~{
                      area_1L_chr <- .y
                      .x %>% purrr::map2_dfr(c(rep("Untreated",2), rep("Treated",2)),
                                             ~{
                                               table_tb <- readxl::read_xlsx(paste0(dir_1L_chr, "\\",fl_nm_1L_chr), sheet =ifelse(area_1L_chr==areas_chr[1],2,3), range = .x)
                                               names(table_tb) <- c("Wave", "From", "To","Mean", "SD","N")
                                               table_tb %>%
                                                 dplyr::mutate(Treatment = .y) %>%
                                                 dplyr::mutate(dplyr::across(c("Mean", "SD"), ~as.numeric(.x))) %>%
                                                 dplyr::relocate(Treatment, .before = "Mean")
                                             } ) %>%
                        dplyr::mutate(Area = area_1L_chr) %>%
                        dplyr::relocate(Area, .before = "Treatment")
                    }) %>% dplyr::mutate(dplyr::across(c("From", "To"), 
                                                       ~ .x %>% purrr::map_chr(~paste0(lubridate::month(.x, label = T)  %>% as.character()," ", lubridate::year(.x)))))
  return(population_k10_tb)
}
import_project_data <- function (path_to_private_1L_chr, dir_1L_chr, custom_1L_chr = character(0), 
                                 r_dir_1L_chr = "R", divider_1L_chr = "\\", names_ls = NULL, 
                                 type_1L_chr = c("raw", "experts", "custom", "forecasts", 
                                                 "processed", "modelling", "pooled", "population", "regressions", 
                                                 "results", "simulation", "summaries", "validation")) 
{
  type_1L_chr <- match.arg(type_1L_chr)
  if (type_1L_chr %in% c("custom", "forecasts", "pooled", "summaries", 
                         "validation")) {
    if (type_1L_chr == "custom") {
      assertthat::assert_that(!identical(custom_1L_chr, 
                                         character(0)))
      destination_1L_chr <- custom_1L_chr
    }
    else {
      destination_1L_chr <- type_1L_chr
    }
    if (is.null(names_ls)) {
      path_1L_chr <- paste0(path_to_private_1L_chr, 
                            divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                            divider_1L_chr, destination_1L_chr)
      files_chr <- setdiff(list.files(path_1L_chr),
                           list.dirs(path_1L_chr, recursive = FALSE, full.names = FALSE))
      names_ls <- files_chr %>% 
        stringr::str_sub(end = -5) %>% 
        as.list()
    }
    data_ls <- purrr::map(names_ls, ~readRDS(paste0(path_to_private_1L_chr, 
                                                    divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                                                    divider_1L_chr, destination_1L_chr, divider_1L_chr, 
                                                    .x, ".RDS"))) %>% stats::setNames(names_ls %>% purrr::flatten_chr())
  }
  if (type_1L_chr == "raw") {
    test_1L_lgl <- assertthat::assert_that(!is.null(names_ls))
    data_ls <- purrr::map(names_ls, ~readxl::read_xlsx(paste0(path_to_private_1L_chr, 
                                                              divider_1L_chr, dir_1L_chr, divider_1L_chr, .x)))
  }
  if (type_1L_chr == "experts") {
    data_ls <- purrr::map(names_ls, ~readxl::read_xlsx(paste0(path_to_private_1L_chr, 
                                                              divider_1L_chr, dir_1L_chr, divider_1L_chr, "SEE", 
                                                              divider_1L_chr, .x), skip = 1)) %>% stats::setNames(names_ls %>% 
                                                                                                                    purrr::flatten_chr() %>% stringr::str_sub(end = -6))
  }
  if (type_1L_chr == "modelling") {
    if (is.null(names_ls)) {
      names_ls = list("unimputed", "imputed")
    }
    data_ls <- names_ls %>% purrr::map(~{
      path_1L_chr <- paste0(path_to_private_1L_chr, divider_1L_chr, 
                            dir_1L_chr, divider_1L_chr, r_dir_1L_chr, divider_1L_chr, 
                            .x)
      files_chr <- setdiff(list.files(path_1L_chr),
                           list.dirs(path_1L_chr, recursive = FALSE, full.names = FALSE))
      files_chr %>% purrr::map(~readRDS(paste0(path_1L_chr, 
                                               "/", .x))) %>% stats::setNames(stringr::str_sub(files_chr, 
                                                                                               end = -5))
    }) %>% stats::setNames(paste0(names_ls %>% unlist(), 
                                  "_ls"))
  }
  if (type_1L_chr == "population") {
    data_ls <- c("real_imputed_ls", "fully_synthetic_ls", 
                 "synthetic_tests_ls") %>% purrr::map(~readRDS(paste0(path_to_private_1L_chr, 
                                                                      divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                                                                      divider_1L_chr, "population", divider_1L_chr, .x, 
                                                                      ".RDS"))) %>% stats::setNames(c("real_imputed_ls", 
                                                                                                      "fully_synthetic_ls", "synthetic_tests_ls"))
  }
  if (type_1L_chr == "processed") {
    if (is.null(names_ls)) {
      names_ls = list("processed_ls.RDS")
    }
    data_ls <- readRDS(paste0(path_to_private_1L_chr, divider_1L_chr, 
                              dir_1L_chr, divider_1L_chr, r_dir_1L_chr, divider_1L_chr, 
                              names_ls[[1]]))
  }
  if (type_1L_chr == "regressions") {
    data_ls <- make_regressions_ls()
    data_ls <- names(data_ls) %>% purrr::map(~readRDS(paste0(path_to_private_1L_chr, 
                                                             divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                                                             divider_1L_chr, "regressions", divider_1L_chr, .x, 
                                                             ".RDS"))) %>% stats::setNames(names(data_ls))
  }
  if (type_1L_chr == "results") {
    data_ls <- list(D_Ready4useDyad = ready4use::Ready4useDyad(), 
                    clinic_ls = list(), clinic_stage_ls = list(), distress_ls = list(), 
                    full_combos_ls = list(), stage_ls = list(), total_ls = list())
    data_ls <- names(data_ls) %>% purrr::map(~readRDS(paste0(path_to_private_1L_chr, 
                                                             divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                                                             divider_1L_chr, "sim_results", divider_1L_chr, .x, 
                                                             ".RDS"))) %>% stats::setNames(names(data_ls))
  }
  if (type_1L_chr == "simulation") {
    data_ls <- list(models_ls = readRDS(paste0(path_to_private_1L_chr, 
                                               divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                                               divider_1L_chr, "regressions", divider_1L_chr, "models_ls.RDS")), 
                    pooled_ls = import_project_data(path_to_private_1L_chr, 
                                                    dir_1L_chr = dir_1L_chr, r_dir_1L_chr = r_dir_1L_chr, 
                                                    divider_1L_chr = divider_1L_chr, names_ls = names_ls, 
                                                    type_1L_chr = "pooled"), Synthetic_r4 = readRDS(paste0(path_to_private_1L_chr, 
                                                                                                           divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                                                                                                           divider_1L_chr, "population", divider_1L_chr, 
                                                                                                           "fully_synthetic_ls.RDS"))$Synthetic_r4, unit_costs_tb = import_project_data(path_to_private_1L_chr, 
                                                                                                                                                                                        dir_1L_chr = dir_1L_chr, r_dir_1L_chr = r_dir_1L_chr, 
                                                                                                                                                                                        divider_1L_chr = divider_1L_chr, names_ls = NULL, 
                                                                                                                                                                                        type_1L_chr = "processed")$costs_unit@ds_tb)
  }
  return(data_ls)
}
import_results_batches <- function (batches_1L_int = integer(0), dir_1L_chr) {
  files_chr <- list.files(dir_1L_chr, full.names = F)
  files_chr <- files_chr[endsWith(files_chr, ".RDS")] %>% sort()
  if(identical(batches_1L_int, integer(0))){
    batches_1L_int <- length(files_chr)
  }
  results_ls <- 1:batches_1L_int %>% purrr::reduce(.init = list(), 
                                                   ~{
                                                     additions_ls <- readRDS(paste0(dir_1L_chr, "/", files_chr[.y]))
                                                     if(length(additions_ls)==1){
                                                       additions_ls <- additions_ls[[1]]
                                                     }
                                                     if (identical(.x, list())) {
                                                       additions_ls
                                                     } else {
                                                       y_dyad_ls <- make_model_dyad_ls(X_Ready4useDyad = .x$Y_Ready4useDyad, 
                                                                                       Y_Ready4useDyad = additions_ls$Y_Ready4useDyad) %>% 
                                                         update_mismatched_vars()
                                                       z_dyad_ls <- make_model_dyad_ls(X_Ready4useDyad = .x$Z_Ready4useDyad, 
                                                                                       Y_Ready4useDyad = additions_ls$Z_Ready4useDyad) %>% 
                                                         update_mismatched_vars()
                                                       list(Y_Ready4useDyad = renewSlot(y_dyad_ls$X_Ready4useDyad, 
                                                                                        "ds_tb", dplyr::bind_rows(y_dyad_ls$X_Ready4useDyad@ds_tb, 
                                                                                                                  y_dyad_ls$Y_Ready4useDyad@ds_tb)), Z_Ready4useDyad = renewSlot(z_dyad_ls$X_Ready4useDyad, 
                                                                                                                                                                                 "ds_tb", dplyr::bind_rows(z_dyad_ls$X_Ready4useDyad@ds_tb, 
                                                                                                                                                                                                           z_dyad_ls$Y_Ready4useDyad@ds_tb)))
                                                     }
                                                   })
  return(results_ls)
}