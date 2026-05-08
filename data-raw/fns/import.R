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
import_results_batches <- function (batches_1L_int = integer(0), dir_1L_chr = character(0), drop_params_1L_lgl = FALSE, 
                                    ratify_1L_lgl = FALSE, suffix_1L_chr = "", 
                                    use_chr = character(0),
                                    Y_MimicRepos = MimicRepos()
                                    # , y_1L_chr = "Y_Ready4useDyad", z_1L_chr = "Z_Ready4useDyad"
                                    ) {
  if(identical(dir_1L_chr, character(0))){
    dir_1L_chr <- manufacture(Y_MimicRepos, suffix_1L_chr = suffix_1L_chr, type_1L_chr = "batch_to", what_1L_chr = "sim_ws_dirs_chr") 
  }
  files_chr <- list.files(dir_1L_chr, full.names = F)
  files_chr <- files_chr[endsWith(files_chr, ".RDS")] %>% sort()

  if(identical(batches_1L_int, integer(0))){
    batches_int <- stringr::str_extract(files_chr, "\\d+") %>% as.integer() %>% sort()
    batches_1L_int <- max(batches_int) #length(files_chr)
  }else{
    batches_int <- 1:batches_1L_int
  }
  if(drop_params_1L_lgl){
    param_names_chr <- setdiff(ingest(Y_MimicRepos, batches_int = batches_1L_int, type_1L_chr = "ParamDraws") %>% names(), "Iteration")
  }
  if(ratify_1L_lgl){
    # pass_1L_lgl <- ratify(Y, batches_int = integer(0), type_1L_chr = "ParamDraws") 
    pass_1L_lgl <- ratify(Y, batches_int = batches_int , type_1L_chr = "ParamDraws") 
  }
  results_ls <- 
    # batches_int %>% 
    paste0(dir_1L_chr, "/", files_chr) %>%
    purrr::reduce(.init = list(), 
                                                   ~{
                                                     additions_ls <- readRDS(
                                                       .y
                                                       # paste0(dir_1L_chr, "/", files_chr[.y])
                                                       )
                                                     if(!identical(use_chr, character(0))){
                                                       additions_ls <- additions_ls[use_chr] 
                                                     }
                                                     if(drop_params_1L_lgl){
                                                       additions_ls <- additions_ls %>% purrr::map(~{
                                                           .x %>% renewSlot("ds_tb", dplyr::select(.x@ds_tb, - tidyselect::all_of(param_names_chr)))
                                                         
                                                       })
                                                     }
                                                     if(length(additions_ls)==1){ # NOT SURE WHY THIS IS HERE AND MAY REQUIRE AMMENDMENT TO LAST BIT OF FUNCTION LOGIC
                                                       additions_ls <- additions_ls[[1]]
                                                     }else{
                                                       ## Renames list elements to X_Ready4useDyad and Y_Ready4useDyad if they are not already named that.
                                                       additions_ls <- make_model_dyad_ls(X_Ready4useDyad = additions_ls[[1]], Y_Ready4useDyad = additions_ls[[2]]) %>% 
                                                         update_mismatched_vars() 
                                                     }
                                                     if (identical(.x, list())) {
                                                       additions_ls
                                                     } else {
                                                       # current_ls <- .x
                                                       .x %>% purrr::map2(additions_ls, ~{
                                                         dyad_ls <- make_model_dyad_ls(X_Ready4useDyad = .x, Y_Ready4useDyad = .y) %>% 
                                                           update_mismatched_vars()
                                                         renewSlot(dyad_ls$X_Ready4useDyad, 
                                                                   "ds_tb", dplyr::bind_rows(dyad_ls$X_Ready4useDyad@ds_tb, 
                                                                                             dyad_ls$Y_Ready4useDyad@ds_tb))
                                                       })
                                                     }
                                                   })
  results_ls <- list(Y_Ready4useDyad = results_ls$X_Ready4useDyad, Z_Ready4useDyad = results_ls$Y_Ready4useDyad)
  return(results_ls)
}