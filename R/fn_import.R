#' Import project data
#' @description import_project_data() is an Import function that reads a data object in its native format and converts it to an R object. Specifically, this function implements an algorithm to import project data. The function returns Data (a list).
#' @param path_to_private_1L_chr Path to private (a character vector of length one)
#' @param dir_1L_chr Directory (a character vector of length one)
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param divider_1L_chr Divider (a character vector of length one), Default: '\'
#' @param names_ls Names (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("raw", "experts", "processed", "modelling", "pooled", "population", 
#'    "regressions", "results", "simulation")
#' @return Data (a list)
#' @rdname import_project_data
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom purrr map flatten_chr
#' @importFrom readxl read_xlsx
#' @importFrom stats setNames
#' @importFrom stringr str_sub
#' @importFrom ready4use Ready4useDyad
#' @keywords internal
import_project_data <- function (path_to_private_1L_chr, dir_1L_chr, r_dir_1L_chr = "R", 
    divider_1L_chr = "\\", names_ls = NULL, type_1L_chr = c("raw", 
        "experts", "processed", "modelling", "pooled", "population", 
        "regressions", "results", "simulation")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
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
            dir_1L_chr <- paste0(path_to_private_1L_chr, divider_1L_chr, 
                dir_1L_chr, divider_1L_chr, r_dir_1L_chr, divider_1L_chr, 
                .x)
            list.files(dir_1L_chr) %>% purrr::map(~readRDS(paste0(dir_1L_chr, 
                "/", .x))) %>% stats::setNames(stringr::str_sub(list.files(dir_1L_chr), 
                end = -5))
        }) %>% stats::setNames(paste0(names_ls %>% unlist(), 
            "_ls"))
    }
    if (type_1L_chr == "pooled") {
        data_ls <- purrr::map(names_ls, ~readRDS(paste0(path_to_private_1L_chr, 
            divider_1L_chr, dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
            divider_1L_chr, type_1L_chr, divider_1L_chr, .x, 
            ".RDS"))) %>% stats::setNames(names_ls %>% purrr::flatten_chr())
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
