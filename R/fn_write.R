#' Write project comma separated variables files
#' @description write_project_csvs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write project comma separated variables files. The function is called for its side effects and does not return a value.
#' @param model_data_ls Model data (a list)
#' @param path_to_private_1L_chr Path to private (a character vector of length one)
#' @param processed_dir_1L_chr Processed directory (a character vector of length one)
#' @param divider_1L_chr Divider (a character vector of length one), Default: '\'
#' @return No return value, called for side effects.
#' @rdname write_project_csvs
#' @export 
#' @importFrom purrr walk pluck walk2
#' @importFrom ready4 procureSlot
#' @importFrom stringr str_remove
#' @keywords internal
write_project_csvs <- function (model_data_ls, path_to_private_1L_chr, processed_dir_1L_chr, 
    divider_1L_chr = "\\") 
{
    c("unimputed", "imputed") %>% purrr::walk(~{
        type_1L_chr <- .x
        element_ls <- model_data_ls %>% purrr::pluck(paste0(.x, 
            "_ls"))
        element_ls %>% purrr::walk2(names(element_ls), ~{
            write.csv(ready4::procureSlot(.x, "ds_tb"), paste0(path_to_private_1L_chr, 
                "\\", processed_dir_1L_chr, "\\", "csv", "\\", 
                type_1L_chr, "\\", stringr::str_remove(.y, "_r4"), 
                ".csv"), row.names = FALSE)
        })
    })
}
#' Write projectDS
#' @description write_project_RDS() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write projectds. The function is called for its side effects and does not return a value.
#' @param data_ls Data (a list)
#' @param path_to_private_1L_chr Path to private (a character vector of length one)
#' @param processed_dir_1L_chr Processed directory (a character vector of length one)
#' @param divider_1L_chr Divider (a character vector of length one), Default: '\'
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return No return value, called for side effects.
#' @rdname write_project_RDS
#' @export 
#' @importFrom stringr str_remove_all
#' @importFrom purrr walk pluck walk2
#' @keywords internal
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
                processed_dir_1L_chr, divider_1L_chr, r_dir_1L_chr, 
                divider_1L_chr, type_1L_chr))) {
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
#' Write project workspace
#' @description write_project_ws() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write project workspace. The function is called for its side effects and does not return a value.
#' @param path_to_private_1L_chr Path to private (a character vector of length one)
#' @param processed_dir_1L_chr Processed directory (a character vector of length one)
#' @param divider_1L_chr Divider (a character vector of length one), Default: '\'
#' @return No return value, called for side effects.
#' @rdname write_project_ws
#' @export 
#' @importFrom purrr walk
#' @keywords internal
write_project_ws <- function (path_to_private_1L_chr, processed_dir_1L_chr, divider_1L_chr = "\\") 
{
    if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
        processed_dir_1L_chr))) {
        dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
            processed_dir_1L_chr))
    }
    c("csv", "R") %>% purrr::walk(~{
        if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
            processed_dir_1L_chr, divider_1L_chr, .x))) {
            dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                processed_dir_1L_chr, divider_1L_chr, .x))
        }
        if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
            processed_dir_1L_chr, divider_1L_chr, .x, divider_1L_chr, 
            "unimputed"))) {
            dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                processed_dir_1L_chr, divider_1L_chr, .x, divider_1L_chr, 
                "unimputed"))
        }
        if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
            processed_dir_1L_chr, divider_1L_chr, .x, divider_1L_chr, 
            "imputed"))) {
            dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
                processed_dir_1L_chr, divider_1L_chr, .x, divider_1L_chr, 
                "imputed"))
        }
    })
    if (!dir.exists(paste0(path_to_private_1L_chr, divider_1L_chr, 
        processed_dir_1L_chr, divider_1L_chr, "R", divider_1L_chr, 
        "regressions"))) {
        dir.create(paste0(path_to_private_1L_chr, divider_1L_chr, 
            processed_dir_1L_chr, divider_1L_chr, "R", divider_1L_chr, 
            "regressions"))
    }
}
#' Write raw Minimum Dataset data
#' @description write_raw_mds_data() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write raw minimum dataset data. The function is called for its side effects and does not return a value.
#' @param raw_mds_data_ls Raw Minimum Dataset data (a list)
#' @param path_to_raw_dir_1L_chr Path to raw directory (a character vector of length one)
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return No return value, called for side effects.
#' @rdname write_raw_mds_data
#' @export 
#' @importFrom purrr walk2
#' @keywords internal
write_raw_mds_data <- function (raw_mds_data_ls, path_to_raw_dir_1L_chr, r_dir_1L_chr = "R") 
{
    raw_mds_data_ls %>% purrr::walk2(names(raw_mds_data_ls), 
        ~saveRDS(.x, file = paste0(path_to_raw_dir_1L_chr, "/", 
            r_dir_1L_chr, "/", .y, ".RDS")))
}
