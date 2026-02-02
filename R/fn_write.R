#' Write batch
#' @description write_batch() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write batch. The function is called for its side effects and does not return a value.
#' @param batch_1L_int Batch (an integer vector of length one)
#' @param arms_chr Arms (a character vector)
#' @param comparator_fn Comparator (a function)
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one)
#' @param drop_suffix_1L_chr Drop suffix (a character vector of length one)
#' @param extra_draws_fn Extra draws (a function)
#' @param horizon_dtm Horizon (a date vector)
#' @param inputs_ls Inputs (a list)
#' @param intervention_fn Intervention (a function)
#' @param iterations_ls Iterations (a list)
#' @param modifiable_chr Modifiable (a character vector)
#' @param prior_batches_1L_int Prior batches (an integer vector of length one)
#' @param seed_1L_int Seed (an integer vector of length one)
#' @param sensitivities_ls Sensitivities (a list)
#' @param start_dtm Start (a date vector)
#' @param tfmn_ls Transformation (a list)
#' @param utilities_chr Utilities (a character vector)
#' @param write_to_1L_chr Write to (a character vector of length one)
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname write_batch
#' @export 
#' @importFrom rlang exec
#' @importFrom ready4use Ready4useDyad
#' @keywords internal
write_batch <- function (batch_1L_int, arms_chr, comparator_fn, drop_missing_1L_lgl, 
    drop_suffix_1L_chr, extra_draws_fn, horizon_dtm, inputs_ls, 
    intervention_fn, iterations_ls, modifiable_chr, prior_batches_1L_int, 
    seed_1L_int, sensitivities_ls, start_dtm, tfmn_ls, utilities_chr, 
    write_to_1L_chr, ...) 
{
    iterations_int <- iterations_ls[[batch_1L_int]]
    draws_tb <- make_draws_tb(inputs_ls, iterations_int = iterations_int, 
        drop_missing_1L_lgl = drop_missing_1L_lgl, drop_suffix_1L_chr = drop_suffix_1L_chr, 
        seed_1L_int = seed_1L_int + batch_1L_int)
    extras_ls <- list(...)
    if (!is.null(intervention_fn)) {
        args_ls <- list(inputs_ls, arm_1L_chr = arms_chr[1], 
            draws_tb = draws_tb, iterations_int = iterations_int, 
            horizon_dtm = horizon_dtm, modifiable_chr = modifiable_chr, 
            sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
            seed_1L_int = seed_1L_int + batch_1L_int, start_dtm = start_dtm, 
            utilities_chr = utilities_chr) %>% append(extras_ls)
        Y_Ready4useDyad <- rlang::exec(intervention_fn, !!!args_ls)
    }
    else {
        Y_Ready4useDyad <- ready4use::Ready4useDyad()
    }
    if (!is.null(comparator_fn)) {
        args_ls <- list(inputs_ls, arm_1L_chr = arms_chr[2], 
            draws_tb = draws_tb, iterations_int = iterations_int, 
            horizon_dtm = horizon_dtm, modifiable_chr = modifiable_chr, 
            sensitivities_ls = sensitivities_ls, tfmn_ls = tfmn_ls, 
            seed_1L_int = seed_1L_int + batch_1L_int, start_dtm = start_dtm, 
            utilities_chr = utilities_chr) %>% append(extras_ls)
        Z_Ready4useDyad <- rlang::exec(comparator_fn, !!!args_ls)
    }
    else {
        Z_Ready4useDyad <- ready4use::Ready4useDyad()
    }
    output_ls <- list(Y_Ready4useDyad = Y_Ready4useDyad, Z_Ready4useDyad = Z_Ready4useDyad)
    message(paste0("Batch ", batch_1L_int, " completed."))
    if (!dir.exists(write_to_1L_chr)) {
        dir.create(write_to_1L_chr)
    }
    saveRDS(output_ls, paste0(write_to_1L_chr, "/SimBatch", batch_1L_int + 
        prior_batches_1L_int, ".RDS"))
    message(paste0("Output saved as SimBatch", batch_1L_int + 
        prior_batches_1L_int, ".RDS"))
}
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
