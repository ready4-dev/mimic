#' Write batch
#' @description write_batch() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write batch. The function is called for its side effects and does not return a value.
#' @param batch_1L_int Batch (an integer vector of length one)
#' @param arms_chr Arms (a character vector), Default: character(0)
#' @param comparator_fn Comparator (a function), Default: NULL
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_suffix_1L_chr Drop suffix (a character vector of length one), Default: FALSE
#' @param extra_draws_fn Extra draws (a function), Default: NULL
#' @param horizon_dtm Horizon (a date vector), Default: lubridate::years(1)
#' @param inputs_ls Inputs (a list), Default: NULL
#' @param intervention_fn Intervention (a function), Default: NULL
#' @param iterations_ls Iterations (a list), Default: NULL
#' @param modifiable_chr Modifiable (a character vector), Default: character(0)
#' @param prior_batches_1L_int Prior batches (an integer vector of length one), Default: integer(0)
#' @param seed_1L_int Seed (an integer vector of length one), Default: 2001
#' @param sensitivities_ls Sensitivities (a list), Default: NULL
#' @param start_dtm Start (a date vector), Default: Sys.Date()
#' @param tfmn_ls Transformation (a list), Default: NULL
#' @param utilities_chr Utilities (a character vector), Default: character(0)
#' @param write_to_1L_chr Write to (a character vector of length one)
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname write_batch
#' @export 
#' @importFrom lubridate years
#' @importFrom assertthat assert_that
#' @importFrom purrr map pluck
#' @importFrom rlang exec
#' @importFrom stats setNames
#' @importFrom ready4use Ready4useDyad
#' @keywords internal
write_batch <- function (batch_1L_int, arms_chr = character(0), comparator_fn = NULL, 
    draws_tb = NULL, drop_missing_1L_lgl = FALSE, drop_suffix_1L_chr = FALSE, 
    extra_draws_fn = NULL, horizon_dtm = lubridate::years(1), 
    inputs_ls = NULL, intervention_fn = NULL, iterations_ls = NULL, 
    modifiable_chr = character(0), prior_batches_1L_int = integer(0), 
    seed_1L_int = 2001L, sensitivities_ls = NULL, start_dtm = Sys.Date(), 
    tfmn_ls = NULL, utilities_chr = character(0), write_to_1L_chr, 
    X_MimicConfiguration = MimicConfiguration(), ...) 
{
    old_algorithm_1L_lgl <- T
    if (!identical(X_MimicConfiguration, MimicConfiguration())) {
        old_algorithm_1L_lgl <- F
    }
    else {
        X_MimicConfiguration <- make_configuration(arms_chr = arms_chr, 
            drop_missing_1L_lgl = drop_missing_1L_lgl, drop_suffix_1L_chr = drop_suffix_1L_chr, 
            extra_draws_fn = extra_draws_fn, horizon_dtm = horizon_dtm, 
            iterations_ls = iterations_ls, modifiable_chr = modifiable_chr, 
            seed_1L_int = seed_1L_int, sensitivities_ls = sensitivities_ls, 
            start_dtm = start_dtm, synthesis_fn = synthesis_fn, 
            utilities_chr = utilities_chr)
    }
    iterations_int <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, 
        what_1L_chr = "iterations")
    if (is.null(draws_tb)) {
        if (!identical(Y_MimicRepos, MimicRepos())) {
            draws_tb <- ingest(Y_MimicRepos, batches_int = batch_1L_int, 
                type_1L_chr = "ParamDraws")
        }
        else {
            draws_tb <- manufacture(X_MimicConfiguration, batch_1L_int = batch_1L_int, 
                what_1L_chr = "draws_tb")
        }
    }
    test_1L_lgl <- assertthat::assert_that(identical(sort(draws_tb$Iteration), 
        sort(iterations_int)), msg = "Iterations in iteration vector and parameter draws table do not match.")
    extras_ls <- list(...)
    if (!old_algorithm_1L_lgl) {
        output_ls <- purrr::map(X_MimicConfiguration@arms_tb$Arm, 
            ~{
                algorithm_1L_chr <- get_from_lup_obj(X_MimicConfiguration@arms_tb, 
                  target_var_nm_1L_chr = "Algorithm", match_var_nm_1L_chr = "Arm", 
                  match_value_xx = .x)
                function_fn <- X_MimicConfiguration@x_MimicAlgorithms@main_ls %>% 
                  purrr::pluck(algorithm_1L_chr)
                new_args_ls <- manufacture(X_MimicConfiguration, 
                  arm_1L_chr = .x, batch_1L_int = batch_1L_int, 
                  what_1L_chr = "args_all")
                new_args_ls <- update_arguments_ls(new_args_ls, 
                  function_fn = function_fn)
                rlang::exec(function_fn, !!!new_args_ls)
            }) %>% stats::setNames(X_MimicConfiguration@arms_tb$Arm)
    }
    else {
        if (!is.null(intervention_fn)) {
            args_ls <- manufacture(X_MimicConfiguration, arm_1L_chr = arms_chr[1], 
                batch_1L_int = batch_1L_int, what_1L_chr = "args_all")
            args_ls <- update_arguments_ls(new_args_ls, function_fn = intervention_fn)
            Y_Ready4useDyad <- rlang::exec(intervention_fn, !!!args_ls)
        }
        else {
            Y_Ready4useDyad <- ready4use::Ready4useDyad()
        }
        if (!is.null(comparator_fn)) {
            args_ls <- manufacture(X_MimicConfiguration, arm_1L_chr = arms_chr[2], 
                batch_1L_int = batch_1L_int, what_1L_chr = "args_all")
            args_ls <- update_arguments_ls(args_ls, function_fn = comparator_fn)
            Z_Ready4useDyad <- rlang::exec(comparator_fn, !!!args_ls)
        }
        else {
            Z_Ready4useDyad <- ready4use::Ready4useDyad()
        }
        output_ls <- list(Y_Ready4useDyad = Y_Ready4useDyad, 
            Z_Ready4useDyad = Z_Ready4useDyad)
    }
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
