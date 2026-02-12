#' 
#' Renew (update) values
#' @name renew-MimicPopulation
#' @description renew method applied to MimicPopulation
#' @param x An object of class MimicPopulation
#' @param population_ls Population (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("default", "customise", "transform")
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param ... Additional arguments
#' @return x (An object of class MimicPopulation)
#' @rdname renew-methods
#' @aliases renew,MimicPopulation-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicPopulation", function (x, population_ls = NULL, type_1L_chr = c("default", 
    "customise", "transform"), X_MimicConfiguration = MimicConfiguration(), 
    ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "customise") {
        population_ls <- manufacture(x, what_1L_chr = "population_ls")
        population_ls$X_Ready4useDyad <- add_non_helpseekers(population_ls$X_Ready4useDyad, 
            arms_for_non_helpseeking_chr = procure(X_MimicConfiguration, 
                empty_xx = character(0), match_value_xx = T, 
                target_1L_chr = "Arm", type_1L_chr = "Helpseeking adjustment"))
        population_ls$X_Ready4useDyad <- add_non_iar(population_ls$X_Ready4useDyad, 
            arms_for_iar_adjustment_chr = procure(X_MimicConfiguration, 
                empty_xx = character(0), match_value_xx = T, 
                target_1L_chr = "Arm", type_1L_chr = "IAR adjustment"))
        population_ls <- update_population_ls(population_ls)
        x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
    }
    if (type_1L_chr == "transform") {
        x <- renewSlot(x, "x_Ready4useDyad", population_ls$X_Ready4useDyad) %>% 
            renewSlot("y_Ready4useDyad", population_ls$Y_Ready4useDyad) %>% 
            renewSlot("z_Ready4useDyad", population_ls$Z_Ready4useDyad)
    }
    return(x)
})
#' 
#' Renew (update) values
#' @name renew-MimicConfiguration
#' @description renew method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param arm_1L_chr Arm (a character vector of length one), Default: character(0)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param iterations_int Iterations (an integer vector), Default: integer(0)
#' @param tx_prefix_1L_chr Treatment prefix (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("default", "form")
#' @param what_1L_chr What (a character vector of length one), Default: c("population")
#' @param ... Additional arguments
#' @return x (An object of class MimicConfiguration)
#' @rdname renew-methods
#' @aliases renew,MimicConfiguration-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicConfiguration", function (x, arm_1L_chr = character(0), draws_tb = NULL, iterations_int = integer(0), 
    tx_prefix_1L_chr = character(0), type_1L_chr = c("default", 
        "form"), what_1L_chr = c("population"), ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "population") {
        if (type_1L_chr == "form") {
            population_ls <- add_enter_model_event(X_Ready4useDyad = X_MimicConfiguration@x_MimicInputs@y_Ready4useDyad, 
                default_fn = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls$default_fn, 
                derive_fn_ls = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls$derive_ls, 
                horizon_dtm = X_MimicConfiguration@horizon_dtm, 
                modifiable_chr = X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls$update_fn(X_MimicConfiguration@modifiable_chr), 
                start_dtm = X_MimicConfiguration@start_dtm, tfmn_ls = X_MimicConfiguration@x_MimicAlgorithms@transformations_ls, 
                tx_duration_dtm = procure(X_MimicConfiguration, 
                  match_value_xx = arm_1L_chr, empty_xx = NULL, 
                  target_1L_chr = "Treatment duration"), arm_1L_chr = arm_1L_chr, 
                default_args_ls = list(sensitivities_ls = X_MimicConfiguration@x_MimicAlgorithms@sensitivities_ls), 
                draws_tb = draws_tb, iterations_int = iterations_int, 
                tidy_cols_1L_lgl = T, tx_prefix_1L_chr = tx_prefix_1L_chr) %>% 
                update_population_ls(population_ls = NULL, type_1L_chr = type_1L_chr)
            x <- renewSlot(x, "x_MimicPopulation", renew(x@x_MimicPopulation, 
                population_ls = population_ls, type_1L_chr = "transform"))
        }
    }
    return(x)
})
