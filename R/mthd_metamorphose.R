#' 
#' Metamorphose a model module to a model module of a different (non-inheriting) class
#' @name metamorphose-MimicConfiguration
#' @description metamorphose method applied to MimicConfiguration
#' @param x An object of class MimicConfiguration
#' @param arm_1L_chr Arm (a character vector of length one), Default: 'NA'
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param draws_tb Draws (a tibble), Default: NULL
#' @param env_ls Environment list (a list of environments), Default: list()
#' @param Y_Ready4Module PARAM_DESCRIPTION, Default: Ready4Module()
#' @param ... Additional arguments
#' @return Y_Ready4Module (An object)
#' @rdname metamorphose-methods
#' @aliases metamorphose,MimicConfiguration-method
#' @export 
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", "MimicConfiguration", function (x, arm_1L_chr = NA_character_, batch_1L_int = integer(0), 
    draws_tb = NULL, env_ls = list(),  
    Y_Ready4Module = Ready4Module(), ...) 
{
    if (inherits(Y_Ready4Module, "MimicPopulation")) {
        population_ls <- manufacture(x, arm_1L_chr = arm_1L_chr, 
            batch_1L_int = batch_1L_int, draws_tb = draws_tb, 
            type_1L_chr = "entry", 
            what_1L_chr = c("population_ls"))
        Y_Ready4Module <- renew(Y_Ready4Module, population_ls = population_ls, 
            type_1L_chr = "transform")
        Y_Ready4Module <- renew(Y_Ready4Module, env_ls = env_ls, 
            type_1L_chr = "customise", X_MimicConfiguration = x)
    }
    return(Y_Ready4Module)
})
