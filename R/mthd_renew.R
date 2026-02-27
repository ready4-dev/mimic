#' 
#' Renew (update) values
#' @name renew-MimicActive
#' @description renew method applied to MimicActive
#' @param x An object of class MimicActive
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param env_ls Environment list (a list of environments), Default: list()
#' @param type_1L_chr Type (a character vector of length one), Default: c("trigger", "customise", "filter", "schedule")
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param X_MimicEvent PARAM_DESCRIPTION, Default: MimicEvent()
#' @param ... Additional arguments
#' @return x (An object of class MimicActive)
#' @rdname renew-methods
#' @aliases renew,MimicActive-method
#' @export 
#' @importFrom rlang exec
#' @importFrom dplyr mutate
#' @importFrom lubridate NA_Date_
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicActive", function (x, batch_1L_int = integer(0), env_ls = list(), type_1L_chr = c("trigger", 
    "customise", "filter", "schedule"), X_MimicConfiguration = MimicConfiguration(), 
    X_MimicEvent = MimicEvent(), ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr %in% c("filter")) {
        args_ls <- manufactureSlot(X_MimicEvent, "x_MimicEligible", 
            type_1L_chr = type_1L_chr)
        x <- renewSlot(x, "x_Ready4useDyad", rlang::exec(add_ineligible, 
            x@x_Ready4useDyad, !!!args_ls))
    }
    if (type_1L_chr == "customise") {
        if (!is.null(X_MimicConfiguration@x_MimicAlgorithms@processing_ls$customise_fn)) {
            x <- renewSlot(x, "x_Ready4useDyad", rlang::exec(X_MimicConfiguration@x_MimicAlgorithms@processing_ls$customise_fn, 
                x@x_Ready4useDyad, !!!env_ls))
        }
    }
    if (type_1L_chr == "schedule") {
        x <- renewSlot(x, "x_Ready4useDyad", add_time_to_event(x@x_Ready4useDyad, 
            event_1L_chr = X_MimicEvent@x_MimicSchedule@event_1L_chr, 
            schedule_fn = X_MimicEvent@x_MimicSchedule@functions_ls$schedule_fn, 
            schedule_args_ls = manufacture(X_MimicEvent@x_MimicSchedule@x_MimicArguments, 
                batch_1L_int = batch_1L_int, env_ls = env_ls, 
                what_1L_chr = "args_ls", X_MimicConfiguration = X_MimicConfiguration), 
            step_dtm = X_MimicEvent@x_MimicSchedule@step_dtm))
        print_errors(x@x_Ready4useDyad, vars_chr = X_MimicEvent@x_MimicSchedule@validate_chr, 
            assert_1L_lgl = X_MimicEvent@x_MimicSchedule@assert_1L_lgl, 
            invalid_fn = X_MimicEvent@x_MimicSchedule@functions_ls$invalid_fn)
        x@x_Ready4useDyad <- update_next_date(x@x_Ready4useDyad)
        x@x_Ready4useDyad <- update_next_event(x@x_Ready4useDyad)
        x@x_Ready4useDyad <- add_ineligible(x@x_Ready4useDyad, 
            ineligible_1L_chr = "is.na(ScheduledFor)")
    }
    if (type_1L_chr == "trigger") {
        x@x_Ready4useDyad <- update_current_date(x@x_Ready4useDyad)
        x@x_Ready4useDyad <- update_current_event(x@x_Ready4useDyad)
        args_ls <- manufacture(X_MimicEvent@x_MimicTrigger@x_MimicArguments, 
            batch_1L_int = batch_1L_int, env_ls = env_ls, what_1L_chr = c("args_ls"), 
            X_MimicConfiguration = X_MimicConfiguration)
        x <- renewSlot(x, "x_Ready4useDyad", rlang::exec(X_MimicEvent@x_MimicTrigger@functions_ls$action_fn, 
            x@x_Ready4useDyad, !!!args_ls))
        x <- renewSlot(x, "x_Ready4useDyad@ds_tb", x@x_Ready4useDyad@ds_tb %>% 
            dplyr::mutate(NextEvent = NA_character_, ScheduledFor = lubridate::NA_Date_))
    }
    return(x)
})
#' 
#' Renew (update) values
#' @name renew-MimicPopulation
#' @description renew method applied to MimicPopulation
#' @param x An object of class MimicPopulation
#' @param batch_1L_int Batch (an integer vector of length one), Default: integer(0)
#' @param env_ls Environment list (a list of environments), Default: list()
#' @param population_ls Population (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: c("trigger", "customise", "filter", "event", "reset", "schedule", 
#'    "switch", "transform")
#' @param use_1L_chr Use (a character vector of length one), Default: 'Y'
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @param X_MimicConfiguration PARAM_DESCRIPTION, Default: MimicConfiguration()
#' @param X_MimicEvent PARAM_DESCRIPTION, Default: MimicEvent()
#' @param ... Additional arguments
#' @return x (An object of class MimicPopulation)
#' @rdname renew-methods
#' @aliases renew,MimicPopulation-method
#' @export 
#' @importFrom rlang exec
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicPopulation", function (x, batch_1L_int = integer(0), env_ls = list(), population_ls = NULL, 
    type_1L_chr = c("trigger", "customise", "filter", "event", 
        "reset", "schedule", "switch", "transform"), use_1L_chr = "Y", 
    what_1L_chr = character(0), X_MimicConfiguration = MimicConfiguration(), 
    X_MimicEvent = MimicEvent(), ...) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "customise") {
        population_ls <- manufacture(x, what_1L_chr = "population_ls")
        population_ls$X_Ready4useDyad <- renew(x@x_MimicActive, 
            type_1L_chr = type_1L_chr, env_ls = env_ls, X_MimicConfiguration = X_MimicConfiguration) %>% 
            procureSlot("x_Ready4useDyad")
        x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
    }
    if (type_1L_chr == "event") {
        x <- renew(x, batch_1L_int = batch_1L_int, env_ls = env_ls, 
            type_1L_chr = "filter", X_MimicConfiguration = X_MimicConfiguration, 
            X_MimicEvent = X_MimicEvent)
        x <- renew(x, batch_1L_int = batch_1L_int, env_ls = env_ls, 
            type_1L_chr = "schedule", X_MimicConfiguration = X_MimicConfiguration, 
            X_MimicEvent = X_MimicEvent)
        x <- renew(x, batch_1L_int = batch_1L_int, env_ls = env_ls, 
            type_1L_chr = "trigger", X_MimicConfiguration = X_MimicConfiguration, 
            X_MimicEvent = X_MimicEvent)
        x <- renew(x, batch_1L_int = batch_1L_int, env_ls = env_ls, 
            type_1L_chr = "reset", X_MimicConfiguration = X_MimicConfiguration, 
            X_MimicEvent = X_MimicEvent, what_1L_chr = "Y")
        x <- renew(x, batch_1L_int = batch_1L_int, env_ls = env_ls, 
            type_1L_chr = "reset", X_MimicConfiguration = X_MimicConfiguration, 
            X_MimicEvent = X_MimicEvent, what_1L_chr = "Z")
    }
    if (type_1L_chr %in% c("filter", "schedule", "trigger")) {
        if (nrow(x@x_MimicActive@x_Ready4useDyad@ds_tb) > 0) {
            population_ls <- manufacture(x, what_1L_chr = "population_ls")
            X_MimicActive <- renew(x@x_MimicActive, batch_1L_int = batch_1L_int, 
                env_ls = env_ls, type_1L_chr = type_1L_chr, X_MimicConfiguration = X_MimicConfiguration, 
                X_MimicEvent = X_MimicEvent)
            population_ls$X_Ready4useDyad <- X_MimicActive@x_Ready4useDyad
            if (type_1L_chr %in% c("filter", "schedule")) {
                population_ls <- update_population_ls(population_ls, 
                  split_var_1L_chr = "InModel", type_1L_chr = "split", 
                  use_1L_chr = ifelse(type_1L_chr == "filter", 
                    use_1L_chr, ifelse(is.na(X_MimicEvent@x_MimicSchedule@use_1L_chr), 
                      use_1L_chr, X_MimicEvent@x_MimicSchedule@use_1L_chr)))
            }
            x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
        }
    }
    if (type_1L_chr == "reset") {
        if (what_1L_chr == "Y") {
            X_Ready4useDyad <- x@y_Ready4useDyad
        }
        if (what_1L_chr == "Z") {
            X_Ready4useDyad <- x@z_Ready4useDyad
        }
        if (nrow(X_Ready4useDyad@ds_tb) > 0) {
            population_ls <- manufacture(x, what_1L_chr = "population_ls")
            args_ls <- manufactureSlot(X_MimicEvent, "x_MimicEligible", 
                append_ls = list(ineligible_1L_chr = "TRUE"), 
                type_1L_chr = type_1L_chr)
            if (what_1L_chr == "Y") {
                population_ls$Y_Ready4useDyad <- rlang::exec(add_ineligible, 
                  X_Ready4useDyad, !!!args_ls)
            }
            if (what_1L_chr == "Z") {
                population_ls$Z_Ready4useDyad <- rlang::exec(add_ineligible, 
                  X_Ready4useDyad, !!!args_ls)
            }
            population_ls <- update_population_ls(population_ls, 
                split_var_1L_chr = "InModel", type_1L_chr = "join", 
                use_1L_chr = what_1L_chr)
            x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
        }
    }
    if (type_1L_chr == "switch") {
        population_ls <- manufacture(x, what_1L_chr = "population_ls") %>% 
            update_population_ls(type_1L_chr = type_1L_chr, use_1L_chr = what_1L_chr)
        x <- renew(x, population_ls = population_ls, type_1L_chr = "transform")
    }
    if (type_1L_chr == "transform") {
        x <- renewSlot(x, "x_MimicActive@x_Ready4useDyad", population_ls$X_Ready4useDyad) %>% 
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
#' @param env_ls Environment list (a list of environments), Default: list()
#' @param what_1L_chr What (a character vector of length one), Default: c("legacy")
#' @param ... Additional arguments
#' @return x (An object of class MimicConfiguration)
#' @rdname renew-methods
#' @aliases renew,MimicConfiguration-method
#' @export 
#' @importFrom purrr modify_at
#' @importFrom ready4 renew
methods::setMethod("renew", "MimicConfiguration", function (x, env_ls = list(), what_1L_chr = c("legacy"), ...) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr == "legacy") {
        if (is.na(x@modifiable_chr[1])) {
            x <- renewSlot(x, "modifiable_chr", character(0))
        }
        if (identical(x@x_MimicAlgorithms@main_ls, list("UPDATE"))) {
            x <- renewSlot(x, "x_MimicAlgorithms", renewSlot(x@x_MimicAlgorithms, 
                "main_ls", env_ls$main_ls))
        }
        if (identical(x@x_MimicAlgorithms@processing_ls$initialise_ls, 
            list("UPDATE"))) {
            new_ls <- make_simulation_fns_ls("processing", initialise_ls = env_ls$initialise_ls)
            new_ls <- new_ls$initialise_ls
            new_ls <- x@x_MimicAlgorithms@processing_ls %>% purrr::modify_at(.at = "initialise_ls", 
                ~new_ls)
            x <- renewSlot(x, "x_MimicAlgorithms@processing_ls", 
                new_ls)
        }
    }
    return(x)
})
