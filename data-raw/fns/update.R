update_current_date <- function (X_Ready4useDyad) {
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(CurrentDate = dplyr::case_when(ScheduledFor>EndDate ~ lubridate::NA_Date_,
                                                                              T ~ ScheduledFor)))
  return(X_Ready4useDyad)
}
update_current_event <- function (X_Ready4useDyad) 
{
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(CurrentEvent = dplyr::case_when(is.na(CurrentDate) ~ NA_character_,
                                                                               T ~ NextEvent)))
  return(X_Ready4useDyad)
}
update_gender <- function(data_tb){
  data_tb <- data_tb %>%
    dplyr::mutate(gender = dplyr::case_when(gender %in% c("Other","Prefer not to say") ~ "OtherPNTS",
                                            T ~ gender))
  return(data_tb)
}

update_k10_event_schedule <-  function(X_Ready4useDyad,
                                       type_1L_chr = c("Model", "Table")){
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr=="Table"){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c("CurrentDate", "ScheduledFor"), ~ dplyr::case_when(k10_part==1 ~treatment_start, T ~ .x))))
  }
  return(X_Ready4useDyad)
}
update_mismatched_vars <- function(model_dyad_ls = make_model_dyad_ls,
                                   type_1L_chr = c("drop", "rename")){
  type_1L_chr <- match.arg(type_1L_chr)
  X_Ready4useDyad <- model_dyad_ls$X_Ready4useDyad
  Y_Ready4useDyad <- model_dyad_ls$Y_Ready4useDyad
  mismatch_1_chr <- setdiff(names(X_Ready4useDyad@ds_tb), names(Y_Ready4useDyad@ds_tb))
  mismatch_2_chr <- setdiff(names(Y_Ready4useDyad@ds_tb), names(X_Ready4useDyad@ds_tb))
  if(type_1L_chr=="rename"){
    if(length(mismatch_2_chr)>0){
      assertthat::assert_that(length(mismatch_2_chr) == length(mismatch_1_chr))
      reconciliations_chr <- purrr::map_chr(1:length(mismatch_1_chr),
                                            ~{
                                              replacement_1L_int <- .x
                                              (strsplit(mismatch_1_chr[.x],"")[[1]] == strsplit(mismatch_2_chr[.x],"")[[1]]) %>%
                                                purrr::map2_chr(strsplit(mismatch_1_chr[.x],"")[[1]],
                                                                ~ ifelse(.x,.y, paste0("x",replacement_1L_int))) %>%
                                                paste0(collapse = "")
                                            })
      model_dyad_ls <- 1:length(mismatch_1_chr) %>%
        purrr::reduce(.init = model_dyad_ls,
                      ~{
                        list(X_Ready4useDyad = renewSlot(.x$X_Ready4useDyad, "ds_tb", .x$X_Ready4useDyad@ds_tb %>% dplyr::rename(!!rlang::sym(reconciliations_chr[.y]) := !!rlang::sym(mismatch_1_chr[.y]))), 
                             Y_Ready4useDyad = renewSlot(.x$Y_Ready4useDyad, "ds_tb", .x$Y_Ready4useDyad@ds_tb %>% dplyr::rename(!!rlang::sym(reconciliations_chr[.y]) := !!rlang::sym(mismatch_2_chr[.y]))))
                      })
    }
  }
  if(type_1L_chr == "drop"){
    model_dyad_ls <- list(X_Ready4useDyad = renewSlot(model_dyad_ls$X_Ready4useDyad, "ds_tb", model_dyad_ls$X_Ready4useDyad@ds_tb %>% dplyr::select(-dplyr::any_of(mismatch_1_chr))), 
                          Y_Ready4useDyad = renewSlot(model_dyad_ls$Y_Ready4useDyad, "ds_tb", model_dyad_ls$Y_Ready4useDyad@ds_tb %>% dplyr::select(-dplyr::any_of(mismatch_2_chr))))
  }
  return(model_dyad_ls)
}
update_order <- function (X_Ready4useDyad,
                          structural_chr = make_structural_vars(data_1L_chr = character(0), uid_1L_chr = "UID"),
                          type_1L_chr = c("rows", "columns")) {
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr %in% c("both","rows")){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                   dplyr::arrange(Iteration, UID))
  }
  if(type_1L_chr %in% c("both","columns")){
    params_chr <- names(X_Ready4useDyad@ds_tb)[startsWith(names(X_Ready4useDyad@ds_tb), "Param")] %>% sort() 
    main_chr <-  setdiff(names(X_Ready4useDyad@ds_tb), c(structural_chr, params_chr)) %>% sort()
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 X_Ready4useDyad@ds_tb %>%
                                   dplyr::select(tidyselect::any_of(c(structural_chr,
                                                                      main_chr,
                                                                      params_chr))))
  }
  return(X_Ready4useDyad)
}
update_population_classes <- function (X_Ready4useDyad, tfmn_ls = NULL) 
{
  tfmn_ls <- tfmn_ls %>% purrr::keep_at(intersect(names(tfmn_ls), names(X_Ready4useDyad@ds_tb)))
  if (!is.null(tfmn_ls) & !identical(tfmn_ls, purrr::keep_at(list(X=NULL),"Y"))) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 purrr::reduce(1:length(tfmn_ls), .init = X_Ready4useDyad@ds_tb, 
                                               ~{
                                                 fn <- tfmn_ls[[.y]]
                                                 .x %>% dplyr::mutate(`:=`(!!rlang::sym(names(tfmn_ls)[.y]), 
                                                                           fn(!!rlang::sym(names(tfmn_ls)[.y]))))
                                               }))
  }
  return(X_Ready4useDyad)
}
update_population_ls <- function(population_ls = NULL,
                                 X_Ready4useDyad = ready4use::Ready4useDyad(),
                                 type_1L_chr = c("split", "join", "form"),
                                 use_1L_chr = c("Y", "Z")
){
  type_1L_chr <- match.arg(type_1L_chr)
  use_1L_chr <- match.arg(use_1L_chr)
  if(type_1L_chr == "form"){
    population_ls <- list(X_Ready4useDyad = X_Ready4useDyad,
                          Y_Ready4useDyad = renewSlot(X_Ready4useDyad, "ds_tb",
                                                      X_Ready4useDyad@ds_tb %>% dplyr::filter(F)),
                          Z_Ready4useDyad  = renewSlot(X_Ready4useDyad, "ds_tb",
                                                       X_Ready4useDyad@ds_tb %>% dplyr::filter(F)))
  }
  if(type_1L_chr == "split"){
    if(use_1L_chr == "Y"){
      population_ls$Y_Ready4useDyad <- renewSlot(population_ls$Y_Ready4useDyad, "ds_tb", 
                                                 dplyr::bind_rows(population_ls$Y_Ready4useDyad@ds_tb %>% 
                                                                    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~as.numeric(.x))),
                                                                  population_ls$X_Ready4useDyad@ds_tb %>% dplyr::filter(is.na(CurrentDate))  %>% 
                                                                    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~as.numeric(.x)))))
    }
    if(use_1L_chr == "Z"){
      population_ls$Z_Ready4useDyad <- renewSlot(population_ls$Z_Ready4useDyad, "ds_tb", 
                                                 dplyr::bind_rows(population_ls$Z_Ready4useDyad@ds_tb %>% 
                                                                    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~as.numeric(.x))),
                                                                  population_ls$X_Ready4useDyad@ds_tb %>% dplyr::filter(is.na(CurrentDate))  %>% 
                                                                    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~as.numeric(.x)))))
    }
    population_ls$X_Ready4useDyad <- renewSlot(population_ls$X_Ready4useDyad, "ds_tb", population_ls$X_Ready4useDyad@ds_tb %>% dplyr::filter(!is.na(CurrentDate)))
  }
  if(type_1L_chr == "join"){
    if(use_1L_chr == "Y"){
      data_tb <- population_ls$Y_Ready4useDyad@ds_tb
    }
    if(use_1L_chr == "Z"){
      data_tb <- population_ls$Z_Ready4useDyad@ds_tb
    }
    population_ls$X_Ready4useDyad <- renewSlot(population_ls$X_Ready4useDyad, "ds_tb", 
                                               population_ls$X_Ready4useDyad@ds_tb %>% 
                                                 dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                                             ~as.numeric(.x))) %>% 
                                                 dplyr::bind_rows(data_tb %>% 
                                                                    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~as.numeric(.x)))) %>%
                                                 dplyr::arrange(Iteration, UID))
    if(use_1L_chr == "Y"){
      population_ls$Y_Ready4useDyad <- renewSlot(population_ls$Y_Ready4useDyad, "ds_tb",
                                                 population_ls$Y_Ready4useDyad@ds_tb  %>% dplyr::filter(F))
    }
    if(use_1L_chr == "Z"){
      population_ls$Z_Ready4useDyad <- renewSlot(population_ls$Z_Ready4useDyad, "ds_tb",
                                                 population_ls$Z_Ready4useDyad@ds_tb  %>% dplyr::filter(F))
    }
  }
  return(population_ls)
}
update_predictions_ds <- function (Y_Ready4useDyad, adjustment_1L_dbl = 0, do_int = 1:5, # NEEDS GENERALISING
                                   follow_up_1L_int = 12L, maintain_for_1L_int = 0L, sensitivities_ls = make_sensitivities_ls(), 
                                   tfmn_1L_chr = "NTF", tfmn_ls = make_class_tfmns(), 
                                   utility_1L_chr = c("AQoL6D"), 
                                   var_1L_chr = character(0), with_1L_chr = "_sim_mean") 
{
  utility_1L_chr <- utility_1L_chr[1]
  var_1L_chr <- make_conditional_vars(utility_1L_chr, follow_up_1L_int = follow_up_1L_int, 
                                      fup_var_1L_chr = var_1L_chr, type_1L_chr = "fup")
  if (1 %in% do_int) {
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                                                                          !!rlang::sym(paste0(var_1L_chr, with_1L_chr))))
  }
  if (2 %in% do_int) { # THIS PART NEEDS TO BE GENERALISED - ONLY APPROPRIATE FOR USE WITH CHU-9D and AQoL-6D
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                                                                          !!rlang::sym(var_1L_chr) %>% purrr::map_dbl(~max(min(.x, 
                                                                                                                               1), 
                                                                                                                           ifelse(utility_1L_chr == "CHU9D", -0.2118, 0.03)))))
  }
  if (3 %in% do_int | 4 %in% do_int | 6 %in% do_int) {
    yrs_1L_dbl <- make_conditional_vars(utility_1L_chr, follow_up_1L_int = follow_up_1L_int, 
                                        type_1L_chr = "years")
  }
  if (3 %in% do_int) {
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(utility_1L_chr, 
                                                                                              "_change")), (!!rlang::sym(var_1L_chr) - !!rlang::sym(utility_1L_chr)) %>% 
                                                                            as.double)) %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(utility_1L_chr, 
                                                                                                                                   "_QALYs")), (((!!rlang::sym(var_1L_chr) + !!rlang::sym(utility_1L_chr))/2) %>% 
                                                                                                                                                  as.double()) * yrs_1L_dbl))
  }
  if (4 %in% do_int | 6 %in% do_int | 7 %in% do_int) {
    Y_Ready4useDyad <- add_outcome_time_vars(Y_Ready4useDyad, 
                                             outcome_1L_chr = utility_1L_chr, add_adjustments_1L_lgl = (4 %in% 
                                                                                                          do_int), follow_up_1L_int = follow_up_1L_int, 
                                             fup_var_1L_chr = var_1L_chr, maintain_for_1L_int = maintain_for_1L_int)
    end_var_1L_chr <- make_conditional_vars(utility_1L_chr, 
                                            follow_up_1L_int = follow_up_1L_int, fup_var_1L_chr = var_1L_chr, 
                                            type_1L_chr = "end")
    start_var_1L_chr <- make_conditional_vars(utility_1L_chr, 
                                              follow_up_1L_int = follow_up_1L_int, fup_var_1L_chr = var_1L_chr, 
                                              type_1L_chr = "start")
  }
  if (4 %in% do_int) {
    Y_Ready4useDyad <- add_qalys_sensitivities(Y_Ready4useDyad, 
                                               end_var_1L_chr = end_var_1L_chr, start_var_1L_chr = start_var_1L_chr, 
                                               utility_1L_chr = utility_1L_chr, type_1L_chr = "legacy")
    do_int <- c(do_int, 8) %>% unique()
  }
  if (5 %in% do_int) {
    Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", 
                                                 type_1L_chr = "update")
  }
  if (6 %in% do_int) {
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                                 Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(utility_1L_chr, 
                                                                                                  "_QALYs")), !!rlang::sym(paste0(utility_1L_chr, 
                                                                                                                                  "_QALYs")) + (((!!rlang::sym(start_var_1L_chr) + 
                                                                                                                                                    !!rlang::sym(end_var_1L_chr))/2) %>% as.double()) * 
                                                                                !!rlang::sym(paste0(utility_1L_chr, "_years")))))
  }
  if (7 %in% do_int) {
    Y_Ready4useDyad <- add_qalys_sensitivities(Y_Ready4useDyad, 
                                               sensitivities_ls = sensitivities_ls, utility_1L_chr = utility_1L_chr)
  }
  if (8 %in% do_int) {
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                                 Y_Ready4useDyad@ds_tb %>% dplyr::select(-tidyselect::any_of(paste0(utility_1L_chr, 
                                                                                                    c("_days", "_years", "_multiplier", "_adjusted")))))
  }
  return(Y_Ready4useDyad)
}
update_previous <- function (X_Ready4useDyad, modifiable_chr = character(0), pattern_1L_chr = "{col}_previous") 
{
  modifiable_chr <- intersect(modifiable_chr, names(X_Ready4useDyad@ds_tb))
  if (!identical(modifiable_chr, character(0))) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(dplyr::all_of(modifiable_chr), 
                                                                                       ~.x, .names = pattern_1L_chr)))
  }
  return(X_Ready4useDyad)
}
update_project_test_cmprsns <- function(X_Ready4useDyad){
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>%
                                 dplyr::filter(!Variable %in% c("Iteration", "UID", "Adult", "Age", "treatment_count", "gad7", "phq9", 
                                                                "Minutes", "Minutes_change", 
                                                                c("k10", "AQoL6D", "CHU9D"), paste0(c("k10", "AQoL6D", "CHU9D"),"_change"),
                                                                paste0(c("k10", "AQoL6D", "CHU9D"),"_previous"),
                                                                "AQoL6D_QALYs", "CHU9D_QALYs")) %>%
                                 dplyr::mutate(Variable = Variable %>% stringr::str_replace_all("_previous", "") %>% 
                                                 stringr::str_replace_all("_12_Weeks", " at follow-up") %>% 
                                                 stringr::str_replace_all("_start", " at start") %>%
                                                 stringr::str_replace_all("_", " ") %>%
                                                 stringr::str_replace_all("k10", "K10")) %>%
                                 dplyr::arrange(Variable))
  return(X_Ready4useDyad)
}
update_qalys <- function (X_Ready4useDyad, add_sensitivity_1L_lgl = FALSE, adjustment_1L_dbl = 0, 
                          follow_up_1L_int = integer(0), maintain_for_1L_int = 0, sensitivities_ls = make_sensitivities_ls(), 
                          tidy_1L_lgl = FALSE, utilities_chr = c("CHU9D", "AQoL6D")) 
{
  sensitivity_1L_int <- integer(0)
  if (add_sensitivity_1L_lgl) {
    sensitivity_1L_int <- 7
  }
  X_Ready4useDyad <- utilities_chr %>% purrr::reduce(.init = X_Ready4useDyad, 
                                                     ~{
                                                       Y_Ready4useDyad <- .x
                                                       if (!paste0(.y, "_QALYs") %in% names(Y_Ready4useDyad@ds_tb)) {
                                                         Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, 
                                                                                      "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(.y, 
                                                                                                                                                                "_QALYs")), 0)))
                                                       }
                                                       Y_Ready4useDyad <- update_previous(Y_Ready4useDyad, 
                                                                                          modifiable_chr = paste0(.y, "_QALYs"))
                                                       Y_Ready4useDyad %>% update_predictions_ds(adjustment_1L_dbl = adjustment_1L_dbl, 
                                                                                                 do_int = c(6, sensitivity_1L_int, 8), follow_up_1L_int = follow_up_1L_int, 
                                                                                                 utility_1L_chr = .y, maintain_for_1L_int = maintain_for_1L_int, 
                                                                                                 sensitivities_ls = sensitivities_ls)
                                                     })
  if (tidy_1L_lgl) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::select(-tidyselect::any_of(paste0(utilities_chr, 
                                                                                                    "_QALYS"))))
  }
  return(X_Ready4useDyad)
}
update_scenario_names <- function (forecasts_tb, after_1L_chr = character(0), before_1L_chr = character(0), 
                                   prefix_1L_chr = "scenario_", reference_1L_chr = "Status quo", others_chr = character(0),
                                   tfmn_1_fn = as.numeric, tfmn_2_fn = scales::percent) 
{
  forecasts_tb <- forecasts_tb %>% dplyr::mutate(Scenario = dplyr::case_when(!Scenario %in%
                                                                               c(reference_1L_chr, others_chr) ~ paste0(before_1L_chr, tfmn_2_fn(tfmn_1_fn(stringr::str_remove_all(Scenario, 
                                                                                                                                                                                   prefix_1L_chr))), after_1L_chr), T ~ Scenario))
  return(forecasts_tb)
}
update_scheduled_date <- function (X_Ready4useDyad, 
                                   increment_1L_int = integer(0), 
                                   target_1L_int = integer(0), variable_1L_chr = character(0),
                                   type_1L_chr = c("End", "Day")) 
{
  type_1L_chr <- match.arg(type_1L_chr)
  if (type_1L_chr == "End") {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c("CurrentDate", 
                                                                                         "ScheduledFor"), ~EndDate)))
  }
  if (type_1L_chr == "Day") {
    if (identical(increment_1L_int, integer(0))) {
      if(!identical(variable_1L_chr, character(0))){
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                     X_Ready4useDyad@ds_tb %>% dplyr::mutate(IncrementDays = !!rlang::sym(variable_1L_chr)))
      }else{
        assertthat::assert_that(!identical(target_1L_int, 
                                           integer(0)))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                     X_Ready4useDyad@ds_tb %>% dplyr::mutate(IncrementDays = target_1L_int - (lubridate::time_length((CurrentDate - StartDate), "days"))))
      }
    } else {
      X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                   X_Ready4useDyad@ds_tb %>% dplyr::mutate(IncrementDays = increment_1L_int))
    }
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c("ScheduledFor"), 
                                                                                       ~CurrentDate + lubridate::days(IncrementDays))))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::select(-IncrementDays))
  }
  return(X_Ready4useDyad)
}
update_test_ds <- function(X_Ready4useDyad,
                           modifiable_chr,
                           pattern_1L_chr = "{col}_1_year",
                           period_dtm = lubridate::years(1),
                           type_1L_chr = c("all", "main", "change", "zero")){ #c("k10", "AQoL6D", "CHU9D")
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "all"){
    X_Ready4useDyad <- update_test_ds(X_Ready4useDyad, modifiable_chr = modifiable_chr, type_1L_chr = "main") %>%
      update_test_ds(modifiable_chr = modifiable_chr, type_1L_chr = "change")
    X_Ready4useDyad <- update_previous(X_Ready4useDyad, modifiable_chr = modifiable_chr) %>%
      update_previous(modifiable_chr = modifiable_chr, pattern_1L_chr = pattern_1L_chr) %>%
      update_previous(modifiable_chr = paste0(modifiable_chr, "_change")) %>%
      update_test_ds(modifiable_chr = modifiable_chr, type_1L_chr = "zero")
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                   dplyr::mutate(StartDate = Date, CurrentDate = Date + period_dtm , EndDate = Date + period_dtm)) %>%
      update_tx_start_end()
  }
  if(type_1L_chr == "main"){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 modifiable_chr %>%
                                   purrr::reduce(.init = X_Ready4useDyad@ds_tb,
                                                 ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := !!rlang::sym(.y) + !!rlang::sym(paste0(.y,"_change")))))
  }
  if(type_1L_chr == "change"){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 modifiable_chr %>%
                                   purrr::reduce(.init = X_Ready4useDyad@ds_tb,
                                                 ~ .x %>% dplyr::mutate(!!rlang::sym(paste0(.y,"_change")) := !!rlang::sym(.y) - !!rlang::sym(paste0(.y,"_previous")))))
  }
  if(type_1L_chr == "zero"){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 modifiable_chr %>%
                                   purrr::reduce(.init = X_Ready4useDyad@ds_tb,
                                                 ~ .x %>% dplyr::mutate(!!rlang::sym(paste0(.y,"_change")) := 0)))
  }
  return(X_Ready4useDyad)
}
update_tx_start_end <- function (X_Ready4useDyad, prefix_1L_chr = "treatment", tx_duration_dtm = lubridate::weeks(12)) 
{
  if (!paste0(prefix_1L_chr,"_change") %in% names(X_Ready4useDyad@ds_tb)) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_change")) := NA_character_))
  }
  if (!paste0(prefix_1L_chr, "_count") %in% names(X_Ready4useDyad@ds_tb)) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_count")) := 0))
  }
  if (!paste0(prefix_1L_chr, "_start") %in% names(X_Ready4useDyad@ds_tb)) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_start")) := lubridate::NA_Date_))
  }
  if (!paste0(prefix_1L_chr, "_measurement") %in% names(X_Ready4useDyad@ds_tb)) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_measurement")) := lubridate::NA_Date_))
  }
  if (!paste0(prefix_1L_chr, "_fraction") %in% names(X_Ready4useDyad@ds_tb)) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_fraction")) := NA_real_))
  }
  if (!paste0(prefix_1L_chr, "_status_previous") %in% names(X_Ready4useDyad@ds_tb)) {
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
                                 X_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_status_previous")) := NA_character_))
  }
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_count")) := dplyr::case_when(as.character(!!rlang::sym(paste0(prefix_1L_chr, "_change"))) == 
                                                                                                                   "Start" ~ !!rlang::sym(paste0(prefix_1L_chr, "_count")) + 1, as.character(!!rlang::sym(paste0(prefix_1L_chr, "_change"))) == 
                                                                                                                   "End" & as.character(!!rlang::sym(paste0(prefix_1L_chr, "_status_previous"))) == 
                                                                                                                   "Waitlist" ~ !!rlang::sym(paste0(prefix_1L_chr, "_count")) + 1, T ~ !!rlang::sym(paste0(prefix_1L_chr, "_count"))), 
                                               !!rlang::sym(paste0(prefix_1L_chr, "_start")) := dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, "_start"))) ~ 
                                                                                                                   !!rlang::sym(paste0(prefix_1L_chr, "_start")), is.na(!!rlang::sym(paste0(prefix_1L_chr, "_start"))) & as.character(!!rlang::sym(paste0(prefix_1L_chr, "_status"))) == 
                                                                                                                   "Treatment" ~ purrr::map_vec(CurrentDate, ~{
                                                                                                                     sample(seq(.x - tx_duration_dtm, .x, by = "day"), 
                                                                                                                            1)
                                                                                                                   }), T ~ lubridate::NA_Date_), !!rlang::sym(paste0(prefix_1L_chr, "_measurement")) := dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, "_measurement"))) ~ 
                                                                                                                                                                                                                           !!rlang::sym(paste0(prefix_1L_chr, "_measurement")), is.na(!!rlang::sym(paste0(prefix_1L_chr, "_measurement"))) & 
                                                                                                                                                                                                                           !is.na(!!rlang::sym(paste0(prefix_1L_chr, "_start"))) ~ !!rlang::sym(paste0(prefix_1L_chr, "_start")) + tx_duration_dtm, 
                                                                                                                                                                                                                         T ~ lubridate::NA_Date_), !!rlang::sym(paste0(prefix_1L_chr, "_fraction")) := dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, "_fraction"))) ~ 
                                                                                                                                                                                                                                                                                                                          !!rlang::sym(paste0(prefix_1L_chr, "_fraction")), !!rlang::sym(paste0(prefix_1L_chr, "_start")) >= StartDate & 
                                                                                                                                                                                                                                                                                                                          !!rlang::sym(paste0(prefix_1L_chr, "_measurement")) <= EndDate ~ 1, T ~ lubridate::as.period(purrr::map2_vec(EndDate, 
                                                                                                                                                                                                                                                                                                                                                                                                                                       !!rlang::sym(paste0(prefix_1L_chr, "_measurement")), ~as.Date(ifelse(is.na(.y), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            lubridate::NA_Date_, min(.x, .y, na.rm = T)))) - 
                                                                                                                                                                                                                                                                                                                                                                                                                         CurrentDate)/tx_duration_dtm)))
  return(X_Ready4useDyad)
}
update_with_imputed <- function(project_dss_ls,
                                age_1L_chr = "Age",
                                employment_1L_chr = "employment_status",
                                gender_1L_chr = "gender",
                                imputation_args_ls = NULL,
                                imputations_fn = mice::mice,
                                platform_1L_chr = "platform",
                                recode_lup_r3 = make_project_recode_lup(),
                                uid_1L_chr = "case_number",
                                type_1L_chr = c("modelled", "fixed"),
                                what_chr = c("contacts", "outcomes", "overview")){
  type_1L_chr <- match.arg(type_1L_chr)
  imputed_dss_ls <- project_dss_ls %>% purrr::map2(names(project_dss_ls),
                                                ~ {
                                                  if(.y %in% what_chr){
                                                    if(inherits(.x, "Ready4useDyad")){
                                                      ds_tb <- .x@ds_tb
                                                    }else{
                                                      ds_tb <- .x
                                                    }
                                                    ####
                                                    
                                                    if(type_1L_chr == "fixed"){
                                                      ds_tb <- ds_tb %>% dplyr::mutate(dplyr::across(c(employment_1L_chr, gender_1L_chr), ~ ifelse(is.na(.x),"Missing",.x)))
                                                      # ds_tb <- c(employment_1L_chr, atsi_status) %>% #
                                                      #   purrr::reduce(.init = ds_tb,
                                                      #                 ~ {
                                                      #                   if (!is.numeric(.x %>% dplyr::pull(!!rlang::sym(.y)))) {
                                                      #                     .x %>% dplyr::mutate(`:=`(!!rlang::sym(.y),
                                                      #                                               recode_lup_r3 %>% 
                                                      #                                                 ready4show::manufacture.ready4show_correspondences(.x %>% dplyr::select(!!rlang::sym(.y)), flatten_1L_lgl = TRUE)))
                                                      #                   }
                                                      #                 })
                                                      # ds_tb <- ds_tb %>% dplyr::mutate(!!rlang::sym(gender_1L_chr) := dplyr::case_when(!(!!rlang::sym(gender_1L_chr) %in% c("Female", "Male", "Prefer not to say")) ~ "Other",
                                                      #                                                                        T ~ !!rlang::sym(gender_1L_chr)))
                                                      platforms_chr <- ds_tb %>% dplyr::pull(!!rlang::sym(platform_1L_chr)) %>% unique()
                                                      mean_ages_int <- platforms_chr %>% purrr::map_int(~ds_tb %>%
                                                                                                          dplyr::filter(!!rlang::sym(platform_1L_chr) == .x) %>%
                                                                                                          dplyr::pull(!!rlang::sym(age_1L_chr)) %>%
                                                                                                          purrr::discard(is.na) %>%
                                                                                                          mean() %>%
                                                                                                          round(0))
                                                      ds_tb <- ds_tb %>% dplyr::mutate(!!rlang::sym(age_1L_chr) := dplyr::case_when(is.na(!!rlang::sym(age_1L_chr)) ~ !!rlang::sym(platform_1L_chr) %>% purrr::map_int(~mean_ages_int[which(.x==platforms_chr)]),
                                                                                                                                    T ~ !!rlang::sym(age_1L_chr)))
                                                      
                                                    }
                                                    
                                                    ####
                                                    
                                                    if(inherits(.x, "Ready4useDyad")){
                                                      X <- .x
                                                      X@ds_tb <- ds_tb
                                                      X
                                                    }else{
                                                      ds_tb
                                                    }
                                                    
                                                  }else{
                                                    .x
                                                  }
                                                })
  return(imputed_dss_ls)
  
}
