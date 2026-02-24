update_arguments_ls <- function(args_ls,
                                function_fn){
  allowed_chr <- names(formals(function_fn))
  if(!"..." %in% allowed_chr){
    args_ls <- args_ls[intersect(allowed_chr, names(args_ls))]
  }
  return(args_ls)
}
update_current_date <- function (X_Ready4useDyad) {
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(CurrentDate = dplyr::case_when(ScheduledFor>EndDate ~ lubridate::NA_Date_,
                                                                              T ~ ScheduledFor)))
  return(X_Ready4useDyad)
}
update_current_event <- function (X_Ready4useDyad) {
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(CurrentEvent = dplyr::case_when(is.na(CurrentDate) ~ NA_character_,
                                                                               T ~ NextEvent)))
  return(X_Ready4useDyad)
}
update_episodes_lup <- function(episodes_lup_tb,
                                dates_chr, 
                                part_one_1L_chr, 
                                program_true_1L_chr){ 
  episodes_lup_tb <- episodes_lup_tb %>% 
    dplyr::mutate(!!rlang::sym(program_true_1L_chr) := dplyr::case_when(!!rlang::sym(part_one_1L_chr) & extra_logic & episode_end_date < as.Date(dates_chr[2]) & episode_end_date > as.Date(dates_chr[1]) ~ FALSE,
                                                                        !!rlang::sym(part_one_1L_chr) & extra_logic & episode_end_date > as.Date(dates_chr[3]) ~ FALSE,
                                                                        !!rlang::sym(part_one_1L_chr) & !extra_logic ~ FALSE,
                                                                        T ~ !!rlang::sym(program_true_1L_chr)))
  return(episodes_lup_tb)
}
update_gender <- function(data_tb){
  data_tb <- data_tb %>%
    dplyr::mutate(gender = dplyr::case_when(gender %in% c("Other","Prefer not to say") ~ "OtherPNTS",
                                            T ~ gender))
  return(data_tb)
}
update_intervention_name <- function(data_tb,
                                     new_1L_chr = "Comparator",
                                     old_1L_chr = "FlexPsych",
                                     var_nm_1L_chr = "Intervention"){
  data_tb <- dplyr::mutate(data_tb, !!rlang::sym(var_nm_1L_chr) := !!rlang::sym(var_nm_1L_chr) %>% stringr::str_replace_all(old_1L_chr, new_1L_chr))
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
update_mds_modelling_ds <- function(X_Ready4useDyad,
                                    imputations_int = 1, # Generalise - create argument to pass FlexPsych used later
                                    sample_ls = NULL,
                                    filter_true_1L_chr = "FlexPsych"){
  if(".imp" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- X_Ready4useDyad %>%
      renewSlot("ds_tb",
                X_Ready4useDyad@ds_tb %>% dplyr::filter(.imp %in% imputations_int))
  }
  X_Ready4useDyad <- X_Ready4useDyad %>%
    renewSlot("ds_tb",
              X_Ready4useDyad@ds_tb %>% 
                dplyr::group_by(UID) %>% dplyr::arrange(UID, Episode) %>% 
                dplyr::summarise(OneYearCutOffDate = dplyr::first(first_service_date) + lubridate::years(1),
                                 TotalEpisodes = max(Episode)) %>%
                dplyr::left_join(X_Ready4useDyad@ds_tb) %>%
                dplyr::mutate(YearOne = (first_service_date <= OneYearCutOffDate)) %>%
                # dplyr::filter(.imp==1) %>% 
                dplyr::rename(IARPractitioner = iar_dst_practitioner_level_of_care,
                              Intervention = InterventionGroup) %>%
                dplyr::mutate(MedicalUseMins = GPUseMins + PsychiatristUseMins + OtherMedicalUseMins) %>%
                dplyr::mutate(EpisodeDurationCategory = dplyr::case_when(EpisodeDurationDays < 100 ~ "Under 100 days",
                                                                         EpisodeDurationDays >= 100 & EpisodeDurationDays < 200 ~ "100-200 days",
                                                                         EpisodeDurationDays >= 200 ~ "200 days or more",
                                                                         T ~ NA_character_) %>% as.factor() ) %>%
                dplyr::mutate(SubthresholdDisorder = dplyr::case_when(Diagnosis == "Sub-syndromal" ~ T,
                                                                      is.na(Diagnosis) ~ NA,
                                                                      T ~ F)) %>%
                dplyr::mutate(referral_date = dplyr::case_when(referral_date > first_service_date ~ lubridate::NA_Date_,
                                                               T ~ referral_date
                )) %>%
                dplyr::group_by(UID) %>%
                dplyr::arrange(UID, Episode) %>%
                dplyr::mutate(last_service_date_previous = dplyr::lag(last_service_date, default = lubridate::NA_Date_)) %>%
                dplyr::mutate(referral_date = dplyr::case_when(!is.na(last_service_date_previous) & !is.na(referral_date) & referral_date< last_service_date_previous ~ lubridate::NA_Date_,
                                                               T ~ referral_date
                )) %>%
                dplyr::mutate(DaysToYearOneRepresentation = dplyr::case_when(is.na(referral_date)  ~ NA_real_,
                                                                             !YearOne ~ NA_real_,
                                                                             T ~ as.numeric(first_service_date - last_service_date_previous))) %>%
                dplyr::mutate(DaysToYearOneRepresentation = dplyr::lead(DaysToYearOneRepresentation, default = NA_real_)) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(WaitInDays = as.numeric(first_service_date - referral_date)) %>%
                dplyr::filter(InScope | Intervention == filter_true_1L_chr) %>% # Generalise
                dplyr::mutate(Age = dplyr::case_when(Age>100 ~ 100, T ~ Age),
                              IRSADQuintile = as.factor(IRSADQuintile),
                              IARPractitioner = as.factor(IARPractitioner))
    )
  if(!is.null(sample_ls)){
    X_Ready4useDyad <- X_Ready4useDyad %>%
      renewSlot("ds_tb",
                1:length(sample_ls) %>% purrr::reduce(.init = X_Ready4useDyad@ds_tb,
                                                      ~ {
                                                        group_1L_chr <- names(sample_ls)[.y]
                                                        use_1L_dbl <- sample_ls[[.y]]
                                                        use_all_tb <- .x %>% dplyr::filter(Intervention != group_1L_chr)
                                                        sample_tb <-  .x %>% dplyr::filter(Intervention == group_1L_chr)
                                                        samples_1L_int <- round(length(unique(sample_tb$UID)) * use_1L_dbl,0)
                                                        keep_int <- sample(unique(sample_tb$UID), size=samples_1L_int, replace = F)
                                                        sample_tb <- sample_tb %>% dplyr::filter(UID %in% keep_int)
                                                        dplyr::bind_rows(use_all_tb, sample_tb) %>% dplyr::arrange(UID, Episode)
                                                      })
      )
  }
  return(X_Ready4useDyad)
}
update_minute_var_nms <- function(data_tb,
                                  type_1L_chr = c("undo", "do")){
  type_1L_chr <- match.arg(type_1L_chr)
  rename_chr <- c("ClinicalPsychologistUseMins", 
                  "PsychiatristUseMins",
                  "GPUseMins",
                  "OtherMedicalUseMins", 
                  "NurseUseMins",
                  "OtherUseMins")
  if(type_1L_chr == "undo"){
    rename_chr <- paste0(rename_chr, "_change") %>%
      intersect(names(data_tb))
  }else{
    rename_chr <- rename_chr %>%
      intersect(names(data_tb))
  }
  if(!identical(rename_chr, character(0))){
    if(type_1L_chr == "undo"){
      data_tb <- data_tb %>% 
        dplyr::rename_with(~stringr::str_replace_all(.x,"_change",""),
                           .cols = rename_chr)
      
    }else{
      data_tb <- data_tb %>% 
        dplyr::rename_with(~paste0(.x,"_change"),
                           .cols = rename_chr)
    }
  }
  return(data_tb)
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
update_next_date <- function (X_Ready4useDyad) {
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(ScheduledFor = dplyr::case_when(ScheduledFor>EndDate ~ lubridate::NA_Date_,
                                                                              T ~ ScheduledFor)))
  return(X_Ready4useDyad)
}
update_next_event <- function (X_Ready4useDyad) 
{
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% 
                                 dplyr::mutate(NextEvent = dplyr::case_when(is.na(ScheduledFor) ~ NA_character_,
                                                                               T ~ NextEvent)))
  return(X_Ready4useDyad)
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
update_partial_results <- function(X_Ready4useDyad = ready4use::Ready4useDyad(),
                                   utilities_chr,
                                   update_fn = function(X_Ready4useDyad){identity(X_Ready4useDyad)},
                                   combined_suffixes_chr = c("","S01", "S02", "S10", "S11", "S12"),
                                   timestamp_1L_chr = get_timestamp(),
                                   # outcome_suffixes_chr = c("","_YR1_S1", "_YR1_S2"),
                                   ...) {#results_xx$total_ls$X
  outcome_sensitivities_chr <- setdiff(combined_suffixes_chr %>% purrr::map_chr(~stringr::str_sub(.x,start =3)) %>% unique() %>% sort(),c("","0"))
  outcome_suffixes_chr <- c("",
                            if(!identical(character(0), outcome_sensitivities_chr)){
                              paste0(timestamp_1L_chr,paste0("_S", outcome_sensitivities_chr)) 
                              }else{
                                character(0)
                                })
  # outcome_suffixes_chr = c("","_YR1_S1", "_YR1_S2"),
  qalys_chr <- purrr::map(outcome_suffixes_chr, ~ paste0(paste0(utilities_chr, "_QALYs"),.x)) %>% purrr::flatten_chr()
  icers_chr <- purrr::map(combined_suffixes_chr, ~ paste0(paste0("ICER_",utilities_chr), paste0(ifelse(.x=="","","_"), .x))) %>% purrr::flatten_chr()
  ces_chr <- purrr::map(combined_suffixes_chr, ~ paste0(paste0("CE_",utilities_chr), paste0(ifelse(.x=="","","_"), .x))) %>% purrr::flatten_chr()
  extras_ls <- list(...)
  args_ls <- list(X_Ready4useDyad = X_Ready4useDyad) %>% append(extras_ls)
  X_Ready4useDyad <- rlang::exec(update_fn, !!!args_ls)
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                               combined_suffixes_chr %>%
                                 purrr::reduce(.init = X_Ready4useDyad@ds_tb,
                                               ~{
                                                 data_tb <- .x
                                                 cost_1L_chr <- ifelse(.y %in% c("",combined_suffixes_chr[combined_suffixes_chr %>% stringr::str_sub(start=2, end=2) =="0"]), 
                                                                       "Cost", 
                                                                       paste0("Cost_S",stringr::str_sub(.y,start=2, end=2))) # "Cost_S1"
                                                 effect_1L_chr <- ifelse(.y %in% c("", "S10"), "_QALYs", 
                                                                         paste0(paste0("_QALYs",ifelse(length(outcome_suffixes_chr)<2, "ERROR",stringr::str_sub(outcome_suffixes_chr[2],end = -2))), # "_QALYs_YR1_S", 
                                                                                 stringr::str_sub(.y,start = -1)))
                                                 last_1L_chr <- .y
                                                 purrr::reduce(utilities_chr,
                                                               .init = data_tb,
                                                               ~{
                                                                 .x %>% 
                                                                   add_dominated(effect_1L_chr = paste0(.y,effect_1L_chr),
                                                                                 cost_1L_chr = cost_1L_chr,
                                                                                 suffix_1L_chr = paste0("_", .y, last_1L_chr)) %>%
                                                                   add_icer(effect_1L_chr = paste0(.y,effect_1L_chr),
                                                                            cost_1L_chr = cost_1L_chr,
                                                                            suffix_1L_chr = paste0("_", .y, last_1L_chr)) %>%
                                                                   add_cost_effectiveness(cost_1L_chr = cost_1L_chr, 
                                                                                          effect_1L_chr = paste0(.y,effect_1L_chr), 
                                                                                          suffix_1L_chr = paste0("_", .y, last_1L_chr), 
                                                                                          threshold_1L_dbl = threshold_1L_dbl)
                                                               })
                                               }))
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
                                 # switch_chr = c("X","Y"),
                                 type_1L_chr = c("split", "join", "form", "switch"),
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
  if(type_1L_chr == "switch"){
    names_chr <- names(population_ls)
    switch_chr = c("X",use_1L_chr)
    indices_int <- switch_chr[1:2] %>% purrr::map_int(~which(startsWith(names_chr,.x)))
    new_int <- indices_int[2:1]
    population_ls <- population_ls %>% setNames(names_chr[1:length(names_chr) %>% purrr::map_int(~ifelse(.x %in% indices_int,new_int[.x],.x))])
    population_ls <- population_ls[names_chr]
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
update_processed_tb <- function(data_tb,
                                first_eight_1L_lgl = NA,
                                program_1L_chr = NA_character_){
  
  if(!is.na(program_1L_chr)){
    data_tb <- data_tb %>% dplyr::filter(Program == program_1L_chr)
  }
  if(!is.na(first_eight_1L_lgl)){
    if(first_eight_1L_lgl){
      data_tb <- data_tb %>% dplyr::filter(FirstEight) 
    }else{
      data_tb <- data_tb %>% dplyr::filter(!FirstEight) 
    }
  }
  data_tb <- data_tb %>%
    dplyr::group_by(Report) %>%
    dplyr::summarise(Service = dplyr::first(Service),
                     FirstEight = dplyr::first(FirstEight),
                     Start = dplyr::first(Start),
                     End = dplyr::first(End))
  return(data_tb)
}
update_project_2_configuration<- function (X_MimicConfiguration,
                                           batch_1L_int,
                                           arms_chr = character(0),
                                           arms_for_intervention_costs_chr = character(0),
                                           arms_for_offsets_chr = character(0), 
                                           arms_for_non_helpseeking_chr = character(0), 
                                           arms_for_iar_adjustment_chr = character(0), 
                                           extra_draws_fn = NULL,
                                           horizon_dtm = lubridate::years(1), 
                                           inputs_ls = NULL, 
                                           iterations_int = integer(0), 
                                           modifiable_chr = make_project_2_vars("modify"),
                                           seed_1L_int = 2001L, 
                                           sensitivities_ls = make_project_2_sensitivities_ls(), 
                                           start_dtm = Sys.Date(), 
                                           tfmn_ls = make_class_tfmns(), 
                                           tx_duration_dtm = lubridate::weeks(12), 
                                           treatment_ls = NULL,
                                           utilities_chr = c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2"),
                                           utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr)){
  if(!identical(X_MimicConfiguration, MimicConfiguration())){
    # X_MimicConfiguration <- renewSlot(X_MimicConfiguration, "seed_1L_int",
    #                                   X_MimicConfiguration@seed_1L_int + batch_1L_int)
    # if(is.na(X_MimicConfiguration@modifiable_chr[1])){
    #   X_MimicConfiguration <- renewSlot(X_MimicConfiguration, "modifiable_chr",
    #                                     character(0))
    # }
    # if(identical(X_MimicConfiguration@x_MimicAlgorithms@main_ls, list("UPDATE"))){
    #   X_MimicConfiguration <- renewSlot(X_MimicConfiguration, "x_MimicAlgorithms",
    #                                     renewSlot(X_MimicConfiguration@x_MimicAlgorithms, "main_ls", list(`Project 2` = predict_project_2_pathway)))
    # }
    # if(identical(X_MimicConfiguration@x_MimicAlgorithms@processing_ls$initialise_ls, list("UPDATE"))){
    #   new_ls <- make_simulation_fns_ls("processing",
    #                                    initialise_ls = make_project_2_initialise_ls(derive_ls = X_MimicConfiguration@x_MimicAlgorithms@x_MimicUtility@mapping_ls))
    #   new_ls <- new_ls$initialise_ls
    #   new_ls <- X_MimicConfiguration@x_MimicAlgorithms@processing_ls %>% purrr::modify_at(.at = "initialise_ls", ~new_ls)
    #   X_MimicConfiguration <- renewSlot(X_MimicConfiguration,"x_MimicAlgorithms@processing_ls", new_ls)
    # }
    if(identical(names(X_MimicConfiguration@arms_tb), "Arm")){
      arms_extras_ls <- make_project_2_arms_extras_ls(X_MimicConfiguration@arms_tb$Arm,
                                                      arms_for_iar_adjustment_chr = arms_for_iar_adjustment_chr,
                                                      arms_for_intervention_costs_chr = arms_for_intervention_costs_chr,
                                                      arms_for_non_helpseeking_chr = arms_for_non_helpseeking_chr,
                                                      arms_for_offsets_chr = arms_for_offsets_chr,
                                                      treatment_ls = treatment_ls,
                                                      tx_duration_dtm = tx_duration_dtm)
      X_MimicConfiguration <- renewSlot(X_MimicConfiguration, 
                                        "arms_tb",
                                        make_arms_tb(X_MimicConfiguration@arms_tb,
                                                     settings_ls = arms_extras_ls))
    }
  }else{
    iterations_ls <- purrr::map(1:batch_1L_int,
                                ~ {
                                  if(.x == batch_1L_int){
                                    iterations_int
                                  }else{
                                    integer(0)
                                  }
                                })
    arms_extras_ls <- make_project_2_arms_extras_ls(arms_chr,
                                                    arms_for_iar_adjustment_chr = arms_for_iar_adjustment_chr,
                                                    arms_for_intervention_costs_chr = arms_for_intervention_costs_chr,
                                                    arms_for_non_helpseeking_chr = arms_for_non_helpseeking_chr,
                                                    arms_for_offsets_chr = arms_for_offsets_chr,
                                                    treatment_ls = treatment_ls,
                                                    tx_duration_dtm = tx_duration_dtm)
    #####
    X_MimicConfiguration <- make_configuration(
      # add_logic_fn = identity, 
      arms_chr = arms_chr, 
      drop_missing_1L_lgl = T,
      drop_suffix_1L_chr = "_mean",
      extra_draws_fn = extra_draws_fn,
      horizon_dtm = horizon_dtm,
      initialise_ls = make_project_2_initialise_ls(derive_ls = utility_fns_ls),
      inputs_ls = inputs_ls,
      iterations_ls = iterations_ls,
      main_ls = list(`Project 2` = predict_project_2_pathway),
      modifiable_chr = modifiable_chr,
      seed_1L_int = seed_1L_int - batch_1L_int, ## Note edit - Only currently relevant if draws are created within algorithm
      sensitivities_ls = sensitivities_ls,
      start_dtm = start_dtm,
      synthesis_fn = NULL, ## DIFFERENT
      transformations_ls = tfmn_ls, 
      utilities_chr = utilities_chr,
      utility_fns_ls = utility_fns_ls,
      arms_extras_ls = arms_extras_ls)
  }
  return(X_MimicConfiguration)
}
update_project_2_param_names <- function(params_tb,
                                         intervention_1L_chr){
  params_tb <- params_tb %>%
    dplyr::mutate(Parameter = Parameter %>%
                    stringr::str_replace("AmbulanceOffset", "Ambulance attendance") %>%
                    stringr::str_replace("ExcludedAdjustment", " adjustment (base case)") %>%
                    stringr::str_replace("IARAdjustment", " adjustment for unmeasured IAR assessments") %>%
                    stringr::str_replace("ClinicalPsychologist", "Clinical psychologist") %>%
                    stringr::str_replace("OtherMedical", "Other medical") %>%
                    stringr::str_replace("ExternalIAR", "IAR-DST assessment") %>%
                    stringr::str_replace("ComparatorFixed", "Comparator fixed") %>%
                    stringr::str_replace("InterventionFixed", paste0(intervention_1L_chr," fixed")) %>%
                    stringr::str_replace("CostPerMin", " cost per minute") %>%
                    stringr::str_replace("Cost", " cost") %>%
                    stringr::str_replace("ProbProxy", " offset probability") %>%
                    stringr::str_replace_all("HasIAR", "Has an IAR-DST assessment - ") %>%
                    stringr::str_replace_all("InHouseIAR", "Proportion of IAR-DST assessments performed by treating service - ") %>%
                    stringr::str_replace_all("NonHelpSeekers", "Proportion of individuals who are non-help seeking - Comparator")
    ) %>%
    dplyr::mutate(Parameter = dplyr::case_when(endsWith(Parameter, "Low") ~ "One year change in K10 for untreated individuals with low distress",
                                               endsWith(Parameter, "Moderate") ~ "One year change in K10 for untreated individuals with moderate distress",
                                               endsWith(Parameter, "VeryHigh") ~ "One year change in K10 for untreated individuals with very high distress",
                                               endsWith(Parameter, "High") ~ "One year change in K10 for untreated individuals with high distress", T ~ Parameter)) %>%
    dplyr::arrange(Parameter)
  return(params_tb)
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
update_providers_tb <- function(providers_tb,
                                var_1L_chr = "practitioner_category"){
  providers_tb <- providers_tb %>%
    dplyr::mutate(!!rlang::sym(var_1L_chr) := !!rlang::sym(var_1L_chr) %>% 
                    purrr::map_chr(~{
                      value_1L_chr <-switch(.x, "Clinical Psychologist", "General Psychologist", "Social Worker", "Occupational Therapist", "Mental Health Nurse",
                                            "Aboriginal and Torres Strait Islander Health/Mental Health Worker",
                                            "Low Intensity Mental Health Worker", "General Practitioner", "Psychiatrist", "Other Medical", "Other", 
                                            "Psychosocial Support Worker", "Peer Support Worker")
                      value_1L_chr <- ifelse(is.null(value_1L_chr), NA_character_, value_1L_chr)
                      value_1L_chr
                    }))
  return(providers_tb)
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
                                 dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr, "_count")) := dplyr::case_when(as.character(!!rlang::sym(paste0(prefix_1L_chr, "_change"))) == "Start" ~ !!rlang::sym(paste0(prefix_1L_chr, "_count")) + 1, 
                                                                                                                 as.character(!!rlang::sym(paste0(prefix_1L_chr, "_change"))) == "End" & as.character(!!rlang::sym(paste0(prefix_1L_chr, "_status_previous"))) == "Waitlist" ~ !!rlang::sym(paste0(prefix_1L_chr, "_count")) + 1, 
                                                                                                                 T ~ !!rlang::sym(paste0(prefix_1L_chr, "_count"))), 
                                               !!rlang::sym(paste0(prefix_1L_chr, "_start")) := dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, "_start"))) ~ !!rlang::sym(paste0(prefix_1L_chr, "_start")), 
                                                                                                                 is.na(!!rlang::sym(paste0(prefix_1L_chr, "_start"))) & as.character(!!rlang::sym(paste0(prefix_1L_chr, "_status"))) == "Treatment" ~ purrr::map_vec(CurrentDate, 
                                                                                                                                                                                                                                                                     ~{
                                                                                                                                                                                                                                                                       sample(seq(.x - tx_duration_dtm, .x, by = "day"), 1)
                                                                                                                                                                                                                                                                       }), 
                                                                                                                 T ~ lubridate::NA_Date_), 
                                               !!rlang::sym(paste0(prefix_1L_chr, "_measurement")) := dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, "_measurement"))) ~ !!rlang::sym(paste0(prefix_1L_chr, "_measurement")), 
                                                                                                                       is.na(!!rlang::sym(paste0(prefix_1L_chr, "_measurement"))) &  !is.na(!!rlang::sym(paste0(prefix_1L_chr, "_start"))) ~ !!rlang::sym(paste0(prefix_1L_chr, "_start")) + tx_duration_dtm, 
                                                                                                                       T ~ lubridate::NA_Date_), 
                                               !!rlang::sym(paste0(prefix_1L_chr, "_fraction")) := dplyr::case_when(!is.na(!!rlang::sym(paste0(prefix_1L_chr, "_fraction"))) ~ !!rlang::sym(paste0(prefix_1L_chr, "_fraction")), 
                                                                                                                    !!rlang::sym(paste0(prefix_1L_chr, "_start")) >= StartDate & !!rlang::sym(paste0(prefix_1L_chr, "_measurement")) <= EndDate ~ 1, 
                                                                                                                    T ~ lubridate::as.period(purrr::map2_vec(EndDate, !!rlang::sym(paste0(prefix_1L_chr, "_measurement")), ~as.Date(ifelse(is.na(.y), lubridate::NA_Date_, min(.x, .y, na.rm = T)))) - CurrentDate)/tx_duration_dtm)))
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
