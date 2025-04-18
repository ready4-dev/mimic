update_current_date <- function(X_Ready4useDyad){
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                               X_Ready4useDyad@ds_tb %>% dplyr::mutate(CurrentDate = ScheduledFor))
  return(X_Ready4useDyad)
}
update_current_event <- function(X_Ready4useDyad){
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(CurrentEvent = NextEvent))
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
update_population_classes <- function(X_Ready4useDyad,
                                      tfmn_ls = NULL){
  if(!is.null(tfmn_ls)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 purrr::reduce(1:length(tfmn_ls),
                                               .init = X_Ready4useDyad@ds_tb,
                                               ~ {
                                                 fn <- tfmn_ls[[.y]]
                                                 .x %>%  dplyr::mutate(!!rlang::sym(names(tfmn_ls)[.y]) := fn(!!rlang::sym(names(tfmn_ls)[.y])))
                                                 })) 
  }
  return(X_Ready4useDyad)
}
update_predictions_ds <- function(Y_Ready4useDyad,
                                  adjustment_1L_dbl = 0,
                                  var_1L_chr = character(0),
                                  do_int = 1:5,
                                  follow_up_1L_int = 12L,
                                  maintain_for_1L_int = 0L,
                                  suffixes_chr = c("_YR1","_YR1_S1","_YR1_S2"),
                                  tfmn_1L_chr = "NTF",
                                  utility_1L_chr = c("AQoL6D","CHU9D"),
                                  with_1L_chr = "_sim_mean"){
  utility_1L_chr <- match.arg(utility_1L_chr)
  if(identical(var_1L_chr, character(0))){
    var_1L_chr <- paste0(utility_1L_chr,"_",follow_up_1L_int,"_Weeks")
  }
  if(utility_1L_chr == "CHU9D"){
    class_fn <- youthvars::youthvars_chu9d_adolaus
    min_1L_dbl <- -0.2118
  }
  if(utility_1L_chr == "AQoL6D"){
    class_fn <- youthvars::youthvars_aqol6d_adol
    min_1L_dbl <- 0.03
  }
  if(1 %in% do_int){
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% 
      dplyr::mutate(!!rlang::sym(var_1L_chr) := !!rlang::sym(paste0(var_1L_chr, with_1L_chr)))
  }
  if(2 %in% do_int){
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(var_1L_chr) := !!rlang::sym(var_1L_chr) %>% purrr::map_dbl(~max(min(.x,1), min_1L_dbl)))#
  }
  if(3 %in% do_int | 4 %in% do_int | 6 %in% do_int){
    qaly_vars_chr <- paste0(paste0(utility_1L_chr,"_QALYs"), c("",suffixes_chr))
    if(!identical(follow_up_1L_int, integer(0))){
      yrs_1L_dbl <- lubridate::weeks(follow_up_1L_int)/lubridate::years(1)
    }else{
      yrs_1L_dbl <- numeric(0)
    }
  }
  if(3 %in% do_int){
    Y_Ready4useDyad@ds_tb <- Y_Ready4useDyad@ds_tb %>% 
      dplyr::mutate(!!rlang::sym(paste0(utility_1L_chr, "_change")) := (!!rlang::sym(var_1L_chr) - !!rlang::sym(utility_1L_chr)) %>% as.double) %>%
      dplyr::mutate(!!rlang::sym(qaly_vars_chr[1]) := (((!!rlang::sym(var_1L_chr) + !!rlang::sym(utility_1L_chr))/2) %>% as.double()) * yrs_1L_dbl) 
  }
  if(4 %in% do_int | 6 %in% do_int | 7 %in% do_int){
    maintain_yrs_1L_dbl <- lubridate::weeks(maintain_for_1L_int)/lubridate::years(1)
    if(!identical(follow_up_1L_int, integer(0))){
      multiplier_1L_dbl <- 1 + maintain_yrs_1L_dbl/yrs_1L_dbl
      adjusted_yrs_1L_dbl <- yrs_1L_dbl + maintain_yrs_1L_dbl
      start_var_1L_chr <- utility_1L_chr
      end_var_1L_chr <- var_1L_chr
      Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb",
                                   Y_Ready4useDyad@ds_tb %>%
                                     dplyr::mutate(!!rlang::sym(paste0(utility_1L_chr,"_years")) := yrs_1L_dbl,
                                                   !!rlang::sym(paste0(utility_1L_chr,"_multiplier")) := multiplier_1L_dbl,
                                                   !!rlang::sym(paste0(utility_1L_chr,"_adjusted")) := adjusted_yrs_1L_dbl))
    }else{
      start_var_1L_chr <- paste0(utility_1L_chr, "_previous")
      end_var_1L_chr <- utility_1L_chr
      Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb",
                                   Y_Ready4useDyad@ds_tb %>%
                                     dplyr::mutate(!!rlang::sym(paste0(utility_1L_chr,"_days")) := lubridate::days(!!rlang::sym(paste0(utility_1L_chr,"_date")) - !!rlang::sym(paste0(utility_1L_chr,"_date_previous"))),
                                                   !!rlang::sym(paste0(utility_1L_chr,"_years")) := (!!rlang::sym(paste0(utility_1L_chr,"_days"))/lubridate::years(1)),
                                                   !!rlang::sym(paste0(utility_1L_chr,"_multiplier")) := 1 + maintain_yrs_1L_dbl/(!!rlang::sym(paste0(utility_1L_chr,"_years"))),
                                                   !!rlang::sym(paste0(utility_1L_chr,"_adjusted")) := !!rlang::sym(paste0(utility_1L_chr,"_years")) + maintain_yrs_1L_dbl))
    }
  }
  if(4 %in% do_int){
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                                 Y_Ready4useDyad@ds_tb %>%
                                   dplyr::mutate(!!rlang::sym(end_var_1L_chr) := !!rlang::sym(end_var_1L_chr) %>% purrr::map_dbl(~max(min(.x,1), min_1L_dbl)) %>% class_fn()) %>%
                                   dplyr::mutate(!!rlang::sym(qaly_vars_chr[2]) := !!rlang::sym(qaly_vars_chr[1])*!!rlang::sym(paste0(utility_1L_chr,"_multiplier")) + (((!!rlang::sym(end_var_1L_chr) + !!rlang::sym(start_var_1L_chr))/2) %>% as.double()) * (1-!!rlang::sym(paste0(start_var_1L_chr,"_adjusted")))) %>%
                                   dplyr::mutate(!!rlang::sym(qaly_vars_chr[3]) := !!rlang::sym(qaly_vars_chr[1])*!!rlang::sym(paste0(utility_1L_chr,"_multiplier")) + (((!!rlang::sym(start_var_1L_chr ))) %>% as.double()) * (1-!!rlang::sym(paste0(start_var_1L_chr,"_adjusted")))) %>%
                                   dplyr::mutate(!!rlang::sym(qaly_vars_chr[4]) := !!rlang::sym(qaly_vars_chr[1])*!!rlang::sym(paste0(utility_1L_chr,"_multiplier")) + (((!!rlang::sym(end_var_1L_chr))) %>% as.double()) * (1-!!rlang::sym(paste0(start_var_1L_chr,"_adjusted")))) %>%
                                   dplyr::select(-tidyselect::any_of(paste0(utility_1L_chr, c("_days", "_years", "_multiplier", "_adjusted")))) %>%
                                   dplyr::arrange(UID))
  }
  if(5 %in% do_int){
    Y_Ready4useDyad <- Y_Ready4useDyad %>% renew(what_1L_chr = "dictionary", type_1L_chr = "update")
  }
  if(6 %in% do_int){
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                                 Y_Ready4useDyad@ds_tb %>%
                                   dplyr::mutate(!!rlang::sym(qaly_vars_chr[1]) := !!rlang::sym(qaly_vars_chr[1]) + (((!!rlang::sym(start_var_1L_chr) + !!rlang::sym(end_var_1L_chr))/2) %>% as.double()) * !!rlang::sym(paste0(utility_1L_chr,"_years"))) 
                                 # %>% 
                                 #   dplyr::select(-tidyselect::any_of(paste0(utility_1L_chr, c("_days", "_years", "_multiplier", "_adjusted")))) 
    )
  }
  if(7 %in% do_int){
    Y_Ready4useDyad <- add_QALYs_sensitivities(Y_Ready4useDyad, suffixes_chr = suffixes_chr, utility_1L_chr = utility_1L_chr)
  }
  if(8 %in% do_int){
    Y_Ready4useDyad <-  renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% 
                                    dplyr::select(-tidyselect::any_of(paste0(utility_1L_chr, c("_days", "_years", "_multiplier", "_adjusted")))))
  }
  return(Y_Ready4useDyad)
}
update_previous <- function(X_Ready4useDyad,
                            modifiable_chr = character(0),
                            pattern_1L_chr = "{col}_previous"){
  if(!identical(modifiable_chr, character(0))){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 X_Ready4useDyad@ds_tb %>%
                                   dplyr::mutate(dplyr::across(modifiable_chr, ~.x, .names = pattern_1L_chr)))
  }
  return(X_Ready4useDyad)
}
update_qalys <- function(X_Ready4useDyad,
                         add_sensitivity_1L_lgl = FALSE,
                         adjustment_1L_dbl = 0,
                         follow_up_1L_int = integer(0),
                         maintain_for_1L_int = 0,
                         tidy_1L_lgl = FALSE,
                         utilities_chr = c("AQoL6D", "CHU9D")){
  sensitivity_1L_int <- integer(0)
  if(add_sensitivity_1L_lgl){
    sensitivity_1L_int <- 7
  }
  X_Ready4useDyad <- utilities_chr %>% purrr::reduce(.init = X_Ready4useDyad,
                                                     ~ {
                                                       Y_Ready4useDyad <- .x
                                                       if(!paste0(.y, "_QALYs") %in% names(Y_Ready4useDyad@ds_tb)){
                                                         Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", Y_Ready4useDyad@ds_tb %>% dplyr::mutate(!!rlang::sym(paste0(.y, "_QALYs")) := 0))
                                                       }
                                                       Y_Ready4useDyad <- update_previous(Y_Ready4useDyad, modifiable_chr = paste0(.y, "_QALYs"))
                                                       # Add sensitivity calculations
                                                       Y_Ready4useDyad %>%
                                                         update_predictions_ds(adjustment_1L_dbl = adjustment_1L_dbl, do_int = c(6,sensitivity_1L_int,8), follow_up_1L_int = follow_up_1L_int, utility_1L_chr = .y, maintain_for_1L_int = maintain_for_1L_int) 
                                                     })
  if(tidy_1L_lgl){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::select(- tidyselect::any_of(paste0(utilities_chr, "_QALYS")))) 
  }
  return(X_Ready4useDyad)
}
update_scheduled_date <-  function(X_Ready4useDyad,
                                   type_1L_chr = c("End")){
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr=="End"){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(c("CurrentDate", "ScheduledFor"), ~ EndDate)))
  }
  return(X_Ready4useDyad)
}
update_tx_start_end <- function(X_Ready4useDyad,
                                tx_duration_dtm = lubridate::weeks(12)){
  if(!"treatment_change" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_change = NA_character_))
  }
  if(!"treatment_count" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_count = 0))
  }
  if(!"treatment_start" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_start = lubridate::NA_Date_))
  }
  if(!"treatment_measurement" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_measurement = lubridate::NA_Date_))
  }
  if(!"treatment_fraction" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_fraction = NA_real_))
  }
  if(!"treatment_status_previous" %in% names(X_Ready4useDyad@ds_tb)){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", X_Ready4useDyad@ds_tb %>% dplyr::mutate(treatment_status_previous = NA_character_))
  }
  X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                               X_Ready4useDyad@ds_tb %>%
                                 dplyr::mutate(treatment_count = dplyr::case_when(as.character(treatment_change) =="Start" ~ treatment_count +1,
                                                                                  as.character(treatment_change) =="End" &  as.character(treatment_status_previous) == "Waitlist" ~ treatment_count +1,
                                                                                  T ~ treatment_count),
                                               treatment_start = dplyr::case_when(!is.na(treatment_start) ~ treatment_start,
                                                                                  is.na(treatment_start) & as.character(treatment_status)=="Treatment" ~ purrr::map_vec(CurrentDate,~{sample(seq(.x - tx_duration_dtm, .x, by = "day"), 1)}),
                                                                                  T ~ lubridate::NA_Date_),
                                               treatment_measurement = dplyr::case_when(!is.na(treatment_measurement) ~ treatment_measurement,
                                                                                is.na(treatment_measurement) & !is.na(treatment_start) ~ treatment_start + tx_duration_dtm,
                                                                                T ~ lubridate::NA_Date_),
                                               treatment_fraction = dplyr::case_when(!is.na(treatment_fraction) ~ treatment_fraction,
                                                                                     treatment_start >= StartDate & treatment_measurement <= EndDate ~ 1,
                                                                                     T ~ lubridate::as.period(purrr::map2_vec(EndDate,treatment_measurement, ~ as.Date(ifelse(is.na(.y),lubridate::NA_Date_,min(.x,.y, na.rm=T)))) - CurrentDate) / tx_duration_dtm)
                                 ))
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
