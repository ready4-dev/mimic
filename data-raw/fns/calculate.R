calculate_aqol8d_from_k10 <- function(age_1L_dbl,
                                      k10_1L_dbl,
                                      norway_1L_lgl = FALSE,
                                      source_1L_chr = c("10.1192/bjp.bp.113.136036")){
  aqol8d_1L_dbl <- exp(0.204665 + ((-3.617134) * (k10_1L_dbl/100)) + ((0.0537131)*as.numeric(norway_1L_lgl)))
  return(aqol8d_1L_dbl)
}
calculate_erp_for_areas <- function(raw_erp_tb = NULL,
                                    path_1L_chr = character(0),
                                    areas_chr, 
                                    count_1L_chr = character(0),
                                    name_1L_chr = character(0),
                                    summarise_1L_lgl = TRUE,
                                    var_1L_chr = "LGA"){ 
  
  if(identical(count_1L_chr, character(0))){
    count_1L_chr <- "Estimated.resident.population..Persons..no....Data.year..2023."
  }
  if(identical(name_1L_chr, character(0))){
    name_1L_chr <- "Local.Government.Areas.2021.name"
  }
  if(is.null(raw_erp_tb)){
    raw_erp_tb <- read.csv(path_1L_chr)
  }
  erp_for_areas_tb <- raw_erp_tb %>% 
    dplyr::select(tidyselect::all_of(c(name_1L_chr, count_1L_chr))) %>% 
    dplyr::filter(!!rlang::sym(name_1L_chr) %in% areas_chr) 
  if(!summarise_1L_lgl){
    erp_for_areas_tb <- erp_for_areas_tb  %>% dplyr::group_by(!!rlang::sym(name_1L_chr))
  }
  erp_for_areas_tb <- erp_for_areas_tb %>%
    dplyr::summarise(!!rlang::sym(var_1L_chr) := sum(!!rlang::sym(count_1L_chr))) 
  
  erp_for_areas_dbl <- erp_for_areas_tb %>%
    dplyr::pull(!!rlang::sym(var_1L_chr))
  if(!summarise_1L_lgl){
    erp_for_areas_dbl <- stats::setNames(erp_for_areas_dbl, erp_for_areas_tb %>% dplyr::pull(!!rlang::sym(name_1L_chr)))
  }
  return(erp_for_areas_dbl)
}
calculate_eq5d_from_k10 <- function(age_1L_dbl,
                                    k10_1L_dbl,
                                    germany_1L_lgl = FALSE,
                                    beta_age_1L_dbl = -0.01382,
                                    beta_constant_1L_dbl = 3.52220,
                                    beta_k10_1L_dbl = -0.06476,
                                    source_1L_chr = c("10.1192/bjo.2018.21", 
                                                      "10.1192/bjp.bp.113.136036")){
  source_1L_chr <- match.arg(source_1L_chr)
  if(source_1L_chr == "10.1192/bjo.2018.21"){
    eq5d_1L_dbl <- exp(beta_constant_1L_dbl + beta_age_1L_dbl * age_1L_dbl + beta_k10_1L_dbl  * k10_1L_dbl)/(1 + exp(beta_constant_1L_dbl + beta_age_1L_dbl * age_1L_dbl + beta_k10_1L_dbl  * k10_1L_dbl))
  }else{
    eq5d_1L_dbl <- 0.8644649 + ((-2.926161)  * (k10_1L_dbl/100)^2) + ((-0.0387056)*as.numeric(germany_1L_lgl))
  }
  return(eq5d_1L_dbl)
}
calculate_offset_prob_proxy <- function(area_erp_1L_dbl,
                                        exposed_1L_dbl,
                                        rate_1L_dbl,
                                        risk_1L_dbl,
                                        denominator_1L_dbl = 100000,
                                        time_1L_dbl = 1){
  averted_rate_1L_dbl <- (rate_1L_dbl/risk_1L_dbl)*(1-risk_1L_dbl) 
  averted_1L_dbl <- area_erp_1L_dbl/denominator_1L_dbl * averted_rate_1L_dbl 
  
  effect_1L_dbl <- averted_1L_dbl/exposed_1L_dbl 
  if(effect_1L_dbl!=0){
    probability_proxy_1L_dbl <- heemod::rate_to_prob(abs(effect_1L_dbl), to = time_1L_dbl)
  }else{
    probability_proxy_1L_dbl <- 0
  }
  if(effect_1L_dbl<=0){
    probability_proxy_1L_dbl <- probability_proxy_1L_dbl*-1
  }
  return(probability_proxy_1L_dbl)
}
calculate_sf6d_from_k10 <- function(female_1L_lgl,
                                    k10_1L_dbl,
                                    beta_female_moderate_1L_dbl = -0.059,
                                    beta_female_high_1L_dbl = -0.124,
                                    beta_male_moderate_1L_dbl = -0.055,
                                    beta_male_high_1L_dbl = -0.123,
                                    beta_constant_1L_dbl = 0.805,
                                    source_1L_chr = c("10.1016/j.jval.2024.12.002",
                                                      "10.1192/bjp.bp.113.136036")){
  source_1L_chr <- match.arg(source_1L_chr)
  if(source_1L_chr == "10.1016/j.jval.2024.12.002"){
    beta_moderate_1L_dbl <- ifelse(female_1L_lgl, beta_female_moderate_1L_dbl, beta_male_moderate_1L_dbl)
    beta_high_1L_dbl <- ifelse(female_1L_lgl, beta_female_high_1L_dbl, beta_male_high_1L_dbl)
    beta_adjustment_1L_dbl <- ifelse(k10_1L_dbl<=15, 0, ifelse(k10_1L_dbl>=22, beta_high_1L_dbl, beta_moderate_1L_dbl))
    sf6d_1L_dbl <- beta_constant_1L_dbl + beta_adjustment_1L_dbl
  }else{
    sf6d_1L_dbl <- exp((-0.0059462) + ((-2.165542)  * (k10_1L_dbl/100)) + ((1.319628)  * (k10_1L_dbl/100)^2) )
  }
  return(sf6d_1L_dbl)
}
