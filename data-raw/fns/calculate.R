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