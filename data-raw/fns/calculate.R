calculate_aqol8d_from_k10 <- function(age_1L_dbl,
                                      k10_1L_dbl,
                                      norway_1L_lgl = FALSE,
                                      source_1L_chr = c("10.1192/bjp.bp.113.136036")){
  aqol8d_1L_dbl <- exp(0.204665 + ((-3.617134) * (k10_1L_dbl/100)) + ((0.0537131)*as.numeric(norway_1L_lgl)))
  return(aqol8d_1L_dbl)
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
