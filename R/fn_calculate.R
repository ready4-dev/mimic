#' Calculate Assessment of Quality of Life Eight Dimension from K10
#' @description calculate_aqol8d_from_k10() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life eight dimension from k10. The function returns an Assessment of Quality of Life Eight Dimension (a double vector of length one).
#' @param age_1L_dbl Age (a double vector of length one)
#' @param k10_1L_dbl K10 (a double vector of length one)
#' @param norway_1L_lgl Norway (a logical vector of length one), Default: FALSE
#' @param source_1L_chr Source (a character vector of length one), Default: c("10.1192/bjp.bp.113.136036")
#' @return an Assessment of Quality of Life Eight Dimension (a double vector of length one)
#' @rdname calculate_aqol8d_from_k10
#' @export 
#' @keywords internal
calculate_aqol8d_from_k10 <- function (age_1L_dbl, k10_1L_dbl, norway_1L_lgl = FALSE, source_1L_chr = c("10.1192/bjp.bp.113.136036")) 
{
    aqol8d_1L_dbl <- exp(0.204665 + ((-3.617134) * (k10_1L_dbl/100)) + 
        ((0.0537131) * as.numeric(norway_1L_lgl)))
    return(aqol8d_1L_dbl)
}
#' Calculate EQ5D from K10
#' @description calculate_eq5d_from_k10() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate eq5d from k10. The function returns EQ5D (a double vector of length one).
#' @param age_1L_dbl Age (a double vector of length one)
#' @param k10_1L_dbl K10 (a double vector of length one)
#' @param germany_1L_lgl Germany (a logical vector of length one), Default: FALSE
#' @param beta_age_1L_dbl Beta age (a double vector of length one), Default: -0.01382
#' @param beta_constant_1L_dbl Beta constant (a double vector of length one), Default: 3.5222
#' @param beta_k10_1L_dbl Beta K10 (a double vector of length one), Default: -0.06476
#' @param source_1L_chr Source (a character vector of length one), Default: c("10.1192/bjo.2018.21", "10.1192/bjp.bp.113.136036")
#' @return EQ5D (a double vector of length one)
#' @rdname calculate_eq5d_from_k10
#' @export 
#' @keywords internal
calculate_eq5d_from_k10 <- function (age_1L_dbl, k10_1L_dbl, germany_1L_lgl = FALSE, beta_age_1L_dbl = -0.01382, 
    beta_constant_1L_dbl = 3.5222, beta_k10_1L_dbl = -0.06476, 
    source_1L_chr = c("10.1192/bjo.2018.21", "10.1192/bjp.bp.113.136036")) 
{
    source_1L_chr <- match.arg(source_1L_chr)
    if (source_1L_chr == "10.1192/bjo.2018.21") {
        eq5d_1L_dbl <- exp(beta_constant_1L_dbl + beta_age_1L_dbl * 
            age_1L_dbl + beta_k10_1L_dbl * k10_1L_dbl)/(1 + exp(beta_constant_1L_dbl + 
            beta_age_1L_dbl * age_1L_dbl + beta_k10_1L_dbl * 
            k10_1L_dbl))
    }
    else {
        eq5d_1L_dbl <- 0.8644649 + ((-2.926161) * (k10_1L_dbl/100)^2) + 
            ((-0.0387056) * as.numeric(germany_1L_lgl))
    }
    return(eq5d_1L_dbl)
}
#' Calculate Estimatedesident Population for areas
#' @description calculate_erp_for_areas() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate estimatedesident population for areas. The function returns Estimatedesident Population for areas (a double vector).
#' @param raw_erp_tb Raw Estimatedesident Population (a tibble), Default: NULL
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param areas_chr Areas (a character vector)
#' @param count_1L_chr Count (a character vector of length one), Default: character(0)
#' @param name_1L_chr Name (a character vector of length one), Default: character(0)
#' @param summarise_1L_lgl Summarise (a logical vector of length one), Default: TRUE
#' @param var_1L_chr Variable (a character vector of length one), Default: 'LGA'
#' @return Estimatedesident Population for areas (a double vector)
#' @rdname calculate_erp_for_areas
#' @export 
#' @importFrom dplyr select filter group_by summarise pull
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @keywords internal
calculate_erp_for_areas <- function (raw_erp_tb = NULL, path_1L_chr = character(0), areas_chr, 
    count_1L_chr = character(0), name_1L_chr = character(0), 
    summarise_1L_lgl = TRUE, var_1L_chr = "LGA") 
{
    if (identical(count_1L_chr, character(0))) {
        count_1L_chr <- "Estimated.resident.population..Persons..no....Data.year..2023."
    }
    if (identical(name_1L_chr, character(0))) {
        name_1L_chr <- "Local.Government.Areas.2021.name"
    }
    if (is.null(raw_erp_tb)) {
        raw_erp_tb <- read.csv(path_1L_chr)
    }
    erp_for_areas_tb <- raw_erp_tb %>% dplyr::select(tidyselect::all_of(c(name_1L_chr, 
        count_1L_chr))) %>% dplyr::filter(!!rlang::sym(name_1L_chr) %in% 
        areas_chr)
    if (!summarise_1L_lgl) {
        erp_for_areas_tb <- erp_for_areas_tb %>% dplyr::group_by(!!rlang::sym(name_1L_chr))
    }
    erp_for_areas_tb <- erp_for_areas_tb %>% dplyr::summarise(`:=`(!!rlang::sym(var_1L_chr), 
        sum(!!rlang::sym(count_1L_chr))))
    erp_for_areas_dbl <- erp_for_areas_tb %>% dplyr::pull(!!rlang::sym(var_1L_chr))
    if (!summarise_1L_lgl) {
        erp_for_areas_dbl <- stats::setNames(erp_for_areas_dbl, 
            erp_for_areas_tb %>% dplyr::pull(!!rlang::sym(name_1L_chr)))
    }
    return(erp_for_areas_dbl)
}
#' Calculate offset probability proxy
#' @description calculate_offset_prob_proxy() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate offset probability proxy. The function returns Probability proxy (a double vector of length one).
#' @param area_erp_1L_dbl Area Estimatedesident Population (a double vector of length one)
#' @param exposed_1L_dbl Exposed (a double vector of length one)
#' @param rate_1L_dbl Rate (a double vector of length one)
#' @param risk_1L_dbl Risk (a double vector of length one)
#' @param denominator_1L_dbl Denominator (a double vector of length one), Default: 1e+05
#' @param time_1L_dbl Time (a double vector of length one), Default: 1
#' @return Probability proxy (a double vector of length one)
#' @rdname calculate_offset_prob_proxy
#' @export 
#' @importFrom heemod rate_to_prob
#' @keywords internal
calculate_offset_prob_proxy <- function (area_erp_1L_dbl, exposed_1L_dbl, rate_1L_dbl, risk_1L_dbl, 
    denominator_1L_dbl = 1e+05, time_1L_dbl = 1) 
{
    averted_rate_1L_dbl <- (rate_1L_dbl/risk_1L_dbl) * (1 - risk_1L_dbl)
    averted_1L_dbl <- area_erp_1L_dbl/denominator_1L_dbl * averted_rate_1L_dbl
    effect_1L_dbl <- averted_1L_dbl/exposed_1L_dbl
    if (effect_1L_dbl != 0) {
        probability_proxy_1L_dbl <- heemod::rate_to_prob(abs(effect_1L_dbl), 
            to = time_1L_dbl)
    }
    else {
        probability_proxy_1L_dbl <- 0
    }
    if (effect_1L_dbl <= 0) {
        probability_proxy_1L_dbl <- probability_proxy_1L_dbl * 
            -1
    }
    return(probability_proxy_1L_dbl)
}
#' Calculate Short Form - Six Dimension from K10
#' @description calculate_sf6d_from_k10() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate short form - six dimension from k10. The function returns a Short Form - Six Dimension (a double vector of length one).
#' @param female_1L_lgl Female (a logical vector of length one)
#' @param k10_1L_dbl K10 (a double vector of length one)
#' @param beta_female_moderate_1L_dbl Beta female moderate (a double vector of length one), Default: -0.059
#' @param beta_female_high_1L_dbl Beta female high (a double vector of length one), Default: -0.124
#' @param beta_male_moderate_1L_dbl Beta male moderate (a double vector of length one), Default: -0.055
#' @param beta_male_high_1L_dbl Beta male high (a double vector of length one), Default: -0.123
#' @param beta_constant_1L_dbl Beta constant (a double vector of length one), Default: 0.805
#' @param source_1L_chr Source (a character vector of length one), Default: c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036")
#' @return a Short Form - Six Dimension (a double vector of length one)
#' @rdname calculate_sf6d_from_k10
#' @export 
#' @keywords internal
calculate_sf6d_from_k10 <- function (female_1L_lgl, k10_1L_dbl, beta_female_moderate_1L_dbl = -0.059, 
    beta_female_high_1L_dbl = -0.124, beta_male_moderate_1L_dbl = -0.055, 
    beta_male_high_1L_dbl = -0.123, beta_constant_1L_dbl = 0.805, 
    source_1L_chr = c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036")) 
{
    source_1L_chr <- match.arg(source_1L_chr)
    if (source_1L_chr == "10.1016/j.jval.2024.12.002") {
        beta_moderate_1L_dbl <- ifelse(female_1L_lgl, beta_female_moderate_1L_dbl, 
            beta_male_moderate_1L_dbl)
        beta_high_1L_dbl <- ifelse(female_1L_lgl, beta_female_high_1L_dbl, 
            beta_male_high_1L_dbl)
        beta_adjustment_1L_dbl <- ifelse(k10_1L_dbl <= 15, 0, 
            ifelse(k10_1L_dbl >= 22, beta_high_1L_dbl, beta_moderate_1L_dbl))
        sf6d_1L_dbl <- beta_constant_1L_dbl + beta_adjustment_1L_dbl
    }
    else {
        sf6d_1L_dbl <- exp((-0.0059462) + ((-2.165542) * (k10_1L_dbl/100)) + 
            ((1.319628) * (k10_1L_dbl/100)^2))
    }
    return(sf6d_1L_dbl)
}
