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
