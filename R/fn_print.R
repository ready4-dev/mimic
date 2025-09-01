#' Print comparisons
#' @description print_cmprsns() is a Print function that prints output to console. Specifically, this function implements an algorithm to print comparisons. The function returns Table (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: c("HTML", "PDF", "Word")
#' @param what_1L_chr What (a character vector of length one), Default: c("gtsummary", "df", "null")
#' @return Table (an output object of multiple potential types)
#' @rdname print_cmprsns
#' @export 
#' @importFrom ready4show print_in_format make_table_fns_ls make_table_fn
#' @keywords internal
print_cmprsns <- function (data_xx, output_type_1L_chr = c("HTML", "PDF", "Word"), 
    what_1L_chr = c("gtsummary", "df", "null")) 
{
    output_type_1L_chr <- match.arg(output_type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    table_xx <- data_xx %>% ready4show::print_in_format(output_type_1L_chr = output_type_1L_chr, 
        table_fns_ls = ready4show::make_table_fns_ls(html_table_fn = ready4show::make_table_fn("HTML", 
            what_1L_chr = what_1L_chr), pdf_table_fn = ready4show::make_table_fn("PDF", 
            what_1L_chr = what_1L_chr), word_table_fn = ready4show::make_table_fn("Word", 
            what_1L_chr = what_1L_chr)))
    return(table_xx)
}
#' Print errors
#' @description print_errors() is a Print function that prints output to console. Specifically, this function implements an algorithm to print errors. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param vars_chr Variables (a character vector)
#' @param assert_1L_lgl Assert (a logical vector of length one), Default: FALSE
#' @param invalid_fn Invalid (a function), Default: function(x) (is.na(x) | is.nan(x) | is.null(x) | x == -Inf | 
#'    x == Inf)
#' @return No return value, called for side effects.
#' @rdname print_errors
#' @export 
#' @importFrom dplyr filter if_any summarise across
#' @importFrom tidyselect any_of
#' @importFrom ready4 make_list_phrase
#' @importFrom assertthat assert_that
#' @keywords internal
print_errors <- function (X_Ready4useDyad, vars_chr, assert_1L_lgl = FALSE, invalid_fn = function(x) (is.na(x) | 
    is.nan(x) | is.null(x) | x == -Inf | x == Inf)) 
{
    problems_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(dplyr::if_any(tidyselect::any_of(vars_chr), 
        ~invalid_fn(.x)))
    if (nrow(problems_tb) > 0) {
        events_1L_chr <- problems_tb$CurrentEvent %>% unique() %>% 
            ready4::make_list_phrase()
        iterations_1L_int <- problems_tb$Iteration %>% unique() %>% 
            ready4::make_list_phrase()
        summary_tb <- problems_tb %>% dplyr::summarise(dplyr::across(tidyselect::any_of(vars_chr), 
            ~any(invalid_fn(.x))))
        message_1L_chr <- paste0("A total of ", nrow(problems_tb), 
            "invalid cases for the following variables", ready4::make_list_phrase(names(summary_tb)[summary_tb %>% 
                t()]), " at events ", events_1L_chr, " and for iterations ", 
            iterations_1L_int, ".")
    }
    else {
        message_1L_chr <- "Validation test passed"
    }
    if (assert_1L_lgl) {
        test_1L_lgl <- assertthat::assert_that(nrow(problems_tb) == 
            0, msg = message_1L_chr)
    }
    else {
        print(message_1L_chr)
    }
}
