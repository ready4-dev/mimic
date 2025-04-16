#' Get this from that
#' @description get_this_from_that() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get this from that. The function returns This (an output object of multiple potential types).
#' @param this_1L_chr This (a character vector of length one), Default: 'first'
#' @param that_ls That (a list), Default: list(first = 5, second = 6)
#' @return This (an output object of multiple potential types)
#' @rdname get_this_from_that
#' @export 
#' @importFrom purrr pluck
get_this_from_that <- function (this_1L_chr = "first", that_ls = list(first = 5, second = 6)) 
{
    this_1L_chr <- match.arg(this_1L_chr)
    not_1L_chr <- "This is an example function. Delete when customising this template repository."
    this_xx <- that_ls %>% purrr::pluck(this_1L_chr)
    return(this_xx)
}
