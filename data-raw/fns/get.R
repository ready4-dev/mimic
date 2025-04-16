get_this_from_that <- function(this_1L_chr = "first",
                               that_ls = list(first = 5, second = 6)){
  this_1L_chr <- match.arg(this_1L_chr)
  not_1L_chr <- "This is an example function. Delete when customising this template repository."
  this_xx <- that_ls %>% purrr::pluck(this_1L_chr)
  return(this_xx)
}
