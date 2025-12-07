print_cmprsns <- function (data_xx, 
                           output_type_1L_chr = c("HTML", "PDF", "Word"), 
                           what_1L_chr = c("gtsummary", "df", "null"),
                           html_table_fn = NULL,
                           pdf_table_fn = NULL, 
                           word_table_fn = NULL){
  output_type_1L_chr <- match.arg(output_type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(is.null(html_table_fn)){
    html_table_fn <- ready4show::make_table_fn("HTML", 
                                               what_1L_chr = what_1L_chr)
  }
  if(is.null(pdf_table_fn)){
    pdf_table_fn <- ready4show::make_table_fn("PDF", 
                                              what_1L_chr = what_1L_chr)
  }
  if(is.null(word_table_fn)){
    word_table_fn <- ready4show::make_table_fn("Word", 
                                               what_1L_chr = what_1L_chr)
  }
  table_xx <- data_xx %>% ready4show::print_in_format(output_type_1L_chr = output_type_1L_chr, 
                                                      table_fns_ls = ready4show::make_table_fns_ls(html_table_fn = html_table_fn,
                                                                                                   pdf_table_fn = pdf_table_fn, 
                                                                                                   word_table_fn = word_table_fn))
  return(table_xx)
}
print_errors <- function(X_Ready4useDyad,
                         vars_chr, 
                         assert_1L_lgl = FALSE,
                         invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf) ){
  problems_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(dplyr::if_any(tidyselect::any_of(vars_chr), ~ invalid_fn(.x)))
  if(nrow(problems_tb) > 0){
    events_1L_chr <- problems_tb$CurrentEvent %>% unique() %>% ready4::make_list_phrase()
    iterations_1L_int <- problems_tb$Iteration %>% unique() %>% ready4::make_list_phrase()
    summary_tb <- problems_tb %>% dplyr::summarise(dplyr::across(tidyselect::any_of(vars_chr), ~ any(invalid_fn(.x))))
    message_1L_chr <-   paste0("A total of ", nrow(problems_tb), "invalid cases for the following variables", ready4::make_list_phrase( names(summary_tb)[summary_tb %>% t()]), 
                               " at events ", events_1L_chr, " and for iterations ", iterations_1L_int, ".")
    
  }else{
    message_1L_chr <- "Validation test passed"
  }
  if(assert_1L_lgl){
    test_1L_lgl <- assertthat::assert_that(nrow(problems_tb)==0, msg = message_1L_chr)
  }else{
    print(message_1L_chr)
  }
}