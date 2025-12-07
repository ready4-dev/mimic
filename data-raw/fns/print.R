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