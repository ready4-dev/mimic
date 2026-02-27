library(ready4)
library(ready4use)
library(ready4fun)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# Make Edits Here

# Y@b_Ready4useIngest@objects_ls$prototype_lup -> prototype_lup
# prototype_lup <- prototype_lup %>% 
#   tibble::add_case(type_chr = "Date",
#                    val_chr = "lubridate::date()",
#                    pt_ns_chr = "lubrudate",
#                    fn_to_call_chr = "date",
#                    default_val_chr = "Sys.Date()",
#                    old_class_lgl = FALSE) %>%
#   dplyr::arrange(tolower(type_chr))
# Y@b_Ready4useIngest@objects_ls$classes_lup -> classes_lup
# classes_lup <- classes_lup %>%
#   dplyr::bind_rows(prototype_lup %>% tibble::as_tibble() %>%
#                      dplyr::filter(type_chr %in% setdiff(prototype_lup$type_chr, classes_lup$type_chr))) %>%
#   dplyr::arrange(tolower(type_chr))
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 # abbreviations_lup = Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>%
                 #   ready4fun::renew.ready4fun_abbreviations(short_name_chr = c("sa"), long_name_chr = c("sensitivity analysis"), plural_lgl = FALSE) %>%
                 #   dplyr::distinct() %>%
                 #   dplyr::arrange(short_name_chr)# ,
                 # classes_bup_lup =  classes_bup_lup,
                 # classes_lup = classes_lup,
                 # object_type_lup = object_type_lup,
                 # prototype_lup = prototype_lup
                   # Y@b_Ready4useIngest@objects_ls$prototype_lup %>%
                   # # dplyr::filter(type_chr != "period") %>%
                   # tibble::add_case(type_chr = "function",
                   #                  val_chr = "function() NULL",
                   #                  pt_ns_chr = "base",
                   #                  fn_to_call_chr = "",
                   #                  default_val_chr = "function() NULL",
                   #                  old_class_lgl = FALSE) %>%
                   # dplyr::distinct() %>%
                   # dplyr::arrange(tolower(pt_ns_chr), tolower(pt_ns_chr))
                 # ,
                 # seed_obj_lup_tb = seed_obj_lup_tb,
                 # seed_obj_type_lup = seed_obj_type_lup
                 treat_as_words_chr = c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr, c("customisation", "customise")) %>% sort() %>% unique()
               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y, type_1L_chr = "prefer_gh")
