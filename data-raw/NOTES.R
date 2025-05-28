library(ready4)
library(ready4use)
library(ready4fun)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# Make Edits Here
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 # abbreviations_lup = Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>% ready4fun::renew.ready4fun_abbreviations(short_name_chr = "tx", long_name_chr = "treatment", plural_lgl = FALSE) %>% dplyr::arrange(short_name_chr),
                 # classes_bup_lup =  classes_bup_lup,
                 # classes_lup = classes_lup,
                 # object_type_lup = object_type_lup,
                 # prototype_lup = prototype_lup,
                 # seed_obj_lup_tb = seed_obj_lup_tb,
                 # seed_obj_type_lup = seed_obj_type_lup
                 treat_as_words_chr = c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr, c("icer", "onboarded")) %>% sort() %>% unique()
               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y, type_1L_chr = "prefer_gh")
