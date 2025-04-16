library(ready4)
library(ready4use)
library(ready4fun)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# Make Edits Here
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 # abbreviations_lup = abbreviations_lup,
                 # classes_bup_lup =  classes_bup_lup,
                 # classes_lup = classes_lup,
                 # object_type_lup = object_type_lup,
                 # prototype_lup = prototype_lup,
                 # seed_obj_lup_tb = seed_obj_lup_tb,
                 # seed_obj_type_lup = seed_obj_type_lup
                 # treat_as_words_chr = treat_as_words_chr
               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")
