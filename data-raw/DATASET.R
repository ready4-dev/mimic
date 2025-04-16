library(ready4)
library(ready4use)
library(ready4fun)
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Do Something Related To Mental Health Economic Modelling",
                                 pkg_desc_1L_chr = "A description of what this model module library does. Mention that it is developed with the ready4 framework (https://ready4-dev.github.io/ready4/).
                            If a development version, you may wish to note that the software has been made available as part of the process of testing and documenting the library.
                            You can also direct users with any questions about the library to the maintainer's email address (supply email here).",
                                 authors_prsn = c(utils::person(given = "Author1Given",family = "Author1Family",email = "name@example.com", role = c("aut", "cre"), comment = c(ORCID = "0000-0000-0000-0000")),
                                                  utils::person("CopyrightHolder", role = c("cph") # If no copyright holder is to be specified, leave as is. Otherwise update these details.
                                                                )
                                 ),
                                 urls_chr = c("https://ready4-dev.github.io/template/",
                                              "https://github.com/ready4-dev/template",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("knitr","knitrBootstrap","rmarkdown")#,
                                                                       #imports_chr = c(),
                                                                       #depends_chr = c()
  ),
  build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
  check_type_1L_chr = "ready4",
  copyright_holders_chr = "CopyrightHolder", # If no copyright holder is to be specified, leave as is. Otherwise update these details.
  custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("get_this_from_that"
                                                                        #,
                                                                        # all other functions that you plan to include in the main manual are named here.

                                                                        )),##
  dev_pkgs_chr = NA_character_, # Name any development packages imported / suggested / depended on
  lifecycle_stage_1L_chr = "experimental",
  path_to_pkg_logo_1L_chr = "data-raw/logo/default.png",
  piggyback_to_1L_chr = "ready4-dev/ready4", # Modelling project GitHub organisation
  ready4_type_1L_chr = "modelling",
  zenodo_badge_1L_chr = character(0)# Once Zenodo DOI is set up, replace with link in the format of:
    # "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXXX"#
  )
y <- ready4class::ready4class_constructor()
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
#ready4::write_extra_pkgs_to_actions(path_to_dir_1L_chr = ".github/workflows", consent_1L_chr = "Y")
ready4::write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
write_to_tidy_pkg(z$x_ready4fun_manifest,
                  build_vignettes_1L_lgl = TRUE,
                  clean_license_1L_lgl = TRUE,
                  consent_1L_chr = "Y",
                  examples_chr = character(0),
                  project_1L_chr = "Framework",
                  suggest_chr = "pkgload")
# readLines("_pkgdown.yml") %>%
#   stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
#   writeLines(con = "_pkgdown.yml")
write_citation_fl(z$x_ready4fun_manifest)
desc_chr <- readLines("DESCRIPTION")
# ready4::write_citation_cff(z$x_ready4fun_manifest$initial_ls$pkg_desc_ls %>% append(list(Version = desc_chr[desc_chr %>% startsWith("Version")] %>% stringr::str_remove("Version: "))),
#                            citation_chr = readLines("inst/CITATION"),
#                            publisher_1L_chr = "")
index_1L_int <- which(desc_chr=="    person(\"CopyrightHolder\", role = \"cph\")")
if(!identical(index_1L_int, integer(0))){
  c(desc_chr[1:(index_1L_int-2)], stringr::str_sub(desc_chr[(index_1L_int-1)], end = -2), desc_chr[(index_1L_int+1):length(desc_chr)]) %>%
    writeLines("DESCRIPTION")
  devtools::document()
}

