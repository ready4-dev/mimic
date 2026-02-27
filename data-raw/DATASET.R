library(ready4)
library(ready4use)
library(ready4fun)
# library(specific)
library(serious)
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Implement Microsimulations To Economically Evaluate Mental Health Services",
                                 pkg_desc_1L_chr = "Tools for developing simple microsimulations of mental health services and exporting outputs for cost-utility analyses. Designed for use with the ready4 framework (https://ready4-dev.github.io/ready4/).
                            This early development version of mimic has been made available as part of the process of testing and documenting the library.
                            If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
                                 authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton1@monash.edu", role = c("aut", "cph","cre"), comment = c(ORCID = "0000-0001-7407-9194"))),
                                 urls_chr = c("https://ready4-dev.github.io/mimic/",
                                              "https://github.com/ready4-dev/mimic",
                                              "https://ready4-dev.github.io/mimic/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("knitr","knitrBootstrap","rmarkdown"),
                                                                       #imports_chr = c(),
                                                                       depends_chr = c("ready4")
  ),
  build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
  check_type_1L_chr = "ready4",
  copyright_holders_chr = "Matthew Hamilton", # If no copyright holder is to be specified, leave as is. Otherwise update these details.
  custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("predict_comparator_pathway",
                                                                        "predict_digital_pathway",
                                                                        "predict_from_pool",
                                                                        "predict_project_2_pathway",
                                                                        "predict_with_sim"
                                                                        #,
                                                                        # all other functions that you plan to include in the main manual are named here.

                                                                        )),##
  dev_pkgs_chr = c("didgformula", "ready4show", "ready4use","scorz", "specific", "serious", "youthu", "youthvars"), # Name any development packages imported / suggested / depended on
  lifecycle_stage_1L_chr = "experimental",
  path_to_pkg_logo_1L_chr = "data-raw/logo/default.png",
  piggyback_to_1L_chr = "ready4-dev/ready4", # Modelling project GitHub organisation
  ready4_type_1L_chr = "modelling",
  zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15232854.svg)](https://doi.org/10.5281/zenodo.15232854"# 10.5281/zenodo.15232854
  )
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "Active",
                                                                slots_ls = list("x_Ready4useDyad") %>% list(), 
                                                                pt_ls = list("Ready4useDyad") %>% list(),
                                                                class_desc_chr = "Model active population (currently eligible for events).",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Utility",
                                                                slots_ls = list("names_chr", "data_ls", "mapping_ls", "transformations_ls") %>% list(), 
                                                                pt_ls = list("character", "list", "list", "list") %>% list(),
                                                                vals_ls = list(list(
                                                                  data_ls = "list()",
                                                                  mapping_ls = "list()",
                                                                  transformations_ls = "list()")),
                                                                class_desc_chr = "Utility algorithms and supporting data (instrument names, modules, mapping functions and transformation functions).",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Algorithms",
                                                                # Add events_ls or x_MimicEvents (better)
                                                                slots_ls = list("main_ls", #"comparator_fn", "intervention_fn", 
                                                                                "processing_ls",#"extra_draws_fn", "synthesis_fn", 
                                                                                "sensitivities_ls", 
                                                                                "transformations_ls",
                                                                                "x_MimicUtility") %>% list(), 
                                                                pt_ls = list("list", "list", "list", "list", "MimicUtility") %>% list(),
                                                                vals_ls = list(list(main_ls = "make_simulation_fns_ls('main')",
                                                                                    processing_ls = "make_simulation_fns_ls('processing')",
                                                                                    sensitivities_ls = "make_simulation_fns_ls('sensitivity')",
                                                                                    transformations_ls = "make_simulation_fns_ls('transformation')")),
                                                                class_desc_chr = "The core set of functions that define simulation behaviour.",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicUtility") %>% list()),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Inputs",
                                                                slots_ls = list("lookups_ls","models_ls", "x_Ready4useDyad", "y_Ready4useDyad") %>% list(), 
                                                                pt_ls = list("list", "list","Ready4useDyad", "Ready4useDyad") %>% list(),
                                                                vals_ls = list(list(
                                                                  lookups_ls = "list()",
                                                                  models_ls = "list()")),
                                                                class_desc_chr = "Model input data (lookup tables, regression models, population and parameters).",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Population",
                                                                slots_ls = list("x_MimicActive", #"x_Ready4useDyad", 
                                                                                "y_Ready4useDyad", "z_Ready4useDyad") %>% list(), 
                                                                pt_ls = list("MimicActive",#"Ready4useDyad", 
                                                                             "Ready4useDyad", "Ready4useDyad") %>% list(),
                                                                class_desc_chr = "Model dynamic population data container (specifying population subgroups who are active and eligible for the next model event,  who are temporariliy inactive but are still in the model or who have exited the model).",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicActive") %>% list()),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Repos",
                                                                slots_ls = list("batch_to_1L_chr",
                                                                                "draw_to_1L_chr", 
                                                                                "divider_1L_chr",
                                                                                "outp_data_dir_1L_chr",
                                                                                "path_to_keys_1L_chr",
                                                                                "path_to_output_1L_chr",
                                                                                "path_to_param_data_1L_chr",
                                                                                "path_to_private_1L_chr",
                                                                                "processed_dir_1L_chr",
                                                                                "r_dir_1L_chr",
                                                                                "raw_dir_1L_chr",
                                                                                "reports_dir_1L_chr",
                                                                                "x_Ready4useRepos") %>% list(),
                                                                pt_ls = list("character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "character",
                                                                             "Ready4useRepos") %>% list(),
                                                                vals_ls = list(list(batch_to_1L_chr = "'BatchedSimResults'",
                                                                                    draw_to_1L_chr = "'BatchedParamDraws'",
                                                                                    divider_1L_chr = "'\'",
                                                                                    outp_data_dir_1L_chr = "'Output'",
                                                                                    processed_dir_1L_chr = "'Processed'",
                                                                                    r_dir_1L_chr = "'R'",
                                                                                    reports_dir_1L_chr = "'Reports'")),
                                                                class_desc_chr= "Local and remote repositories for model input and output data.",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "EventsList",
                                                                slots_ls = list("events_ls", "last_1L_chr", "main_chr") %>% list(), 
                                                                pt_ls = list("list", "character", "character") %>% list(),
                                                                vals_ls = list(list(
                                                                  events_ls = "list()",
                                                                  last_1L_chr = "'WrapUp'",
                                                                  main_chr = "character(0)")),
                                                                class_desc_chr = "Data on all events included in model.",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Configuration",
                                                                slots_ls = list(
                                                                  "arms_tb",
                                                                  "drop_missing_1L_lgl",                                                                  
                                                                  "drop_suffix_1L_chr",
                                                                  "horizon_dtm", 
                                                                  "iterations_ls",  
                                                                  "modifiable_chr", 
                                                                  "prior_batches_1L_int", # DELETE
                                                                  # Add prefixes_ls
                                                                  "seed_1L_int",
                                                                  "start_dtm",  
                                                                  "tx_prefix_1L_chr",
                                                                  "x_MimicAlgorithms",
                                                                  "x_MimicEventsList",
                                                                  "x_MimicInputs") %>% list(), 
                                                                pt_ls = list(
                                                                  "tbl_df",
                                                                  "logical",
                                                                  "character",
                                                                  "Period",
                                                                  "list",  
                                                                  "character",
                                                                  "integer", 
                                                                  "integer",
                                                                  "Date",#"POSIXt", 
                                                                  "character",
                                                                  "MimicAlgorithms",
                                                                  "MimicEventsList",
                                                                  "MimicInputs"
                                                                  ) %>% list(),
                                                                vals_ls = list(list(
                                                                  arms_tb = "make_arms_tb()",
                                                                  drop_missing_1L_lgl = "FALSE",
                                                                  drop_suffix_1L_chr = "character(0)", #"NA_character_", # "character(0)"
                                                                  horizon_dtm = "lubridate::years(1)",
                                                                  iterations_ls = "make_batches(5, of_1L_int = 20)",
                                                                  modifiable_chr = "character(0)", # "NA_character_", 
                                                                  prior_batches_1L_int = "0L",
                                                                  seed_1L_int = "2001L",
                                                                  start_dtm = "Sys.Date()",
                                                                  tx_prefix_1L_chr = "'Treatment'")),
                                                                class_desc_chr= "Configuration details for a simulation run.",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicAlgorithms","MimicEventsList","MimicInputs"
                                                                                   ) %>% list()),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "Derivations",
                                                                slots_ls = list("method_1L_chr","args_env_ls","args_fixed_ls") %>% list(), 
                                                                pt_ls = list("character","list", "list") %>% list(),
                                                                vals_ls = list(list(
                                                                  args_env_ls = "list()",
                                                                  args_fixed_ls = "list()")),
                                                                class_desc_chr = "Data for deriving method arguments.",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "Arguments",
                                                                slots_ls = list("iterations_1L_lgl","derive_ls","models_ls","x_MimicDerivations") %>% list(), 
                                                                pt_ls = list("logical","list", "list", "MimicDerivations") %>% list(),
                                                                vals_ls = list(list(iterations_1L_lgl = "FALSE",
                                                                                    derive_ls = "list()",
                                                                                    models_ls = "list()")),
                                                                class_desc_chr = "Method argument data.",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicDerivations") %>% list()),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "Schedule",
                                                                slots_ls = list("assert_1L_lgl","event_1L_chr", "functions_ls", "step_dtm","update_type_1L_chr","use_1L_chr","validate_chr","x_MimicArguments") %>% list(), 
                                                                pt_ls = list("logical","character","list","Date","character","character","character",
                                                                             "MimicArguments" ## UPDATE
                                                                             ) %>% list(),
                                                                vals_ls = list(list(
                                                                  assert_1L_lgl = "FALSE",
                                                                  functions_ls = "list(invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf | x <0), schedule_fn = NULL)", #"NA_character_", # "character(0)"
                                                                  step_dtm = "lubridate::days(0)",
                                                                  update_type_1L_chr = "'split'", # "NA_character_", 
                                                                  use_1L_chr = "'Y'",
                                                                  validate_chr = "character(0)")),
                                                                class_desc_chr = "Model event scheduling data.",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicArguments") %>% list()),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "Trigger",
                                                                slots_ls = list("assert_1L_lgl", "event_1L_chr", "functions_ls","use_1L_chr","validate_chr","x_MimicArguments") %>% list(), 
                                                                pt_ls = list("logical","character","list","character","character", "MimicArguments" ## UPDATE
                                                                ) %>% list(),
                                                                vals_ls = list(list(
                                                                  assert_1L_lgl = "FALSE",
                                                                  functions_ls = "list(action_fn = identity, invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x==-Inf | x==Inf))", #"NA_character_", # "character(0)"
                                                                  use_1L_chr = "'Y'",
                                                                  validate_chr = "character(0)")),
                                                                class_desc_chr = "Model event logic data.",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicArguments") %>% list()),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE, 
                                                                name_stub_chr = "Eligible",
                                                                slots_ls = list("ineligible_1L_chr","functions_ls") %>% list(), 
                                                                pt_ls = list("character","list") %>% list(),
                                                                vals_ls = list(list(
                                                                  ineligible_1L_chr = "character(0)",
                                                                  functions_ls = "make_ineligibility_fns_ls()")),
                                                                class_desc_chr = "Model event eligibility logic data.",
                                                                parent_class_chr = "Ready4Module"),
                   ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Event",
                                                                slots_ls = list("x_MimicEligible",
                                                                                "x_MimicSchedule",
                                                                                "x_MimicTrigger") %>% list(), 
                                                                pt_ls = list("MimicEligible",
                                                                             "MimicSchedule", 
                                                                             "MimicTrigger") %>% list(),
                                                                class_desc_chr = "Model event scheduling and event logic data.",
                                                                parent_class_chr = "Ready4Module",
                                                                inc_clss_ls = list("MimicEligible","MimicSchedule", "MimicTrigger") %>% list())
                   
                   
  )
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
#
# Manual step required to address:
# C4_MimicRepos.R C4_MimicSchedule.R @include requires a value.
# This is one of two issues signaling required fixes in ready4pack
# The other is need to order classes tibble based on dependencies.
# Currently, the build workflow will fail unless dependencies are already present in R dir.
#
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
# desc_chr <- readLines("DESCRIPTION")
# ready4::write_citation_cff(z$x_ready4fun_manifest$initial_ls$pkg_desc_ls %>% append(list(Version = desc_chr[desc_chr %>% startsWith("Version")] %>% stringr::str_remove("Version: "))),
#                            citation_chr = readLines("inst/CITATION"),
#                            publisher_1L_chr = "")
# index_1L_int <- which(desc_chr=="    person(\"CopyrightHolder\", role = \"cph\")")
# if(!identical(index_1L_int, integer(0))){
#   c(desc_chr[1:(index_1L_int-2)], stringr::str_sub(desc_chr[(index_1L_int-1)], end = -2), desc_chr[(index_1L_int+1):length(desc_chr)]) %>%
#     writeLines("DESCRIPTION")
#   devtools::document()
# }
paste0(".github/workflows/", c("pkgdown.yaml", "R-CMD-check.yaml")) %>%
  purrr::walk(~{
    path_1L_chr <- .x
    matches_int <- which(readLines(path_1L_chr) %>% startsWith("    # Addresses issue with incompatibility between libcurl4-gnutls-dev and libcurl4-openssl-dev") | readLines(path_1L_chr) %>% startsWith("        # Addresses issue with incompatibility between libcurl4-gnutls-dev and libcurl4-openssl-dev"))
    if(!identical(matches_int,integer(0))){
      readLines(path_1L_chr)[- (matches_int%>%
                                  purrr::map(~.x:(.x+6)) %>% purrr::flatten_int())] %>%
        writeLines(path_1L_chr)
    }
  })

