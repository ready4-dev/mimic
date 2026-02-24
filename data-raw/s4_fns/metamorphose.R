metamorphose_MimicConfiguration <- function(x,
                                            arm_1L_chr = NA_character_,
                                            batch_1L_int = integer(0),
                                            draws_tb = NULL,
                                            env_ls = list(),
                                            tx_prefix_1L_chr = character(0),
                                            Y_Ready4Module = Ready4Module(),
                                            ...){
  if(inherits(Y_Ready4Module, "MimicPopulation")){
    population_ls <- manufacture(x, arm_1L_chr = arm_1L_chr,
                                 batch_1L_int = batch_1L_int,
                                 draws_tb = draws_tb,
                                 tx_prefix_1L_chr = tx_prefix_1L_chr,
                                 type_1L_chr = "entry",
                                 what_1L_chr = c("population_ls"))
    Y_Ready4Module <- renew(Y_Ready4Module, population_ls = population_ls, type_1L_chr = "transform")
    Y_Ready4Module <- renew(Y_Ready4Module, env_ls = env_ls, type_1L_chr = "customise", X_MimicConfiguration = x)
  }
  return(Y_Ready4Module)
}