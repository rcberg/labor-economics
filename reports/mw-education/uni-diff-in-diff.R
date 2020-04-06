library(pacman)
p_load( tidyverse ,
        lubridate ,
        estimatr ,
        future , 
        hrbrthemes 
        )

data_has_uni_df = readRDS("data/export/mw_ed_project_has_uni_pop_data.rds")

# difference in differences, basic

model_sea_uni = 
  lm_robust( 
    data = data_has_uni_df , 
    formula = in_uni_rate_pop ~ binding_local_mw_n + seatac_treat:binding_local_mw_n, 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_sea_sa %>% summary()

model_sea_pop %>% summary()


reg_no_sea_uni_df = data_has_uni_df %>% filter( cbsa_code != 42660 )

model_nosea_uni = 
  lm_robust( 
    data = reg_no_sea_uni_df , 
    formula = in_uni_rate_pop ~ binding_local_mw_n + placebo_treat:binding_local_mw_n, 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )


model_nosea_sa %>% summary()

model_nosea_pop %>% summary()