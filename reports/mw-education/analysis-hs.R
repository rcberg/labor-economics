library(pacman)
p_load( tidyverse ,
        lubridate ,
        estimatr ,
        future , 
        hrbrthemes ,
        plm ,
        Synth )


plan("multiprocess")

data_has_hs_df = readRDS("data/export/mw_ed_project_has_hs_pop_data.rds")

# synthetic control analysis

# balancing the panel of data

force_balance = 
  make.pbalanced(data_has_hs_df , balance.type = c("shared.individuals") , index = c("cbsa_code" , "time_var" ) ) %>%
  select( time_var , 
          msa_name , 
          cbsa_code , 
          date ,
          log_hs ,
          log_hs_lag , 
          in_hs_rate , 
          lag_hs_rate )


synth_df = as.data.frame(force_balance)

# synth time

donor_group = 
  synth_df %>%
  filter( cbsa_code != 42660 &
            cbsa_code != 41940 ) %>%
  select( cbsa_code ) %>% 
  unique() %>%
  unlist() %>%
  as.numeric()

spec_list = list(list("lag_hs_rate" , 1 , "mean"))

for(i in 2:230){
  spec_list[[i]] = list("lag_hs_rate" , i , "mean")
}

synth_wa = dataprep(
  foo = synth_df , 
  unit.variable = "cbsa_code" , 
  dependent = "in_hs_rate" , 
  time.variable = "time_var" , 
  treatment.identifier = 42660 , 
  controls.identifier = donor_group , 
  special.predictors = spec_list , 
  time.predictors.prior = c(1:168),
  time.optimize.ssr = c(1:168),
  unit.names.variable = "msa_name" ,
  time.plot = c(1:168)              
)

synth_out = synth(synth_wa)
synth_tab = 
  synth.tab(
    dataprep.res = synth_wa,
    synth.res = synth_out)

synth_tabs = 
  as.data.frame(synth_tab["tab.w"]) %>%
  rename( synth_weights = "tab.w.w.weights" , 
          msa_name = "tab.w.unit.names",
          cbsa_code = "tab.w.unit.numbers")

synthetic_seattle_df = 
  left_join( synth_df , synth_tabs %>% select(cbsa_code , synth_weights) ) %>%
  mutate( synth_weights = replace_na(synth_weights, 0) ,
          hs_weight = synth_weights*in_hs_rate ) %>%
  group_by( date ) %>%
  summarise( synthetic_seattle = sum(hs_weight) ) %>%
  cbind( actual_seattle = 
           synth_df %>%
           filter( cbsa_code == 42660 ) %>%
           arrange(date) %>%
           select( in_hs_rate , date )
  ) %>%
  select(-actual_seattle.date) %>%
  rename( actual_seattle = "actual_seattle.in_hs_rate")

saveRDS( synthetic_seattle_df , "data/export/synth_hs_results.rds")

ggplot( data = synthetic_seattle_df ) + 
  geom_line( aes( x = date , y = synthetic_seattle ) , 
             linetype = 2 ,
             size = 1) +
  geom_line( aes( x = date , y = actual_seattle )
  ) + 
  theme_ipsum_rc( axis_title_size =  15)


# difference in differences, basic

model_inhs = 
  lm_robust( 
    data = data_has_hs_df , 
    formula = in_hs_rate ~ binding_local_mw_n , 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_inhs_int = 
  lm_robust( 
    data = data_has_hs_df , 
    formula = in_hs_rate ~ binding_local_mw_n + seatac_treat:binding_local_mw_n , 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_loghs = 
  lm_robust( 
    data = data_has_hs_df , 
    formula = log_hs ~ binding_local_mw_n , 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_loghs_int = 
  lm_robust( 
    data = data_has_hs_df , 
    formula = log_hs ~ binding_local_mw_n + + seatac_treat:binding_local_mw_n, 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )


model_inhs %>% summary()

model_inhs_int %>% summary()

model_loghs %>% summary()

model_loghs_int %>% summary()

## removing seattle for robustness

reg_no_sea_df = data_has_hs_df %>% filter( cbsa_code != 42660 )

model_inhs_nosea = 
  lm_robust( 
    data = reg_no_sea_df , 
    formula = in_hs_rate ~ binding_local_mw_n , 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_inhs_int_nosea = 
  lm_robust( 
    data = reg_no_sea_df , 
    formula = in_hs_rate ~ binding_local_mw_n + placebo_treat:binding_local_mw_n , 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_loghs_nosea = 
  lm_robust( 
    data = reg_no_sea_df , 
    formula = log_hs ~ binding_local_mw_n , 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_loghs_int_nosea = 
  lm_robust( 
    data = reg_no_sea_df , 
    formula = log_hs ~ binding_local_mw_n + + placebo_treat:binding_local_mw_n, 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_inhs_nosea %>% summary()

model_inhs_int_nosea %>% summary()

model_loghs_nosea %>% summary()

model_loghs_int_nosea %>% summary()
