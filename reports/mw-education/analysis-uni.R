library(pacman)
p_load( tidyverse ,
        lubridate ,
        estimatr ,
        future , 
        hrbrthemes ,
        plm ,
        Synth )


data_has_uni_df = readRDS("data/export/mw_ed_project_has_uni_pop_data.rds")

# synthetic control analysis

# balancing the panel of data

force_balance = 
  make.pbalanced(data_has_uni_df , balance.type = c("shared.individuals") , index = c("cbsa_code" , "time_var" ) ) %>%
  select( time_var , 
          msa_name , 
          cbsa_code , 
          date ,
          log_uni , 
          log_uni_lag ,
          in_uni_rate_pop ,
          in_uni_rate_sa ,
          lag_uni_rate_pop , 
          lag_uni_rate_sa
          )


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

#spec_list = list(list("lag_uni_rate_sa" , 1 , "mean"))
#
#for(i in 2:230){
#  spec_list[[i]] = list("lag_uni_rate_sa" , i , "mean")
#}
#
#synth_wa_sa = dataprep(
#  foo = synth_df , 
#  unit.variable = "cbsa_code" , 
#  dependent = "in_uni_rate_sa" , 
#  time.variable = "time_var" , 
#  treatment.identifier = 42660 , 
#  controls.identifier = donor_group , 
#  special.predictors = spec_list , 
#  time.predictors.prior = c(1:168),
#  time.optimize.ssr = c(1:168),
#  unit.names.variable = "msa_name" ,
#  time.plot = c(1:168)              
#)
#
#plan("multiprocess")
#
#synth_out_sa = synth(synth_wa_sa)
#synth_tab_sa = 
#  synth.tab(
#    dataprep.res = synth_wa,
#    synth.res = synth_out)
#
#synth_tabs_sa = 
#  as.data.frame(synth_tab_sa["tab.w"]) %>%
#  rename( synth_weights = "tab.w.w.weights" , 
#          msa_name = "tab.w.unit.names",
#          cbsa_code = "tab.w.unit.numbers")
#
#synthetic_seattle_sa_df = 
#  left_join( synth_df , synth_tabs_sa %>% select(cbsa_code , synth_weights) ) %>%
#  mutate( synth_weights = replace_na(synth_weights, 0) ,
#          uni_weight = synth_weights*in_uni_rate_sa ) %>%
#  group_by( date ) %>%
#  summarise( synthetic_seattle = sum(uni_weight) ) %>%
#  cbind( actual_seattle = 
#           synth_df %>%
#           filter( cbsa_code == 42660 ) %>%
#           arrange(date) %>%
#           select( in_uni_rate_sa , date )
#  ) %>%
#  select(-actual_seattle.date) %>%
#  rename( actual_seattle = "actual_seattle.in_uni_rate_sa")
#
#saveRDS(synthetic_seattle_sa_df , "/data/export/synth_seattle_sa_uni_pop.rds")
#
#ggplot( data = synthetic_seattle_sa_df ) + 
#  geom_line( aes( x = date , y = synthetic_seattle ) , 
#             linetype = 2 ,
#             size = 1) +
#  geom_line( aes( x = date , y = actual_seattle )
#  ) + 
#  theme_ipsum_rc( axis_title_size =  15)
#
#


spec_list_sa = list(list("lag_uni_rate_pop" , 1 , "mean"))

for(i in 2:230){
  spec_list_sa[[i]] = list("lag_uni_rate_pop" , i , "mean")
}

synth_wa_pop = dataprep(
  foo = synth_df , 
  unit.variable = "cbsa_code" , 
  dependent = "in_uni_rate_pop" , 
  time.variable = "time_var" , 
  treatment.identifier = 42660 , 
  controls.identifier = donor_group , 
  special.predictors = spec_list_sa , 
  time.predictors.prior = c(1:168),
  time.optimize.ssr = c(1:168),
  unit.names.variable = "msa_name" ,
  time.plot = c(1:168)              
  )

plan("multiprocess")

synth_out_pop = synth(synth_wa_pop)
synth_tab_pop = 
  synth.tab(
    dataprep.res = synth_wa_pop,
    synth.res = synth_out_pop)

synth_tabs_pop = 
  as.data.frame(synth_tab_pop["tab.w"]) %>%
  rename( synth_weights = "tab.w.w.weights" , 
          msa_name = "tab.w.unit.names",
          cbsa_code = "tab.w.unit.numbers")

synthetic_seattle_pop_df = 
  left_join( synth_df , synth_tabs_pop %>% select(cbsa_code , synth_weights) ) %>%
  mutate( synth_weights = replace_na(synth_weights, 0) ,
          uni_weight = synth_weights*in_uni_rate_pop ) %>%
  group_by( date ) %>%
  summarise( synthetic_seattle = sum(uni_weight) ) %>%
  cbind( actual_seattle = 
           synth_df %>%
           filter( cbsa_code == 42660 ) %>%
           arrange(date) %>%
           select( in_uni_rate_pop , date )
           ) %>%
  select(-actual_seattle.date) %>%
  rename( actual_seattle = "actual_seattle.in_uni_rate_pop")

saveRDS(synthetic_seattle_pop_df , "data/export/synth_seattle_pop_uni_pop.rds")


#names( synthetic_seattle_df ) = c( "date" , "synth_sea_pop" , "real_sea_pop" )
#names( synthetic_seattle_sa_df ) = c( "date" , "synth_sea_sa" , "real_sea_sa" )

#data_df = left_join( synthetic_seattle_df , synthetic_seattle_sa_df )
#saveRDS( data_df , "D:/Economics/Projects/labor-economics/data/export/synth_results.rds")


ggplot( data = synthetic_seattle_pop_df ) + 
  geom_line( aes( x = date , y = synthetic_seattle ) , 
             linetype = 2 ,
             size = 1) +
  geom_line( aes( x = date , y = actual_seattle )
  ) + 
  theme_ipsum_rc( axis_title_size =  15)


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
