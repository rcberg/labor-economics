library(tidyverse)
data_has_hs_df = readRDS("data/export/mw_ed_project_has_hs_pop_data.rds")
measure_shock_adj = readRDS("data/export/hs_rate_measure_shock.rds")

# difference in differences, basic

library(plm)
library(lubridate)

force_balance = 
  make.pbalanced(data_has_hs_df , balance.type = c("shared.individuals") , index = c("cbsa_code" , "time_var" ) ) %>%
  as.data.frame() %>%
  filter(  cbsa_code != 41940 ) %>%
  left_join( measure_shock_adj ) %>%
  mutate(dummy = ifelse(date < "2013-01-01" , 1 , 0 ),
         adjusted_hs_rate = in_hs_rate_old + dummy*post_int ) %>%
  group_by( cbsa_code ) %>%
  mutate( lag_adj_hs_rate = dplyr::lag(adjusted_hs_rate , order_by = time_var )) 

nosea_hs_df = 
  force_balance %>%
  filter( cbsa_code != 42660 )

library(estimatr)
model_sea_hs = 
  lm_robust( 
    data = force_balance , 
    formula = adjusted_hs_rate ~ binding_local_mw_n + seatac_treat:binding_local_mw_n, 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )

model_sea_hs_std = 
  lm_robust( 
    data = force_balance , 
    formula = scale(adjusted_hs_rate) ~ scale(binding_local_mw_n) + seatac_treat:scale(binding_local_mw_n), 
    fixed_effects = ~ cbsa_code + date , 
    clusters = cbsa_code 
  )


model_sea_hs %>% summary()

model_sea_hs_std %>% summary()

# placebo treatment distribution 

placebo_est = list()
placebo_std_est = list()

init = 1

for(i in levels(as.factor(nosea_hs_df$cbsa_code)) ){
  nosea_hs_df_i = 
    nosea_hs_df %>% 
    mutate( placebo_treat = ifelse( cbsa_code == i &
                                      date >= "2014-01-01" ,
                                    1 , 
                                    0 ) )
  
  
  model_nosea_hs_i = 
    lm_robust( 
      data = nosea_hs_df_i , 
      formula = adjusted_hs_rate ~ binding_local_mw_n + placebo_treat:binding_local_mw_n, 
      fixed_effects = ~ cbsa_code + date , 
      clusters = cbsa_code 
    )
  
  model_nosea_hs_std_i = 
    lm_robust( 
      data = nosea_hs_df_i , 
      formula = scale(adjusted_hs_rate) ~ scale(binding_local_mw_n) + placebo_treat:scale(binding_local_mw_n), 
      fixed_effects = ~ cbsa_code + date , 
      clusters = cbsa_code 
    )
  
  
  placebo_est[[init]] = model_nosea_hs_i$coefficients[2]
  placebo_std_est[[init]] = model_nosea_hs_std_i$coefficients[2]
  
  init = init + 1
  
}


std_est = 
  c(std_est = placebo_std_est) %>% 
  bind_rows() %>% 
  pivot_longer(1:length(placebo_std_est) ,
               names_to = "estimate" , 
               values_to = "standardized" )

did_est = 
  c(est = placebo_est) %>% 
  bind_rows() %>% 
  pivot_longer(1:length(placebo_est) , 
               names_to = "estimate" , 
               values_to = "unstandardized" )

placebo_distribution = 
  bind_cols( std_est , did_est ) %>% 
  select( standardized , unstandardized ) %>%
  mutate( treated = 0 )


seattle_std = model_sea_hs_std$coefficients[2]

seattle_ustd = model_sea_hs$coefficients[2]


seattle_coefs = 
  data.frame(
    standardized = seattle_std , 
    unstandardized = seattle_ustd , 
    row.names = NULL , 
    treated = 1 
  )

p_value_df = 
  bind_rows( seattle_coefs , placebo_distribution ) %>%
  mutate( stand_rank = 1 + length(standardized) - rank(abs(standardized)) , 
          unstand_rank = 1 + length(unstandardized) - rank(abs(unstandardized)) , 
          p_stand = stand_rank/(1 + length(standardized)) , 
          p_unstand = unstand_rank/(1 + length(unstandardized))
  )

saveRDS(p_value_df, "data/export/hs_did_inference_df.rds")

ggplot( data = p_value_df ) + 
  geom_histogram( aes( x = unstandardized ) , 
                  bins = 50 , 
                  fill = "#00BFC4",
                  color = 'black',
                  alpha = 0.7) + 
  geom_vline( xintercept = seattle_ustd ,
              size = 1 ,
              color = 'red' ) + 
  labs( x = "Unstandardized Coefficients" , 
        y = "Count" ) 


ggplot( data = p_value_df ) + 
  geom_histogram( aes( x = standardized ) , 
                  bins= 50, 
                  fill = "#C77CFF" , 
                  color = 'black',
                  alpha = 0.7
  ) + 
  geom_vline( xintercept = seattle_std  , 
              size = 1 , 
              color = 'red' ) + 
  labs( x = "Standardized Coefficients" ,
        y = "Count" ) 
