library(tidyverse)


data_df = readRDS("data/export/mw_ed_project_has_hs_pop_data.rds")


library(plm)

force_balance = 
  make.pbalanced(data_df , balance.type = c("shared.individuals") , index = c("cbsa_code" , "time_var" ) ) %>%
  as.data.frame() %>%
  filter(  cbsa_code != 41940 )


detrend_function = 
  function(.x ){
    
    temp_data = 
      force_balance %>%
      filter( msa_name == .x  ) %>%
      mutate( dummy = ifelse( date > as.Date("2013-01-01") ,
                              1 ,
                              0 ) )
    
    ts_reg = lm( data = temp_data , 
                 formula = in_hs_rate_old ~ as.factor(dummy) )
      
    out_df = data.frame( msa_name = .x , 
                         post_int = ts_reg$coefficients[2] )
    
    out_df
    
  }


intercepts = map_dfr(unique(force_balance$msa_name), detrend_function )

saveRDS(intercepts  , "data/export/hs_rate_measure_shock.rds")
