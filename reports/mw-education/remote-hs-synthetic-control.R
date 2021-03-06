library( tidyverse )
        #lubridate ,
        #estimatr ,
        #future , 
        #Synth )


data_has_hs_df = readRDS("data/export/mw_ed_project_has_hs_pop_data.rds")
measure_shock_adj = readRDS("data/export/hs_rate_measure_shock.rds")

# synthetic control analysis

# balancing the panel of data

library( plm )

force_balance = 
  make.pbalanced(data_has_hs_df , 
                 balance.type = c("shared.individuals") , 
                 index = c("cbsa_code" , "time_var" ) ) %>%
  select( time_var , 
          msa_name , 
          cbsa_code , 
          date ,
          log_hs ,
          log_hs_lag , 
          in_hs_rate , 
          in_hs_rate_old ,
          lag_hs_rate ,
          lag_hs_rate_old ) %>%
  left_join( measure_shock_adj ) %>%
  mutate(dummy = ifelse(date < "2013-01-01" , 1 , 0 ),
         adjusted_hs_rate = in_hs_rate_old + dummy*post_int ) %>%
  group_by( cbsa_code ) %>%
  mutate( lag_adj_hs_rate = dplyr::lag(adjusted_hs_rate , order_by = time_var )) 


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

spec_list_sa = list(list("lag_adj_hs_rate" , 1 , "mean"))

for(i in 2:230){
  spec_list_sa[[i]] = list("lag_adj_hs_rate" , i , "mean")
}

library(Synth)
synth_wa = dataprep(
  foo = synth_df , 
  unit.variable = "cbsa_code" , 
  dependent = "adjusted_hs_rate" , 
  time.variable = "time_var" , 
  treatment.identifier = 42660 , 
  controls.identifier = donor_group , 
  special.predictors = spec_list_sa , 
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
          hs_weight = synth_weights*adjusted_hs_rate ) %>%
  group_by( date ) %>%
  summarise( synthetic_seattle = sum(hs_weight , na.rm = T) ) %>%
  cbind( actual_seattle = 
           synth_df %>%
           filter( cbsa_code == 42660 ) %>%
           arrange(date) %>%
           select( adjusted_hs_rate , date )
  ) %>%
  select(-actual_seattle.date) %>%
  rename( actual_seattle = "actual_seattle.adjusted_hs_rate")

saveRDS( synthetic_seattle_df , "data/export/synth_hs_results.rds")

ggplot( data = synthetic_seattle_df ) + 
  geom_line( aes( x = date , y = synthetic_seattle ) , 
             linetype = 2 ,
             size = 1) +
  geom_line( aes( x = date , y = actual_seattle )
  ) +
  theme(panel.background = element_rect( fill = 'white' ) , 
        panel.grid =element_line( color = 'darkgrey', linetype = 2))

ggplot( data = synthetic_seattle_df , aes( x = date , y = actual_seattle - synthetic_seattle )) + 
  geom_line( size = 1) +
  geom_hline( yintercept = 0 , color = 'darkgrey' ) + 
  geom_vline( xintercept = as.Date("2014-01-01") , color = 'red' , linetype = 2) +
  theme(panel.background = element_rect( fill = 'white' ) )


synth_treatment_mspe = 
  synthetic_seattle_df %>%
  mutate( sq_pred_err = (actual_seattle - synthetic_seattle)^2 ,
          post = ifelse( date >= "2014-01-01" , 1 , 0 ) ) %>%
  group_by( post ) %>%
  summarise( mean_sq_pred_err = mean(sq_pred_err) ) %>%
  mutate( rmspe = mean_sq_pred_err/dplyr::lag(mean_sq_pred_err) , 
          cbsa_code = 42660 )

## permutation test 

control_df =  
  synth_df %>% 
  filter( cbsa_code %in% donor_group )

##matrix for riverside CA doesnt invert
#usable_donor_list = setdiff(donor_group, 40140)

permutation_fn = 
  function(i , controls = control_df , control_list = donor_group ){
    
    control_data = as.data.frame(controls) 
    
    controls_list_i = 
      setdiff( control_list , i )
    
    synth_dataset_i = 
      Synth::dataprep(
        foo = synth_df , 
        unit.variable = "cbsa_code" , 
        dependent = "adjusted_hs_rate" , 
        time.variable = "time_var" , 
        treatment.identifier = i , 
        controls.identifier = controls_list_i , 
        special.predictors = spec_list_sa , 
        time.predictors.prior = c(1:168),
        time.optimize.ssr = c(1:168),
        unit.names.variable = "msa_name" ,
        time.plot = c(1:168)              
      )
    
    synth_out_i = synth(synth_dataset_i , Sigf.ipop=3)
    
    synth_tab_i = synth.tab(
      dataprep.res = synth_dataset_i,
      synth.res = synth_out_i)
    
    synthetic_i  = 
      as.data.frame(synth_tab_i[["tab.w"]]) %>%
      rename( synth_weights = w.weights , 
              msa_name = unit.names,
              cbsa_code = unit.numbers)
    
    synthetic_i_data_df = 
      left_join( controls , synthetic_i ) %>%
      mutate( synth_weights = replace_na(synth_weights, 0) ,
              hs_weight = synth_weights*adjusted_hs_rate ) %>%
      group_by( date ) %>%
      summarise( synth_est = sum(hs_weight)
      ) %>%
      cbind( actual = 
               controls %>% 
               filter( cbsa_code == i ) %>%
               arrange( date ) %>%
               select(adjusted_hs_rate , date) 
      ) %>%
      # gotta do some cleaning-up
      select(-c("actual.date") ) %>%
      rename( actual = "actual.adjusted_hs_rate"
      )
    
    synth_treatment_mspe = 
      synthetic_i_data_df %>%
      mutate( sq_pred_err = (actual - synth_est)^2 ,
              post = ifelse( date >= "2014-01-01" , 1 , 0 ) ) %>%
      group_by( post ) %>%
      summarise( mean_sq_pred_err = mean(sq_pred_err) ) %>%
      mutate( cbsa_code = i)
    
    list( synthetic_i_data_df ,
          synth_treatment_mspe
    )
  }

library(future)
library(furrr)
plan(multiprocess)

permutations = 
  future_map( donor_group , permutation_fn , .progress = T)

permutation_inference_lst = list()
permutation_values_lst = list()

for(.i in 1:length(permutations)){
  
  permutation_values_lst[[.i]] = permutations[[.i]][1][[1]]
  permutation_inference_lst[[.i]] = permutations[[.i]][2][[1]]
  
}

permutation_inference_df = bind_rows( permutation_inference_lst , .id = "group" )
permutation_values_df = bind_rows( permutation_values_lst , .id = "group") %>%
  left_join( unique(select(.data = permutation_inference_df, group, cbsa_code )) ) %>%
  select( -group)

permutation_inference_df = permutation_inference_df %>% select(-group)

control_rmspe_df = 
  permutation_inference_df %>%
  group_by( cbsa_code ) %>%
  mutate( rmspe = 
            mean_sq_pred_err / dplyr::lag( mean_sq_pred_err , order_by = post ) ) %>% 
  na.omit() %>%
  select(cbsa_code , rmspe ) %>%
  mutate( treated = 0 ) %>%
  left_join( synth_df %>%
               filter( time_var == 0 ) %>%
               select(cbsa_code , msa_name) )

### loading treatment 

treatment_rmspe_df = 
  synthetic_seattle_df %>%
  mutate( sq_pred_err = (actual_seattle - synthetic_seattle)^2 ,
          post = ifelse( date >= "2014-01-01" , 1 , 0 ) ) %>%
  group_by( post ) %>%
  summarise( mean_sq_pred_err = mean(sq_pred_err) ) %>%
  mutate( rmspe = mean_sq_pred_err/dplyr::lag(mean_sq_pred_err) , 
          cbsa_code = 42660 ) %>%
  na.omit() %>%
  select( cbsa_code , rmspe ) %>%
  mutate( treated = 1 ) %>%
  left_join( synth_df %>%
               filter( time_var == 0 ) %>%
               select(cbsa_code , msa_name) )


### rmspe 

pvalue_df = 
  bind_rows( treatment_rmspe_df , control_rmspe_df ) %>% 
  mutate( p_value = 1 - rank(rmspe)/(length(rmspe)+1) ) 

saveRDS( pvalue_df , "data/export/hs_synth_inference_df.rds")

ggplot( data = pvalue_df , aes( x = rmspe , y = stat(count) ) ) + 
  geom_histogram( bins = 30 , fill = "#F8766C" , color = 'black' )  +
  geom_vline( xintercept = 3.1478 , color = 'red' , linetype = 2) +
  labs(x = "RMSPE" , y = "Number of MSAs")+ 
  scale_y_continuous( breaks = c(0,3,6,9,12)) +
  theme( panel.background = element_rect(fill = 'white') , 
         panel.grid = element_line( color = 'darkgrey' , linetype = 3))

actual_synth_series_df = 
  synthetic_seattle_df %>% 
  rename( "synth_est" = "synthetic_seattle" , 
          "actual" = "actual_seattle" ) %>%
  mutate( cbsa_code = 42660 ) %>%
  bind_rows( permutation_values_df ) %>%
  mutate( treated = if_else( cbsa_code == 42660, 1, 0) ) %>%
  left_join( unique( synth_df[,c("cbsa_code","msa_name")]) )

saveRDS( actual_synth_series_df , "data/export/actual_synthetic_hs_rate_panel.rds")

ggplot( ) + 
  geom_line(data = permutation_values_df , 
            aes( x = date , y = actual - synth_est , group = cbsa_code ), 
            alpha = 0.4, color = 'darkgrey' ) + 
  geom_line( data = synthetic_seattle_df , 
             aes(x = date , y = actual_seattle - synthetic_seattle ),
             color = '#00BFC4',
             size = 1 ) + 
  geom_vline( xintercept = as.Date("2014-01-01") , 
              color = 'red', 
              linetype = 2) + 
  labs(x = "Date" , y = "Actual-less-synthetic enrollment share") + 
  theme( panel.background = element_rect(fill = 'white') , 
         panel.grid = element_line( color = 'darkgrey' , linetype = 3))
