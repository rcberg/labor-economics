library( tidyverse )
library( lubridate )
library( plm )


synth_df = readRDS("~/labor-economics/data/export/synth_panel_adj.rds")

# synthetic control analysis

# synth time

donor_group = 
  synth_df %>%
  filter( cbsa_code != 42660 &
          cbsa_code != 41940  ) %>%
  select( cbsa_code ) %>% 
  unique() %>%
  unlist() %>%
  as.numeric()


spec_list_sa = list(list("lag_adj_uni_rate" , 1 , "mean"))

for(i in 2:230){
  spec_list_sa[[i]] = list("lag_adj_uni_rate" , i , "mean")
}

library(Synth)

synth_wa_pop = 
  dataprep(
    foo = synth_df , 
    unit.variable = "cbsa_code" , 
    dependent = "adjusted_uni_rate" , 
    time.variable = "time_var" , 
    treatment.identifier = 42660 , 
    controls.identifier = donor_group , 
    special.predictors = spec_list_sa , 
    time.predictors.prior = c(1:168),
    time.optimize.ssr = c(1:168),
    unit.names.variable = "msa_name" ,
    time.plot = c(1:168)              
  )

library(future)
library(furrr)
plan(multiprocess)

synth_out_pop = synth(synth_wa_pop)
synth_tab_pop = 
  synth.tab(
    dataprep.res = synth_wa_pop,
    synth.res = synth_out_pop)

synth_tabs_pop = 
  as.data.frame(synth_tab_pop["tab.w"]) %>%
  rename( synth_weights = "tab.w.w.weights" , 
          msa_name = "tab.w.unit.names",
          cbsa_code = "tab.w.unit.numbers") %>%
  arrange(desc(synth_weights))

synth_counties = 
  synth_tabs_pop %>% 
  head(15) %>%
  select( -cbsa_code )

saveRDS(synth_counties , "~/labor-economics/data/export/synth_county_weights.rds")

synthetic_seattle_pop_df = 
  left_join( synth_df , select(.data = synth_tabs_pop, cbsa_code , synth_weights) ) %>%
  mutate( synth_weights = replace_na(synth_weights, 0) ,
          uni_weight = synth_weights*adjusted_uni_rate ) %>%
  group_by( date ) %>%
  summarise( synthetic_seattle = sum(uni_weight, na.rm = T) ) %>%
  cbind( actual_seattle = 
           synth_df %>%
           filter( cbsa_code == 42660 ) %>%
           arrange(date) %>%
           select( adjusted_uni_rate , date )
  ) %>%
  select(-actual_seattle.date) %>%
  rename( actual_seattle = "actual_seattle.adjusted_uni_rate")

saveRDS(synthetic_seattle_pop_df , "~/labor-economics/data/export/synth_seattle_data.rds")

synth_treatment_mspe = 
  synthetic_seattle_pop_df %>%
  mutate( sq_pred_err = (actual_seattle - synthetic_seattle)^2 ,
          post = ifelse( date >= "2014-01-01" , 1 , 0 ) ) %>%
  group_by( post ) %>%
  summarise( mean_sq_pred_err = mean(sq_pred_err) ) %>%
  mutate( rmspe = mean_sq_pred_err/dplyr::lag(mean_sq_pred_err) , 
          cbsa_code = 0 )


## synthetic control plot

ggplot( data = synthetic_seattle_pop_df ) + 
  geom_line( aes( x = date , y = actual_seattle) ,
             size = 1) + 
  geom_line( aes( x = date , y = synthetic_seattle) , 
             linetype = 2 , 
             size = 1 ) + 
  geom_vline( xintercept = as.Date("2014-01-01") , color = 'red' ) + 
  theme(panel.background = element_rect(fill = 'white') , 
        panel.grid = element_line( color = 'grey' ))

ggplot( data = synthetic_seattle_pop_df ) + 
  geom_line( aes( x = date , y = actual_seattle-synthetic_seattle) ,
             size = 1 ) + 
  geom_vline( xintercept = as.Date("2014-01-01") , color = 'red' , linetype = 2) + 
  geom_hline( yintercept = 0 , color = 'darkgrey' ) + 
  theme(panel.background = element_rect(fill = 'white'))

### permutation test

control_df =  
  synth_df %>% 
  filter( cbsa_code %in% donor_group )

#matrix for riverside CA doesnt invert
usable_donor_list = setdiff(donor_group, 40140)

permutation_fn = 
  function(i , controls = control_df , control_list = donor_group ){
    
    control_data = as.data.frame(controls) 
    
    controls_list_i = 
      setdiff( control_list , i )
    
    synth_dataset_i = 
      Synth::dataprep(
        foo = synth_df , 
        unit.variable = "cbsa_code" , 
        dependent = "adjusted_uni_rate" , 
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
              uni_weight = synth_weights*adjusted_uni_rate ) %>%
      group_by( date ) %>%
      summarise( synth_est = sum(uni_weight)
      ) %>%
      cbind( actual = 
               controls %>% 
               filter( cbsa_code == i ) %>%
               arrange( date ) %>%
               select(adjusted_uni_rate , date) 
      ) %>%
      # gotta do some cleaning-up
      select(-c("actual.date") ) %>%
      rename( actual = "actual.adjusted_uni_rate"
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
plan(multiprocess)

permutations = 
  future_map( usable_donor_list , permutation_fn , .progress = T)

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
  synthetic_seattle_pop_df %>%
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

saveRDS( pvalue_df , "data/export/uni_synth_inference_df.rds")

ggplot( data = pvalue_df , aes( x = rmspe , y = stat(count) ) ) + 
  geom_histogram( bins = 35 , fill = "#F8766C" , color = 'black' )  +
  labs(x = "RMSPE" , y = "Number of MSAs")+ 
  scale_y_continuous( breaks = c(0,3,6,9,12)) +
  theme( panel.background = element_rect(fill = 'white') , 
         panel.grid = element_line( color = 'darkgrey' , linetype = 3))

actual_synth_series_df = 
  synthetic_seattle_pop_df %>% 
  rename( "synth_est" = "synthetic_seattle" , 
          "actual" = "actual_seattle" ) %>%
  mutate( cbsa_code = 42660 ) %>%
  bind_rows( permutation_values_df ) %>%
  mutate( treated = if_else( cbsa_code == 42660, 1, 0) ) %>%
  left_join( unique( synth_df[,c("cbsa_code","msa_name")]) )

saveRDS( actual_synth_series_df , "data/export/actual_synthetic_uni_rate_panel.rds")

ggplot( ) + 
  geom_line(data = permutation_values_df , 
            aes( x = date , y = actual - synth_est , group = cbsa_code ), 
            alpha = 0.4, color = 'darkgrey' ) + 
  geom_line( data = synthetic_seattle_pop_df , 
             aes(x = date , y = actual_seattle - synthetic_seattle ),
             color = '#00BFC4',
             size = 1 ) + 
  geom_vline( xintercept = as.Date("2014-01-01") , 
              color = 'red', 
              linetype = 2) + 
  labs(x = "Date" , y = "Actual-less-synthetic enrollment share") + 
  theme( panel.background = element_rect(fill = 'white') , 
         panel.grid = element_line( color = 'darkgrey' , linetype = 3))
