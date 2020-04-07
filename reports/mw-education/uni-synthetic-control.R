library(pacman)
p_load( tidyverse ,
        lubridate ,
        future ,
        furrr ,
        hrbrthemes ,
        plm ,
        Synth ,
        tictoc  )


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

plan(multiprocess)

tic()

synth_out_pop = synth(synth_wa_pop)
synth_tab_pop = 
  synth.tab(
    dataprep.res = synth_wa_pop,
    synth.res = synth_out_pop)

toc()

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
  geom_line( aes( x = date , y = synthetic_seattle ) , 
             linetype = 2 ,
             size = 1) +
  geom_line( aes( x = date , y = actual_seattle )
  ) + 
  theme_ipsum_rc( axis_title_size =  15)


### permutation test


control_df =  
  synth_df %>% 
  filter( cbsa_code %in% donor_group )


permutation_fn = function(i , controls = control_df , control_list = donor_group ){
  
  control_data = as.data.frame(controls) 
  
  controls_list_i = 
    setdiff( control_list , i )
  
  synth_dataset_i = Synth::dataprep(
    foo = synth_df , 
    unit.variable = "cbsa_code" , 
    dependent = "in_uni_rate_pop" , 
    time.variable = "time_var" , 
    treatment.identifier = i , 
    controls.identifier = controls_list_i , 
    special.predictors = spec_list_sa , 
    time.predictors.prior = c(1:168),
    time.optimize.ssr = c(1:168),
    unit.names.variable = "msa_name" ,
    time.plot = c(1:168)              
  )
  
  synth_out_i = synth(synth_dataset_i)
  
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
            uni_weight = synth_weights*in_uni_rate_pop ) %>%
    group_by( date ) %>%
    summarise( synth_est = sum(uni_weight)
    ) %>%
    cbind( actual = 
             controls %>% 
             filter( cbsa_code == i ) %>%
             arrange( date ) %>%
             select(in_uni_rate_pop , date) 
    ) %>%
    # gotta do some cleaning-up
    select(-c("actual.date") ) %>%
    rename( actual = "actual.in_uni_rate_pop"
    )
  
  synth_treatment_mspe = synthetic_i_data_df %>%
    mutate( sq_pred_err = (actual - synth_est)^2 ,
            post = ifelse( date >= "2014-01-01" , 1 , 0 ) ) %>%
    group_by( post ) %>%
    summarise( mean_sq_pred_err = mean(sq_pred_err) ) %>%
    mutate( rmspe = mean_sq_pred_err/dplyr::lag(mean_sq_pred_err) ,
            cbsa_code = i)
  
  synth_treatment_mspe
  
}

plan(multiprocess)

synth_rmspe = 
  future_map_dfr( donor_group , permutation_fn , .progress = T ) %>%
  bind_rows( synth_treatment_rmspe ) %>%
  mutate( 
    # using length of the data as denom because this data includes BOTH treated units so it's (length +1) -1 for other treated unit
    p_value = 1-(rank(rmspe)/length(rmspe))  , 
    treated = ifelse( cbsa_code == 42660 ,
                      1 , 
                      0 ) )
