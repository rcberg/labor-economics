library(tidyverse)
library(hrbrthemes)


## difference in differences estimator inference

did_inference_df = 
  readRDS( "data/export/uni_did_inference_df.rds")


seattle_est = 
  did_inference_df %>%
  filter( treated == 1 ) %>%
  select( standardized , 
          unstandardized ) 

seattle_ustd = seattle_est$unstandardized
seattle_std = seattle_est$standardized

ggplot( data = did_inference_df ) + 
  geom_histogram( aes( x = unstandardized ) , 
                  binwidth = 0.001 , 
                  fill = "#00BFC4",
                  color = 'black',
                  alpha = 0.7) + 
  geom_vline( xintercept = seattle_ustd ,
              size = 1 ,
              color = 'red' ) + 
  labs( title = "Difference-in-differences placebo distribution" ,
        x = "Unstandardized estimate" , 
        y = "Count" ,
        caption = "(Red line indicates Seattle coefficient)") +
  theme_ipsum_rc( axis_title_size = 12)

ggplot( data = did_inference_df ) + 
  geom_histogram( aes( x = standardized ) , 
                  binwidth = 0.1 , 
                  fill = "#C77CFF" , 
                  color = 'black',
                  alpha = 0.7
  ) + 
  geom_vline( xintercept = seattle_std  , 
              size = 1 , 
              color = 'red' ) + 
  labs( title = "Difference-in-differences placebo distribution" ,
        x = "Standardized estimate" ,
        y = "Count" ,
        caption = "(Red line indicates Seattle coefficient)") +
  theme_ipsum_rc( axis_title_size = 12)


## synthetic control estimate inference


synth_inference_df = 
  readRDS( "data/export/uni_synth_inference_df.rds" )

seattle_synth = 
  synth_inference_df %>%
  filter( treated == 1 ) %>%
  select( rmspe ) 

seattle_rmspe = 
  seattle_synth$rmspe

ggplot( data = synth_inference_df ) + 
  geom_histogram( aes( x = rmspe ) , 
                  binwidth =  1 , 
                  fill = "#F8766D" , 
                  color = 'black',
                  alpha = 0.7
  ) + 
  geom_vline( xintercept = seattle_rmspe  , 
              size = 1 , 
              color = 'red' ) + 
  labs( title = "Synthetic control RMSPE distribution",
        x = "RMSPE estimate" ,
        y = "Count" ,
        caption = "(Red line indicates Seattle RMSPE)") +
  theme_ipsum_rc( axis_title_size = 12)

## synth group plot

data_df = readRDS("data/export/mw_ed_project_has_uni_pop_data.rds")

data_weights = readRDS("data/export/synth_county_weights.rds")
measure_shock_adj = readRDS("data/export/uni_rate_measure_shock.rds")

data_plot_df = 
  data_df %>% 
  filter( msa_name %in% data_weights$msa_name ) %>%
  left_join( data_weights) %>%
  left_join( measure_shock_adj ) %>%
  mutate(dummy = ifelse(date < "2013-01-01" , 1 , 0 ),
         adjusted_uni = in_uni_rate_pop + dummy*post_int)

synth_plot = 
  ggplot( data = data_plot_df ,
          aes( x = date , y = adjusted_uni ) ) +
  geom_line( size = 1 ) + 
  geom_vline( xintercept = as.Date("2013-01-01") , size = 1 , linetype = 2 , color  = 'red' ) +
  facet_wrap( ~msa_name , nrow = 3 ) +
  labs( title = "Synthetic Seattle university enrollment" , 
        subtitle = "by CBSA" , 
        x = "" , 
        y = "" ) +
  theme_ipsum_tw(axis_title_size = 15) 

synth_plot
