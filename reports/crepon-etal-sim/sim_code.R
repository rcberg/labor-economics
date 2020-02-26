library(tidyverse)

######################
# parameters

v = 200
u = 1000
eff = 1.8
p_T = 0.5
p_C = 0
alpha = 0.5

prob_f = function(x){
  rbinom(n = u ,
         size = 1 , 
         prob = x )
  }

######################
# treatment economy

## "first stage" kind of process were treatment is assigned

indiv_treat_df = tibble( 
  u_t = prob_f(p_T) ,
  u_c = (1-u_t) , 
  effort = eff*u_t + u_c
)

## aggregates calculated from "first stage"

agg_treat_df = indiv_treat_df %>%
  summarise( n_treat = sum(u_t) , 
             u_e = sum(effort) ) %>%
  mutate( theta = v / u_e ,
          p_y = theta^alpha ,
          exit_T = eff*p_y , 
          exit_C = p_y , 
          exit_C_untreated = (v/u)^alpha )

## now that we have aggregates, generate job matches based on (aggregate) probability of matching

treat_df = indiv_treat_df %>% 
  mutate( y = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C) ,
          y_counter = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C_untreated)
  )

## compute all of the outomes

treat_outcomes = tibble( treated =
                           treat_df %>%
                           filter( u_t == 1 ) %>%
                           summarise( sum(y) ) %>% 
                           as.numeric() ,
                         treated_rate = treated / agg_treat_df$n_treat ,
                         untreated = treat_df %>%
                           filter( u_t == 0 ) %>%
                           summarise( sum(y) ) %>% 
                           as.numeric() ,
                         untreated_counter = treat_df %>%
                           filter( u_t == 0 ) %>%
                           summarise( sum(y_counter) ) %>% 
                           as.numeric() ,
                         untreated_rate = untreated / (u - agg_treat_df$n_treat ) ,
                         untreated_counter_rate = untreated_counter / (u - agg_treat_df$n_treat )
)

## treatment effects

treatment_effects = tibble( biased = (treat_outcomes$treated_rate - treat_outcomes$untreated_rate) ,
                            unbiased = (treat_outcomes$treated_rate - treat_outcomes$untreated_counter_rate) 
)

##############################################

# functions

##############################################

treat_prob_fn = function( x ,
                          v = 200 ,
                          u = 1000 ,
                          eff = 1.8 ,
                          p_C = 0 ,
                          alpha = 0.5 ){
  set.seed(53217) 
  # treatment economy
  
  ## "first stage" kind of process were treatment is assigned
  p_T = x
  
  indiv_treat_df = tibble( 
    u_t = prob_f(p_T) ,
    u_c = (1-u_t) , 
    effort = eff*u_t + u_c
  )
  
  ## aggregates calculated from "first stage"
  
  agg_treat_df = indiv_treat_df %>%
    summarise( n_treat = sum(u_t) , 
               u_e = sum(effort) ) %>%
    mutate( theta = v / u_e ,
            p_y = theta^alpha ,
            exit_T = eff*p_y , 
            exit_C = p_y , 
            exit_C_untreated = (v/u)^alpha )
  
  ## now that we have aggregates, generate job matches based on (aggregate) probability of matching
  
  treat_df = indiv_treat_df %>% 
    mutate( y = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C) ,
            y_counter = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C_untreated)
    )
  
  ## compute all of the outomes
  
  treat_outcomes = tibble( treated =
                             treat_df %>%
                             filter( u_t == 1 ) %>%
                             summarise( sum(y) ) %>% 
                             as.numeric() ,
                           treated_rate = treated / agg_treat_df$n_treat ,
                           untreated = treat_df %>%
                             filter( u_t == 0 ) %>%
                             summarise( sum(y) ) %>% 
                             as.numeric() ,
                           untreated_counter = treat_df %>%
                             filter( u_t == 0 ) %>%
                             summarise( sum(y_counter) ) %>% 
                             as.numeric() ,
                           untreated_rate = untreated / (u - agg_treat_df$n_treat ) ,
                           untreated_counter_rate = untreated_counter / (u - agg_treat_df$n_treat )
  )
  
  ## treatment effects
  
  treatment_effects = tibble( biased = (treat_outcomes$treated_rate - treat_outcomes$untreated_rate) ,
                              unbiased = (treat_outcomes$treated_rate - treat_outcomes$untreated_counter_rate) ,
                              prob = p_T
  )
  treatment_effects
}

varying_prob = map_df(seq(0.01, 0.99, 0.01) , 
                      treat_prob_fn )

ggplot( data = varying_prob ) + 
  geom_line( aes(x = prob , y = biased ) , color = 'red' ) + 
  geom_line(aes(x = prob , y = unbiased) )


treat_eff_fn = function( x ,
                          v = 100 ,
                          u = 1000 ,
                          p_T = 0.3 ,
                          p_C = 0 ,
                          alpha = 0.5 ){
  set.seed(53217) 
  # treatment economy
  
  ## "first stage" kind of process were treatment is assigned
  eff = x
  
  indiv_treat_df = tibble( 
    u_t = prob_f(p_T) ,
    u_c = (1-u_t) , 
    effort = eff*u_t + u_c
  )
  
  ## aggregates calculated from "first stage"
  
  agg_treat_df = indiv_treat_df %>%
    summarise( n_treat = sum(u_t) , 
               u_e = sum(effort) ) %>%
    mutate( theta = v / u_e ,
            p_y = theta^alpha ,
            exit_T = eff*p_y , 
            exit_C = p_y , 
            exit_C_untreated = (v/u)^alpha )
  
  ## now that we have aggregates, generate job matches based on (aggregate) probability of matching
  
  treat_df = indiv_treat_df %>% 
    mutate( y = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C) ,
            y_counter = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C_untreated)
    )
  
  ## compute all of the outomes
  
  treat_outcomes = tibble( treated =
                             treat_df %>%
                             filter( u_t == 1 ) %>%
                             summarise( sum(y) ) %>% 
                             as.numeric() ,
                           treated_rate = treated / agg_treat_df$n_treat ,
                           untreated = treat_df %>%
                             filter( u_t == 0 ) %>%
                             summarise( sum(y) ) %>% 
                             as.numeric() ,
                           untreated_counter = treat_df %>%
                             filter( u_t == 0 ) %>%
                             summarise( sum(y_counter) ) %>% 
                             as.numeric() ,
                           untreated_rate = untreated / (u - agg_treat_df$n_treat ) ,
                           untreated_counter_rate = untreated_counter / (u - agg_treat_df$n_treat )
  )
  
  ## treatment effects
  
  treatment_effects = tibble( biased = (treat_outcomes$treated_rate - treat_outcomes$untreated_rate) ,
                              unbiased = (treat_outcomes$treated_rate - treat_outcomes$untreated_counter_rate) ,
                              effort = eff
  )
  treatment_effects
}

varying_eff = map_df(seq(1.1, 3, 0.1) , 
                      treat_eff_fn )

ggplot( data = varying_eff ) + 
  geom_line( aes(x = effort , y = biased ) , color = 'red' ) + 
  geom_line(aes(x = effort , y = unbiased))

treat_alpha_fn = function( x ,
                         v = 100 ,
                         u = 1000 ,
                         p_T = 0.3 ,
                         p_C = 0 ,
                         eff = 1.5 ){
  set.seed(53217) 
  # treatment economy
  
  ## "first stage" kind of process were treatment is assigned
  alpha = x
  
  indiv_treat_df = tibble( 
    u_t = prob_f(p_T) ,
    u_c = (1-u_t) , 
    effort = eff*u_t + u_c
  )
  
  ## aggregates calculated from "first stage"
  
  agg_treat_df = indiv_treat_df %>%
    summarise( n_treat = sum(u_t) , 
               u_e = sum(effort) ) %>%
    mutate( theta = v / u_e ,
            p_y = theta^alpha ,
            exit_T = eff*p_y , 
            exit_C = p_y , 
            exit_C_untreated = (v/u)^alpha )
  
  ## now that we have aggregates, generate job matches based on (aggregate) probability of matching
  
  treat_df = indiv_treat_df %>% 
    mutate( y = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C) ,
            y_counter = prob_f(x = u_t*agg_treat_df$exit_T + u_c*agg_treat_df$exit_C_untreated)
    )
  
  ## compute all of the outomes
  
  treat_outcomes = tibble( treated =
                             treat_df %>%
                             filter( u_t == 1 ) %>%
                             summarise( sum(y) ) %>% 
                             as.numeric() ,
                           treated_rate = treated / agg_treat_df$n_treat ,
                           untreated = treat_df %>%
                             filter( u_t == 0 ) %>%
                             summarise( sum(y) ) %>% 
                             as.numeric() ,
                           untreated_counter = treat_df %>%
                             filter( u_t == 0 ) %>%
                             summarise( sum(y_counter) ) %>% 
                             as.numeric() ,
                           untreated_rate = untreated / (u - agg_treat_df$n_treat ) ,
                           untreated_counter_rate = untreated_counter / (u - agg_treat_df$n_treat )
  )
  
  ## treatment effects
  
  treatment_effects = tibble( biased = (treat_outcomes$treated_rate - treat_outcomes$untreated_rate) ,
                              unbiased = (treat_outcomes$treated_rate - treat_outcomes$untreated_counter_rate) ,
                              alpha = alpha
  )
  treatment_effects
}

varying_alpha = map_df(seq(0.2, 0.99, 0.01) , 
                      treat_alpha_fn )

ggplot( data = varying_alpha ) + 
  geom_line( aes(x = alpha , y = biased ) , color = 'red' ) + 
  geom_line(aes(x = alpha , y = unbiased))
