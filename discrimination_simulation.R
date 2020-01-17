library(tidyverse)

a_c = 0 # "occupation status" intercept
a_w = 10 # wage intercept
sd_c = 1 # "randomness" alt. role of luck in occupation status
b_disc = 1 # parameter governing discrimination penalty "women" face (0-1)
b_c_disc = 1 # discrimination affects your occupational status
b_c_ability = 2 # natural ability determines occupational status
b_w_c = 2 # occupational status leads ot higher wages
b_w_ability = 2 # being skilled pays off 
sim_di = rbinom(10000 , 1 , 0.5)
ability = rnorm(10000)

discrimination = b_disc*sim_di # women are targets of discrimination
C = a_c - b_disc*discrimination + b_c_ability*ability + rnorm(10000, mean = 0 , sd = sd_c) # higher ability sort into higher occupational status
wage = a_w + b_w_c*C + b_w_ability*ability + rnorm(10000) # in this model, regardless of whether you're good at your job or not, higher "occupational status" pays off. similarly no matter your job, being good at your job pays off.


true_wage = lm(wage ~ C + ability ) # this estimates the true wage DGP
ecms_wage = lm(wage ~ sim_di ) # media quotes a wage gap
ecms_wage_bad = lm(wage ~ sim_di + C) # idiot thinks controlling for occupation helps; estimates positive effects which we know for a fact aren't causal


# plot setup stuff

plot_df = bind_cols( C = C , wage = wage , ability = ability , D = discrimination)

chris_theme =  theme( panel.background = element_rect( fill = 'white' ) , 
                      panel.grid = element_line( linetype = 2 , 
                                                 color = 'grey'))
# plotting 

h = ggplot( data = plot_df ,
            aes(x = C , 
                color = as.factor(D) ,
                fill = as.factor(D)
            ) 
)

h + geom_density(alpha = 0.6) + 
  labs(title = "Relationship between Status and Group Membership" ,
       color = "Discrimination" , 
       fill = "Discrimination" ,
       x = "Status" , 
       y = "Density"
  ) +
  geom_vline( xintercept =0  ,
              color = "darkblue" ) + 
  chris_theme


p = ggplot()

g = ggplot( data = plot_df ,
            aes(x = C , 
                y = wage ,
                color = as.factor(D)
            ) 
) 

g + geom_point(alpha = 0.6) + 
  labs(title = "Relationship between Status and Wage" , 
       subtitle = "Discriminated vs. Non-discriminated groups",
       color = "Discrimination" ,
       x = "Status/standing" , 
       y = "Wage"
  ) +
  chris_theme

p + geom_density_2d(data = plot_df %>% filter( D == 1) , 
                    aes(x = C , 
                        y = wage 
                    ) ,
                    color = '#00BCF4' ,
                    bins = 2) + 
  geom_density_2d(data = plot_df %>% filter( D == 0) , 
                  aes(x = C , 
                      y = wage 
                  ) ,
                  color = '#F8766D' , 
                  bins = 2) +
  geom_vline( xintercept = -1 , 
              color = "blue",
              linetype = 2) +
  labs(title = "Relationship between Status and Wage" , 
       subtitle = "Discriminated vs. Non-discriminated groups",
       color = "Discrimination" ,
       x = "Status/standing" , 
       y = "Wage"
  ) +
  theme( panel.background = element_rect( fill = 'white') , 
         panel.grid = element_line( linetype = 2 ,
                                    color = 'grey'))

## below is for generating many samples of the simulation to do some analysis of. not there yet, though,

#reg_sim_function = function(i){
#  
#sim_di = rbinom(10000 , 1 , 0.5)
#ability = rnorm(10000)
#
#discrimination = b_disc*sim_di # women are targets of discrimination
#C = a_c - b_disc*discrimination + b_c_ability*ability + rnorm(10000, mean = 0 , sd = sd_c) # higher ability sort into higher occupational status
#wage = a_w + b_w_c*C + b_w_ability*ability + rnorm(10000) # in this model, regardless of whether you're good at your job or not, higher "occupational status" pays off. similarly no matter your job, being good at your job pays off.
#
#
#true_wage = lm(wage ~ C + ability ) # this estimates the true wage DGP
#ecms_wage = lm(wage ~ sim_di ) # media quotes a wage gap
#ecms_wage_bad = lm(wage ~ sim_di + C) # idiot thinks controlling for occupation helps; estimates positive effects which we know for a fact aren't causal
#
#
#gap_coef = summary(ecms_wage)$coef[2,1]
#bad_coef = summary(ecms_wage_bad)$coef[2,1]
#
#data = bind_cols(gap_coef = gap_coef , 
#                 bad_coef = bad_coef )
#data
#}
#
#sim_df = map_dfr(1:1000, reg_sim_function)
#
#summary(sim_df ,)
#
#chris_theme =  theme( panel.background = element_rect( fill = 'white' ) , 
#                      panel.grid = element_line( linetype = 2 , 
#                                    color = 'grey'))
#
#g = ggplot( data = sim_df  )
#
#g + 
#  geom_density(aes( x = bad_coef) , 
#               fill='darkblue' ,
#               alpha = 0.4) +
#  geom_density(aes( x = gap_coef) , 
#               fill = 'darkred' , 
#               alpha = 0.4) + 
#  geom_vline(aes(xintercept = 0) , 
#             linetype = 2 , 
#             color = 'red') + 
#  labs( title = "Estimated Discrimination Wage Gap Measures",
#        x =  "Coefficient Estimates",
#        y = "Density" ,
#        fill  = "Coefficient") + chris_theme#