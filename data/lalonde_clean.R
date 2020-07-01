library(tidyverse)

nsw_names <- 
  c( "treat" ,
     "age" ,
     "edu" ,
     "black"  ,
     "hispanic" ,
     "married" , 
     "nodegree" , 
     "re75" , 
     "re78" )

cps_psid_names <- 
  c( "treat" ,
     "age" ,
     "edu" ,
     "black"  ,
     "hispanic" ,
     "married" , 
     "nodegree" ,
     "re74" ,
     "re75" , 
     "re78" )

nsw_treat_df <- 
  read.table("http://www.nber.org/~rdehejia/data/nsw_treated.txt" , col.names = nsw_names ) %>%
  mutate( re74 = NA ,
          sample = "nsw" )
  
nsw_control_df <- 
  read.table("http://users.nber.org/~rdehejia/data/nsw_control.txt" , col.names = nsw_names ) %>%
  mutate( sample = "nsw" ,
          re74 = NA )

psid_control_df <- 
  read.table("http://www.nber.org/~rdehejia/data/psid_controls.txt" , col.names = cps_psid_names ) %>%
  mutate( sample = "psid-1" )
psid2_control_df <- 
  read.table("http://www.nber.org/~rdehejia/data/psid2_controls.txt" , col.names = cps_psid_names ) %>%
  mutate( sample = "psid-2" )
psid3_control_df <- 
  read.table("http://www.nber.org/~rdehejia/data/psid3_controls.txt" , col.names = cps_psid_names ) %>%
  mutate( sample = "psid-3" )

cps_control_df <-
  read.table("http://www.nber.org/~rdehejia/data/cps_controls.txt" , col.names = cps_psid_names ) %>%
  mutate( sample = "cps-1" )
cps2_control_df <- 
  read.table("http://www.nber.org/~rdehejia/data/cps2_controls.txt" , col.names = cps_psid_names ) %>%
  mutate( sample = "cps-2" )
cps3_control_df <- 
  read.table("http://www.nber.org/~rdehejia/data/cps3_controls.txt" , col.names = cps_psid_names ) %>%
  mutate( sample = "cps-3" )

lalonde_df <- 
  nsw_treat_df %>%
  bind_rows( nsw_control_df , 
             psid_control_df , 
             psid2_control_df , 
             psid3_control_df , 
             cps_control_df , 
             cps2_control_df , 
             cps3_control_df )

saveRDS( lalonde_df ,
         file = "D:/Economics/Projects/labor-economics/data/raw/lalonde.rds")
