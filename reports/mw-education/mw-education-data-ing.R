library(pacman)
p_load( tidyverse ,
        lubridate ,
        plotly , 
        hrbrthemes )

setwd("D:/Economics/Projects/labor-economics/reports/mw-education")
qcew_msa = readRDS("D:/Economics/Projects/labor-economics/data/raw/qcew_99_19.rds") %>% 
  filter(type == "MSA") %>%
  separate( areaname , sep = " MSA" , 
            into = c("msa", NA)) %>%
  separate( msa , sep = " Mi" , 
            into = c("msa_name" , NA)) %>%
  separate( countyfips , sep = "C" , into = c(NA, "fips") ) %>%
  select( -c(statefips , countycode , statename)
  ) %>%
  mutate( date = ymd(paste(year,month,"01",sep="-")) ,
          cbsa_code = paste(fips,"0",sep = "") %>% as.numeric() ) %>%
  arrange(date, cbsa_code )

qcew_total_and_leisure = 
  qcew_msa %>% 
  filter( owncode == 0 |
            indcode == 1026 ) 

saveRDS(qcew_total_and_leisure , 
        "D:/Economics/Projects/labor-economics/data/export/qcew_total_and_leisure-hospitality.rds")

rm(qcew_msa)

qcew_total_and_leisure = readRDS("D:/Economics/Projects/labor-economics/data/export/qcew_total_and_leisure-hospitality.rds")
metro_cps_2000s = readRDS("D:/Economics/Projects/labor-economics/data/export/metro_cps_2000s_v2.rds") %>% rename(msa_old = metarea)

#msa_cbsa_xwalk = read_xlsx("D:/Economics/Data/msa-old-new-xwalk.xlsx")
#msa_cps_usa_xwalk = readRDS("D:/Economics/Projects/labor-economics/data/export/msa_cps_census_xwalk_v2.rds") %>% 
#  rename(cbsa_code = cbsa_new)

msa_cps_usa_xwalk = readRDS("D:/Economics/Projects/labor-economics/data/export/msa_cps_census_xwalk_v3.rds") %>% 
  rename(cbsa_code = cbsa_new)


msa_cps.subset = left_join( msa_cps_usa_xwalk , metro_cps_2000s) 

saveRDS(msa_cps.subset , 
        "D:/Economics/Projects/labor-economics/data/export/cbsa_msa.rds")

msa_cps.subset = readRDS("D:/Economics/Projects/labor-economics/data/export/cbsa_msa.rds")

msa_qcew_cps.subset = msa_cps.subset %>% 
  left_join(y = qcew_total_and_leisure ) %>%
  select( -c(fips))

saveRDS(msa_qcew_cps.subset , "D:/Economics/Projects/labor-economics/data/export/cps_qcew_subset_merged.rds" )

msa_cps_qcew = readRDS("D:/Economics/Projects/labor-economics/data/export/cps_qcew_subset_merged.rds") 

msa_cps_qcew$ind = recode(.x = msa_cps_qcew$ind , 
                          "10 Total, all industries" = "Total, all industries"  ,
                          "101 Goods-producing" = "Goods-producing" ,
                          "1011 Natural resources and mining" = "Natural resources and mining" ,
                          "1012 Construction" = "Construction"   ,
                          "1013 Manufacturing" = "Manufacturing"  ,
                          "102 Service-providing" = "Service-providing"   ,
                          "1021 Trade, transportation, and utilities" = "Trade, transportation, and utilities" ,
                          "1022 Information" = "Information"         ,
                          "1023 Financial activities" = "Financial activities"  ,
                          "1024 Professional and business services" = "Professional and business services" ,
                          "1025 Education and health services" = "Education and health services"   ,
                          "1026 Leisure and hospitality"  = "Leisure and hospitality"        ,
                          "1027 Other services" = "Other services"  ,
                          "1029 Unclassified" = "Unclassified" )

msa_cps_qcew$statefip =  factor(msa_cps_qcew$statefip ,
                                levels = c( 
                                  1	,  2	,   4	,   5	,   6	,   8	,   9	,   10	,
                                  11	,   12	,   13	,   15	,   16	,   17	,   18	,
                                  19	,   20	,   21	,   22	,   23	,   24	,   25	,   
                                  26	,   27	,   28	,   29	,   30	,   31	,   32	,   
                                  33	,   34	,   35	,   36	,   37	,   38	,   39	,
                                  40	,   41	,   42	,   44	,   45	,   46	,   47  ,   
                                  48	,   49	,   50	,   51	,   53	,   54	,   55	,
                                  56  
                                ),
                                labels = c(
                                  "Alabama"	,"Alaska"	,"Arizona"	,"Arkansas"	,"California"	,"Colorado"	,"Connecticut"	,"Delaware"	,
                                  "District of Columbia"	,"Florida"	,"Georgia"	,"Hawaii"	,"Idaho"	,"Illinois"	,"Indiana"	,"Iowa"	,
                                  "Kansas"	,"Kentucky"	,"Louisiana"	,"Maine"	,"Maryland"	,"Massachusetts"	,"Michigan"	,"Minnesota"	,
                                  "Mississippi"	,"Missouri"	,"Montana"	,"Nebraska"	,"Nevada"	,"New Hampshire"	,"New Jersey"	,"New Mexico"	,
                                  "New York"	,"North Carolina"	,"North Dakota"	,"Ohio" ,"Oklahoma"	,"Oregon"	,"Pennsylvania"	,"Rhode Island"	,
                                  "South Carolina"	,"South Dakota"	,"Tennessee"	,"Texas"	,"Utah"	,"Vermont"	,"Virginia"	,"Washington"	,
                                  "West Virginia"	,"Wisconsin"	,"Wyoming" 
                                )
)

saveRDS(msa_cps_qcew ,"D:/Economics/Projects/labor-economics/data/export/cps_qcew_subset_merged.rds")

wageplot = msa_cps_qcew %>% 
  filter( year == 2019) %>%
  group_by( ind ) %>% 
  summarise(wage = mean(wage_week)) %>% 
  ggplot( aes(x = ind , y = wage)) + geom_col() + coord_flip()

industry_plot = msa_cps_qcew  %>% 
  filter( indcode == 10 ) %>% 
  ggplot( mapping = aes( x = employed , y = employment , color = msa_name) ) + 
  geom_abline( intercept = 0 , slope = 1) +
  geom_point() + theme( legend.position = 'none')

ggplotly(industry_plot)

mw_substate = readRDS("D:/Economics/Projects/labor-economics/data/export/mw_substate.rds")

mw_substate %>% filter( date >= "2019-01-01") %>% summarise( mu= mean(substate_nominal_mw/state_nominal_mw) )

localities = mw_substate %>% filter( date >= "2019-01-01") %>% select( locality , statefip ) 

localities = as.factor(localities$locality) %>% levels() %>% as.data.frame()
write_csv( localities , "D:/Economics/Projects/labor-economics/data/export/substate_mw_localities.csv")
locality_county_cbsa_xwalk = read_csv("D:/Economics/Data/clean-cbsa-county-xwalk.csv")
localities_with_counties = read.csv("D:\\Economics\\Projects\\labor-economics\\data\\export\\substate_mw_localities_counties.csv") %>% 
  rename(countyfips = "county_fips")


localities_county_cbsa = left_join(localities_with_counties, locality_county_cbsa_xwalk %>%
                                     select( -metarea ) )

library(data.table)
mw_substate_cbsa = merge( mw_substate , localities_county_cbsa) %>% as.data.table()

mw_substate_max_cbsa = mw_substate_cbsa[ , .(cbsa_max_mw_n = max(substate_nominal_mw) , 
                                             cbsa_max_mw_r = max(substate_real_mw) ,
                                             state_mw_n = mean(state_nominal_mw) , 
                                             state_mw_r = mean(state_real_mw)) , 
                                         by = .(date, cbsa_code) ]

mw_substate_df = left_join(mw_substate_max_cbsa , 
                           mw_substate_cbsa %>% select( date, cbsa_code , statefip ) ) %>% dplyr::distinct()

mw_state  = readRDS("D:/Economics/Projects/labor-economics/data/export/mw_state.rds") %>% 
  rename( state_mw_n = nominal_mw , 
          state_mw_r = real_mw ) %>%
  filter( date > "1999-12-01" )

data_state_mw  = left_join( 
  msa_cps_qcew %>% 
    select(-c(type , lbl , msa_old , msaname ) )  , 
  mw_state
)

data_mw = left_join(data_state_mw , mw_substate_df %>% select(-c(state_mw_n , 
                                                                 state_mw_r , 
                                                                 statefip ) ) 
) %>%
  mutate( cbsa_max_mw_n = replace_na( cbsa_max_mw_n  , 0 ) ,
          cbsa_max_mw_r = replace_na( cbsa_max_mw_r , 0 ) ,
          binding_local_mw_n = pmax(state_mw_n , cbsa_max_mw_n) , 
          binding_local_mw_r = pmax(state_mw_r , cbsa_max_mw_r)) %>%
  filter( is.na(msa_name) == FALSE )

setwd("D:/Economics/Projects/labor-economics/")

#saveRDS(data_mw,
#        "data/export/cps_qcew_mw_by_msa_full_v2.rds")
#
#full_data_df = readRDS("data/export/cps_qcew_mw_by_msa_full_v2.rds")
#

saveRDS(data_mw,
        "data/export/cps_qcew_mw_by_msa_full_v3.rds")

full_data_df = readRDS("data/export/cps_qcew_mw_by_msa_full_v3.rds")

#pivot_vars = names(full_data_df[,
#                                which( names(full_data_df) == 'establishments'):which( names(full_data_df) == 'employment') ]
#                   )
#
#lnh_sector_df = 
#  full_data_df %>% 
#  filter( ind == "Leisure and hospitality") %>%
#  select( "cbsa_code" , 
#          "date" , 
#          "ind" , 
#          all_of(pivot_vars) )

total_data_df = 
  full_data_df %>% 
  filter( ind == "Total, all industries")

saveRDS( total_data_df , "data/export/msa_mw_project_data_total_emp.rds")


total_data_df = readRDS("data/export/msa_mw_project_data_total_emp.rds")

data_has_hs_df = 
  total_data_df %>% 
  filter( parttime_hs > 0 &
            schoolage > 0 ) %>%
  group_by(msa_name , cbsa_code , date ) %>%
  summarise( population = sum(population) , 
             parttime_hs = sum(parttime_hs),
             parttime_coll = sum(parttime_coll) , 
             fulltime_hs = sum(fulltime_hs),
             fulltime_coll = sum(fulltime_coll),
             employed = sum(employed) , 
             unemployed = sum(unemployed) , 
             unemployed_new = sum(unemployed_new) ,
             schoolage = sum(schoolage) ,
             binding_local_mw_n = max(binding_local_mw_n) , 
             binding_local_mw_r = max(binding_local_mw_r) ,
             establishments = mean(establishments) , 
             wage_bill = mean(wage_bill),
             wage_week = mean(wage_week),
             employment = mean(employment)
  ) %>%
  mutate( 
    time_var = time_length(interval(ymd("2000-01-01"), date ), "month") ,
    in_hs = (parttime_hs + fulltime_hs) ,
    log_hs = log(in_hs) ,
    hs_lag = dplyr::lag(in_hs) , 
    log_hs_lag = dplyr::lag(log_hs) ,
    in_hs_rate  = in_hs/schoolage ,
    lag_hs_rate = dplyr::lag(in_hs_rate) ,
    seatac_treat = ifelse( date > "2014-01-01" & cbsa_code == 42660 , 
                           1 , 
                           0 )  ,
    placebo_treat = ifelse( date > "2014-01-01" , 
                            1 , 
                            0 ) 
  )


saveRDS(data_has_hs_df , "data/export/mw_ed_project_has_hs_pop_data.rds")

data_has_uni_df = 
  total_data_df %>% 
  filter( parttime_coll > 0 &
            schoolage > 0 ) %>%
  group_by(msa_name , cbsa_code , date ) %>%
  summarise( population = sum(population) , 
             parttime_coll = sum(parttime_coll) , 
             fulltime_coll = sum(fulltime_coll),
             employed = sum(employed) , 
             unemployed = sum(unemployed) , 
             unemployed_new = sum(unemployed_new) ,
             schoolage = sum(schoolage) ,
             binding_local_mw_n = max(binding_local_mw_n) , 
             binding_local_mw_r = max(binding_local_mw_r) ,
             establishments = mean(establishments) , 
             wage_bill = mean(wage_bill),
             wage_week = mean(wage_week),
             employment = mean(employment)
  ) %>%
  mutate( 
    time_var = time_length(interval(ymd("2000-01-01"), date ), "month") ,
    in_uni = (parttime_coll + fulltime_coll) ,
    log_uni = log(in_uni) , 
    uni_lag = dplyr::lag(in_uni) ,
    log_uni_lag = dplyr::lag(log_uni) ,
    in_uni_rate_pop = in_uni/population ,
    in_uni_rate_sa = in_uni/schoolage ,
    lag_uni_rate_pop = dplyr::lag(in_uni_rate_pop) ,
    lag_uni_rate_sa = dplyr::lag(in_uni_rate_sa) ,
    seatac_treat = ifelse( date > "2014-01-01" & cbsa_code == 42660 , 
                           1 , 
                           0 )  ,
    placebo_treat = ifelse( date > "2014-01-01" , 
                            1 , 
                            0 ) 
  )


saveRDS(data_has_uni_df , "data/export/mw_ed_project_has_uni_pop_data.rds")


pre_treat = 
  data_has_hs_df %>% 
  filter( date == "2005-10-01" ) %>% 
  select( cbsa_code , in_hs_rate , in_uni_rate ) 

pretreat_plot = 
  pre_treat %>% 
  left_join( total_data_df %>% 
               filter( date == "2017-02-01" ) %>% 
               select( cbsa_code , binding_local_mw_n  ) ) %>% 
  ggplot( aes( x = in_hs_rate , y = binding_local_mw_n , color = cbsa_code) ) + 
  geom_point() + 
  geom_smooth( method = 'lm')

