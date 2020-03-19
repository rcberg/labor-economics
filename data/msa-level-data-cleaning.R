library(pacman)
p_load( tidyverse ,
        lubridate ,
        readxl ,
        ipumsr ,
        plotly)

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

metro_cps_2000s = readRDS("D:/Economics/Projects/labor-economics/data/export/metro_cps_2000s.rds") %>% rename(msa_old = metarea)
mw_substate = readRDS("D:/Economics/Projects/labor-economics/data/export/mw_substate.rds")

msa_cbsa_xwalk = read_xlsx("D:/Economics/Data/msa-old-new-xwalk.xlsx")
msa_cps_usa_xwalk = readRDS("D:/Economics/Projects/labor-economics/data/raw/msa_cps_census_xwalk.rds")

msa_cps = left_join(msa_cbsa_xwalk, msa_cps_usa_xwalk) %>% 
  mutate( na = is.na(msa_old)) %>% 
  filter( na == FALSE ) %>% 
  select(-c(na,
            msaname , 
            ) ) %>% rename("msa_name" = "lbl" )

msa_cps_qcew = left_join( msa_cps , metro_cps_2000s) %>% 
  left_join(qcew_msa) %>%
  select( -c(fips))

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

msa_qcew_cps_merged$statefip =  factor(msa_qcew_cps_merged$statefip ,
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

saveRDS(msa_cps_qcew ,"D:/Economics/Projects/labor-economics/data/export/msa_qcew_cps_merged.rds")

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
       msa_qcew_cps_merged %>% 
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

saveRDS(data_mw,
        "data/export/cps_qcew_mw_by_msa_full.rds")

full_data_df = readRDS("data/export/cps_qcew_mw_by_msa_full.rds")

leisure_ind_data_df = full_data_df %>% 
  filter( owncode == 0 |
            indcode == 1026 ) 

pivot_vars = names(leisure_ind_data_df[,28:33])

dumb_cast_function = function(i){
  sector_var_df = leisure_ind_data_df %>% 
    select( "cbsa_code" , 
            "date" , 
            "ind" , 
            i ) %>%
    dcast( formula = cbsa_code + date ~ ind , mean,  value.var = i )
  names(sector_var_df) = c("cbsa_code" , "date" , 
                           paste("leisure_sector",i,sep="_") , paste("total",i,sep="_"))
  sector_var_df
}

sector_vars = map(pivot_vars , dumb_cast_function) %>% bind_cols() %>%
  select(-c(cbsa_code1,cbsa_code2,cbsa_code3,cbsa_code4,cbsa_code5 , 
            date1 , date2 , date3 , date4 , date5  , 
            total_wage_us_ratio , 
            total_emp_us_ratio ) 
  ) %>%
  left_join( full_data_df %>% 
               select(cbsa_code , msa_name) %>% 
               dplyr::distinct() )

data_df = full_data_df %>% 
  select(-(22:33)) %>% 
  dplyr::distinct() %>% 
  left_join( sector_vars ) %>% 
  arrange( date , desc(total_wage_week))

saveRDS( data_df , "data/export/cps_qcew_mw_by_msa_total_and_leisure_sectors.rds")
