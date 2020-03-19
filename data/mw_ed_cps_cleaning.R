library(pacman)
p_load( tidyverse ,
        janitor , 
        ipumsr ,
        data.table , 
        lubridate ,
        readxl , 
        plotly , 
        fredr ,
        furrr )
plan(multiprocess)

# data management

## going to put the state factor labels here. un-comment if you need to adjust at all.

#statecodes = c( 
#  1	,  2	,   4	,   5	,   6	,   8	,   9	,   10	,
#  11	,   12	,   13	,   15	,   16	,   17	,   18	,
#  19	,   20	,   21	,   22	,   23	,   24	,   25	,   
#  26	,   27	,   28	,   29	,   30	,   31	,   32	,   
#  33	,   34	,   35	,   36	,   37	,   38	,   39	,
#  40	,   41	,   42	,   44	,   45	,   46	,   47  ,   
#  48	,   49	,   50	,   51	,   53	,   54	,   55	,
#  56  
#)
#
#statelabs = c(
#  "Alabama"	,"Alaska"	,"Arizona"	,"Arkansas"	,"California"	,"Colorado"	,"Connecticut"	,"Delaware"	,
#  "District of Columbia"	,"Florida"	,"Georgia"	,"Hawaii"	,"Idaho"	,"Illinois"	,"Indiana"	,"Iowa"	,
#  "Kansas"	,"Kentucky"	,"Louisiana"	,"Maine"	,"Maryland"	,"Massachusetts"	,"Michigan"	,"Minnesota"	,
#  "Mississippi"	,"Missouri"	,"Montana"	,"Nebraska"	,"Nevada"	,"New Hampshire"	,"New Jersey"	,"New Mexico"	,
#  "New York"	,"North Carolina"	,"North Dakota"	,"Ohio" ,"Oklahoma"	,"Oregon"	,"Pennsylvania"	,"Rhode Island"	,
#  "South Carolina"	,"South Dakota"	,"Tennessee"	,"Texas"	,"Utah"	,"Vermont"	,"Virginia"	,"Washington"	,
#  "West Virginia"	,"Wisconsin"	,"Wyoming" 
#)

## cps data management
#
setwd("D:/Economics/Data/CPS/")
ddi = read_ipums_ddi("cps_00028.xml")
#
### reading-in data
#
#data = read_ipums_micro(ddi) %>% clean_names()
#saveRDS(data, "cps_monthly_2000s.rds")
data = readRDS("cps_monthly_2000s.rds")  %>% 
  select(-asecflag) %>% 
  mutate_all( as.numeric) %>% as.data.table()
#
data = data[,`:=`(woman = ifelse(sex == 2 , wtfinl , 0) ,
               young = ifelse( age > 15 &
                                 age < 20 , wtfinl , 0) , 
               young_man = ifelse( age > 15 &
                                     age < 20 & 
                                     sex == 1 , wtfinl , 0) ,
               in_labor_force = ifelse( labforce == 2 , wtfinl , 0) ,
               civ_emp = ifelse( empstat == 10 , wtfinl , 0) , 
               unemp = ifelse( empstat == 20 | empstat == 21 | empstat == 22 , wtfinl , 0 ) ,
               unemp_new = ifelse( empstat == 22 , wtfinl , 0) ,
               bach = ifelse( educ == 111 , wtfinl , 0 ) , 
               some_coll = ifelse( educ == 81 , wtfinl , 0) ,
               schoolage = ifelse( age > 15 &
                                  age < 25 , wtfinl , 0) ,
               hs_full = ifelse( schlcoll == 2 , wtfinl , 0) , 
               hs_part = ifelse( schlcoll == 3 , wtfinl , 0) , 
               uni_full = ifelse( schlcoll == 4 , wtfinl , 0)  ,
               uni_part = ifelse( schlcoll == 5 , wtfinl , 0)) 
            , ]

saveRDS(data, "cps_monthly_plus_2000s_v2.rds")
data = readRDS("cps_monthly_plus_2000s_v2.rds")

metro_cps = data[ metarea < 9997 ,
                  .(population = sum(wtfinl) ,
                    women = sum(woman), 
                    youth = sum(young), 
                    youth_men = sum(young_man),
                    laborforce = sum(in_labor_force) ,
                    employed = sum(civ_emp) ,
                    unemployed = sum(unemp),
                    unemployed_new = sum(unemp_new) ,
                    bach = sum(bach),
                    some_coll = sum(some_coll) ,
                    schoolage = sum(schoolage) , 
                    fulltime_hs = sum(hs_full) ,
                    parttime_hs = sum(hs_part),
                    fulltime_coll = sum(uni_full) ,
                    parttime_coll = sum(uni_part)) ,
                  by = .( year , month , statefip , metarea) ]

hours  = data[metarea < 9997 & uhrsworkt<999 & empstat == 10 , 
                        .(avg_hours = weighted.mean(uhrsworkt , w = wtfinl)) , 
                        by = .(year, month , statefip , metarea) ]

metro_cps = merge( metro_cps, hours) %>%
  mutate( date = ymd(paste(year,month,"01", ssep = "-"))) %>%
  arrange( date , statefip ) %>%
  mutate_at( .vars = vars(population:parttime_coll) , 
             .funs = round )

setwd("D:/Economics/Projects/labor-economics")
saveRDS(metro_cps , "data/export/metro_cps_2000s_v2.rds")
#
### minimum wage data management
#
#state_mw_pre2017 = read_xlsx("D:/Economics/Projects/VZ_historicalminwage/exports/VZ_state_monthly.xlsx") %>% 
#  clean_names() %>%
#  separate(col = monthly_date , 
#           into = c("year" , "month" ) ,
#           sep = "m" ) %>%
#  arrange(year,month,state_fips_code) %>%
#  mutate_at( .vars = vars(year:monthly_state_maximum) , 
#             .funs = as.numeric )%>%
#  mutate( date = ymd(paste(year,month,"01", sep = "-")) ) %>%
#  select( -c(name, state_abbreviation) ) %>%
#  dplyr::rename( statefip = state_fips_code , 
#                 fed_mw = monthly_federal_maximum , 
#                 state_mw = monthly_state_maximum)
#
#state_mw_updates = read_xlsx("D:/Economics/Projects/VZ_historicalminwage/rawdata/VZ_StateMinimumWage_Changes.xlsx") %>% 
#  clean_names() %>%
#  mutate( 
#    date_monthly = ymd( 
#      paste(year, month, "01" , sep = "-")
#    ) , 
#      date_chg = if_else( day>1 , 
#                          date_monthly + months(1) , 
#                          date_monthly ) 
#    ) 
#
#state_mw_post2017 = state_mw_pre2017 %>%
#  expand(  year= 2016:2019 , month = 1:12 , statefip   ) %>%
#  cbind(state_mw =  state_mw_pre2017 %>%
#          filter( year == 2016 & month == 7) %>% 
#          select(state_mw ) 
#  ) %>%
#  mutate( date = ymd( 
#    paste( year, month, "01", sep="-") )
#    )%>%
#  filter( date > ymd("2016-07-01") ) %>%
#  merge( state_mw_updates %>% 
#           filter( date_chg > ymd("2016-07-01") & date_chg < ymd("2020-01-01") ) %>%
#           select( statefip = statefips , 
#                   date = date_chg , 
#                   vz_mw) ,
#         all = T ) %>%
#  mutate(vz_mw = replace_na(vz_mw , 0 ) , 
#         mw = pmax(vz_mw , state_mw)) %>% 
#  arrange( year , statefip , month)
#
#state_levels =  levels(as.factor(state_mw_post2017$statefip))
#
#st_mw_f = function(i){
#  
#  init = state_mw_post2017 %>% 
#    filter( statefip == i )
#  
#  for(t in 2:41){
#    init$mw[t] = pmax( init$mw[(t-1)] , 
#                       init$vz_mw[t] )
#  }
#  init
#}
#
#state_mw_append = map_dfr(state_levels,
#                          st_mw_f )
#
#fredr_set_key(Sys.getenv("FRED_API_KEY"))
#cpi = fredr( series_id = "CPIAUCSL" ,
#             frequency = "m" ,
#             observation_start = ymd("1974-05-01") , 
#             observation_end = ymd("2019-12-01")) %>%
#  select(date , value) %>%
#  rename( cpi = value)
#
#state_mw = state_mw_pre2017 %>%
#  select( date , 
#          statefip , 
#          nominal_mw = state_mw ) %>%
#  rbind( state_mw_append %>% select(date , statefip , nominal_mw = mw) ) %>% 
#  merge(cpi) %>%
#  mutate( real_mw = (193.7/cpi)*nominal_mw )
#
#state_mw$statefip = factor( state_mw$statefip , 
#                            levels = statecodes , 
#                            labels = statelabs )
#
#saveRDS( state_mw , "D:/Economics/Projects/labor-economics/data/export/mw_state.rds")
#
#
#substate_mw_pre2017 = read_xlsx( "D:/Economics/Projects/VZ_historicalminwage/exports/VZ_substate_monthly.xlsx") %>%
#  clean_names() %>%
#  separate(col = monthly_date , 
#           into = c("year" , "month" ) ,
#           sep = "m" ) %>%
#  arrange(year,month,state_fips_code) %>%
#  mutate_at( .vars = vars(year:local_state_min_wage) , 
#             .funs = as.numeric ) %>%
#  mutate( date = ymd(paste(year,month,"01", sep = "-")) ) %>%
#  select( -c(state, state_abbreviation) ) %>%
#  rename(statefip = state_fips_code , 
#         locality = city_county ) %>%
#  arrange(date, locality)
#
#substate_mw_updates = read_xlsx("D:/Economics/Projects/VZ_historicalminwage/rawdata/VZ_SubstateMinimumWage_Changes.xlsx") %>% 
#  clean_names() %>%
#  mutate( 
#    date_monthly = ymd( 
#      paste(year, month, "01" , sep = "-")
#    ) , 
#    date_chg = if_else( day>1 , 
#                        date_monthly + months(1) , 
#                        date_monthly ) 
#  ) %>%
#  select( statefip = statefips , 
#          locality , 
#          mw_upd = vz_mw , 
#          date = date_chg ) %>%
#  arrange( date, locality )
#
#substate_mw_post2017 = substate_mw_pre2017 %>%
#  expand(  year= 2016:2019 , month = 1:12 , locality   ) %>%
#  mutate( date = ymd( 
#    paste( year, month, "01", sep="-") )
#  )%>%
#  filter( date > ymd("2016-07-01") ) %>%
#  cbind(substate_mw =  substate_mw_pre2017 %>%
#          filter( year == 2016 & month == 7) %>% 
#          select(substate_mw = monthly_maximum ) 
#  ) %>%
#  merge( substate_mw_updates %>% 
#           filter( date > ymd("2016-07-01") & date < ymd("2020-01-01") ) %>%
#           select( locality ,
#                   date , 
#                   mw_upd ),
#         all = T ) %>%
#  mutate(mw_upd = replace_na(mw_upd , 0 ) , 
#         mw = pmax(mw_upd , substate_mw)) %>% 
#  arrange( locality , date) 
#
#substate_levels = levels(as.factor(substate_mw_post2017$locality))
#
#ss_mw_f = function(i){
#  
#  init = substate_mw_post2017 %>% 
#    filter( locality == i )
#  
#  for(t in 2:41){
#    init$mw[t] = pmax( init$mw[(t-1)] , 
#                       init$mw_upd[t] )
#  }
#  init
#}
#
#substate_mw_append = map_dfr(substate_levels,
#                          ss_mw_f )
#
#substate_states_xw = substate_mw_pre2017 %>% 
#  filter( date == ymd("2004-01-01") ) %>% 
#  select( locality, statefip)
#
#substate_mw = substate_mw_pre2017 %>%
#  select( date , 
#          locality , 
#          substate_nominal_mw = monthly_maximum ) %>%
#  rbind( substate_mw_append %>% select(date , locality , substate_nominal_mw = mw) ) %>%
#  arrange( locality, date) %>% merge(substate_states_xw)
#
#substate_mw$statefip = factor( substate_mw$statefip , 
#                               levels = statecodes , 
#                               labels = statelabs )
#
#state_mw = readRDS("D:/Economics/Projects/labor-economics/data/export/mw_state.rds") %>%
#  filter(date >= ymd("2004-01-01"))
# 
#substate_mw_df = merge( x = substate_mw , 
#                        y = state_mw , 
#                        all.x  = T ) %>%
#  rename( state_nominal_mw = nominal_mw , 
#          state_real_mw = real_mw ) %>%
#  mutate( substate_real_mw = (193.7/cpi)*substate_nominal_mw ,
#          locality = ifelse( statefip == "District of Columbia" , 
#                             "DC" , 
#                             locality))
#
#saveRDS( substate_mw_df , "D:/Economics/Projects/labor-economics/data/export/mw_substate.rds")
#
## commuting zone data management
#
