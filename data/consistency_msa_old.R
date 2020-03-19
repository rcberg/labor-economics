library(pacman)
p_load(ipumsr , dplyr)

ddi_usa = read_ipums_ddi("D:\\Economics\\Data\\Census\\usa_00008.xml")
ddi_cps = read_ipums_ddi("D:\\Economics\\Data\\CPS\\cps_00028.xml")

census_met = ipums_val_labels( ddi_usa , var = METAREA ) %>% mutate(census = val*10) 
cps_met = ipums_val_labels( ddi_cps , var = METAREA ) %>% rename( cps = val ) %>% mutate( cps_fix = cps ,
                                                                                          cps_old = cps )

met_consistent = left_join(census_met , cps_met)

#
#met_consistent_3 = cps_met %>% mutate( census = cps ) %>% left_join(census_met)

for(i in 1:length(met_consistent$cps) ) {
  if( is.na(met_consistent$cps[i]) == TRUE & 
      is.na(met_consistent$census[i]) == FALSE ){
    met_consistent$cps_fix[i] = met_consistent$census[i]
    met_consistent$cps_old[i] = met_consistent$census[i]
  }
}

consistency_test = met_consistent %>% 
  mutate( flag = ifelse((census - cps_fix) != 0, 
                        1, 
                        0) )



met_consistent = met_consistent %>% 
  mutate(cps_fix = census ) 

met_consistent_2 = left_join(
  cps_met %>% select(-(cps_fix)) , 
  met_consistent %>% select(cps_old, cps_fix)
) %>% 
  arrange(cps_old)


for(i in 2:length(met_consistent_2$cps) ) {
  if(met_consistent_2$cps[i] == met_consistent_2$cps[i-1] + 1){
    met_consistent_2$cps_fix[i] = met_consistent_2$cps_fix[i-1]
  } 
}

for(i in 1:length(met_consistent_2$cps) ) {
  if(is.na(met_consistent_2$cps_fix[i]) == FALSE ){
    met_consistent_2$cps[i] = met_consistent_2$cps_fix[i]
  }
}

msa_consistent_old = met_consistent_2 %>% 
  select(cps, cps_old, lbl) %>% 
  filter( !(lbl == "Abilene, TX" & cps == 60)) %>%
  rename( msa = cps ,
          msa_old = cps_old )

setwd("D:/Economics/Projects/labor-economics/data/export")

cbsa_msa_xwalk = read.csv("D:/Economics/Projects/labor-economics/data/usa_00009.csv") 
cbsa_msa_xwalk2 = read_excel("D:/Economics/Projects/labor-economics/data/raw/msa_cbsa_xwalk.xlsx") %>% rename(cbsa_new = cbsa) 
test = cbsa_msa_xwalk %>% 
  filter(METAREA > 0 & MET2013 > 0 ) %>% 
  rename( msa = "METAREAD" ,
          cbsa_code = "MET2013" ) %>% 
  select( msa, cbsa_code) %>%
  distinct()


test = left_join(msa_consistent_old , cbsa_msa_xwalk2) 

msa_consistent = test %>% 
  filter( msa < 9997 & is.na(cbsa_new) == F )

#saveRDS(msa_consistent, 
#        "msa_cps_census_xwalk_v2.rds")
saveRDS(msa_consistent, 
        "msa_cps_census_xwalk_v3.rds")
