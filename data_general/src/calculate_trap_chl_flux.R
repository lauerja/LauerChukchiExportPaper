library('tidyverse')

## This script runs on pre-processed data - need depth integrated chl csv and trap and ctd chl csvs
## Also requires trap metadata csv

#### Read in Preprocessed Data ####
# trap_chl <- read.csv('../processed_data/trap_chl_processed.csv')%>%
trap_chl <- read_csv('../data_measured_chl/trap_chl_processed.csv')%>%
  select(sampleNum, chlMean, chlSd, phaeoMean, phaeoSd)%>%
  unique()

# diChl_aboveTrap <- read.csv('../processed_data/depth_integrated_chl_above_traps.csv')%>% # deployment only
diChl_aboveTrap <- read_csv('../processed_data/depth_integrated_biomass_above_traps.csv')%>%
  mutate(sampleNum = trap, .keep = 'unused',
         diChl_mg_m2 = ifelse(dichlMean_mg_m2 == 0, NA, dichlMean_mg_m2))%>% # divide by 0 is bad later and only impacts 3 ice-tethered traps, set to NA here
  filter(deployment != 'T001') # no one cares about the test deployment

metadata <- read.csv('../data_station_meta/trap_metadata.csv')%>%
  drop_na('Recover_Station')%>%
  filter(Deployment != 'T001')%>% # no one cares about the test deployment
  mutate(deploy_station_actual = Deploy_Station, # T026 actual deployed at station 174, but station 167 ctd data will be used
         Deploy_Station = ifelse(Deployment == 'T026', 167, Deploy_Station), # different cast used to fill in T026 data because no CTD cast at deployment
         recoverTime = mdy_hm(Recover_DateTime_UTC),
         deployTime = mdy_hm(Deploy_DateTime_UTC),
         timeOut = as.numeric(difftime(recoverTime, deployTime, units = 'days')),
         sampleNum = Sample_Number) #,
         # lat_band = ifelse(Recover_Lat >= 71, 'North',
         #                   ifelse(Recover_Lat >= 70.7, 'Middle',
         #                          'South')))


#### Compute Flux ####
volumes <- metadata %>%
  select(Deployment, Sample_Number, Recovery_total_volume_L)%>%
  mutate(deployment = Deployment,
         sampleNum = Sample_Number,
         vol = as.numeric(Recovery_total_volume_L), .keep = 'none')

surf_area <- 0.05^2 * pi # 10cm diameter = 0.05m radius. Area = pi*r^2 

flux <- left_join(trap_chl, volumes)%>%
  left_join(., select(metadata, sampleNum, timeOut))%>%
  mutate(chlTotal = chlMean * vol *0.001, # ug/L * volume (L) * conversion from ug to mg - gives chl in mg
         chlPerArea = chlTotal / surf_area, # chl (mg) divided by surface area in m^2 - gives mg/m^2
         flux_mg_m2_day = chlPerArea / timeOut,  # chl/area (mg/m2) divided by time out in days - gives mg/m2/day
         phaeoTotal = phaeoMean * vol *0.001,
         phaeoPerArea = phaeoTotal / surf_area,
         phaeo_flux_mg_m2_day = phaeoPerArea / timeOut)%>%
  select(-chlTotal, -chlPerArea, -phaeoTotal, -phaeoPerArea)%>%
  unique()

normalized_flux <- left_join(diChl_aboveTrap, flux)%>%
  mutate(normFluxPerDay = flux_mg_m2_day / diChl_mg_m2,
         normFluxPercent = normFluxPerDay * 100)%>%
  select(sampleNum, deployment, station, section, trap_depth, flux_mg_m2_day, 
         normFluxPerDay, normFluxPercent, diChl_mg_m2, chlMean, chlSd, phaeo_flux_mg_m2_day, phaeoMean, phaeoSd, 
         vol, timeOut) # selecting all like this order better

# metadata_with_normalized_flux <- left_join(normalized_flux, metadata)
metadata_with_normalized_flux <- left_join(metadata, normalized_flux)

write_csv(metadata_with_normalized_flux, '../processed_data/trap_metadata_with_flux.csv') # trap_metadata_normalized_flux_newMethod.csv
