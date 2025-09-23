library(tidyverse)

trap_data <- read_csv('../processed_data/all_trap_data_for_figs_3_4_5_6_7.csv')

trap_metadata <- read_csv('../data_station_meta/trap_metadata.csv')

# t1 columns: Deployment, Trap_Depth, Lat, Lon, date, recover date, recover ice, Lateral Density Grad
# t2 columns: Deployment, Trap_Depth, fluxChl, fluxC, fluxN
# note: pivot Trap_Depth, LateralDensityGrad wider so shallow and deep are separate columns
table_data <- trap_data %>%
  left_join(., trap_metadata)%>%
  select(Deployment, Trap_Depth, depthCat,
         LATITUDE, LONGITUDE,
         Deploy_Date_Local, Recover_Date_Local,
         iceObsRecover, LateralDensityGrad,
         fluxChl, fluxC, fluxN)%>%
  mutate(depthCat_simple = ifelse(depthCat %in% c('shallow', 'iceTethered'), 'Shallow', 'Deep'),
         .keep = 'unused')%>%
  pivot_wider(names_from = depthCat_simple,
              values_from = c(LATITUDE, LONGITUDE, Trap_Depth, LateralDensityGrad, fluxChl, fluxC, fluxN))


table_2 <- trap_data %>%
  left_join(., trap_metadata)%>%
  select(Deployment, Trap_Depth,
         LATITUDE, LONGITUDE,
         Deploy_Date_Local, Recover_Date_Local,
         iceObsRecover, LateralDensityGrad)
