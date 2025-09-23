library(tidyverse)

data <- read_csv('../../data_general/processed_data/all_trap_data_for_figs_3_4_5_6_7.csv')

data_slim <- data %>%
  select(Sample_Number, Deployment, depth, depthCat, transect, iceObs,
         average_drift_speed_km_hr, average_drift_speed_cm_s, LateralDensityGrad, inFront,
         diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
         dichlMean_full_waterCol_mg_m2, dipoc_full_waterCol_mg_m2, dipon_full_waterCol_mg_m2,
         fluxChl, fluxPhaeo, fluxC, fluxN, CNratio)

data_slim_wcOnly <- filter(data_slim, depth >= 8)

data_slim_tetheredOnly <- filter(data_slim, depth < 8)


overall_mean <- data_slim %>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))

#### Means for Reporting ####
mean_by_ice <- mean_sd_by_ice <- data_slim %>%
  mutate(group = iceObs)%>%
  group_by(group) %>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Ice Coverage All Traps')

mean_by_ice_wcOnly <- data_slim_wcOnly %>%
  mutate(group = iceObs)%>%
  group_by(group) %>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Ice Coverage Water Column Traps')

mean_by_ice_tetheredOnly <- data_slim_tetheredOnly %>%
  mutate(group = iceObs)%>%
  group_by(group) %>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Ice-Tethered Traps (<8m) Only')
  

mean_by_front <- data_slim %>%
  mutate(inFront = ifelse(Sample_Number == 'T011D', F, inFront))%>%
  # mutate(group = ifelse(inFront == T, 'In Front', 'Out of Front'))%>%
  mutate(group = ifelse(depthCat == 'iceTethered', 'zzzIce-Tethered',
                        ifelse(inFront == T, 'Front', 'Out of Front')))%>%
  group_by(group)%>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Front All Data')

mean_by_front_and_ice <- data_slim %>%
  mutate(inFront = ifelse(Sample_Number == 'T011D', F, inFront))%>%
  mutate(group = ifelse(inFront == T, 'In Front', 'Out of Front'),
         group = paste(group, iceObs, sep = ' '))%>%
  # mutate(front = ifelse(depthCat == 'iceTethered', 'Ice-Tethered',
  #                       ifelse(inFront == T, 'Front', 'Out of Front')))%>%
  group_by(group)%>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Front All Data - Separated by Ice')

mean_by_front_tetheredSeparate <- data_slim %>%
  # mutate(group = ifelse(inFront == T, 'In Front', 'Out of Front'))%>%
  mutate(group = ifelse(depthCat == 'iceTethered', 'Ice-Tethered',
                        ifelse(inFront == T, 'Front', 'Out of Front')))%>%
  group_by(group)%>%
  summarise(across(
    c(LateralDensityGrad, depth,
      diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2,
      fluxChl, fluxPhaeo, fluxC, fluxN, CNratio),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Front Ice-Tethered Separate')

allSummaryStats <- bind_rows(mean_by_front, mean_by_front_tetheredSeparate,
                             mean_by_front_and_ice, mean_by_ice,
                             mean_by_ice_tetheredOnly, mean_by_ice_wcOnly)%>%
  select(GroupVar, everything())

write_csv(allSummaryStats, '../summary_statistics.csv')


#### Whole WC Depth Integrated Means (must do only 1 per station or the SD will be thrown off) ####
di_mean_by_ice_trapDepth <- mean_sd_by_ice <- data_slim %>%
  mutate(group = paste(depthCat, iceObs, sep = ' '))%>%
  select(group, diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2)%>%
  unique()%>%
  group_by(group) %>%
  summarise(across(
    c(diChl_aboveTrap_mg_m2, diPOC_aboveTrap_mg_m2, diPON_aboveTrap_mg_m2),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Ice Coverage All Traps')

#### Whole WC Depth Integrated Means (must do only 1 per station or the SD will be thrown off) ####
di_mean_by_ice <- mean_sd_by_ice <- data_slim %>%
  mutate(group = iceObs)%>%
  select(group, dichlMean_full_waterCol_mg_m2, dipoc_full_waterCol_mg_m2, dipon_full_waterCol_mg_m2)%>%
  unique()%>%
  group_by(group) %>%
  summarise(across(
    c(dichlMean_full_waterCol_mg_m2, dipoc_full_waterCol_mg_m2, dipon_full_waterCol_mg_m2),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))%>%
  mutate(GroupVar = 'Ice Coverage All Traps')

#### Tables 1 and 2 ####
lat_lon <- data %>%
  select(Deployment, LATITUDE, LONGITUDE)%>%
  drop_na()%>%
  unique()

table_metadata <- read_csv('../../data_general/data_station_meta/trap_metadata.csv')%>%
  select(Deployment, Deploy_Date_Local, Recover_Date_Local) %>%
  unique()%>%
  right_join(., lat_lon)%>%
  select(Deployment, LATITUDE, LONGITUDE, Deploy_Date_Local, Recover_Date_Local)%>%
  drop_na()%>%
  unique()

table_data <- data %>%
  mutate(depthCat_simple = ifelse(depthCat %in% c('shallow', 'iceTethered'), 'Shallow', 'Deep'),
         .keep = 'unused')%>%
  select(Deployment, Trap_Depth, depthCat_simple,
         iceObs, iceObsRecover, LateralDensityGrad,
         fluxChl, fluxC, fluxN)%>%
  pivot_wider(names_from = depthCat_simple,
              values_from = c(Trap_Depth, LateralDensityGrad, fluxChl, fluxC, fluxN))%>%
  left_join(., table_metadata)%>%
  mutate(iceTethered = ifelse(Trap_Depth_Shallow <= 8, 'Ice-Tethered', 'Water Col'))


# t1 columns: Deployment, Trap_Depth, Lat, Lon, date, recover date, recover ice, Lateral Density Grad
# t2 columns: Deployment, Trap_Depth, fluxChl, fluxC, fluxN
# note: pivot Trap_Depth, LateralDensityGrad wider so shallow and deep are separate columns
table1 <- table_data %>%
  select(iceObs, iceTethered, Deployment, Trap_Depth_Shallow, Trap_Depth_Deep,
         LATITUDE, LONGITUDE, Deploy_Date_Local, Recover_Date_Local,
         iceObsRecover, LateralDensityGrad_Shallow, LateralDensityGrad_Deep)

table2 <- table_data %>%
  select(iceObs, iceTethered, Deployment, Trap_Depth_Shallow, Trap_Depth_Deep,
         fluxChl_Shallow, fluxChl_Deep,
         fluxC_Shallow, fluxC_Deep,
         fluxN_Shallow, fluxN_Deep)

#### Write CSV files ####
write_csv(table1, '../tables/table1.csv')
write_csv(table2, '../tables/table2.csv')
