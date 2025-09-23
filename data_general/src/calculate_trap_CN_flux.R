library(tidyverse)
library(lubridate)

## Independent Flux Calculation to double check numbers

#### Useful Constants ####
molMassN = 14.0067 #g/mol
molMassC = 12.011 #g/mol
trapSurfArea = 0.05^2 * pi #m^2 # 10cm diameter = 0.05m radius. Area = pi*r^2 

ea_data <- read.csv('../processed_data//all_sedimentTrap_EA_data.csv')%>%
  mutate(Sample = str_replace(Sample, '([ds])$', toupper),
         Sample = str_replace_all(Sample, '^TO(?=\\d{2}[dDsS])', 'T0'))


trap_metadata <- read_csv('../data_station_meta/trap_metadata.csv')%>%
  filter(is.na(Sample_Number) == F)

trapSlim <- trap_metadata %>%
  mutate(Sample = Sample_Number,
         totalVol_L = as.numeric(Recovery_total_volume_L),
         deploy = mdy_hm(Deploy_DateTime_UTC),
         recover = mdy_hm(Recover_DateTime_UTC),
         timeOut_days = as.numeric(recover - deploy))%>%
  select(Sample, totalVol_L, vol_filtered_L, timeOut_days, vol_Flag)%>%
  left_join(., select(ea_data, Sample, Run, c_umol, n_umol))%>%
  mutate(c_umol = ifelse(c_umol <= 0, 0.0, c_umol),
         n_umol = ifelse(n_umol <= 0, 0.0, n_umol),
         c_negFlag = ifelse(c_umol <= 0, T, F),
         n_negFlag = ifelse(n_umol <= 0, T, F))%>%
  mutate(c_inTrap_umol = c_umol / vol_filtered_L * totalVol_L, #umol /L * L = umol
         c_inTrap_mg = c_inTrap_umol * molMassC * 0.001, # molMass in g/mol = 0.001 mg/umol
         fluxC = c_inTrap_mg / trapSurfArea / timeOut_days, # gives carbon flux in mg/m^2/day
         n_inTrap_umol = n_umol / vol_filtered_L * totalVol_L,
         n_inTrap_mg = n_inTrap_umol * molMassN * 0.001, # molMass in g/mol = 0.001 mg/umol
         fluxN = n_inTrap_mg / trapSurfArea / timeOut_days,
         CNratio = c_inTrap_umol / n_inTrap_umol)

forCSV <- mutate(trapSlim, Sample_Number = Sample)%>%
  select(Sample_Number, timeOut_days, fluxC, fluxN, CNratio)%>%
  right_join(., trap_metadata)

## Data matches - minor shifts because i dont use drift correction nwo

write_csv(forCSV, '../processed_data/trap_metadata_CNflux.csv')
