library('tidyverse')

## Changes column names and pulls trap station chl out of all ctd chl

chl_data_path <- '../data_measured_chl/ctd_chl.csv'
chl_col_names <- c('sampleNum', 'replicate', 'station', 'depth_m', 'blank',
                   'f0', 'fa', 'volFiltered_ml', 'volExtracted_ml', 'chla_ug_L',
                   'phaeo_ug_L', 'chlMean', 'chlStdev', 'chlCV', 'phaeoMean',
                   'phaeoStdev', 'phaeoCV')

chl_data <- read.csv(chl_data_path, skip = 1, col.names = chl_col_names)%>%
  mutate(ctd_TF = str_detect(depth_m, '^\\d'),
         trap_TF = str_detect(depth_m, 'Trap'))%>%
  filter(ctd_TF == T | trap_TF == T)%>%
  group_by(sampleNum)%>%
  mutate(chlMean = mean(na.omit(chla_ug_L)),
         phaeoMean = mean(na.omit(phaeo_ug_L)),
         chlSd = sd(na.omit(chla_ug_L)),
         phaeoSd = sd(na.omit(phaeo_ug_L)))%>%
  ungroup()

ctd_chl <- filter(chl_data, ctd_TF == T)%>%
  mutate(depth_m = as.numeric(depth_m))%>%
  select(-trap_TF, -ctd_TF)
  # select(sampleNum, replicate, station, depth_m, chlaConc, 
  #        phaeoConc, chlMean, chlSd, phaeoMean, phaeoSd)

trap_chl <- filter(chl_data, trap_TF == T)%>%
  # mutate(depth_m = as.numeric(depth_m))%>%
  select(-trap_TF, -ctd_TF)

write_csv(ctd_chl, '../data_measured_chl/ctd_chl_processed.csv')

write_csv(trap_chl, '../data_measured_chl/trap_chl_processed.csv')
