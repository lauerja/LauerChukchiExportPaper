library(tidyverse)
library(patchwork)

#### Useful Constants ####
# Molar Masses of Nitrogen and Carbon (g/mol = mg/mmol = ug/umol)
molMassN = 14.0067
molMassC = 12.011

#### Metadata ####
# distances <- read.csv('../data/station_distance_key.csv')%>%
distances <- read_csv('../../data_directory/metadata/station_metadata_laps.csv')%>%
  filter(cast == 1)%>%
  mutate(section = str_extract(section_name_full, '^.+(?=-)'))%>%
  drop_na(section)%>%
  group_by(section)%>%
  mutate(
    # dist_km = max(dist_km) - dist_km,
    Station = station)%>%
  ungroup()%>%
  select(Station, dist_km)%>%
  unique()

station_meta <- read_delim('../../data_directory/metadata/ctd_stations_SKQ202310S_ToTS.txt')%>%
  mutate(sectFull = `Section Name`,
         wcDepth = `Bottom Depth`,
         Station = as.numeric(str_extract(StationCast, '(?<=S)\\d{3}')),
         Cast = as.numeric(str_extract(StationCast, '(?<=C)\\d{2}')),
         lineFull = str_extract(sectFull, '^[:alnum:]+'),
         line = ifelse(str_detect(lineFull, '^DBO') == T,
                       lineFull,
                       ifelse(str_detect(lineFull, '^T') == T,
                              'trap',
                              str_extract(lineFull, '^\\D+'))),
         lineStation = ifelse(line == 'trap',
                              as.numeric(str_extract(lineFull, '\\d{3}')),
                              as.numeric(str_extract(sectFull, '\\d{1,3}(?=($|\\s/))'))),
         lap = ifelse(str_detect(line, '^H') == T,
                      str_extract(lineFull, '\\d$'),
                      NA))%>%
  filter(Cast == 1)%>%
  select(Station, line, lineStation, lap, Lat, Lon, Date, Time, wcDepth)%>%
  left_join(., distances)

#### Sediment Chl Data ####
sedimentChl_allLaps <- read_csv('../../data_directory/sediments/sediment_chl.csv')%>%
  mutate(Station = as.numeric(Station))%>%
  select(Station, chlMass_mg_per_m2)%>%
  left_join(., station_meta)

sedimentChl <- sedimentChl_allLaps %>%
  group_by(line, lineStation)%>%
  summarize(chlmean = mean(chlMass_mg_per_m2, na.rm=T),
            chlsd = sd(chlMass_mg_per_m2, na.rm=T))

#### Data ####
sampleMasses <- read_csv('../../data_directory/sediments/sediment_sample_masses.csv')%>%
  mutate(runDate = as.numeric(runDate),
         acidified = ifelse(Acidified == 'acid', T, F),
         SedMass_mg = tin_subSampleMass_mg,
         Station = ArrigoNumber)%>%
  replace_na(list(acidified = F))

sampleDensity <- sampleMasses %>%
  select(Station, replicate, mass_emptyFalcon_g, `sample_density_g/m2`)%>%
  drop_na(mass_emptyFalcon_g)%>%
  filter(mass_emptyFalcon_g != 'not_yet_measured')%>%
  mutate(sampleDensity_g_per_m2 = as.numeric(`sample_density_g/m2`), .keep = 'unused')

eaData <- read_csv('../../data_directory/sediments/sediment_carbon_and_nitrogen_data.csv',
                   col_types = cols(Sample = col_character()))%>%
  mutate(c_mg = 10^-3 * c_umol * molMassC,
         n_mg = 10^-3 * n_umol * molMassN,
         cn_ratio = c_umol / n_umol,
         runDate = as.numeric(str_extract(Run, '^\\d+')))

sampleMetadata <- sampleMasses %>%
  select(runDate, Station, replicate, Sample,
         acidified, arrigo_replicate_acid, carbon_measurement,
         SedMass_mg)%>%
  left_join(., sampleDensity)

everything <- left_join(eaData, sampleMetadata)%>%
  mutate(c_gperg = c_mg / SedMass_mg,
         n_gperg = n_mg / SedMass_mg,
         c_gperm2 = c_gperg * sampleDensity_g_per_m2,
         n_gperm2 = n_gperg * sampleDensity_g_per_m2)

important_parts <- everything %>%
  select(Station, replicate, carbon_measurement, c_gperg, c_gperm2, n_gperg, n_gperm2, cn_ratio)%>%
  left_join(., station_meta)

total_carbon <- important_parts %>%
  filter(carbon_measurement == 'total')

organic_carbon <- important_parts %>%
  filter(carbon_measurement == 'organic')

cn_lineStation <- important_parts %>%
  filter(cn_ratio <= 50)%>%
  drop_na(line)%>%
  group_by(line, lineStation, carbon_measurement)%>%
  summarize(C_mean_gperm2 = mean(c_gperm2, na.rm=T),
            C_sd_gperm2 = sd(c_gperm2, na.rm=T),
            N_mean_gperm2 = mean(n_gperm2, na.rm=T),
            N_sd_gperm2 = sd(n_gperm2, na.rm=T),
            C_mean_gperg = mean(c_gperg, na.rm=T),
            C_sd_gperg = sd(c_gperg, na.rm=T),
            N_mean_gperg = mean(n_gperg, na.rm=T),
            N_sd_gperg = sd(n_gperg, na.rm=T),
            cn_ratio_mean = mean(cn_ratio, na.rm=T),
            cn_ratio_sd = sd(cn_ratio, na.rm=T),
            dist_sd = sd(dist_km, na.rm = T),
            dist_km = mean(dist_km, na.rm = T))

#### Bar Charts ####

cBar <- filter(cn_lineStation, carbon_measurement == 'organic')%>%
  # ggplot(., aes(x = lineStation, y = C_mean_gperm2))+ #, fill = carbon_measurement))+
  ggplot(., aes(x = dist_km, y = C_mean_gperm2))+
  # geom_bar(stat = 'identity',
  #          position = position_dodge(0.9),
  #          width = 5)+
  geom_point()+
  geom_errorbar(aes(ymin = C_mean_gperm2 - C_sd_gperm2,
                    ymax = C_mean_gperm2 + C_sd_gperm2),
                width = 0.25,
                position = position_dodge(0.9))+
  scale_x_reverse()+
  facet_wrap(~line)+
  labs(x = 'Distance (km)',
       y = bquote('Sediment Organic Carbon ('*g~m^-2*')'))+
  theme_classic()
cBar

nBar <- filter(cn_lineStation, carbon_measurement == 'organic')%>%
# nBar <- cn_lineStation%>%
  # ggplot(., aes(x = lineStation, y = N_mean_gperm2))+ #, fill = carbon_measurement))+
  ggplot(., aes(x = dist_km, y = N_mean_gperm2))+
  geom_point()+
  # geom_bar(stat = 'identity',
  #          position = position_dodge(0.9),
  #          width = 10)+
  geom_errorbar(aes(ymin = N_mean_gperm2 - N_sd_gperm2,
                    ymax = N_mean_gperm2 + N_sd_gperm2),
                width = 0.25,
                position = position_dodge(0.9))+
  scale_x_reverse()+
  facet_wrap(~line)+
  labs(x = 'Distance (km)',
       y = bquote('Sediment Nitrogen ('*g~m^-2*')'))+
  theme_classic()
nBar

ratioBar <- filter(cn_lineStation, carbon_measurement == 'organic')%>%
  ggplot(., aes(x = dist_km, y = cn_ratio_mean))+ #, fill = carbon_measurement))+
  geom_point()+
  # geom_bar(stat = 'identity',
  #          position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = cn_ratio_mean - cn_ratio_sd,
                    ymax = cn_ratio_mean + cn_ratio_sd),
                width = 0.25,
                position = position_dodge(0.9))+
  scale_x_reverse()+
  facet_wrap(~line)+
  labs(x = 'Distance (km)',
       y = bquote('Carbon to Nitrogen Ratio (g C '*g^-1*' N)'))+
  theme_classic()
ratioBar


c_gpergBar <- ggplot(cn_lineStation, aes(x = lineStation,
                                         y = C_mean_gperg,
                                         fill = carbon_measurement))+
  geom_bar(stat = 'identity',
           position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = C_mean_gperg - C_sd_gperg,
                    ymax = C_mean_gperg + C_sd_gperg),
                width = 0.25,
                position = position_dodge(0.9))+
  scale_x_reverse()+
  facet_wrap(~line)+
  theme_classic()
c_gpergBar

n_gpergBar <- ggplot(cn_lineStation, aes(x = lineStation, y = N_mean_gperg, fill = carbon_measurement))+
  geom_bar(stat = 'identity',
           position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = N_mean_gperg - N_sd_gperg,
                    ymax = N_mean_gperg + N_sd_gperg),
                width = 0.25,
                position = position_dodge(0.9))+
  scale_x_reverse()+
  facet_wrap(~line)+
  theme_classic()
n_gpergBar

chlBar <- left_join(sedimentChl, select(important_parts, line, lineStation, dist_km))%>%
  filter(line %in% c('HR', 'HRE'))%>%
  ggplot(., 
         aes(x = dist_km, y = chlmean))+
# chlBar <- ggplot(filter(sedimentChl, line %in% c('HR', 'HRE')), aes(x = lineStation, y = chlmean))+
  # geom_bar(stat = 'identity')+
  geom_point()+
  geom_errorbar(aes(ymin = chlmean - chlsd,
                    ymax = chlmean + chlsd),
                width = 0.25)+
  scale_x_reverse()+
  facet_wrap(~line)+
  theme_classic()
chlBar

compare_gperm2_plots <- nBar / cBar / chlBar
compare_gperm2_plots

# compare_gperg_plots <- n_gpergBar / c_gpergBar / chlBar
compare_gperg_plots <- nBar / cBar / ratioBar
compare_gperg_plots

compare_chl_ratio_plots <- ratioBar / chlBar
compare_chl_ratio_plots

#### Sed Chlorophyll Statistical Test ####
sedChl_forStats <- sedimentChl_allLaps %>% 
  mutate(location = paste(line, lineStation),
         inFront = ifelse(lineStation %in% c(3, 4, 5, 6), T, F))%>%
  filter(line == 'HRE')

locationAnova <- aov(chlMass_mg_per_m2 ~ location, data = sedChl_forStats)
summary(locationAnova)
TukeyHSD(locationAnova)

frontTtest <- t.test(chlMass_mg_per_m2~inFront, data = sedChl_forStats)
frontTtest

#### Means for Reporting ####
line_and_front_chlMeans <- sedimentChl_allLaps %>% 
  mutate(location = paste(line, lineStation),
         inFront = ifelse(location %in% c('HRE 3', 'HRE 4', 'HRE 5', 'HRE 6'), T, F))%>%
  group_by(line, inFront)%>%
  summarize(meanSedChl_mg_per_m2 = mean(chlMass_mg_per_m2),
            sdSedChl_mg_per_m2 = sd(chlMass_mg_per_m2))

line_and_front_CN_means <- important_parts%>% 
  mutate(location = paste(line, lineStation),
         inFront = ifelse(location %in% c('HRE 3', 'HRE 4', 'HRE 5', 'HRE 6'), T, F))%>%
  filter(carbon_measurement == 'organic')%>%
  filter(cn_ratio <= 25,
         cn_ratio >= 0)%>%
  # group_by(line, inFront)%>%
  # group_by(line)%>%
  summarize(meanC_g_per_m2 = mean(c_gperm2),
            sdC_g_per_m2 = sd(c_gperm2),
            meanN_g_per_m2 = mean(n_gperm2),
            sdN_g_per_m2 = sd(n_gperm2),
            mean_CN = mean(cn_ratio),
            sd_CN = sd(cn_ratio))
