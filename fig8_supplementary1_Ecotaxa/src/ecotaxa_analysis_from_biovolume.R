library(tidyverse)
library(wesanderson)

#### Read in STACANI and Other Metadata ####
# iceObserved <- read_csv('../data/bridge_cam_ice_labels_041124.csv')%>%
iceObserved <- read_csv('../../bridge_cam_ice_labels_updated072425.csv')%>%
  mutate(station = STNNBR, ice_class = BRIDGE_CAM_ICE_OBS)%>%
  select(station, ice_class)%>%
  mutate(ice_class = ifelse(station == 29, 'miz', ice_class))

stacani_ctd <- read_csv('../data/stacani.csv')%>%
  mutate(station = sta,
         cast = ca,
         depth = ni,
         sample_id = samp,
         .keep = 'unused')%>%
  left_join(iceObserved)

stacani_trap <- read_csv('../../trap_metadata/trap_stacani.csv')%>%
  mutate(sample_id = Sample_Number,
         deployment = Deployment,
         depth = Trap_Depth,
         depthCat = ifelse(Trap_Depth < 5, 'ice',
                           ifelse(Trap_Depth < 20, 'shallow', 'deep')),
         station_deploy = Deploy_Station,
         station_recover = Recover_Station,
         .keep = 'none') #%>%
# select(sample_id, deployment, station_deploy, station_recover, Deploy_Type, Trap_Depth, depthCat)%>%
# left_join(., mutate(iceObserved, station_deploy = station, ice_class_deploy = ice_class, .keep='unused'))%>%
# left_join(., mutate(iceObserved, station_recover = station, ice_class_recover = ice_class, .keep='unused'))

stacani_trap_deploy_focus <- stacani_trap%>%
  mutate(station = station_deploy)%>%
  left_join(., iceObserved)%>%
  select(sample_id, deployment, station, depth, depthCat, ice_class)

stacani_trap_recover_focus <- stacani_trap%>%
  mutate(station = station_recover)%>%
  left_join(., iceObserved)%>%
  select(sample_id, deployment, station, depth, depthCat, ice_class)

stacani_trap_overall <- stacani_trap%>%
  left_join(., mutate(iceObserved, station_deploy = station, ice_class_deploy = ice_class, .keep='unused'))%>%
  left_join(., mutate(iceObserved, station_recover = station, ice_class_recover = ice_class, .keep='unused'))

planktoscope_log <- read_csv('../data/PlanktoScope Log Sheet - Source Samples-2.csv')%>%
  mutate(sample_id = `(CTD/Ice/Experiment)\nArrigo Number`,
         station_chr = `Station ID (or Local Sampling Timestamp)`,
         cast = `(CTD/Net)\nCast Number`,
         station_numeric = ifelse(str_detect(station_chr, '^S') == T, 
                                  as.numeric(str_extract(station_chr, '(?<=S)\\d{3}')),
                                  NA),
         notes = `Sample collection notes`)

stacani_iceCore <- filter(planktoscope_log, `Sample Type` == 'Ice Core')%>%
  mutate(sample_id = as.numeric(sample_id),
         station = station_numeric)%>%
  select(sample_id, station, notes)

#### Read in Ecotaxa Data (processed already by read_in_and_compute_biovolume.R) ####
# ecotaxa_ctd <- read.csv('../results/ctd_ecotaxa_export_with_biovolume.csv')%>%
ecotaxa_ctd <- read.csv('../results/ctd_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  left_join(stacani_ctd)%>%
  mutate(sample_id = as.character(sample_id),
         sample_type = 'ctd',
         # depthCat = factor(ifelse(depth <= 5, '1-5',
         #                          ifelse(depth <= 10, '5-10',
         #                                 '10+')),
                           # levels = c('1-5', '5-10', '10+'),
                           # ordered = T),
         # sample_type = paste('ctd', depthCat, sep = '_'),
         unique_id = paste('ctdCast', cast, 'station', station, 'depth', depth, sep = '_'))%>%
  select(-cast, -depth) #, -depthCat)

# ecotaxa_iceCore <- read.csv('../results/iceCore_ecotaxa_export_with_biovolume.csv')%>%
ecotaxa_iceCore <- read.csv('../results/iceCore_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  left_join(stacani_iceCore)%>%
  mutate(sample_id = as.character(sample_id),
         ice_class = 'ice',
         sample_type = 'ice_core',
         unique_id = paste('ice_core_station', station))%>%
  select(-notes)

# ecotaxa_trap <- read.csv('../results/trap_ecotaxa_export_with_biovolume.csv')%>%
ecotaxa_trap <- read.csv('../results/trap_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  # group_by(sample_id, object_annotation_category)%>%
  # summarize(
  #   total_biovolume_plain_mLperL= sum(biovolume_plain_mLperL),
  #   total_biovolume_riddled_mLperL = sum(biovolume_riddled_mLperL),
  #   total_biovolume_ellipsoid_mLperL = sum(biovolume_ellipsoid_mLperL),
  #   object_count = n()
  # )%>%
  ungroup()%>%
  left_join(stacani_trap_deploy_focus)%>%
  mutate(sample_id = as.character(sample_id),
         sample_type = paste(depthCat, 'trap', sep = '_'),
         unique_id = paste(deployment, depthCat, 'trap', depth, sep = '_'))%>%
  select(-deployment, -depth, -depthCat)

#### Filter CTD and IceCore Data at trap stations, then make facet plot ####
trap_stations <- stacani_trap %>%
  filter(station_deploy != 15)%>% # Drop the test trap
  pull(station_deploy)%>%
  unique(.)

ecotaxa_trapStations <- bind_rows(ecotaxa_ctd, ecotaxa_iceCore)%>%
  bind_rows(., ecotaxa_trap)%>%
  filter(station %in% trap_stations,
         object_annotation_category != 'transparent')%>%
  mutate(sample_type = factor(sample_type,
                               levels = c("ice_core",
                                          "ice_trap",
                                          'ctd',
                                          # "ctd_1-5",
                                          # "ctd_5-10",
                                          # "ctd_10+",
                                          "shallow_trap",
                                          "deep_trap"),
                               ordered = TRUE))
# %>%
#   select(-biovolume_plain_um3_permL, -biovolume_riddled_um3_permL, -biovolume_ellipsoid_um3_permL)

trap_stations_sample_metadata <- ecotaxa_trapStations %>%
  select(sample_id, station, ice_class, sample_type)%>%
  unique()

general_annotation_categories <- ecotaxa_trapStations %>%
  select(object_annotation_category, general_annotation_category)%>%
  mutate(general_annotation_category = ifelse(object_annotation_category %in% c('single_pennate',
                                                                                'Gyrosigma',
                                                                                'Melosira arctica',
                                                                                'Odontella aurita',
                                                                                'Asteroplanus',
                                                                                'Nitzschia longissima',
                                                                                'Diploneis',
                                                                                'Pinnularia sp.',
                                                                                'Dinoflagellates',
                                                                                'Haslea',
                                                                                'Tintinnida'),
                                              'Other Species', general_annotation_category))%>%
  mutate(general_annotation_category = ifelse(general_annotation_category == 'single_pennate',
                                              'Other Species', general_annotation_category))%>%
  unique()

ecotaxa_trapStations_sampleLevel <- ecotaxa_trapStations%>%
  mutate(biovolume = biovolume_mL_L)%>%
  filter(object_annotation_category != 'bubble')%>%
  # filter(object_annotation_category != 'detritus')%>%
  # pivot_longer(
  #   cols = contains("biovolume"),          # Columns containing 'biovolume' to pivot
  #   names_to = "method",  # New column for biovolume-related names
  #   values_to = "biovolume" # New column for biovolume values
  # )%>%
  # mutate(method = ifelse(str_detect(method, 'riddled'), 'riddled',
  #                        ifelse(str_detect(method, 'ellipsoid'), 'ellipsoid',
  #                               'plain')))%>%
  select(sample_id, object_annotation_category, method, biovolume)%>%
  complete(sample_id, object_annotation_category, method, fill = list(biovolume = 0.0))%>%
  group_by(sample_id, method)%>%
  mutate(sample_biovolume = sum(biovolume),
         object_biovolume_fraction = biovolume / sample_biovolume)%>%
  ungroup()%>%
  group_by(sample_id, object_annotation_category, method)%>%
  summarize(category_biovolume_fraction = sum(object_biovolume_fraction),
            category_biovolume_in_sample = sum(biovolume),
            sample_biovolume = unique(sample_biovolume))%>%
  unique()%>%
  ungroup()%>%
  ## double check proves no difference in any row - uncomment to check
  # mutate(cat_biovolume_fraction_again = category_biovolume / sample_biovolume,
  #        double_check = round(category_biovolume_fraction, 0.0001) == round(cat_biovolume_fraction_again, 0.0001))
  left_join(., trap_stations_sample_metadata)%>%
  left_join(., general_annotation_categories)

ecotaxa_trapStations_sampleMeans <- ecotaxa_trapStations_sampleLevel %>%
  group_by(sample_type, ice_class, method)%>%
  mutate(mean_sample_bv = mean(sample_biovolume),
         sd_sample_bv = sd(sample_biovolume))%>%
  ungroup()%>%
  group_by(sample_type, ice_class, method, general_annotation_category)%>%
  # group_by(sample_type, ice_class, method, object_annotation_category)%>%
  summarize(mean_bv_fraction = mean(category_biovolume_fraction),
            sd_bv_fraction = sd(category_biovolume_fraction),
            mean_category_bv = mean(category_biovolume_in_sample),
            sd_category_bv = sd(category_biovolume_in_sample),
            mean_sample_bv = unique(mean_sample_bv),
            sd_sample_bv = unique(sd_sample_bv))%>%
  ungroup()

# fractional_biovolume_plot <- ggplot(filter(ecotaxa_trapStations_sampleMeans, method == 'riddled'),
                                    # aes(x = object_annotation_category,
fractional_biovolume_plot <- ecotaxa_trapStations_sampleMeans %>%
  # filter(method == 'riddled')%>%
  filter(general_annotation_category != 'bubble')%>%
  # filter(general_annotation_category != 'detritus')%>%
  mutate(ice_facet = case_when(
    ice_class == 'ice' ~ 'Consolidated Ice',
    ice_class == 'miz' ~ 'Marginal Ice Zone',
    ice_class == 'ow' ~ 'Open Water'),
    depth_facet = fct_recode(sample_type,
                             "Ice Core" = "ice_core",
                             "Ice Trap" = "ice_trap",
                             'Water Column' = 'ctd',
                             # '1-5 m CTD' = 'ctd_1-5',
                             # '5-10 m CTD' = 'ctd_5-10',
                             # '10+ m CTD' = 'ctd_10+',
                             'Shallow Trap' = 'shallow_trap',
                             'Deep Trap' = 'deep_trap'))%>%
  ggplot(.,
         aes(x = reorder(general_annotation_category, -mean_bv_fraction),
             y = mean_bv_fraction,
             fill = ice_class))+
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_bv_fraction - sd_bv_fraction,
                    ymax = mean_bv_fraction + sd_bv_fraction),
                position = position_dodge(width = 0.9),
                width = 0.25)+
  coord_cartesian(ylim = c(0, 1.0))+
  scale_fill_manual(values = wes_palette('Zissou1')[c(1, 3, 5)],
                  labels=c('ice' = 'Consolidated Ice',
                           'miz' = 'Marginal Ice Zone',
                           'ow' = 'Open Water'),
                  name = 'Ice Coverage')+
  labs(x=NULL, y = 'Mean Biovolume Fraction')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # facet_grid(vars(sample_type))
  # facet_grid(sample_type ~ ice_class)
  facet_grid(depth_facet ~ ice_facet)
fractional_biovolume_plot

ggsave("fractional_biovolume.png",
       plot = fractional_biovolume_plot,
       width = 12, height = 8)  

mean_biovolume_plot <- ecotaxa_trapStations_sampleMeans%>%
  filter(general_annotation_category != 'bubble')%>%
  filter(general_annotation_category != 'detritus')%>%
  filter(general_annotation_category != 'Thala')%>%
  mutate(ice_facet = case_when(
    ice_class == 'ice' ~ 'Consolidated Ice',
    ice_class == 'miz' ~ 'Marginal Ice Zone',
    ice_class == 'ow' ~ 'Open Water'),
    depth_facet = fct_recode(sample_type,
                             "Ice Core" = "ice_core",
                             "Ice Trap" = "ice_trap",
                             'Water Column' = 'ctd',
                             # '1-5 m CTD' = 'ctd_1-5',
                             # '5-10 m CTD' = 'ctd_5-10',
                             # '10+ m CTD' = 'ctd_10+',
                             'Shallow Trap' = 'shallow_trap',
                             'Deep Trap' = 'deep_trap'))%>%
  ggplot(.,
                             # aes(x = object_annotation_category,
                             aes(x = reorder(general_annotation_category, -mean_bv_fraction),
                                 y = mean_category_bv,
                                 fill = ice_class))+
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_category_bv - sd_category_bv,
                    ymax = mean_category_bv + sd_category_bv),
                position = position_dodge(width = 0.9),
                width = 0.25)+
  # coord_cartesian(ylim = c(0, 2.5))+
  coord_cartesian(ylim=c(0,0.5))+
  scale_fill_manual(values = wes_palette('Zissou1')[c(1, 3, 5)],
                    labels=c('ice' = 'Consolidated Ice',
                             'miz' = 'Marginal Ice Zone',
                             'ow' = 'Open Water'),
                    name = 'Ice Coverage')+
  labs(x=NULL, y = bquote('Mean Biovolume ('*mg~mL^-1*')'))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # facet_grid(vars(sample_type))
  facet_grid(depth_facet ~ ice_facet)
mean_biovolume_plot

ggsave("mean_biovolume.png",
       plot = mean_biovolume_plot,
       width = 12, height = 8)  

sample_biovolume_plot <- filter(ecotaxa_trapStations_sampleMeans) %>% #, method == 'riddled')%>%
  select(sample_type, ice_class, mean_sample_bv, sd_sample_bv)%>%
  unique()%>%
  ggplot(.,
         aes(x = sample_type,
             y = mean_sample_bv,
             fill = ice_class))+
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_sample_bv - sd_sample_bv,
                    ymax = mean_sample_bv + sd_sample_bv),
                position = position_dodge(width = 0.9),
                width = 0.25)+
  scale_fill_manual(values = wes_palette('Zissou1')[c(1, 3, 5)],
                     labels=c('ice' = 'Consolidated Ice',
                              'miz' = 'Marginal Ice Zone',
                              'ow' = 'Open Water'),
                     name = 'Ice Coverage')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # facet_grid(vars(sample_type))
sample_biovolume_plot

#### Old Method - Sum of Biovolume in Sample, mean across samples ####
ecotaxa_trapStations <- bind_rows(ecotaxa_ctd, ecotaxa_iceCore)%>%
  bind_rows(., ecotaxa_trap)%>%
  filter(station %in% trap_stations,
         object_annotation_category != 'transparent')%>%
  group_by(sample_type, ice_class, object_annotation_category)%>%
  summarise(mean_biovol = mean(biovolume_mL_L),
            sd_biovol = sd(biovolume_mL_L))%>%
  # summarise(mean_biovol = mean(total_biovolume_plain_mLperL),
  #           mean_biovol_excluded = mean(total_biovolume_riddled_mLperL),
  #           mean_biovol_ellipsoid = mean(total_biovolume_ellipsoid_mLperL),
  #           sd_biovol = sd(total_biovolume_plain_mLperL),
  #           sd_biovol_excluded = sd(total_biovolume_riddled_mLperL),
  #           sd_biovol_ellipsoid = sd(total_biovolume_ellipsoid_mLperL),
  #           mean_object_count = mean(object_count),
  #           sd_object_count = sd(object_count))%>%
  ungroup()%>%
  mutate(sample_type = factor(sample_type,
                              levels = c("ice_core",
                                         "ice_trap",
                                         "ctd_1-5",
                                         "ctd_5-10",
                                         "ctd_10+",
                                         "shallow_trap",
                                         "deep_trap"),
                              ordered = TRUE))

ggplot(filter(ecotaxa_trapStations, ice_class == 'ice'),
       aes(x = reorder(object_annotation_category, -mean_biovol),
           y = mean_biovol))+
           # y = mean_biovol_ellipsoid))+
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_biovol - sd_biovol,
                    ymax = mean_biovol + sd_biovol),
  # geom_errorbar(aes(ymin = mean_biovol_ellipsoid - sd_biovol_ellipsoid,
  #                   ymax = mean_biovol_ellipsoid + sd_biovol_ellipsoid),
                position = position_dodge(width = 0.9),
                width = 0.25)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(vars(sample_type))

ggplot(filter(ecotaxa_trapStations, ice_class == 'ice'),
       aes(x = reorder(object_annotation_category, -mean_biovol),
           y = mean_biovol))+
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_biovol - sd_biovol,
                    ymax = mean_biovol + sd_biovol),
                position = position_dodge(width = 0.9),
                width = 0.25)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(vars(sample_type))

# ggplot(filter(ecotaxa_trapStations, ice_class == 'ice'),
#        aes(x = reorder(object_annotation_category, -mean_biovol),
#            y = mean_object_count))+
#   geom_bar(stat = 'identity', position = position_dodge()) +
#   geom_errorbar(aes(ymin = mean_object_count - sd_object_count,
#                     ymax = mean_object_count + sd_object_count),
#                 position = position_dodge(width = 0.9),
#                 width = 0.25)+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   facet_grid(vars(sample_type))


#### Trap Sample Mean Biovolume ####
ice_classes <- select(ecotaxa_trap, sample_id, unique_id, ice_class)%>% unique()

sample_mean_biovolume_plot <- ecotaxa_trap %>%
  # mutate(object_annotation_category = as.factor(object_annotation_category),
  #        sample_id = as.factor(sample_id))  %>%
  filter(object_annotation_category != 'transparent',
         object_annotation_category != 'detritus',
         object_annotation_category != 'bubble')%>%
  group_by(object_annotation_category, sample_id)%>%
  # group_by(object_annotation_category, unique_id)%>%
  summarise(total_biovol = sum(biovolume_mL_L))%>%
  ungroup()%>%
  complete(object_annotation_category, sample_id,
           fill = list(total_biovol = 0))%>%
  left_join(., ice_classes)%>%
  group_by(object_annotation_category, ice_class)%>%
  summarise(mean_biovol = mean(total_biovol),
            sd_biovol = sd(total_biovol))%>%
  ggplot(., aes(x = reorder(object_annotation_category, -mean_biovol),
                y = mean_biovol))+
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_biovol - sd_biovol,
                    ymax = mean_biovol + sd_biovol),
                position = position_dodge(width = 0.9),
                width = 0.25)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(vars(ice_class), scale = 'free_y')
sample_mean_biovolume_plot

