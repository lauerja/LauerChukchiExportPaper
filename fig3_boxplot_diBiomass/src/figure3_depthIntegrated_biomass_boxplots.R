library(tidyverse)

allTrapData <- read_csv('../../data_directory/sediment_trap_data.csv')%>%
  mutate(trap_depth = Trap_Depth)

wcTrapData <- allTrapData %>%
  filter(trap_depth >= 8)%>%
  drop_na(inFront)

bridge_cam_ice <- read_csv('../../data_directory/metadata/observed_ice_edge_bridgecam_updated072425.csv')%>%
  mutate(iceObs = ice_obs, .keep = 'unused')

wcBiomass_allStations <- read_csv('../../data_directory/ctd_and_water_column/depth_integrated_biomass_full_wc_all_stations.csv')%>%
  left_join(., bridge_cam_ice)

wcBiomass_trapStations <- allTrapData %>%
  select(Deployment, transect, iceObs, station,
         dipoc_full_waterCol_mg_m2,
         dipon_full_waterCol_mg_m2,
         dichlMean_full_waterCol_mg_m2)%>%
  unique()

#### Statistical Tests ####

# No significant difference between MIZ and Ice, both greater than OW - CHL
summary(aov(dichlMean_full_waterCol_mg_m2~iceObs, wcBiomass_allStations))
TukeyHSD(aov(dichlMean_full_waterCol_mg_m2~iceObs, wcBiomass_allStations))

# No significant difference between MIZ and Ice, both greater than OW - POC
summary(aov(dipoc_full_waterCol_mg_m2~iceObs, wcBiomass_allStations))
TukeyHSD(aov(dipoc_full_waterCol_mg_m2~iceObs, wcBiomass_allStations))

# No significant difference between MIZ and Ice, both greater than OW - PON
summary(aov(dipon_full_waterCol_mg_m2~iceObs, wcBiomass_allStations))
TukeyHSD(aov(dipon_full_waterCol_mg_m2~iceObs, wcBiomass_allStations))

wcChl <- ggplot(wcBiomass_allStations,
                aes(x = iceObs,
                    y = dichlMean_full_waterCol_mg_m2))+
  geom_boxplot()+
  theme_classic()+
  labs(title = 'All Box Stations diChl a')
wcChl

wcC <- wcBiomass_allStations%>%
  # mutate(dipoc_full_waterCol_mg_m2=dipoc_full_waterCol_mg_m2 / 12)%>%
  ggplot(.,
         aes(x = iceObs,
             y = dipoc_full_waterCol_mg_m2))+
  geom_boxplot()+
  theme_classic()+
  labs(title = 'All Box Stations diPOC')
wcC

wcN <- ggplot(wcBiomass_allStations,
              aes(x = iceObs,
                  y = dipon_full_waterCol_mg_m2))+
  geom_boxplot()+
  theme_classic()+
  labs(title = 'All Box Stations diPON')
wcN

mean_wcBiomass_allStations <- wcBiomass_allStations%>%
  group_by(iceObs)%>%
  summarize(mean_diChl = mean(dichlMean_full_waterCol_mg_m2, na.rm = T),
            sd_diChl = sd(dichlMean_full_waterCol_mg_m2, na.rm = T),
            mean_diPOC = mean(dipoc_full_waterCol_mg_m2, na.rm = T),
            sd_diPOC = sd(dipoc_full_waterCol_mg_m2, na.rm = T),
            mean_diPON = mean(dipon_full_waterCol_mg_m2, na.rm = T),
            sd_diPON = sd(dipon_full_waterCol_mg_m2, na.rm = T))

#### Make One Boxplot with All Depth Integrals ####
wcTrapData_long <- wcTrapData %>%
  rename(
    `POC_full` = dipoc_full_waterCol_mg_m2,
    `PON_full` = dipon_full_waterCol_mg_m2,
    `Chl_full` = dichlMean_full_waterCol_mg_m2,
    `POC_above` = diPOC_aboveTrap_mg_m2,
    `PON_above` = diPON_aboveTrap_mg_m2,
    `Chl_above` = diChl_aboveTrap_mg_m2
  ) %>%
  select(Sample_Number, iceObs, Trap_Depth,
         POC_full, PON_full, Chl_full,
         POC_above, PON_above, Chl_above) %>%
  pivot_longer(
    cols = c(POC_full, POC_above, PON_full, PON_above, Chl_full, Chl_above),
    names_to = c("variable", "integration_depth"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    integration_depth_pretty = case_when(
      integration_depth == "full" ~ "Full\nwater column",
      integration_depth == "above" & Trap_Depth < 20 ~ "0 - 15 m\nAbove\nshallow trap",
      integration_depth == "above" & Trap_Depth >= 20 ~ "0 - 30 m\nAbove\ndeep trap",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(integration_depth), !is.na(value))


atC <- filter(wcTrapData_long, variable == 'POC')%>%
  ggplot(., aes(x = integration_depth_pretty,
                y = value,
                color = iceObs))+
  geom_boxplot()+
  scale_color_manual(values = c(
    "ice" = "#d63a41", 
    "miz" = "#FCCE50",
    "ow" = "#44C7FF"
  ))+
  annotate("text",
           x = -Inf, y = Inf,
           label = "b",
           hjust = -1, vjust = 1, size = 10) +
  ylab(bquote('Depth-integrated POC ('*mg~m^-2*')'))+
  xlab(NULL)+
  theme_classic() +
  theme(text = element_text(size = 16), #legend.position = 'none',
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)
  )
atC

atN <- filter(wcTrapData_long, variable == 'PON')%>%
  ggplot(., aes(x = integration_depth_pretty,
                y = value,
                # fill = iceObs
                color = iceObs
  ))+
  geom_boxplot()+
  scale_color_manual(values = c(
    "ice" = "#d63a41", 
    "miz" = "#FCCE50",
    "ow" = "#44C7FF"
  ))+
  annotate("text",
           x = -Inf, y = Inf,
           label = "c",
           hjust = -1, vjust = 1, size = 10) +
  ylab(bquote('Depth-integrated PON ('*mg~m^-2*')'))+
  xlab(NULL)+
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = 'none',
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)
  )
atN

atChl <- filter(wcTrapData_long, variable == 'Chl')%>%
  ggplot(., aes(x = integration_depth_pretty,
                y = value,
                color = iceObs))+
  geom_boxplot()+
  scale_color_manual(values = c(
    "ice" = "#d63a41", 
    "miz" = "#FCCE50",
    "ow" = "#44C7FF"
  ))+
  annotate("text",
           x = -Inf, y = Inf,
           label = "a",
           hjust = -1, vjust = 1, size = 10) +
  ylab(bquote('Depth-integrated Chl '*italic(a)*' ('*mg~m^-2*')'))+
  xlab(NULL)+
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = 'none',
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)
  )
atChl

#### Save Boxplots ####
ggsave('../plot/boxplot_diPOC_withKey.png', atC, width = 4.5, height = 6, dpi = 600)
ggsave('../plot/boxplot_diPOC.png', atC + theme(legend.position = 'none'), width = 4.5, height = 6, dpi = 600)
ggsave('../plot/boxplot_diPON.png', atN, width = 4.5, height = 6, dpi = 600)
ggsave('../plot/boxplot_diChl.png', atChl, width = 4.5, height = 6, dpi = 600)



depth_integration_table <- wcTrapData_long%>%
  group_by(iceObs, variable, integration_depth_pretty)%>%
  summarize(mean = mean(value),
            sd = sd(value))%>%
  pivot_wider(names_from = variable, values_from = c(mean, sd))


## Does Pattern Hold for Shallow Traps (yes to all) ####
depthStats_shallow <- wcTrapData_long %>%
  filter(integration_depth != 'full',
         Trap_Depth <= 20)%>%
  pivot_wider(names_from = variable, values_from = value)

kruskal.test(POC ~ iceObs, depthStats_shallow)
pairwise.wilcox.test(depthStats_shallow$POC, depthStats_shallow$iceObs)

kruskal.test(PON ~ iceObs, depthStats_shallow)
pairwise.wilcox.test(depthStats_shallow$PON, depthStats_shallow$iceObs)

kruskal.test(Chl ~ iceObs, depthStats_shallow)
pairwise.wilcox.test(depthStats_shallow$Chl, depthStats_shallow$iceObs)


### For Deep? ####
depthStats_deep <- wcTrapData_long %>%
  filter(integration_depth != 'full',
         Trap_Depth > 20)%>%
  pivot_wider(names_from = variable, values_from = value)

kruskal.test(POC ~ iceObs, depthStats_deep)
pairwise.wilcox.test(depthStats_deep$POC, depthStats_deep$iceObs)

kruskal.test(PON ~ iceObs, depthStats_deep)
pairwise.wilcox.test(depthStats_deep$PON, depthStats_deep$iceObs)

kruskal.test(Chl ~ iceObs, depthStats_deep)
pairwise.wilcox.test(depthStats_deep$Chl, depthStats_deep$iceObs)

## Full Water Column ####

kruskal.test(dipoc_full_waterCol_mg_m2 ~ iceObs, wcBiomass_trapStations)
pairwise.wilcox.test(wcBiomass_trapStations$dipoc_full_waterCol_mg_m2,
                     wcBiomass_trapStations$iceObs)

kruskal.test(dipon_full_waterCol_mg_m2 ~ iceObs, wcBiomass_trapStations)
pairwise.wilcox.test(wcBiomass_trapStations$dipon_full_waterCol_mg_m2,
                     wcBiomass_trapStations$iceObs)

kruskal.test(dichlMean_full_waterCol_mg_m2 ~ iceObs, wcBiomass_trapStations)
pairwise.wilcox.test(wcBiomass_trapStations$dichlMean_full_waterCol_mg_m2,
                     wcBiomass_trapStations$iceObs)

