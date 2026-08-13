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
  geom_boxplot(size = 1)+
  theme_classic()+
  labs(title = 'All Box Stations diChl a')
wcChl

wcC <- wcBiomass_allStations%>%
  # mutate(dipoc_full_waterCol_mg_m2=dipoc_full_waterCol_mg_m2 / 12)%>%
  ggplot(.,
         aes(x = iceObs,
             y = dipoc_full_waterCol_mg_m2))+
  geom_boxplot(size = 1)+
  theme_classic()+
  labs(title = 'All Box Stations diPOC')
wcC

wcN <- ggplot(wcBiomass_allStations,
              aes(x = iceObs,
                  y = dipon_full_waterCol_mg_m2))+
  geom_boxplot(size = 1)+
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
                fill = iceObs,
                color = iceObs))+
  geom_boxplot(alpha = 0.4)+
  scale_fill_manual(values = c(
    "ice" = "#d63a41",
    # "miz" = "#FCCE50",
    "miz" = "#FFA500",
    "ow" = "#44C7FF"
  ),
  labels=c('ice' = 'UI',
           'miz' = 'MIZ',
           'ow' = 'OW'))+
  scale_color_manual(values = c(
    "ice" = "#d63a41",
    # "miz" = "#FCCE50",
    "miz" = "#FFA500",
    "ow" = "#44C7FF"
  ),
  labels=c('ice' = 'UI',
           'miz' = 'MIZ',
           'ow' = 'OW'))+
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
                fill = iceObs,
                color = iceObs))+
  geom_boxplot(alpha = 0.4)+
  scale_fill_manual(values = c(
    "ice" = "#d63a41",
    # "miz" = "#FCCE50",
    "miz" = "#FFA500",
    "ow" = "#44C7FF"
  ),
  labels=c('ice' = 'UI',
           'miz' = 'MIZ',
           'ow' = 'OW'))+
  scale_color_manual(values = c(
    "ice" = "#d63a41",
    # "miz" = "#FCCE50",
    "miz" = "#FFA500",
    "ow" = "#44C7FF"
  ),
  labels=c('ice' = 'UI',
           'miz' = 'MIZ',
           'ow' = 'OW'))+
  annotate("text",
           x = -Inf, y = Inf,
           label = "c",
           hjust = -1, vjust = 1, size = 10) +
  ylab(bquote('Depth-Integrated PN ('*mg~m^-2*')'))+
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
                fill = iceObs,
                color = iceObs))+
  geom_boxplot(alpha = 0.4)+
  scale_fill_manual(values = c(
    "ice" = "#d63a41",
    # "miz" = "#FCCE50",
    "miz" = "#FFA500",
    "ow" = "#44C7FF"
  ),
  labels=c('ice' = 'UI',
           'miz' = 'MIZ',
           'ow' = 'OW'))+
  scale_color_manual(values = c(
    "ice" = "#d63a41",
    # "miz" = "#FCCE50",
    "miz" = "#FFA500",
    "ow" = "#44C7FF"
  ),
  labels=c('ice' = 'UI',
           'miz' = 'MIZ',
           'ow' = 'OW'))+
  annotate("text",
           x = -Inf, y = Inf,
           label = "a",
           hjust = -1, vjust = 1, size = 10) +
  ylab(bquote('Depth-integrated Chl '*italic(a)*' ('*mg~m^-2*')'))+
  xlab(NULL)+
  theme_classic() +
  theme(
    text = element_text(size = 16),
    legend.position = 'none',
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



#### Combine Plots with Patchwork ####
library(patchwork)

atN_legend_centered <- atN +
  labs(fill = NULL,
       color = NULL)+
  theme(    legend.position = "inside",
            legend.position.inside = c(0.4, 0.9),
            # legend.key.size = unit(0.05, "npc"),
            legend.text = element_text(size = 22.5))
atN_legend_centered


atC_noLegend <- atC + theme(legend.position = 'none')

patchPlot <- atChl + atC_noLegend + atN_legend_centered
patchPlot

ggsave("../plot/boxplot_di_multipanel.png",
       plot = patchPlot,
       width = 13.5, height = 6, dpi = 600,
       bg = "transparent")


