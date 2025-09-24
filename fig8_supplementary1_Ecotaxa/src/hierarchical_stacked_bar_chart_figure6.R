library(tidyverse)
library(scales)

#### Read in STACANI and Other Metadata ####
iceObserved <- read_csv('../../data_directory/metadata/observed_ice_edge_bridgecam_updated072425.csv')%>%
  mutate(ice_class = ice_obs)%>%
  select(station, ice_class)%>%
  mutate(ice_class = ifelse(station == 29, 'miz', ice_class))

stacani_ctd <- read_csv('../../data_directory/metadata/stacani.csv')%>%
  mutate(station = sta,
         cast = ca,
         depth = ni,
         sample_id = samp,
         .keep = 'unused')%>%
  left_join(iceObserved)

stacani_trap <- read_csv('../../data_directory/metadata/trap_stacani.csv')%>%
  mutate(sample_id = Sample_Number,
         deployment = Deployment,
         depth = Trap_Depth,
         depthCat = ifelse(Trap_Depth < 5, 'ice',
                           ifelse(Trap_Depth < 20, 'shallow', 'deep')),
         station_deploy = Deploy_Station,
         station_recover = Recover_Station,
         .keep = 'none')

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

planktoscope_log <- read_csv('../../data_directory/taxonomy/PlanktoScope Log Sheet - Source Samples-2.csv')%>%
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

trap_stations <- stacani_trap %>%
  filter(station_deploy != 15)%>% # Drop the test trap
  pull(station_deploy)%>%
  unique(.)

#### Read in Ecotaxa Data ####
ecotaxa_ctd <- read.csv('../../data_directory/taxonomy/ctd_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  left_join(stacani_ctd)%>%
  mutate(sample_id = as.character(sample_id),
         sample_type = 'ctd',
         unique_id = paste('ctdCast', cast, 'station', station, 'depth', depth, sep = '_'))%>%
  select(-cast, -depth) #, -depthCat)


ecotaxa_iceCore <- read.csv('../../data_directory/taxonomy/iceCore_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  left_join(stacani_iceCore)%>%
  mutate(sample_id = as.character(sample_id),
         ice_class = 'ice',
         sample_type = 'ice_core',
         unique_id = paste('ice_core_station', station))%>%
  select(-notes)

ecotaxa_trap <- read.csv('../../data_directory/taxonomy/trap_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  ungroup()%>%
  left_join(stacani_trap_deploy_focus)%>%
  mutate(sample_id = as.character(sample_id),
         sample_type = paste(depthCat, 'trap', sep = '_'),
         unique_id = paste(deployment, depthCat, 'trap', depth, sep = '_'))%>%
  select(-deployment, -depth, -depthCat)


## Ensure Proper Classification of Traps
miz_deployments <- c('T002', 'T005', 'T009')
ow_deployments <- c('T004', 'T010', 'T017', 'T018', 'T022', 'T026', 'T029')


## Properly Calculate Relative Abundance####
ecotaxa_trapStations <- bind_rows(ecotaxa_ctd, ecotaxa_iceCore)%>%
  bind_rows(., ecotaxa_trap)%>%
  mutate(
    object_annotation_hierarchy = case_when(
      ## Large Diatom Group Fixes ##
      object_annotation_hierarchy == "living>Eukaryota>Diatoms>diatom long+chained" ~
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom Chain Long",
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom Chain Long>Unclassified Diatom Chain Long>Unclassified Diatom Chain Long>Unclassified Diatom Chain Long",
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom>Unclassified Diatom>Unclassified Diatom>Unclassified Diatom",
        object_annotation_hierarchy == "living>pennate diatom" ~
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Pennate Diatom",
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Pennate Diatom>Unclassified Pennate Diatom>Unclassified Pennate Diatom>Unclassified Pennate Diatom",
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom Pennate>Unclassified Diatom Pennate>Unclassified Diatom Pennate>Unclassified Diatom Pennate",
      
      ## Diatom Temp Group Fixes ##
      str_detect(object_annotation_hierarchy, "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>pennate>pennate [1-5] temp") ~
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Pennate Diatom Single",
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Pennate Diatom>Unclassified Pennate Diatom>Unclassified Pennate Diatom>Unclassified Pennate Diatom",
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom Pennate>Unclassified Diatom Pennate>Unclassified Diatom Pennate>Unclassified Diatom Pennate",
      str_detect(object_annotation_hierarchy, "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>pennate>pennate [6-9] temp") ~
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Pennate Diatom Chain",
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Pennate Diatom>Unclassified Pennate Diatom>Unclassified Pennate Diatom>Unclassified Pennate Diatom",
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom Pennate>Unclassified Diatom Pennate>Unclassified Diatom Pennate>Unclassified Diatom Pennate",
      str_detect(object_annotation_hierarchy, "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>centric>centric [1-2] temp") ~
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Centric Diatom Chain",
        # "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Centric Diatom>Unclassified Centric Diatom>Unclassified Centric Diatom>Unclassified Centric Diatom",
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Unclassified Diatom Centric>Unclassified Diatom Centric>Unclassified Diatom Centric>Unclassified Diatom Centric",
      
      ## Dinoflagellates and Cilliates
      object_annotation_hierarchy == "living>Eukaryota>Dinoflagellates" ~
        # "living>Eukaryota>Harosa>Alveolata>Myzoza>Dinoflagellata",
        "living>Eukaryota>Harosa>Alveolata>Myzoza>Dinoflagellata>Unclassified Dynoflagellate>Unclassified Dynoflagellate>Unclassified Dynoflagellate>Unclassified Dynoflagellate",
      object_annotation_hierarchy == "living>Eukaryota>Harosa>Alveolata>Ciliophora>ciliate-other" ~
        # "living>Eukaryota>Harosa>Alveolata>Ciliophora",
        "living>Eukaryota>Harosa>Alveolata>Ciliophora>Unclassified Ciliate>Unclassified Ciliate>Unclassified Ciliate>Unclassified Ciliate>Unclassified Ciliate",
    
      ## Species Level Fixes ##
      object_annotation_hierarchy == "living>temporary>Fragilariopsis oceanica" ~
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Bacillariophytina>Bacillariophyceae>Fragilariopsis>Fragilariopsis oceanica",
      object_annotation_hierarchy == "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Bacillariophytina>Bacillariophyceae>Navicula>Navicula sp." ~
        "living>Eukaryota>Harosa>Stramenopiles>Ochrophyta>Bacillariophyta>Bacillariophytina>Bacillariophyceae>Navicula",
      
      ## Not Living ##
      object_annotation_hierarchy == "not-living>detritus" ~
        "not living>Detritus>Detritus>Detritus>Detritus>Detritus>Detritus>Detritus>Detritus>Detritus",
      
      TRUE ~ object_annotation_hierarchy  # keep all other values unchanged
    )
  )%>%
  separate(
    col = object_annotation_hierarchy,
    into = c("liveCell", "domain", "supergroup", "phylum_group", "phylum", 
             "clade", "subphylum", "class", "genus", 'species'), # (10)
    sep = ">",
    fill = "right",
    remove = FALSE
  ) %>%
  mutate(
    across(liveCell:species, ~ replace_na(.x, "Unclassified")),
    # genus = ifelse(clade == 'Bacillariophyta', paste(genus, '(Diatom)'), genus)
    )%>%
  filter(station %in% trap_stations,
         object_annotation_category != 'transparent',
         object_annotation_category != 'bubble')%>%
  mutate(sample_type = factor(sample_type,
                              levels = c("ice_core",
                                         "ice_trap",
                                         'ctd',
                                         "shallow_trap",
                                         "deep_trap"),
                              ordered = TRUE))


trap_stations_sample_metadata <- ecotaxa_trapStations %>%
  select(sample_id, station, ice_class, sample_type)%>%
  unique()

### Compute Relative Abundance at Multiple Taxonomic Levels, starting with the most specific
# Define the ranks you want to summarize
tax_levels <- c('liveCell', "domain", "phylum", "clade", "class", "genus", 'species')

# Loop over levels and bind results
relAbundance <- purrr::map_dfr(
  tax_levels,
  ~ ecotaxa_trapStations %>%
    group_by(ice_class, sample_type, !!sym(.x)) %>%
    summarize(category_bv = sum(biovolume_mL_L), .groups = "drop_last") %>%
    mutate(
      total_bv = sum(category_bv),
      total_bv_noDetritus = sum(if_else(
        !str_detect(!!sym(.x), "Detritus|not living"),
        category_bv,
        0
      )),
      relativeAbundance = category_bv / total_bv,
      relativeAbundance_noDetritus = if_else(
        !str_detect(!!sym(.x), "Detritus|not living"),
        category_bv / total_bv_noDetritus,
        NA_real_
      ),
      tax_level = .x,
      tax_name = !!sym(.x)
    ) %>%
    ungroup()
) 

tailGroups<- relAbundance %>%
  filter(tax_level == 'genus')%>%
  group_by(tax_name)%>%
  mutate(tailGroup = ifelse(mean(relativeAbundance) < 0.01, T, F))%>%
  ungroup()%>%
  select(ice_class, sample_type, tax_level, tax_name, tailGroup)%>%
  filter(tailGroup==T)%>%
  unique()


relAbundance_all <- relAbundance %>%
  group_by(tax_name, tax_level)%>%
  mutate(tax_name = ifelse(max(relativeAbundance) <= 0.02, 'Other', as.character(tax_name)))%>%
  # Recompute totals if you want the pooled "Other" to include all low-abundance taxa
  group_by(ice_class, sample_type, tax_level, tax_name) %>%
  summarize(category_bv = sum(category_bv, na.rm = TRUE),
            relativeAbundance = sum(relativeAbundance, na.rm = TRUE),
            relativeAbundance_noDetritus = sum(relativeAbundance_noDetritus, na.rm = T),
            .groups = "drop")%>%
  mutate(
    ice_facet = case_when(
      ice_class == 'ice' ~ 'Consolidated Ice',
      ice_class == 'miz' ~ 'Marginal Ice Zone',
      ice_class == 'ow' ~ 'Open Water'),
    depth_facet = fct_recode(sample_type,
                             "Ice Core" = "ice_core",
                             "Ice-tethered Trap" = "ice_trap",
                             'Water Column' = 'ctd',
                             'Shallow Trap' = 'shallow_trap',
                             'Deep Trap' = 'deep_trap'))

### Faceted by classification level ###
ggplot(relAbundance_all,
       aes(x = depth_facet,
           y = relativeAbundance,
           fill = tax_name)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(tax_level ~ ice_facet)+
  theme_classic()

# Loop over tax levels
tax_levels <- unique(relAbundance_all$tax_level)

walk(tax_levels, function(level) {
  
  df_level <- relAbundance_all %>% filter(tax_level == level)
  
  # Identify taxa
  other_taxa <- setdiff(unique(df_level$tax_name), c("Unclassified", "Other", "Detritus"))
  
  # # Define factor levels: Detritus first, other taxa sorted alphabetically, Unclassified last
  tax_order <- c("Detritus", sort(other_taxa), "Unclassified", "Other")
  df_level <- df_level %>% mutate(tax_name = factor(tax_name, levels = tax_order))
  
  # Create color palette: plasma for other taxa, fixed colors for Detritus/Unclassified
  palette <- viridis::viridis(n = length(other_taxa), option = "plasma")
  fill_colors <- c(
    "Detritus" = "gray",
    setNames(palette, other_taxa),
    "Other" = "red",
    "Unclassified" = 'green'
  )
  
  # Create the plot
  p <- ggplot(df_level, aes(x = depth_facet,
                            y = relativeAbundance,
                            fill = tax_name)) +
    geom_bar(stat = "identity", position = "stack", color = 'white') +
    facet_wrap(~ ice_facet) +
    scale_fill_manual(values = fill_colors) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # ggtitle(paste("Relative Abundance at", level, "level"))+
    labs(x = NULL, y = NULL, fill = str_to_title(level))
  
  print(p)
  # Optional: save each plot
  # ggsave(filename = paste0("../relAbundance_plots/relAbundance_", level, ".png"), plot = p, width = 8, height = 5)
})

### No detritus version ###
walk(tax_levels, function(level) {
  
  df_level <- relAbundance_all %>% filter(tax_level == level)
  
  # Identify taxa
  other_taxa <- setdiff(unique(df_level$tax_name[ df_level$tax_name != "Detritus" ]), c("Unclassified", "Other"))
  
  # # Define factor levels: Detritus first, other taxa sorted alphabetically, Unclassified last
  tax_order <- c(sort(other_taxa), "Unclassified", "Other")
  df_level <- df_level %>% mutate(tax_name = factor(tax_name, levels = tax_order))
  
  # Create color palette: plasma for other taxa, fixed colors for Detritus/Unclassified
  palette <- viridis::viridis(n = length(other_taxa), option = "plasma")
  fill_colors <- c(
    setNames(palette, other_taxa),
    "Other" = "red",
    "Unclassified" = 'green'
  )
  
  # Create the plot
  p <- ggplot(df_level, aes(x = depth_facet,
                            # y = relativeAbundance,
                            y = relativeAbundance_noDetritus,
                            fill = tax_name)) +
    geom_bar(stat = "identity", position = "stack", color = 'white') +
    facet_wrap(~ ice_facet) +
    scale_fill_manual(values = fill_colors) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # ggtitle(paste("Relative Abundance at", level, "level"))+
    labs(x = NULL, y = NULL, fill = str_to_title(level))
  
  print(p)
  # Optional: save each plot
  # ggsave(filename = paste0("../relAbundance_plots/relAbundance_", level, "_noDetritus.png"), plot = p, width = 8, height = 5)
})

## Genus Level Plot Only ##
genus_relAbundance <- relAbundance_all %>% filter(tax_level == 'genus')
  
# Identify taxa
other_taxa <- setdiff(unique(genus_relAbundance$tax_name), c("Unclassified", "Other", "Detritus"))
  
# Define factor levels: Detritus first, other taxa sorted alphabetically, Unclassified last
tax_order <- c("Detritus", "Unclassified", "Other", sort(other_taxa))
genus_relAbundance <- genus_relAbundance %>% mutate(tax_name = factor(tax_name, levels = tax_order))
  
# Create color palette:
palette <- c(
  "#a6cee3", # light blue
  "#1f78b4", # dark blue
  "#b2df8a", # light green
  "#33a02c", # dark green
  "#fb9a99", # light red
  "#e31a1c", # dark red
  "#cab2d6", # light purple
  "#6a3d9a", # dark purple
  "#fdbf6f", # light orange
  "#ff7f00", # dark orange
  "#ffff99"  # yellow
)
fill_colors <- c(
  "Detritus" = "gray",
  setNames(palette, other_taxa),
  "Other" = "brown",
  # "Other" = "red",
  "Unclassified" = 'green'
)
  
# Create the plot
stackedBar <- genus_relAbundance %>%
  ggplot(., aes(x = depth_facet,
                y = relativeAbundance,
                fill = tax_name)) +
  geom_bar(stat = "identity", position = "stack")+ #, color = 'white') +
  facet_wrap(~ ice_facet) +
  scale_fill_manual(values = fill_colors) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = NULL, y = NULL, fill = 'Genus')
stackedBar

ggsave('../plot/tax_stacked_bar.png', stackedBar, 
       width = 7, height = 6, dpi = 600)

genus_relAbundance_wideReport <- genus_relAbundance %>%
  # unite(sample, ice_facet, depth_facet, sep = " ") %>%
  unite(sample, depth_facet, ice_facet, sep = " ") %>%
  arrange(sample)%>%
  mutate(relativeAbundance = relativeAbundance * 100)%>%
  select(sample, tax_name, relativeAbundance)%>%
  pivot_wider(
    names_from = sample,
    values_from = relativeAbundance
  )

iceTethered_only <- genus_relAbundance_wideReport %>%
  select(tax_name, 'Ice-tethered Trap Consolidated Ice')

openWater_only <- genus_relAbundance_wideReport %>%
  select(tax_name, 'Deep Trap Open Water', 'Shallow Trap Open Water')

howManyUnidentifiedDiatoms <- genus_relAbundance_wideReport %>%
  filter(str_detect(tax_name, "Diatom")) %>%
  select(-tax_name) %>% 
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))

liveCell_relAbundance_wideReport <- relAbundance_all %>%
  filter(tax_level == 'liveCell') %>%
  # unite(sample, ice_facet, depth_facet, sep = " ") %>%
  unite(sample, depth_facet, ice_facet, sep = " ") %>%
  arrange(sample)%>%
  mutate(relativeAbundance = relativeAbundance * 100)%>%
  select(sample, tax_name, relativeAbundance)%>%
  pivot_wider(
    names_from = sample,
    values_from = relativeAbundance
  )

phylum_relAbundance_wideReport <- relAbundance_all %>%
  filter(tax_level == 'phylum') %>%
  unite(sample, depth_facet, ice_facet, sep = " ") %>%
  arrange(sample)%>%
  mutate(relativeAbundance_noDetritus = relativeAbundance_noDetritus * 100)%>%
  select(sample, tax_name, relativeAbundance_noDetritus)%>%
  pivot_wider(
    names_from = sample,
    values_from = relativeAbundance_noDetritus
  )

