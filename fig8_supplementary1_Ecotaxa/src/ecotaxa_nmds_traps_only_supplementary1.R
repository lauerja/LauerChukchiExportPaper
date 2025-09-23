library(tidyverse)
library(vegan)
library(plotly)
library(RVAideMemoire)
library(wesanderson)

#### Read in STACANI and Other Metadata ####
# iceObserved <- read_csv('../data/bridge_cam_ice_labels_041124.csv')%>%
iceObserved <- read_csv('../../data_general/data_station_meta/observed_ice_edge_bridgecam_updated072425.csv')%>%
  mutate(ice_class = ice_obs)%>%
  select(station, ice_class)%>%
  mutate(ice_class = ifelse(station == 29, 'miz', ice_class))

stacani_trap <- read_csv('../data/trap_stacani.csv')%>%
  mutate(sample_id = Sample_Number,
         deployment = Deployment,
         depth = Trap_Depth,
         depthCat = ifelse(Trap_Depth < 5, 'ice',
                           ifelse(Trap_Depth < 20, 'shallow', 'deep')),
         station_deploy = Deploy_Station,
         station_recover = Recover_Station,
         station = station_deploy,
         # # Uncomment for recovery-focused STACANI. Metadata currently coming from deploy station
         # station = station_recover,
         .keep = 'none') %>%
  # select(sample_id, deployment, station_deploy, station_recover, station, Deploy_Type, Trap_Depth, depthCat)%>%
  left_join(., iceObserved)

# write_csv(stacani_trap, '../../trapAnalysis_byLap/grc25_map_figure/data/trap_stacani.csv')

#### Read in Data ####
# trap <- read_csv('../processed_data/trap_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
#   left_join(., stacani_trap)%>%
#   mutate(sample_id = as.character(sample_id),
#          sample_type = 'trap',
#          unique_id = paste(deployment, depthCat, 'trap', depth, sep = '_'),
#          # nmds_group = paste(depthCat, 'trap', sep = '_'))%>%
#          # nmds_group = paste(ice_class, depthCat, 'trap', sep = '_'))%>%
#          nmds_group = paste(ice_class, 'trap', sep = '_'))%>%
#   # filter(depthCat != 'ice')%>%
#   # filter(ice_class == 'ice')%>%
#   # filter(ice_class == 'miz')%>%
#   # filter(ice_class == 'ow')%>%
#   group_by(unique_id, sample_type, ice_class, depthCat, nmds_group, general_annotation_category)%>%
#   summarise(count = n(),
#             biovolume = sum(biovolume_mL_L))%>%
#   ungroup()

#### Hierarchical Data ####
trap <- read_csv('../processed_data/trap_ecotaxa_export_with_biovolume_threshold97_method.csv')%>%
  left_join(., stacani_trap)%>%
  mutate(
    sample_id = as.character(sample_id),
    sample_type = 'trap',
    unique_id = paste(deployment, depthCat, 'trap', depth, sep = '_'),
    nmds_group = paste(ice_class, 'trap', sep = '_'))%>%
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
  filter(
         object_annotation_category != 'transparent',
         object_annotation_category != 'bubble')%>%
  group_by(unique_id, sample_type, ice_class, depthCat, nmds_group, genus)%>% #general_annotation_category)%>%
  summarise(count = n(),
            biovolume = sum(biovolume_mL_L))%>%
  ungroup()


#### Prepare Matrices ####

data <- trap %>%
  # filter(general_annotation_category != 'bubble',
  #        # general_annotation_category != 'detritus',
  #        # general_annotation_category != 'unidentified cells',
  #        general_annotation_category != 'transparent')%>%
  group_by(unique_id)%>%
  mutate(totalBV = sum(biovolume),
         relative_biovolume = biovolume / totalBV)%>%
  ungroup()%>%
  select(-totalBV)

grouping_vars <- data %>%
  select(unique_id, sample_type, ice_class, depthCat, nmds_group)%>%
  mutate(depthCat = factor(depthCat, levels = c('core', 'ice', '1-5', '5-10', '10+', 'shallow', 'deep')))%>%
  distinct()

## This is important for statistical testing
nmds_groups <- select(data, unique_id, nmds_group, ice_class, depthCat) %>%
  distinct() %>%
  column_to_rownames(var = "unique_id") %>%
  pull(nmds_group) %>%
  factor()

# counts <- select(data, unique_id, general_annotation_category, count)%>%
#   pivot_wider(names_from = general_annotation_category,
counts <- select(data, unique_id, genus, count)%>%
  pivot_wider(names_from = genus,
              values_from = count,
              values_fill = list(count = 0))%>%
  column_to_rownames(var = "unique_id")%>%
  as.matrix()

# biovolume <- select(data, unique_id, general_annotation_category, biovolume)%>%
#   pivot_wider(names_from = general_annotation_category,
biovolume <- select(data, unique_id, genus, biovolume)%>%
  pivot_wider(names_from = genus,
              values_from = biovolume,
              values_fill = list(biovolume = 0))%>%
  column_to_rownames(var = "unique_id")%>%
  as.matrix()
  
# relBiovolume <- select(data, unique_id, general_annotation_category, relative_biovolume)%>%
#   pivot_wider(names_from = general_annotation_category,
relBiovolume <- select(data, unique_id, genus, relative_biovolume)%>%
  pivot_wider(names_from = genus,
              values_from = relative_biovolume,
              values_fill = list(relative_biovolume = 0))%>%
  column_to_rownames(var = "unique_id")%>%
  as.matrix()

#### Attempt NMDS ####
set.seed(42)

# test_NMDS = metaMDS(biovolume,
test_NMDS = metaMDS(relBiovolume,
                    distance = 'bray',
                    engine = 'monoMDS',
                    k=3, # needed for 3d plotting
                    # k=2,
                    weakties = T,
                    model = 'global',
                    maxit = 300,
                    try = 50,
                    trymax = 100)
stressplot(test_NMDS)

nmds_df <- as.data.frame(test_NMDS$points) %>%
  rownames_to_column('unique_id')%>%
  left_join(., grouping_vars)

# Plot
nmds_plot <- ggplot(nmds_df, aes(x = MDS1, y = MDS2,
                    # color = depthCat,
                    color = ice_class))+ #,
                    # shape = depthCat)) +
  geom_point(size = 3) +
  stat_ellipse(geom = "polygon", type = 't', alpha = 0, aes(color = ice_class, group = ice_class)) +
  # stat_ellipse(geom = "polygon", type = 't', alpha = 0.2, aes(fill = depthCat, group = depthCat), color = NA) +
  scale_color_manual(values = c("#d63a41","#FCCE50","#44C7FF"),
                     labels=c('ice' = 'Consolidated Ice',
                              'miz' = 'Marginal Ice Zone',
                              'ow' = 'Open Water'),
                     name = 'Ice Coverage')+
  # scale_fill_manual(values = wes_palette('Zissou1')[c(1, 3, 5)],
  #                   labels=c('ice' = 'Consolidated Ice',
  #                            'miz' = 'Marginal Ice Zone',
  #                            'ow' = 'Open Water'),
  #                   name = 'Ice Coverage')+
  # scale_fill_discrete(labels=c('ice' = 'Consolidated Ice',
  #                              'miz' = 'Marginal Ice Zone\n(Front)',
  #                              'ow' = 'Open Water'))+
  theme_minimal()

nmds_plot


ggsave('../plot/trap_nmds.jpeg', nmds_plot,
       height = 6, width = 7, dpi = 600)

# 3d Plot

# # Prepare data for 3D plotting
# nmds_df <- as.data.frame(test_NMDS$points) %>%
#   mutate(nmds_group = nmds_groups)%>%
#   rownames_to_column('unique_id')
# 
# # Create a 3D scatter plot
# plot_ly(nmds_df,
#         x = ~MDS1, y = ~MDS2, z = ~MDS3,
#         color = ~ice_class,
#         # color = ~depthCat,
#         # symbol = ~depthCat,
#         # colors = "Set2",
#         type = 'scatter3d',
#         mode = 'markers+text',
#         marker = list(size = 5) #,
#         # text= ~nmds_df$unique_id
#         ) %>%
#   layout(scene = list(xaxis = list(title = 'MDS1'),
#                       yaxis = list(title = 'MDS2'),
#                       zaxis = list(title = 'MDS3')),
#          legend = list(title = list(text = 'NMDS Group')))

#### Stats (PERMANOVA) ####
# Create a data frame for nmds groups with proper column naming
nmds_groups_df <- data.frame(unique_id = rownames(biovolume))%>%
  left_join(grouping_vars)

# Create a Bray-Curtis distance matrix from the biovolume data
distance_matrix <- vegdist(biovolume, method = "bray")

# Run PERMANOVA using Bray-Curtis distance
permanova_result <- adonis2(distance_matrix ~ ice_class,
# permanova_result <- adonis2(distance_matrix ~ ice_class * depthCat,
# permanova_result <- adonis2(distance_matrix ~ depthCat, 
                            data = nmds_groups_df, 
                            method = 'bray', 
                            permutations = 999)

# Display results
print(permanova_result)

beta <- betadisper(distance_matrix, nmds_groups)
permutest(beta, permutations = 999)

TukeyHSD(beta)
## Significant adonis2 (permanova) indicates groups differ significantly
## Significant permutest indicates there is a difference in group dispersion that may be driving the difference in composition
## A significant adonis2 with an insignificant permutest indicates differences in community composition are driving the adonis2 difference

#### Pairwise Check (RVAideMemoire) ####
# Pairwise PERMANOVA using Bray-Curtis distance
pairwise_results <- pairwise.perm.manova(distance_matrix, 
                                         nmds_groups, 
                                         nperm = 999, 
                                         p.method = "fdr")

# Display the results
print(pairwise_results)


# Generate all possible pairwise comparisons of groups
group_combinations <- combn(levels(nmds_groups), 2, simplify = FALSE)

# Run permutest for each pairwise combination
pairwise_permutest_results <- lapply(group_combinations, function(groups) {
  subset_idx <- nmds_groups %in% groups
  dist_matrix <- vegdist(biovolume[subset_idx, ], method = "bray")
  subset_data <- nmds_groups_df[subset_idx, , drop = FALSE]
  # subset_data <- grouping_vars[subset_idx, , drop = FALSE]
  permanova_result <- adonis2(dist_matrix ~ nmds_group, 
                              data = subset_data,
                              method = 'bray',
                              permutations = 999)
  permanova_p <- permanova_result$`Pr(>F)`[1]
  beta_subset <- betadisper(dist_matrix, nmds_groups[subset_idx], sqrt.dist = TRUE)
  permutest_result <- permutest(beta_subset, permutations = 999, pairwise=T)
  permutest_p <- permutest_result$tab$`Pr(>F)`[1]
  data.frame(pair = paste(groups, collapse = " vs "),
             permanova_p_val = permanova_p,
             permutest_p_val = permutest_p)
})
pairwise_results_dataframe <- bind_rows(pairwise_permutest_results)%>%
  mutate(interpretation = case_when(
    permanova_p_val <= 0.05 & permutest_p_val <= 0.05 ~ 'different dispersion',
    permanova_p_val <= 0.05 & permutest_p_val > 0.05 ~ 'SIG DIF Composition',
    permanova_p_val > 0.05 & permutest_p_val <= 0.05 ~ 'different dispersion, same community',
    permanova_p_val > 0.05 & permutest_p_val > 0.05 ~ 'NOT SIG - no difference'
  ))
print(pairwise_results_dataframe)
