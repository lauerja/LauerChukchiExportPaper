# source('./hierarchical_stacked_bar_chart_figure6.R')


ice_tethered_deployments <- stacani_trap %>%
  filter(depthCat == 'ice')%>%
  mutate(deployment_type = 'ice-tethered')%>%
  select(deployment, deployment_type)

stacani_trap <- stacani_trap %>%
  left_join(., ice_tethered_deployments)%>%
  mutate(deployment_type = ifelse(is.na(deployment_type),
                                  'free-drifting',
                                  deployment_type))

# Define the ranks you want to summarize
tax_levels <- c('liveCell', "domain", "phylum", "clade", "class", "genus", 'species')

# Loop over levels and bind results - grouped by sample_id
relAbundance <- purrr::map_dfr(
  tax_levels,
  ~ ecotaxa_trapStations %>%
    group_by(sample_id, ice_class, sample_type, !!sym(.x)) %>%
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

# Group rare taxa as "Other" - now considering sample_id
relAbundance_all <- relAbundance %>%
  group_by(tax_name, tax_level) %>%
  mutate(tax_name = ifelse(max(relativeAbundance) <= 0.02, 'Other', as.character(tax_name))) %>%
  ungroup() %>%
  # Recompute totals for pooled "Other" taxa by sample
  group_by(sample_id, ice_class, sample_type, tax_level, tax_name) %>%
  summarize(
    category_bv = sum(category_bv, na.rm = TRUE),
    relativeAbundance = sum(relativeAbundance, na.rm = TRUE),
    # Handle NA values properly for relativeAbundance_noDetritus
    relativeAbundance_noDetritus = if_else(
      all(is.na(relativeAbundance_noDetritus)),
      NA_real_,
      sum(relativeAbundance_noDetritus, na.rm = TRUE)
    ),
    .groups = "drop"
  )

trap_relAbundance <- left_join(stacani_trap, relAbundance_all) %>%
  mutate(
    ice_facet = case_when(
      # ice_class == 'ice' ~ 'Consolidated Ice',
      ice_class == 'ice' & deployment_type == 'ice-tethered' ~ 'Ice-Tethered',
      ice_class == 'ice' & deployment_type == 'free-drifting' ~ 'Free-Drifting in Ice',
      ice_class == 'miz' ~ 'Marginal Ice Zone',
      ice_class == 'ow' ~ 'Open Water'
    ) %>%
      factor(levels = c('Ice-Tethered', 'Free-Drifting in Ice', #'Consolidated Ice', 
                        'Marginal Ice Zone', 'Open Water')),
    depth_facet = fct_recode(sample_type,
                             "Ice Core" = "ice_core",
                             # "Ice-tethered Trap" = "ice_trap",
                             'Upper Trap' = 'ice_trap',
                             'Water Column' = 'ctd',
                             'Upper Trap' = 'shallow_trap',
                             'Lower Trap' = 'deep_trap') %>%
      fct_relevel("Ice Core", #"Ice-tethered Trap", 
                  "Water Column", 
                  "Upper Trap", "Lower Trap"))

## Genus Level Plot Only - Filter for genus
genus_relAbundance <- trap_relAbundance %>% filter(tax_level == 'genus')

# Identify taxa
# other_taxa <- setdiff(unique(genus_relAbundance$tax_name), c("Unclassified", "Other", "Detritus"))

# Define factor levels: Detritus first, other taxa sorted alphabetically, Unclassified last
tax_order <- c("Detritus", "Unclassified", "Other", sort(other_taxa))
genus_relAbundance <- genus_relAbundance %>% 
  mutate(tax_name = factor(tax_name, levels = tax_order))

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
  "Unclassified" = 'green'
)



faceted_stackedBar <- genus_relAbundance %>%
  ggplot(aes(x = deployment,
             y = relativeAbundance,
             fill = tax_name)) +
  geom_bar(stat = "identity", position = "stack") +
  # facet_wrap(~ ice_facet) +
  scale_fill_manual(values = fill_colors) +
  facet_grid(depth_facet~ice_facet, scales = 'free')+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL, 
    y = NULL, 
    fill = 'Genus'
  )

faceted_stackedBar

# Save plot with deployment# Save plot with deployment name
ggsave(
  filename = '../plot/trap_faceted_tax_stacked_bar.png', 
  plot = faceted_stackedBar, 
  width = 9, 
  height = 6, 
  dpi = 600)

# Loop through each unique deployment
for (deployment_id in unique(genus_relAbundance$deployment)) {
  
  # Subset data for this deployment
  deployment_data <- genus_relAbundance %>%
    filter(deployment == !!deployment_id)
  
  # Create the plot
  stackedBar <- deployment_data %>%
    ggplot(aes(x = depth_facet,
               y = relativeAbundance,
               fill = tax_name)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ ice_facet) +
    scale_fill_manual(values = fill_colors) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = NULL, 
      y = NULL, 
      fill = 'Genus',
      title = paste("Deployment:", deployment_id)
    )
  
  # Print to viewer
  print(stackedBar)
  
  # Save plot with deployment name
  ggsave(
    filename = paste0('../plot/tax_stacked_bar_deployment_', deployment_id, '.png'), 
    plot = stackedBar, 
    width = 7, 
    height = 6, 
    dpi = 600
  )
}
