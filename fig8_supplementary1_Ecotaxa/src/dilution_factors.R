library(tidyverse)

dilution_factors <- full_join(read_csv('../data/PlanktoScope Log Sheet - Source Samples-2.csv'), 
                              read_csv('../data/PlanktoScope Log Sheet - Sample Adjustments.csv'))%>%
  select(`(CTD/Ice/Experiment)\nArrigo Number`,
         `tots-prakash Sample ID`,
         `(CTD/Ice)\nArrigo Bottle Name`,
         `Sample collection notes`,
         `Station ID`,
         `Required Concentration Adjustment`,
         `(Concentrating)\nFilter Mesh Size (um)`,
         `Overall Dilution Factor`,
         `Notes`
         )%>%
  unique()%>%
  mutate(sample_id = `(CTD/Ice/Experiment)\nArrigo Number`,
         prakash_sample_id = `tots-prakash Sample ID`,
         dilution_factor = as.numeric(`Overall Dilution Factor`),
         planktoscope_stationID = `Station ID`,
         planktoscope_bottle_name = `(CTD/Ice)\nArrigo Bottle Name`,
         planktoscope_collection_notes = `Sample collection notes`,
         required_concentration_adjustment = `Required Concentration Adjustment`,
         concentrating_filter_mesh_size = `(Concentrating)\nFilter Mesh Size (um)`,
         planktoscope_adjustment_notes = `Notes`,
         .keep = 'none'
         )%>%
  drop_na(dilution_factor)%>%
  # Correct Typo that resulted in duplicate Arrigo Number
  mutate(sample_id = ifelse(sample_id == '418' & planktoscope_bottle_name == "PlanktoScope Depth 2 (Surface)",
                            '419',
                            sample_id))%>%
  # 'adjusted-207' was rerun as 'adjusted-208' with different dilution factor
  # filter out 'adjusted-207' to prevent use of incorrect dilution factor
  filter(prakash_sample_id != 'adjusted-207',
         prakash_sample_id != 'adjusted-594') # sample swap - accidental rerun of earlier sample

write_csv(dilution_factors, '../processed_data/planktoscope_dilution_factors.csv')
