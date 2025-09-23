library(tidyverse)

## Runs Accessory Script that reads in dilution factors from planktoscope metadata
source('dilution_factors.R')

#### Stuff that Should be Edited by each user ####

## The script operates on a list of files in a specific directory
## I export with separation on the 'sample' level, and store all exported files in a folder
## In this section, paste in a file path from the script to the export folder and to a folder where you want the result saved

## Define a directory where you want final file saved:
results_folder = '../processed_data'

## Which Dataset ##
dataset = 'trap'
# dataset = 'ctd'
# dataset = 'iceCore'

## Result Filename follows convention: (dataset_name)_ecotaxa_export_with_biovolume
result_filename = paste(dataset, 'ecotaxa_export_with_biovolume', sep = '_')

## Ecotaxa Export Directory follows convention: ../data/ecotaxa_data_(dataset_name)/
export_folder = paste('../data/ecotaxa_data', dataset, sep = '_')

#### Script (Should work for everyone, regardless of computer) ####

## Ecotaxa exports are tab separated values (tsv) files.
file_type = '*.tsv'

## The next line makes a list of all files in the export_folder
files <- list.files(export_folder, pattern = file_type, full.names = T)

## Combines your defined result directory and filename to tell R where to save result
result_filepath <- paste(results_folder, '/', result_filename, '.csv', sep = '')

## creates 3 lists of the various temp categories for later consolidation
single_pennates <- sprintf('pennate %d temp', c(1, 2, 3, 4, 5))
chain_pennates <- sprintf('pennate %d temp', c(6, 7, 8, 9))
chain_centrics <- sprintf('centric %d temp', c(1,2))

## Prepares dilution_factors df for use with function
dilution_factors <- dilution_factors %>%
  select(sample_id, dilution_factor)%>%
  unique()


## Function that reads in the export data and does the biovolume computations
read_in_compute_biovolume <- function(filePath, simplify_TF = F){
  
  ## This function reads in an ecotaxa sample export tab separated values file (.tsv)
  ## Mutate function uses columns in the export tsv to compute biovolume by three different methods
  ## for more information on methods, see here:
  ## http://www.hydroptic.com/assets/uploads/files/documentations/f2990-ecotaxa_tables_from-zooscan-zooprocess_projects.pdf
  
  # Read in the File at the given filePath
  df <- read_tsv(filePath)%>%
    mutate(sample_id = as.character(sample_id))
  
  # If user elects to simplify the dataframe, only the listed columns are preserved
  # The listed columns are either directly used in computations or are useful identifiers of the object/sample
  if(simplify_TF == T){
    df <- select(df,
                 sample_id,
                 object_id,
                 object_annotation_category,
                 object_annotation_hierarchy,
                 object_area,
                 object_area_exc,
                 object_major,
                 object_minor,
                 acq_imaged_volume,
                 process_pixel,
                 object_elongation,
                 object_eccentricity)
  }
  
  # Add the dilution factors here so we have them for computations
  df <- left_join(df, dilution_factors)
  
  # Mutate function handles computations
  df <- mutate(df,
               # process_pixel is in um, convert to mm for ease
               # process_pixel = process_pixel / 1000,
               # Area = object area in pixels * (length of a pixel in um)^2
               area_um2 = object_area * process_pixel**2,
               
               # Area_excluded = object area excluded in pixels * (length of a pixel in um)^2
               area_excluded_um2 = object_area_exc * process_pixel ** 2,
               
               # Major axis = major axis in pixels * length of a pixel
               major_axis_um = object_major * process_pixel,
               
               # Minor axis = Minor axis in pixels * length of a pixel
               minor_axis_um = object_minor * process_pixel,
               
               # Equivalent radius -> area = pi * r^2 => r = sqrt(area/pi)
               equivalent_radius_um = sqrt(area_um2 / pi),
               equivalent_radius_excluded_um = sqrt(area_excluded_um2 / pi),
               
               # Volume of a sphere = (4/3) * pi * radius^3
               spherical_volume_um3 = (4/3) * pi * equivalent_radius_um ** 3,
               spherical_volume_excluded_um3 = (4/3) * pi * equivalent_radius_excluded_um ** 3,
               
               # Volume of an ellipsoid = (4/3) * pi * major_radius * minor_radius_1 * minor_radius_2
               # We assume that the minor radius perpendicular to the plane of the image is
               # equivalent in length to the minor axis in the plane of the image
               # Major and Minor axes are diameters, divide by 2 to get radii
               ellipsoid_volume_um3 = (4/3) * pi * (major_axis_um/2)*(minor_axis_um/2)*(minor_axis_um/2),
               
               # Divide by acquisition imaged volume (mL) to get biovolume
               biovolume_plain_um3_permL = (spherical_volume_um3 / acq_imaged_volume),
               biovolume_riddled_um3_permL = (spherical_volume_excluded_um3 / acq_imaged_volume),
               biovolume_ellipsoid_um3_permL = (ellipsoid_volume_um3 / acq_imaged_volume),
               
               # Divide by dilution factor (dimensionless) to get environmental biovolume
               biovolume_plain_um3_permL = (biovolume_plain_um3_permL / dilution_factor),
               biovolume_riddled_um3_permL = (biovolume_riddled_um3_permL / dilution_factor),
               biovolume_ellipsoid_um3_permL = (biovolume_ellipsoid_um3_permL / dilution_factor),
               
               # Convert um3/mL to mL/L. Dimensional analysis below
               # 1 um^3 / mL * (10^-12 mL / um^3) * (10^3 mL / L) = 10^-9 mL / L
               biovolume_plain_mLperL = (spherical_volume_um3 / acq_imaged_volume) * (10**-9),
               biovolume_riddled_mLperL = (spherical_volume_excluded_um3 / acq_imaged_volume) * (10**-9),
               biovolume_ellipsoid_mLperL = (ellipsoid_volume_um3 / acq_imaged_volume) * (10**-9),
               
               
               # General Annotation Category Consolidates Temp Categories by single or chain pennate or centric
               # NOTE: does NOT replace object_annotation_category, both are preserved in the result file
               general_annotation_category = ifelse(object_annotation_category %in% single_pennates,
                                                    'single_pennate',
                                                    ifelse(object_annotation_category %in% chain_pennates,
                                                           'chain_pennate',
                                                           ifelse(object_annotation_category %in% chain_centrics,
                                                                  'chain_centric', object_annotation_category))))
  
  
  # Function Returns a single dataframe with all export columns and the additional computed values
  return(df)
}

## lapply() used to apply the function to every file in the trap_files list
result_df <- lapply(files, read_in_compute_biovolume,
                    simplify_TF = T)%>%
  # bind_rows() binds all the dataframes in the list together into one big dataframe
  bind_rows(.)

## Writes the resulting dataframe to a CSV file (easy to open in excel)
## row.names = FALSE so you do not get an extra column of row numbers
write.csv(result_df, result_filepath, row.names = FALSE)

#### Pivot Longer, Define Methods, and Save ####
## This Section Does the Thresholding
## The threshold methods are NOT different methods of computing volume, just guidelines for which method to use
## ifelse code chunks evaluate the eccentricity of each individual object and select a method accordingly
result_df_long <- result_df %>%
  mutate(biovolume_plain = biovolume_plain_mLperL,
         biovolume_riddled = biovolume_riddled_mLperL,
         biovolume_ellipsoid = biovolume_ellipsoid_mLperL,
         biovolume_thresh90 = ifelse(object_eccentricity <= 0.90,
                                     biovolume_riddled, biovolume_ellipsoid),
         biovolume_thresh95 = ifelse(object_eccentricity <= 0.95,
                                     biovolume_riddled, biovolume_ellipsoid),
         biovolume_thresh97 = ifelse(object_eccentricity <= 0.97,
                                     biovolume_riddled, biovolume_ellipsoid),
         biovolume_thresh99 = ifelse(object_eccentricity <= 0.99,
                                     biovolume_riddled, biovolume_ellipsoid),
         biovolume_min = pmin(biovolume_riddled, biovolume_ellipsoid))%>%
  select(sample_id,
         object_id,
         object_annotation_hierarchy,
         object_annotation_category,
         general_annotation_category,
         object_elongation,
         object_eccentricity,
         biovolume_riddled,
         biovolume_ellipsoid,
         biovolume_thresh90,
         biovolume_thresh95,
         biovolume_thresh97,
         biovolume_thresh99,
         biovolume_min)%>%
  pivot_longer(
    cols = contains("biovolume"), # Columns containing 'area' to pivot
    names_to = "method",  # New column for area-related names
    values_to = "biovolume" # New column for area values
  )%>%
  mutate(method = case_when(
    str_detect(method, 'riddled') ~ 'riddled',
    str_detect(method, 'ellipsoid') ~ 'ellipsoid',
    str_detect(method, 'thresh90') ~ 'threshold_0.90',
    str_detect(method, 'thresh95') ~ 'threshold_0.95',
    str_detect(method, 'thresh97') ~ 'threshold_0.97',
    str_detect(method, 'thresh99') ~ 'threshold_0.99',
    str_detect(method, 'min') ~ 'minimum',
    TRUE ~ 'plain'),
    method = factor(method, levels = c('plain',
                                       'ellipsoid',
                                       'riddled', 
                                       'minimum_riddled_ellipsoid',
                                       'threshold_0.90',
                                       'threshold_0.95',
                                       'threshold_0.97',
                                       'threshold_0.99')))%>%
  mutate(biovolume_mL_L = biovolume,
         .keep = 'unused')

## Save All Methods
write.csv(result_df_long, paste(results_folder, '/', result_filename, '_all_methods_long.csv', sep = ''), row.names = FALSE)

## Filter Only One Method (0.97 Threshold) and Save
threshold97_result_df <- result_df_long %>%
  filter(method == 'threshold_0.97')

write.csv(threshold97_result_df, paste(results_folder, '/', result_filename, '_threshold97_method.csv', sep = ''), row.names = FALSE)
