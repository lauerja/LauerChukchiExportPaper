library(tidyverse)
library(lubridate)

### Note: I suggest adding a check on the drift correction regression to monitor effectiveness
### I have added a flag variable for the initial curve fit, but not the drift correction step
### I suggest a similar flag for drift correction, but fit for this regression will not be as
### good as for the standard curve, meaning we may need a more creative solution
### - JL 072024

#### Useful Constants ####
# Molar Masses of Nitrogen and Carbon (g/mol)
molMassN = 14.0067
molMassC = 12.011

# weight percent N and C of Acetanilide Standard 
acetPercentN = 0.103633042
acetPercentC = 0.710937749

#### Specify Data Location and Type ####

## NOTE this read in only works if all .xlsx files in data_exports file are EA data files

# All Exported Data is currently stored in the 'data_exports' folder
data_folder_path = '../sediment_trap_EA_runs/Data_Exports'
final_data_file_path = '../processed_data/all_sedimentTrap_EA_data.csv'
# final_data_file_path = '../results/all_sediment_EA_data.csv'

# files of interest are excel files (.xlsx extension)
file_type = '*.xlsx'

files <- list.files(data_folder_path, pattern = file_type, full.names = T)


#### Read In Fxn ####
read_in_data <- function(filePath){
  # Print the filePath of the file being read in
    print(filePath)
  
  # Read in the File at the given filePath
  df <- readxl::read_excel(filePath)%>%
  # Index of row indicates position on sample carousel
    rownames_to_column(.) %>%
  # Extract Run information from FilePath
    mutate(Run = str_extract(filePath, '(?<=/)[:digit:].+(?=.xlsx)'),
  # Change name of RunNum Variable and make numeric
           RunNum = as.numeric(rowname),
  # Designate 'test' Samples - known unknowns
           # test = ifelse(Type == 'UNK' & Mass_mg != 1.00, T, F),
           test = ifelse(Type == 'UNK' & Mass_mg != 1.00, T,
                         ifelse(Type %in% c('BlankSTD', 'BypassSTD', 'STD_GOOD'), T, F)),
           Type = ifelse(Type == 'By-Pass', 'STD', Type),
  # Compute N content of Standards and test samples
           n_mass_ug = ifelse(test == T | Type == 'STD',
                              Mass_mg * acetPercentN * 1000, NA),
  # ug / (g/mol) = umol
           n_umol = n_mass_ug / molMassN, # ug / molar mass = umol 
  # Compute C content of Standards and test samples
           c_mass_ug = ifelse(test == T | Type == 'STD',
                              Mass_mg * acetPercentC * 1000, NA),
  # ug / (g/mol) = umol
           c_umol = c_mass_ug / molMassC) %>%

  # Select Necessary Columns
    select(Run, RunNum, Sample, Type, test, Mass_mg,
           NitrogenArea, n_mass_ug, n_umol,
           CarbonArea, c_mass_ug, c_umol)
  
  # Function Returns a single dataframe with the selected columns
  return(df)
}

#### Regression and Drift Correction Fxn ####
regression_and_drift_correction <- function(df){
  # Establish Standard Curves
  stds <- filter(df, Type == 'STD')
  # stds <- filter(df, Type %in% c('STD', 'BlankSTD', 'BypassSTD'))
  n_std_curve <- lm(n_umol ~ NitrogenArea, stds)
  c_std_curve <- lm(c_umol ~ CarbonArea, stds)
  
  # Use Standard Curves to Compute umol from Peak Area
  data <- df %>%
    mutate(c_slope = coef(c_std_curve)[2],
           c_int = coef(c_std_curve)[1],
           c_r2 = summary(c_std_curve)$r.squared,
  # badFit_Flag shows TRUE if curve fit is bad - Indicates Bad Standard
           c_badFit_Flag = ifelse(c_r2 <= 0.95, T, F),
           c_computed_umol = c_slope * CarbonArea + c_int,
           c_residual = c_umol - c_computed_umol,
           c_propResidual = c_residual / c_computed_umol,
           n_slope = coef(n_std_curve)[2],
           n_int = coef(n_std_curve)[1],
           n_r2 = summary(n_std_curve)$r.squared,
  # badFit_Flag shows TRUE if curve fit is bad - Indicates Bad Standard
           n_badFit_Flag = ifelse(n_r2 <= 0.95, T, F),
           n_computed_umol = n_slope * NitrogenArea + n_int,
           n_residual = n_umol - n_computed_umol,
           n_propResidual = n_residual / n_computed_umol)
  
  # Compute Drift Using Test Samples
  tests <- filter(data, test == T)
  if (nrow(tests) < 3) {
    warning("Too few test samples to fit drift correction for run: ", unique(df$Run))
    # Plot Std Curve and Return
    n_plot <- ggplot(stds, aes(x = NitrogenArea, y = n_umol)) +
      geom_point(aes(shape = Run))+
      geom_text(aes(label = RunNum), nudge_x = 0.1, nudge_y = 0.2)+
      labs(title = paste(as.character(data$Run[[1]]), 'N Standards', sep = '\n'))+
      xlab('Peak Area')+
      ylab('N [umol]')
    print(n_plot)
    
    c_plot <- ggplot(stds, aes(x = CarbonArea, y = c_umol)) +
      geom_point(aes(shape = Run))+
      geom_text(aes(label = RunNum), nudge_x = 1.5, nudge_y = 1.5)+
      labs(title = paste(as.character(data$Run[[1]]), 'C Standards', sep = '\n'))+
      xlab('Peak Area')+
      ylab('C [umol]')
    print(c_plot)
    return(data)
  }
  
  n_drift_curve <- lm(n_propResidual ~ RunNum, tests)
  c_drift_curve <- lm(c_propResidual ~ RunNum, tests)
  
  # Plot Std Curve and Return
  n_plot <- ggplot(stds, aes(x = NitrogenArea, y = n_umol)) +
    geom_point(aes(shape = Run))+
    geom_text(aes(label = RunNum), nudge_x = 0.1, nudge_y = 0.2)+
    geom_point(data = tests, color = 'cornflowerblue', aes(shape = Run))+
    geom_text(data = tests, aes(label = RunNum), nudge_x = 0.1, nudge_y = 0.2)+
    labs(title = paste(as.character(data$Run[[1]]), 'N Standards and Tests', sep = '\n'))+
    xlab('Peak Area')+
    ylab('N [umol]')
  print(n_plot)
  
  c_plot <- ggplot(stds, aes(x = CarbonArea, y = c_umol)) +
    geom_point(aes(shape = Run))+
    geom_text(aes(label = RunNum), nudge_x = 1.5, nudge_y = 1.5)+
    geom_point(data = tests, color = 'orange', aes(shape = Run))+
    geom_text(data = tests, aes(label = RunNum), nudge_x = 1.5, nudge_y = 1.5)+
    labs(title = paste(as.character(data$Run[[1]]), 'C Standards and Tests', sep = '\n'))+
    xlab('Peak Area')+
    ylab('C [umol]')
  print(c_plot)
  
  # Fail-safe for NA coefficients (bad fit or perfect collinearity)
  if (any(is.na(coef(n_drift_curve))) || any(is.na(coef(c_drift_curve)))) {
    warning("Drift correction regression failed for run: ", unique(df$Run))
    return(data)
  }
  
  # Drift Correct the Computed Values
  data <- data %>%
    mutate(cDC_slope = coef(c_drift_curve)[2],
           cDC_int = coef(c_drift_curve)[1],
           cDC_r2 = summary(c_drift_curve)$r.squared,
           cDC_pvalue = summary(c_drift_curve)$coefficients["RunNum","Pr(>|t|)"],
           c_driftCorrected_umol = c_computed_umol + c_computed_umol * (cDC_slope * RunNum + cDC_int),
           nDC_slope = coef(n_drift_curve)[2],
           nDC_int = coef(n_drift_curve)[1],
           nDC_r2 = summary(n_drift_curve)$r.squared,
           nDC_pvalue = summary(n_drift_curve)$coefficients["RunNum","Pr(>|t|)"],
           n_driftCorrected_umol = n_computed_umol + n_computed_umol * (nDC_slope * RunNum + nDC_int))
  
  # Plot Drift Correction Slope
  nDC_plot <- ggplot(tests, aes(x = RunNum, y = n_propResidual)) +
    geom_point(color = 'cornflowerblue') +
    geom_smooth(method = 'lm', se = FALSE, color = 'black') +
    labs(title = paste("Nitrogen Drift Residuals -", unique(tests$Run)), y = "N Proportional Residual", x = "RunNum")
  print(nDC_plot)
  
  # Plot Drift Correction Residuals
  cDC_plot <- ggplot(tests, aes(x = RunNum, y = c_propResidual)) +
    geom_point(color = 'orange') +
    geom_smooth(method = 'lm', se = FALSE, color = 'black') +
    labs(title = paste("Carbon Drift Residuals -", unique(tests$Run)), y = "C Proportional Residual", x = "RunNum")
  print(cDC_plot)
  
  return(data)
}

#### Read in Data and Apply Functions ####

data_raw <- lapply(files, read_in_data)%>%
  bind_rows(.)%>%
  mutate(Type = ifelse(Type %in% c('BlankSTD', 'BypassSTD', 'STD_GOOD'), 'STD', Type))%>%
## Run 032024 had an issue in which all standard samples saved over one another as they ran
## we build a standard curve from the bypass, the tin blank, and the final curve
  mutate(flag = ifelse(Run == '032024_std_problem' & Type == 'STD', T, F),
         flag = ifelse(Sample %in% c('Bypass_Acet_B2', 'Tin Blank', 'Acet_C6') & flag == T, F, flag))%>%
  filter(flag != T)

# The D4 Standard (runNum = 6) in the 070924 Run falls far off the Std Curve
# I am dropping this standard to improve std curve fit - JL 07/20/24
data <- data_raw %>%
  # Create badStandard Variable to Flag the bad standards
  mutate(badStandard = ifelse(Type == 'STD_BAD', T,
                              ifelse(Run == '070924' & RunNum == 6, T,
                                     ifelse(Run == '062024' & RunNum == 8, T, F))))%>%
  # Filter badStandard out
  filter(badStandard == F)%>%
  group_by(Run)%>%
  group_split()%>%
  lapply(., regression_and_drift_correction)%>%
  bind_rows(.)%>%
  mutate(
  cDC_badFit_Flag = ifelse(cDC_r2 <= 0.5, TRUE, FALSE),
  cDC_sig = ifelse(cDC_pvalue <= 0.05, TRUE, FALSE),
  nDC_badFit_Flag = ifelse(nDC_r2 <= 0.5, TRUE, FALSE),
  nDC_sig = ifelse(nDC_pvalue <= 0.05, TRUE, FALSE))

# Read in STACANI file here if needed to provide lat/lon, date, time, etc.
# station_meta <- read_csv('../results/tots_station_metadata.csv')


#### Write Data to CSV ####

## Note: if the drift correction fit is significant, drift corrected values are used
## If the drift correction is not significant or NA, raw value is used
samples <- filter(data, Type == 'UNK', test == F)%>%
  mutate(c_umol_raw = c_computed_umol,
         c_umol_driftCorrected = c_driftCorrected_umol,
         c_umol = ifelse(cDC_sig == F | is.na(cDC_sig)==T, c_umol_raw, c_umol_driftCorrected),
         c_umol = ifelse(c_umol < 0, NA, c_umol), # less than zero value means below blank - not detectable
         c_g = molMassC * c_umol / 10^6,
         n_umol_raw = n_computed_umol,
         n_umol_driftCorrected = n_driftCorrected_umol,
         n_umol = ifelse(nDC_sig == F| is.na(nDC_sig)==T, n_umol_raw, n_umol_driftCorrected),
         n_umol = ifelse(n_umol < 0, NA, n_umol), # less than zero value means below blank - not detectable
         n_g = molMassN * n_umol / 10^6,
         ArrigoNumber = as.numeric(str_extract(Sample, '^\\d+')),
         acidified = ifelse(str_detect(Sample, 'acid')==T, T, F),
         Sample = as.character(Sample))%>%
  select(Run, # Date of the EA Run
         RunNum, # Position of Sample in Sample Carousel (1-32)
         Sample, # Sample Name as entered into EA Sample Table
         ArrigoNumber, # Numeric Arrigo Number Extracted from Sample Name
         acidified, # True for acidified samples containing only organic carbon, false for total carbon samples
         c_umol, # Carbon content in sample (umol carbon) - divide by Liters filtered for uM
         c_g, # Carbon Content in sample (g carbon)
         c_badFit_Flag, # Flag for bad carbon standard curve - if True a standard in this run is bad
         n_umol, # Nitrogen content in sample (umol carbon) - divide by Liters filtered for uM
         n_g, # Nitrogen content in sample (g nitrogen)
         n_badFit_Flag)  # Flag for bad nitrogen standard curve - if True a standard in this run is bad

write_csv(samples, final_data_file_path)
