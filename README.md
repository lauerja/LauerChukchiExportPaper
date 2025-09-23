Code used for data processing and figure generation

data_general houses raw data and scripts used to create intermediaries used in many figures

### Order of Operations for Using this Repository ###
1. run process_raw_ctd_compute_potentialDensity.ipynb
        - This file works from individual cast CTD files
        - compiles all casts into single .csv file for future analysis
        - Creates file: final_qc_CTD_data.csv
2. run process_raw_bottle_file.ipynb
        - This file works from the publically available bottle file: SKQ202310S_hy1.csv
        - cleans up variable names and types, adds some additional metadata variables using re$
        - Creates file: final_qc_bottle_file.csv
3. run
4. run
5. run
   
### At this point all figures can be generated. Follow README.txt intructions in each subdirectory for specific guides ###
