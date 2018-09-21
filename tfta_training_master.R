# tfta_training_master.R
# Jessica Tin
# 21 Sept 2018
#
# Compiles training CSVs from each participant's data folder into a single master
# CSV for TFTA.
#

#### LOAD PACKAGES ####
source("/Volumes/PerrachioneLab/software/r-scripts/load_packages.R")
load_packages("dplyr", "readr")

#### SET WORKING DIRECTORY ####
setwd("/Volumes/PerrachioneLab/projects/TFTA/Analysis") # Mac
#setwd("/PerrachioneLab/projects/TFTA") # Linux
#setwd("R:/PerrachioneLab/projects/TFTA") # Windows

#### IMPORT INDIVIDUAL CSVs ####
# find all CSV files starting with p####_ within project folder > Experiment > data
# (default location for psychopy output; change file.path if CSVs are elsewhere)
csv_files <- list.files(path = file.path("..","Data"),
                        pattern = "^p[0-9]{4}_.*training.*\\.csv$",
                        full.names = TRUE, recursive = TRUE)

# preallocate list to save all individual CSV data to
master_list <- vector("list", length = length(csv_files))

# loop through each CSV file
for(f in csv_files) {
    print(paste0("[",match(f, csv_files),"/",length(csv_files),"] ",f))

    # read in CSV as data frame
    df_csv <- suppressWarnings(suppressMessages( # hide warning about empty column
        read_csv(f, col_types = "iccciiciiiicdcidcidccccic-"))) %>%
        select(
            # specify which columns to keep
            # master column name = original column name (if different)
            participant,
            condition,
            day,
            section,
            trial = trials.thisTrialN,
            word1,
            word2,
            talker1,
            talker2,
            word_resp,
            talker_resp,
            rt = key_resp_5.rt,
            correct = key_resp_5.corr,
            talker_trained = talker,
            vowel_trained = vowel) %>%

        # only keep rows with trial info
        filter(!is.na(trial)) %>%

#### MODIFY AND CREATE COLUMNS ####
        # convert rt to ms
        mutate(rt = rt * 1000)

#### COMPILE AND SAVE MASTER CSV ####
    # add this CSV data to the master list
    master_list[match(f, csv_files)] <- list(df_csv)
}

# create master data frame from master_list data frames
master_df <- rbindlist(master_list)

# save resulting data frame as a CSV with timestamp within project folder > Analysis
timestamp <- format(Sys.time(), tz = "EST5EDT", format = "%m-%d-%y_%H%M")
write_csv(master_df, file.path(paste0("master_training_",timestamp,".csv")))
