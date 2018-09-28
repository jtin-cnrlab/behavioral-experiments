# tfta_training_master.R
# Jessica Tin
# 28 Sept 2018
#
# Compiles training CSVs from each participant's data folder into a single master
# CSV for TFTA.
#

#### SET SERVER PATH ####
PerrachioneLab <- "/Volumes/PerrachioneLab" # Mac
#PerrachioneLab <- "/PerrachioneLab" # Linux
#PerrachioneLab <- "R:/PerrachioneLab" # Windows
setwd(file.path(PerrachioneLab, "projects", "TFTA", "Analysis"))

#### LOAD PACKAGES ####
source(file.path(PerrachioneLab, "software", "r-scripts", "load_packages.R"))
load_packages("dplyr", "readr")

#### IMPORT INDIVIDUAL CSVs ####
# find all CSV files starting with p####_ within project folder > Data > p####
# (change file.path if CSVs are elsewhere)
csv_files <- list.files(path = grep("exclude",
                                    list.dirs(path = file.path("..", "Data"),
                                              recursive = FALSE),
                                    value = TRUE, invert = TRUE),
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
master_df <- bind_rows(master_list)

# save resulting data frame as a CSV (in project folder > Analysis)
write_csv(master_df, "master_training.csv")
