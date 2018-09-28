# tfta_test_master.R
# Jessica Tin
# 28 Sept 2018
#
# Compiles pre- and post-test CSVs from each participant's data folder into a
# single master CSV for TFTA.
#

#### SET SERVER PATH ####
PerrachioneLab <- "/Volumes/PerrachioneLab" # Mac
#PerrachioneLab <- "/PerrachioneLab" # Linux
#PerrachioneLab <- "R:/PerrachioneLab" # Windows
setwd(file.path(PerrachioneLab, "projects", "TFTA", "Analysis"))

#### LOAD PACKAGES ####
source(file.path(PerrachioneLab, "software", "r-scripts", "load_packages.R"))
load_packages("dplyr", "readr", "tidyr")

#### IMPORT INDIVIDUAL CSVs ####
# find all CSV files starting with p####_ within project folder > Data > p####
# (change file.path if CSVs are elsewhere)
csv_files <- list.files(path = grep("exclude",
                                    list.dirs(path = file.path("..", "Data"),
                                              recursive = FALSE),
                                    value = TRUE, invert = TRUE),
                        pattern = "^p[0-9]{4}_.*test.*\\.csv$",
                        full.names = TRUE, recursive = TRUE)

# preallocate list to save all individual CSV data to
master_list <- vector("list", length = length(csv_files))

# loop through each CSV file
for(f in csv_files) {
    print(paste0("[",match(f, csv_files),"/",length(csv_files),"] ",f))

    # read in CSV as data frame
    df_csv <- suppressWarnings(suppressMessages( # hide warning about empty column
        read_csv(f, col_types = "cccciiiiicdcdcidcidccci-"))) %>%
        select(
            # specify which columns to keep
            # master column name = original column name (if different)
            participant,
            test,
            block,
            trial = trials.thisTrialN,
            talker,
            stimulus,
            rt = key_resp_3.rt,
            correct = key_resp_3.corr,
            order,
            condition
        ) %>%

        # only keep rows with trial info
        filter(!is.na(trial), talker != "start") %>%

#### MODIFY AND CREATE COLUMNS ####
        # convert rt to ms
        mutate(rt = rt * 1000) %>%

        # fix mislabeled condition
        rowwise() %>%
        mutate(condition = ifelse((participant %in% c("p3135", "p0002", "p1513", "p2282")) &&
                                      (condition == "ioua_mixed_pairA_setB"),
                                  "none_mixed_pairA_setB", condition)) %>%

        # split condition column
        separate(condition, c("carrier", "single_mixed", "vowel_pair", "talker_set"))

#### COMPILE AND SAVE MASTER CSV ####
    # add this CSV data to the master list
    master_list[match(f, csv_files)] <- list(df_csv)
}

# create master data frame from master_list data frames
master_df <- bind_rows(master_list)

# save resulting data frame as a CSV (in project folder > Analysis)
write_csv(master_df, "master_test.csv")
