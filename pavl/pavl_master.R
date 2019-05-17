# pavl_master.R
# Jessica Tin
# 19 Oct 2018
#
# Compiles data from every CSV in PAVL's data folder into a single master CSV.
#

#### SET SERVER PATH ####
PerrachioneLab <- "/Volumes/PerrachioneLab" # Mac
#PerrachioneLab <- "/PerrachioneLab" # Linux
#PerrachioneLab <- "R:/PerrachioneLab" # Windows
setwd(file.path(PerrachioneLab, "projects", "PAVL"))

#### LOAD PACKAGES ####
source(file.path(PerrachioneLab, "software", "r-scripts", "load_packages.R"))
load_packages("dplyr", "magrittr", "readr", "stringr")

#### IMPORT INDIVIDUAL CSVs ####
# find all CSV files starting with p####_ within project folder > Experiment > data
# (default location for psychopy output; change file.path if CSVs are elsewhere)
csv_files <- list.files(path = file.path("Experiment","data"),
                        pattern = "^p[0-9]{4}_.*\\.csv$",
                        full.names = TRUE)

# preallocate list to save all individual CSV data to
master_list <- vector("list", length = length(csv_files))

# loop through each CSV file
for (f in csv_files) {
    print(paste0("[",match(f, csv_files),"/",length(csv_files),"] ",f))

    # read in CSV as data frame
    df_csv <- suppressMessages(read_csv(f)) %>% # hide warning messages about empty columns
        select(
            # specify which columns to keep
            # master column name = original column name (if different)
            participant,
            trial,
            talker,
            exposure_response = key_resp_3.keys,
            exposure_accuracy = key_resp_3.corr,
            exposure_rt = key_resp_3.rt,
            test_response = key_resp_5.keys,
            test_accuracy = key_resp_5.corr,
            test_rt = key_resp_5.rt,
            condition = expName,
            date
        ) %>%

        # only keep rows with trial info
        filter(!is.na(trial)) %>%

#### MODIFY AND CREATE COLUMNS ####
        mutate(
            # change None values to NA
            exposure_response = ifelse(exposure_response == "None", NA, exposure_response),
            test_response = ifelse(test_response == "None", NA, test_response),

            # remove "PAVL" from condition name
            condition = substr(condition, 6, 11),

            # format dates
            date = as.POSIXct(strptime(date, "%Y_%b_%d_%H%M")),

            # take experiment info from condition column
            experiment = str_to_upper(substr(condition, 6, 6)),

            # take order info from condition column
            order = ifelse(substr(condition, 1, 2) == substr(condition, 4, 5),
                           "SAME", "DIFF"),

            # add block number by 40s for trials 1-200, by 10s for trials 201-250
            block = ifelse(trial <= 200,
                           ((trial - 1) %/% 40) + 1,
                           ((trial - 201) %/% 10) + 1)
        ) %>%

        # sum up accurate trials
        mutate(
            exposure_acc_sum = sum(exposure_accuracy, na.rm  = TRUE),
            test_acc_sum = sum(test_accuracy, na.rm  = TRUE)) %>%

            #exposure_acc_sum = ifelse(is.na(exposure_accuracy), NA, exposure_acc_sum),
            #test_acc_sum = ifelse(is.na(test_accuracy), NA, test_acc_sum)) %>%

        # sum up accuracy by block
        group_by(block) %>%
        mutate(block_acc_sum = sum(test_accuracy))

#### COMPILE AND SAVE MASTER CSV ####
    # add this CSV data to the master list
    master_list[match(f, csv_files)] <- list(df_csv)
}
# create master data frame from master_list data frames
master_df <- bind_rows(master_list)

# create dated list of participants to determine run order
dates <- master_df %>%
    ungroup() %>%
    group_by(participant, condition) %>%
    summarize(date = unique(date)) %>%
    arrange(participant, date)
dates$run <- rep(c(1,2), length(unique(dates$participant)))

master_df %<>% left_join(dates) %>% # add run info to master
    # reorder columns
    select(participant, trial, talker,
           exposure_response, exposure_accuracy, exposure_acc_sum, exposure_rt,
           test_response, test_accuracy, test_acc_sum, test_rt,
           block, block_acc_sum,
           condition, experiment, order, run, date) %>%

    # sort by date
    arrange(date) %>%

    # convert date back to original string after sorting (to prevent time zone
    # format issues with write_csv())
    mutate(date = format(date, "%Y_%b_%d_%H%M"))

# save resulting data frame as a CSV with timestamp within project folder > Analysis
timestamp <- format(Sys.time(), tz = "EST5EDT", format = "%m-%d-%y_%H%M")
write_csv(master_df, file.path("Analysis", paste0(timestamp,"_master.csv")))
