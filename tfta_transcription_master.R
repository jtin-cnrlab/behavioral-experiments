# tfta_transcription_master.R
# Jessica Tin
# 21 Dec 2018
#
# Compiles transcription CSVs from each participant's data folder into a single
# master CSV for TFTA.
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
# find all files within project folder > Analysis > transcription-scoring-completed
csv_files <- list.files(file.path("transcription-scoring-completed"),
                        full.names = TRUE)

# preallocate list to save all individual CSV data to
master_list <- vector("list", length = length(csv_files))

# loop through each CSV file
for (f in csv_files) {
    print(paste0("[",match(f, csv_files),"/",length(csv_files),"] ",f))

#### COMPILE AND SAVE MASTER CSV ####
    # add this CSV data to the master list
    master_list[match(f, csv_files)] <- list(read_csv(f, col_types = "ccciiccciiii"))
}

# create master data frame from master_list data frames
master_df <- bind_rows(master_list) %>%
    mutate(talker_set = ifelse(talker <= 4, "setA", "setB"),
           vowel_pair = ifelse(word_vowel %in% c("u", "o"), "pairA", "pairB")) %>%
    mutate_if(is.character, as.factor)

# save resulting data frame as a CSV (in project folder > Analysis)
print("Saving: master_transcription.csv")
write_csv(master_df, "master_transcription.csv")
print(file.path(getwd(), "master_transcription.csv"))
