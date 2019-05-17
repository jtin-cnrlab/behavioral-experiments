PerrachioneLab <- "/Volumes/PerrachioneLab"
setwd(file.path(PerrachioneLab, "projects", "TFTA", "Analysis"))

source(file.path(PerrachioneLab, "software", "r-scripts", "load_packages.R"))
load_packages("dplyr", "magrittr", "readr", "tidyr", "purrr")

source(file.path(PerrachioneLab, "software", "r-scripts", "within_3SD.R"))

test <- read_csv("master_test_stats.csv", col_types = "cciicicdiiccccddcccc")


#### 1 ####
aggregate(rt ~ single_mixed*carrier*trained_t*test, mean, data = test)
aggregate(rt ~ single_mixed*carrier*trained_t*test, sd, data = test)

# equivalent to...
test_aggregates <- test %>%
    group_by(single_mixed, carrier, trained_t, test) %>%
    summarize(mean_rt = mean(rt, na.rm = TRUE),
              sd_rt = sd(rt, na.rm = TRUE)) %T>%
    print()


#### 2 ####
# directly copied from tfta_plots.R ("READ IN AND PREPROCESS TRAINING DATA" and
# "TRAINING PLOTS" sections)

# read in master CSV
master_training <- read_csv("master_training.csv", col_types = "cciiicciicccdicc")

training <- master_training %>%
    filter(
        # only plot data from participants who have finished day 3 of training
        participant %in% unique(filter(master_training, day == 3)$participant)) %>%

    # identify trials with log rt within 3 standard deviations of mean log rt
    group_by(participant, day) %>%
    mutate(rt_all = rt,
           rt = within_3SD(rt)) %>%
    ungroup() %>%

    mutate(
        # add half-section and quarter-section labels
        hsection = section + (trial %/% 74)/2,
        qsection = section + (trial %/% 37)/4,

        # compute sensitivity measures
        #
        #                RESPONSE
        #               1   |   2
        #             - - - - - - -
        # STIMULUS  1 | hit | miss |
        #             - - - - - - -
        #           2 | fa  | crej |
        #             - - - - - - -
        # (1 = "same", 2 = "different"; 144 "same" trials, 448 "different" trials)
        resp_signal = ifelse(
            condition == "talker",
            # talker condition
            pmap_chr(list(stimulus = cresp_talker, response = resp),
                     function(stimulus, response)
                         case_when(
                             (stimulus == "num_1" && response == "num_1") ~ "hit",
                             (stimulus == "num_1" && response == "num_2") ~ "miss",
                             (stimulus == "num_2" && response == "num_1") ~ "fa",
                             (stimulus == "num_2" && response == "num_2") ~ "crej")),
            # word condition
            pmap_chr(list(stimulus = cresp_word, response = resp),
                     function(stimulus, response)
                         case_when(
                             (stimulus == "num_1" && response == "num_1") ~ "hit",
                             (stimulus == "num_1" && response == "num_2") ~ "miss",
                             (stimulus == "num_2" && response == "num_1") ~ "fa",
                             (stimulus == "num_2" && response == "num_2") ~ "crej")
            )
        )
    ) %>%

    # convert all variables except RT columns to factors (+ include paRTicipant)
    mutate_at(vars(-contains("rt"), participant), as.factor)

training_day <- training %>%
    group_by(participant, day) %>%
    summarize(
        # if hit rate = 1, adjust to 1 - 1/2N
        hit_rate = ifelse(
            sum(resp_signal == "hit") != sum(resp_signal %in% c("hit", "miss")),
            sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
            1 - 1/(2*sum(resp_signal %in% c("hit", "miss")))),
        # if false alarm rate = 0, adjust to 1/2N
        fa_rate = ifelse(
            sum(resp_signal == "fa") > 0,
            sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
            1/(2*sum(resp_signal %in% c("fa", "crej")))),
        dprime = qnorm(hit_rate) - qnorm(fa_rate),
        mean_rt = mean(rt, na.rm = TRUE)) %>%
    group_by(day) %>%
    summarize(se_dprime = sd(dprime)/sqrt(n()),
              mean_dprime = mean(dprime),
              mean_hit_rate = mean(hit_rate),
              mean_fa_rate = mean(fa_rate),
              se_rt = sd(mean_rt)/sqrt(n()),
              mean_rt = mean(mean_rt),
              n = n()) %T>%
    print()


#### 3 ####
# directly copied from tfta_plots.R ("SAVE TRAINING GROUPS", "READ IN AND PREPROCESS\
# TRANSCRIPTION DATA", and "TRANSCRIPTION PLOT" sections)
training_groups <- training %>%
    group_by(participant) %>%
    summarize_at(vars(talker_trained, vowel_trained), unique)

master_transcription <- read_csv("master_transcription.csv",
                                 col_types = "ciciiccciiiicc")

transcription <- master_transcription %>%
    # add training info (with training_groups participant factors as characters)
    left_join(mutate_all(training_groups, as.character), by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%

    # convert all variables except score columns to factors
    mutate_at(vars(-whole_word, -initial, -vowel, -final), as.factor)

tx_bar <- transcription %>%
    gather(key = part, value = correct, c(9:12)) %>% # cols 9-12 = scores
    group_by(participant, trained_t, part) %>%
    summarize(accuracy = sum(correct)/n()) %>%
    group_by(trained_t, part) %>%
    summarize(se_accuracy = sd(accuracy)/sqrt(n()),
              mean_accuracy = mean(accuracy),
              n = n()) %T>%
    print()


#### 4 ####
aggregate(correct ~ participant, mean, data = test)
mean(aggregate(correct ~ participant, mean, data = test)$correct)

# equivalent to...
accuracy_aggregate <- test %>%
    group_by(participant) %>%
    summarize(mean_accuracy = mean(correct)) %T>%
    print()
mean(accuracy_aggregate$mean_accuracy)

