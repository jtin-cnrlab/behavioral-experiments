# tfta_plots.R
# Jessica Tin
# 3 Jan 2019
#
# Creates plots for TFTA.
#

#### SET SERVER PATH ####
PerrachioneLab <- "/Volumes/PerrachioneLab" # Mac
#PerrachioneLab <- "/PerrachioneLab" # Linux
#PerrachioneLab <- "R:/PerrachioneLab" # Windows
setwd(file.path(PerrachioneLab, "projects", "TFTA", "Analysis"))

#### LOAD FUNCTIONS AND PACKAGES ####
source(file.path(PerrachioneLab, "software", "r-scripts", "load_packages.R"))
load_packages("dplyr", "magrittr", "readr", "tidyr", "purrr", "ggplot2", "viridis",
              "scales")

source(file.path(PerrachioneLab, "software", "r-scripts", "within_3SD.R"))

#### UPDATE MASTER CSVs ####
# skip this section if master CSVs are up to date
# must change server path within each script if not /Volumes/PerrachioneLab
source("tfta_training_master.R") # training data
source("tfta_test_master.R") # test data
source("tfta_transcription_master.R") # transcription data

#### READ IN AND PREPROCESS TRAINING DATA ####
# read in master CSV
master_training <- read_csv("master_training.csv", col_types = "cciiicciicccdicc")

training <- master_training %>%
    filter(
        # only plot data from participants who have finished day 3 of training
        participant %in% unique(filter(master_training, day == 3)$participant),

        # remove skipped trials
        !is.na(rt)) %>%

    # only keep trials with log rt within 3 standard deviations of mean log rt
    group_by(participant, day) %>%
    mutate(rt_3SD = within_3SD(rt)) %>%
    ungroup() %>%
    filter(!is.na(rt_3SD)) %>%

    mutate(
        # add half-section and quarter-section labels
        hsection = section + (trial %/% 74)*.5,
        qsection = section + (trial %/% 37)*.25,

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

    # convert all variables except RT columns to factors (+ manually include paRTicipant)
    mutate_at(vars(-contains("rt"), participant), as.factor)

#### SAVE TRAINING GROUPS ####
# save which participants trained on which talkers/vowels
training_groups <- training %>%
    group_by(participant) %>%
    summarize_at(vars(talker_trained, vowel_trained), unique)
write_csv(training_groups, "training_groups.csv")

#### READ IN AND PREPROCESS TEST DATA ####
# read in master CSV
master_test <- read_csv("master_test.csv", col_types = "cciiicdiicccc")

# add carrier duration info
carriers <- read_csv(file.path("..", "Stimuli", "carrier_durations.csv"),
                     col_types = "_id") %>%
    mutate(carrier_duration = carrier_duration * 1000)
master_test %<>% left_join(carriers, by = c("talker" = "talker_number"))

test <- master_test %>%
    filter(
        # only plot data from participants who have finished post-test
        participant %in% unique(filter(master_test, test == "post")$participant),

        # remove skipped trials
        !is.na(rt)) %>%

    # adjust rt for carrier trials
    mutate(rt = pmap_dbl(list(c = carrier, r = rt, d = carrier_duration),
                         function(c, r, d) ifelse(c == "ioua", r - d, r))) %>%
    filter(rt > 0) %>% # exclude trials answered before target word

    # only keep trials with log rt within 3 standard deviations of mean log rt
    group_by(participant, test) %>%
    mutate(rt_3SD = within_3SD(rt)) %>%
    ungroup() %>%
    filter(!is.na(rt_3SD),

           # only keep correct trials
           correct == 1) %>%

    # add training info (with training_groups participant factors as characters)
    left_join(mutate_all(training_groups, as.character), by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%

    # convert all variables except RT columns and duration to factors (+ manually
    # include paRTicipant)
    mutate_at(vars(-contains("rt"), -carrier_duration, participant), as.factor)

# refactor variables
test$test <- factor(test$test, levels = c("pre", "post"))
test$single_mixed <- factor(test$single_mixed, levels = c("single", "mixed"))
test$carrier <- factor(test$carrier, levels = c("none", "ioua"),
                       labels = c("No Carrier", "Carrier"))
test$trained_t <- factor(test$trained_t, levels = c("untrained", "trained"),
                         labels = c("Untrained Talkers", "Trained Talkers"))
test$trained_v <- factor(test$trained_v, levels = c("untrained", "trained"),
                         labels = c("Untrained Vowels", "Trained Vowels"))

#### READ IN AND PREPROCESS TRANSCRIPTION DATA ####
master_transcription <- read_csv("master_transcription.csv", col_types = "ciciiccciiiicc")

transcription <- master_transcription %>%
    # add training info (with training_groups participant factors as characters)
    left_join(mutate_all(training_groups, as.character), by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%

    # convert all variables except score columns to factors
    mutate_at(vars(-whole_word, -initial, -vowel, -final), as.factor)

# refactor variables
transcription$trained_t <- factor(transcription$trained_t, levels = c("untrained", "trained"),
                                  labels = c("Untrained Talkers", "Trained Talkers"))
transcription$trained_v <- factor(transcription$trained_v, levels = c("untrained", "trained"),
                                  labels = c("Untrained Vowels", "Trained Vowels"))

#### d' SUMMARY DATAFRAMES ####
summarize_dprime <- function(group_by_vars) {
    group_by_vars <- c("participant", group_by_vars)
    summary_df <- training %>%
        group_by_at(vars(group_by_vars)) %>%
        summarize(# if hit rate = 1, adjust to 1 - 1/2N
                  hit_rate = ifelse(
                      sum(resp_signal == "hit") != sum(resp_signal %in% c("hit", "miss")),
                      sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
                      1 - 1/(2*sum(resp_signal %in% c("hit", "miss")))),
                  # if false alarm rate = 0, adjust to 1/2N
                  fa_rate = ifelse(
                      sum(resp_signal == "fa") > 0,
                      sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
                      1/(2*sum(resp_signal %in% c("fa", "crej")))),
                  dprime = qnorm(hit_rate) - qnorm(fa_rate)) %>%
            group_by_at(vars(group_by_vars[-1])) %>%
            summarize(n = n(),
                      mean_hit_rate = mean(hit_rate),
                      mean_fa_rate = mean(fa_rate),
                      mean_dprime = mean(dprime),
                      error_dprime = sd(dprime)/sqrt(n)) %>%
            ungroup()
    return(summary_df)
}

#### d' PLOTS ####
plot_dprime <- function(df, plot_title) {
    breakdown <- max(setdiff(names(df), c("talker_trained", "n", "mean_hit_rate",
                                          "mean_fa_rate", "mean_dprime", "error_dprime")))


    dprime_plot <- ggplot(data = df, aes_string(x = breakdown, y = "mean_dprime")) +
        scale_y_continuous(limits = c(0, 4), expand = c(0,0)) +
        labs(title = paste("d' by", plot_title), y = "Mean d'", x = "Day") +
        scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
        theme(axis.title.x = element_text(size = 12),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 12),
              axis.line = element_line(),
              axis.ticks.length = unit(0.5, "lines"),
              panel.background = element_blank(),
              legend.position = c(0.06, 0.13),
              legend.background = element_rect(fill = "transparent"),
              plot.caption = element_text(size = 10))

    if ("talker_trained" %in% names(df)) {
        # plot training groups separately
        df_setA <- filter(df, talker_trained == "setA")
        df_setB <- filter(df, talker_trained == "setB")

        dprime_plot <- dprime_plot +
            aes(color = talker_trained) +
            geom_line(aes(group = talker_trained), size = 1) +

            # participants trained on setA
            geom_errorbar(data = df_setA, aes(ymin = mean_dprime - error_dprime,
                                              ymax = mean_dprime + error_dprime),
                          width = 0.1, alpha = 0.5, position = position_nudge(x = -0.005)) +
            geom_point(data = df_setA, size = 3, position = position_nudge(x = -0.005)) +

            # participants trained on setB
            geom_errorbar(data = df_setB, aes(ymin = mean_dprime - error_dprime,
                                              ymax = mean_dprime + error_dprime),
                          width = 0.1, alpha = 0.5, position = position_nudge(x = 0.005)) +
            geom_point(data = df_setB, size = 3, position = position_nudge(x = 0.005)) +

            labs(caption = paste0("setA n = ", unique(df_setA$n), "\nsetB n = ",
                                 unique(df_setB$n)))
    } else {
        # plot training groups together
        dprime_plot <- dprime_plot +
            # all participants
            geom_line(aes(group = 1), size = 1) +
            geom_errorbar(aes(ymin = mean_dprime - error_dprime,
                              ymax = mean_dprime + error_dprime),
                          width = 0.1, alpha = 0.5) +
            geom_point(size = 3) +

            labs(subtitle = "Training Groups Combined",
                 caption = paste("n =", unique(df$n))) +
            theme(plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5))
    }

    if (breakdown != "day") {
        # plot each day side-by-side
        dprime_plot <- dprime_plot +
            facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                                     `2` = "Day 2",
                                                                     `3` = "Day 3"))) +
            scale_x_discrete(labels = function(x) ifelse(endsWith(x, "5"), "",
                                                         paste0("Section ",x))) +
            theme(strip.placement = "outside",
                  strip.text = element_text(size = 12),
                  strip.background = element_blank(),
                  panel.spacing = unit(0, "lines"),
                  axis.title.x = element_blank())
    }

    print(dprime_plot)
}

#### d' BY DAY (TRAINING) ####
# separate training groups
dprime_day <- summarize_dprime(c("talker_trained", "day"))
dprime_day_plot <- plot_dprime(dprime_day, "Day")
ggsave(file.path("plots-training","dprime_byday.png"), width = 12, height = 6)

# combined training groups
dprime_day_combined <- summarize_dprime("day")
dprime_day_combn_plot <- plot_dprime(dprime_day_combined, "Day")
ggsave(file.path("plots-training","dprime_byday_combined.png"), width = 12, height = 6)

#### d' BY SECTION (TRAINING) ####
# separate training groups
dprime_section <- summarize_dprime(c("talker_trained", "day", "section"))
dprime_section_plot <- plot_dprime(dprime_section, "Section")
ggsave(file.path("plots-training","dprime_bysection.png"), width = 12, height = 6)

# combined training groups
dprime_section_combined <- summarize_dprime(c("day", "section"))
dprime_section_combn_plot <- plot_dprime(dprime_section_combined, "Section")
ggsave(file.path("plots-training","dprime_bysection_combined.png"), width = 12, height = 6)

#### d' BY HALF-SECTION (TRAINING) ####
# separate training groups
dprime_hsection <- summarize_dprime(c("talker_trained", "day", "hsection"))
dprime_hsection_plot <- plot_dprime(dprime_hsection, "Half-Section")
ggsave(file.path("plots-training","dprime_byhsection.png"), width = 12, height = 6)

# combined training groups
dprime_hsection_combined <- summarize_dprime(c("day", "hsection"))
dprime_hsection_combn_plot <- plot_dprime(dprime_hsection_combined, "Half-Section")
ggsave(file.path("plots-training","dprime_byhsection_combined.png"), width = 12, height = 6)

#### d' BY QUARTER-SECTION (TRAINING) ####
# separate training groups
dprime_qsection <- summarize_dprime(c("talker_trained", "day", "qsection"))
dprime_qsection_plot <- plot_dprime(dprime_qsection, "Quarter-Section")
ggsave(file.path("plots-training","dprime_byqsection.png"), width = 12, height = 6)

# combined training groups
dprime_qsection_combined <- summarize_dprime(c("day", "qsection"))
dprime_qsection_combn_plot <- plot_dprime(dprime_qsection_combined, "Quarter-Section")
ggsave(file.path("plots-training","dprime_byqsection_combined.png"), width = 12, height = 6)

#### RT SUMMARY DATAFRAMES ####
summarize_rt <- function(group_by_vars) {
    group_by_vars <- c("participant", group_by_vars)
    summary_df <- training %>%
        group_by_at(vars(group_by_vars)) %>%
        summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
        group_by_at(vars(group_by_vars[-1])) %>%
        summarize(n = n(),
                  mean_rt = mean(rt),
                  error_rt = sd(rt)/sqrt(n)) %>%
        ungroup()
    return(summary_df)
}

#### RT BY DAY (TRAINING) ####
rt_day <- summarize_rt(c("talker_trained", "day"))

rt_day_plot <- ggplot(data = rt_day,
                      aes(x = day, y = mean_rt, color = talker_trained)) +
    geom_errorbar(aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    scale_y_continuous(limits = c(600, 1700), breaks = seq.int(600, 1700, 100)) +
    labs(title = "RT by Day", y = "Mean RT (ms)",
         subtitle = "592 trials per day",
         x = "Day",
         caption = paste0("setA n = ",
                          unique(filter(rt_day, talker_trained == "setA")$n),
                          "\nsetB n = ",
                          unique(filter(rt_day, talker_trained == "setB")$n))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    theme(axis.title.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.line = element_line(),
          axis.ticks.length = unit(0.5, "lines"),
          panel.background = element_blank(),
          legend.position = c(0.06, 0.13),
          #legend.background = element_rect(fill = "transparent"),
          plot.caption = element_text(size = 10))
rt_day_plot
ggsave(file.path("plots-training","rt_byday.png"), width = 12, height = 6)

#### RT BY SECTION (TRAINING) ####
rt_section <- summarize_rt(c("talker_trained", "day", "section"))

rt_section_plot <- ggplot(data = rt_section,
                          aes(x = section, y = mean_rt, color = talker_trained)) +
    geom_errorbar(data = filter(rt_section, talker_trained == "setA"),
                  aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5, position = position_nudge(x = -0.005)) +
    geom_errorbar(data = filter(rt_section, talker_trained == "setB"),
                  aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5, position = position_nudge(x = 0.005)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    scale_y_continuous(limits = c(600, 1700), breaks = seq.int(600, 1700, 100)) +
    labs(title = "RT by Section by Day", y = "Mean RT (ms)",
         subtitle = "148 trials per section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ",
                          unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ",
                          unique(filter(rt_section, talker_trained == "setB")$n))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(),
          axis.ticks.length = unit(0.5, "lines"),
          panel.background = element_blank(),
          legend.position = c(0.06, 0.13),
          #legend.background = element_rect(fill = "transparent"),
          plot.caption = element_text(size = 10))
rt_section_plot
ggsave(file.path("plots-training","rt_bysection.png"), width = 12, height = 6)

#### RT BY HALF-SECTION (TRAINING) ####
rt_hsection <- summarize_rt(c("talker_trained", "day", "hsection"))

rt_hsection_plot <- ggplot(data = rt_hsection,
                           aes(x = hsection, y = mean_rt, color = talker_trained)) +
    geom_vline(aes(xintercept = 1), color = "gray70") +
    geom_vline(aes(xintercept = 3), color = "gray80") +
    geom_vline(aes(xintercept = 5), color = "gray80") +
    geom_vline(aes(xintercept = 7), color = "gray80") +
    geom_errorbar(data = filter(rt_hsection, talker_trained == "setA"),
                  aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5, position = position_nudge(x = -0.005)) +
    geom_errorbar(data = filter(rt_hsection, talker_trained == "setB"),
                  aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5, position = position_nudge(x = 0.005)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(endsWith(x, "_1"),
                                                 paste0("Section ",substr(x,1,1)), "")) +
    scale_y_continuous(limits = c(600, 1700), breaks = seq.int(600, 1700, 100)) +
    labs(title = "RT by Half-Section by Day", y = "Mean RT (ms)",
         subtitle = "74 trials per half-section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ",
                          unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ",
                          unique(filter(rt_section, talker_trained == "setB")$n))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(),
          axis.ticks.length = unit(0.5, "lines"),
          panel.background = element_blank(),
          legend.position = c(0.06, 0.13),
          #legend.background = element_rect(fill = "transparent"),
          plot.caption = element_text(size = 10))
rt_hsection_plot
ggsave(file.path("plots-training","rt_byhsection.png"), width = 12, height = 6)

#### RT BY QUARTER-SECTION (TRAINING) ####
rt_qsection <- summarize_rt(c("talker_trained", "day", "qsection"))

rt_qsection_plot <- ggplot(data = rt_qsection,
                           aes(x = qsection, y = mean_rt, color = talker_trained)) +
    geom_vline(aes(xintercept = 1), color = "gray70") +
    geom_vline(aes(xintercept = 5), color = "gray80") +
    geom_vline(aes(xintercept = 9), color = "gray80") +
    geom_vline(aes(xintercept = 13), color = "gray80") +
    geom_errorbar(data = filter(rt_qsection, talker_trained == "setA"),
                  aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5, position = position_nudge(x = -0.005)) +
    geom_errorbar(data = filter(rt_qsection, talker_trained == "setB"),
                  aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                  width = 0.35, alpha = 0.5, position = position_nudge(x = 0.005)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(endsWith(x, "_1"),
                                                 paste0("Section ",substr(x,1,1)), "")) +
    scale_y_continuous(limits = c(600, 1700), breaks = seq.int(600, 1700, 100)) +
    labs(title = "RT by Quarter-Section by Day", y = "Mean RT (ms)",
         subtitle = "37 trials per quarter-section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ",
                          unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ",
                          unique(filter(rt_section, talker_trained == "setB")$n))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(),
          axis.ticks.length = unit(0.5, "lines"),
          panel.background = element_blank(),
          legend.position = c(0.06, 0.13),
          #legend.background = element_rect(fill = "transparent"),
          plot.caption = element_text(size = 10))
rt_qsection_plot
ggsave(file.path("plots-training","rt_byqsection.png"), width = 12, height = 6)

#### TRAINING BOXPLOT: RT BY SECTION BY DAY ####
rt_box <- training %>%
    group_by(participant, talker_trained, day, section) %>%
    summarize(rt = mean(rt_3SD, na.rm = TRUE))

rt_bysection_boxplot <- ggplot(data = rt_box) +
    geom_boxplot(aes(x = section, y = rt, fill = as.character(day),
                     group = interaction(as.character(day), section)), coef = 999) +
    facet_wrap(~ talker_trained, labeller = as_labeller(c(`setA` = "Trained on setA Talkers",
                                                          `setB` = "Trained on setB Talkers")),
               scales = "free_y") +
    scale_x_discrete(labels = function(x) paste0("Section ",x)) +
    scale_y_continuous(limits = c(400, 2800), breaks = seq.int(400, 2800, 150)) +
    labs(title = "Training RT by Section by Day", y = "Mean RT (ms)",
         subtitle = "148 trials per section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ",
                          unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ",
                          unique(filter(rt_section, talker_trained == "setB")$n)),
         fill = "Day") +
    scale_fill_viridis(discrete = TRUE) +
    theme(axis.title.x = element_blank(),
          strip.placement = "outside",
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          legend.position = c(0.13, 0.95),
          legend.direction = "horizontal",
          axis.line = element_line(),
          axis.ticks.length = unit(0.5, "lines"),
          panel.background = element_blank(),
          plot.caption = element_text(size = 10))
rt_bysection_boxplot
ggsave(file.path("plots-training","rt_boxplots.png"), width = 8, height = 6)

#### TEST PLOT: RT, TRAINED VOWELS ####
test_rt_trained <- test %>%
    filter(trained_v == "Trained Vowels") %>%
    group_by(participant, carrier, single_mixed, test, talker_trained, trained_t) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
    group_by(carrier, single_mixed, test, talker_trained, trained_t) %>%
    summarize(mean_rt = mean(mean_rt))
test_rt_trained_plot <- ggplot(data = test_rt_trained,
                               aes(x = single_mixed, y = mean_rt, color = talker_trained,
                                   alpha = test, group = interaction(test, talker_trained))) +
    geom_hline(data = filter(test_rt_trained, talker_trained == "setA"),
               aes(yintercept = mean(mean_rt), linetype = "hlinemean", color = talker_trained,
                   group = talker_trained), alpha = 0.5, show.legend = TRUE) +
    geom_hline(data = filter(test_rt_trained, talker_trained == "setB"),
               aes(yintercept = mean(mean_rt), linetype = "hlinemean", color = talker_trained,
                   group = talker_trained), alpha = 0.5, show.legend = TRUE) +
    geom_line(size = 1) +
    geom_point(aes(shape = test), size = 3.5) +
    scale_y_continuous(limits = c(575, 875), breaks = c(seq.int(600, 900, 50))) +
    facet_grid(carrier ~ trained_t) +
    labs(title = "Pre- vs. Post-Test RT", subtitle = "Trained Vowels Only",
         x = "", y = "Mean RT (ms)", linetype = "means",
         caption = paste0("setA n = ",
                          length(unique(filter(test, talker_trained == "setA")$participant)),
                          "\nsetB n = ",
                          length(unique(filter(test, talker_trained == "setB")$participant)))) +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" μ setA = ",
                                            round(mean(filter(test_rt_trained,
                                                              talker_trained == "setA")$mean_rt),
                                                  digits = 2), " ms\n μ setB = ",
                                            round(mean(filter( test_rt_trained,
                                                               talker_trained == "setB")$mean_rt),
                                                  digits = 2), " ms"))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    scale_alpha_discrete(range = c(0.4, 1)) + # discrete alpha throws unnecessary warning
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.x = unit(1, "lines"),
          plot.caption = element_text(size = 10))
test_rt_trained_plot
ggsave(file.path("plots-test", "rt_trainedv.png"), width = 7.5, height = 6.5)

#### TEST PLOT: RT, ALL VOWELS ####
test_rt_all <- test %>%
    group_by(participant, carrier, single_mixed, test, talker_trained, trained_t) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
    group_by(carrier, single_mixed, test, talker_trained, trained_t) %>%
    summarize(mean_rt = mean(mean_rt))
test_rt_all_plot <- ggplot(data = test_rt_all,
                           aes(x = single_mixed, y = mean_rt, color = talker_trained,
                               alpha = test, group = interaction(test, talker_trained))) +
    geom_hline(data = filter(test_rt_all, talker_trained == "setA"),
               aes(yintercept = mean(mean_rt), linetype = "hlinemean", color = talker_trained,
                   group = talker_trained), alpha = 0.5, show.legend = TRUE) +
    geom_hline(data = filter(test_rt_all, talker_trained == "setB"),
               aes(yintercept = mean(mean_rt), linetype = "hlinemean", color = talker_trained,
                   group = talker_trained), alpha = 0.5, show.legend = TRUE) +
    geom_line(size = 1) +
    geom_point(aes(shape = test), size = 3.5) +
    scale_y_continuous(limits = c(575, 875), breaks = c(seq.int(600, 900, 50))) +
    facet_grid(carrier ~ trained_t) +
    labs(title = "Pre- vs. Post-Test RT", subtitle = "All Vowels",
         x = "", y = "Mean RT (ms)", linetype = "means",
         caption = paste0("setA n = ",
                          length(unique(filter(test, talker_trained == "setA")$participant)),
                          "\nsetB n = ",
                          length(unique(filter(test, talker_trained == "setB")$participant))),
         shape = "test", alpha = "test") +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" μ setA = ",
                                            round(mean(filter(test_rt_all,
                                                              talker_trained == "setA")$mean_rt),
                                                  digits = 2), " ms\n μ setB = ",
                                            round(mean(filter(test_rt_all,
                                                              talker_trained == "setB")$mean_rt),
                                                  digits = 2), " ms"))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    scale_alpha_discrete(range = c(0.4, 1)) + # discrete alpha throws unnecessary warning
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.x = unit(1, "lines"),
          plot.caption = element_text(size = 10))
test_rt_all_plot
ggsave(file.path("plots-test", "rt_allv.png"), width = 7.5, height = 6.5)

#### TEST PLOT: RT, TRAINED VOWELS, COMBINED TRAINING GROUPS ####
test_rt_trained_combn <- test %>%
    filter(trained_v == "Trained Vowels") %>%
    group_by(participant, carrier, single_mixed, test, trained_t) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
    group_by(carrier, single_mixed, test, trained_t) %>%
    summarize(mean_rt = mean(mean_rt))
test_rt_trained_combn_plot <- ggplot(data = test_rt_trained_combn,
                                     aes(x = single_mixed, y = mean_rt,
                                         alpha = test, group = test)) +
    geom_hline(data = test_rt_trained_combn,
               aes(yintercept = mean(mean_rt), linetype = "hlinemean"),
               alpha = 0.5, show.legend = TRUE) +
    geom_line(size = 1) +
    geom_point(aes(shape = test), size = 3.5) +
    scale_y_continuous(limits = c(575, 875), breaks = c(seq.int(600, 900, 50))) +
    facet_grid(carrier ~ trained_t) +
    labs(title = " Pre- vs. Post-Test RT \n(Training Groups Combined)", subtitle = "Trained Vowels Only",
         x = "", y = "Mean RT (ms)", linetype = "mean") +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" μ = ",
                                            round(mean(test_rt_trained_combn$mean_rt),
                                                  digits = 2), " ms"))) +
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
    scale_alpha_discrete(range = c(0.4, 1)) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.x = unit(1, "lines"),
          plot.caption = element_text(size = 10))
test_rt_trained_combn_plot
ggsave(file.path("plots-test", "rt_trainedv_combined.png"), width = 7.5, height = 6.5)

#### TRANSCRIPTION BOXPLOT: ACCURACY BY TRAINED/UNTRAINED TALKERS, TRAINED VOWELS ####
tx_trainedv <- transcription %>%
    filter(trained_v == "trained") %>%
    gather(key = part, value = score, c(9:12)) %>% # cols 9-12 = scores
    group_by(participant, talker_trained, trained_t, part) %>%
    summarize(score_total = sum(score), n = n()) %>%
    mutate(score_percent = score_total/n)
tx_trainedv$part <- factor(tx_trainedv$part, levels = c("whole_word", "initial", "vowel", "final"),
                           labels = c("whole\nword", "initial", "vowel", "final"))
tx_trainedv_segments <- tx_trainedv %>%
    select(-score_total, -n) %>%
    spread(key = trained_t, value = score_percent) %>%
    rename(y = untrained, yend = trained) %>%
    mutate(x = match(part, levels(part)) - 0.1,
           xend = match(part, levels(part)) + 0.1)
tx_trainedv$trained_t <- factor(tx_trainedv$trained_t, levels = c("untrained", "trained"),
                                labels = c("Untrained Talkers", "Trained Talkers"))

tx_trainedv_plot <- ggplot(data = tx_trainedv, aes(x = part, y = score_percent*100)) +
    geom_hline(aes(yintercept = mean(tx_trainedv$score_percent)*100, linetype = "hlinemean"),
               color = "gray75", show.legend = TRUE) +
    geom_boxplot(aes(fill = trained_t), coef = 999, position = position_dodge(.9)) +
    geom_segment(data = tx_trainedv_segments,
                 aes(x = x, y = y*100, xend = xend, yend = yend*100, color = participant),
                 alpha = 0.9) +
    geom_point(data = filter(tx_trainedv, trained_t == "Untrained Talkers"),
               aes(group = participant), alpha = 0.6, position = position_nudge(x = -.1)) +
    geom_point(data = filter(tx_trainedv, trained_t == "Trained Talkers"),
               aes(group = participant), alpha = 0.6, position = position_nudge(x = .1)) +
    facet_grid(~ talker_trained, labeller = as_labeller(c(`setA` = "Trained on setA Talkers",
                                                          `setB` = "Trained on setB Talkers"))) +
    scale_y_continuous(limits = c(0, 101), breaks = seq.int(0,100,10),
                       labels = function(x) paste0(x,"%"), expand = c(0,0),
                       sec.axis = sec_axis(~.*.96, name = "Words Correct\n",
                                           breaks = seq.int(0, 96, 12))) +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" mean = ",
                                            round(mean(tx_trainedv$score_percent)*100,
                                                  digits = 2), "%"))) +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    scale_fill_viridis(discrete = TRUE, option = "inferno", begin = .2, end = .7,
                       direction = -1) +
    labs(title = "Transcription Accuracy", subtitle = "Trained Vowels Only",
         x = "", y = "Percent Correct",
         caption = paste0("setA n = ",
                          length(unique(filter(tx_trainedv, talker_trained == "setA")$participant)),
                          "\nsetB n = ",
                          length(unique(filter(tx_trainedv, talker_trained == "setB")$participant)))) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          #axis.line.y = element_line(),
          legend.position = c(.88,.12),
          legend.title = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          axis.ticks.length = unit(0.35, "lines"),
          plot.caption = element_text(size = 10))
tx_trainedv_plot
ggsave(file.path("plots-transcription", "accuracy_trainedvowels.png"), width = 8.5, height = 7.5)

#### TRANSCRIPTION BOXPLOT: TRAINED VOWELS, COMBINED TRAINING GROUPS ####
tx_trainedv_combn <- transcription %>%
    filter(trained_v == "trained") %>%
    gather(key = part, value = score, c(9:12)) %>% # cols 9-12 = scores
    group_by(participant, trained_t, part) %>%
    summarize(score_total = sum(score), n = n()) %>%
    mutate(score_percent = score_total/n)
tx_trainedv_combn$part <- factor(tx_trainedv_combn$part,
                                 levels = c("whole_word", "initial", "vowel", "final"),
                                 labels = c("whole\nword", "initial", "vowel", "final"))
tx_trainedv_combn_segments <- tx_trainedv_combn %>%
    select(-score_total, -n) %>%
    spread(key = trained_t, value = score_percent) %>%
    rename(y = untrained, yend = trained) %>%
    mutate(x = match(part, levels(part)) - 0.1,
           xend = match(part, levels(part)) + 0.1)
tx_trainedv_combn$trained_t <- factor(tx_trainedv_combn$trained_t,
                                      levels = c("untrained", "trained"),
                                      labels = c("Untrained Talkers", "Trained Talkers"))

tx_trainedv_combn_plot <- ggplot(data = tx_trainedv_combn, aes(x = part, y = score_percent*100)) +
    geom_hline(aes(yintercept = mean(tx_trainedv_combn$score_percent)*100, linetype = "hlinemean"),
               color = "gray75", show.legend = TRUE) +
    geom_boxplot(aes(fill = trained_t), coef = 999, position = position_dodge(.9)) +
    geom_segment(data = tx_trainedv_combn_segments,
                 aes(x = x, y = y*100, xend = xend, yend = yend*100, color = participant),
                 alpha = 0.9) +
    geom_point(data = filter(tx_trainedv_combn, trained_t == "Untrained Talkers"),
               aes(group = participant), alpha = 0.6, position = position_nudge(x = -.1)) +
    geom_point(data = filter(tx_trainedv_combn, trained_t == "Trained Talkers"),
               aes(group = participant), alpha = 0.6, position = position_nudge(x = .1)) +
    scale_y_continuous(limits = c(0, 101), breaks = seq.int(0,100,10),
                       labels = function(x) paste0(x,"%"), expand = c(0,0),
                       sec.axis = sec_axis(~.*.96, name = "Words Correct\n",
                                           breaks = seq.int(0, 96, 12))) +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" mean = ",
                                            round(mean(tx_trainedv_combn$score_percent)*100,
                                                  digits = 2), "%"))) +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    scale_fill_viridis(discrete = TRUE, option = "inferno", begin = .2, end = .7,
                       direction = -1) +
    labs(title = "Transcription Accuracy\n(Training Groups Combined)", subtitle = "Trained Vowels Only",
         x = "", y = "Percent Correct",
         caption = paste0("n = ",
                          length(unique(tx_trainedv_combn$participant)))) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          #axis.line.y = element_line(),
          legend.position = c(.88,.12),
          legend.title = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          axis.ticks.length = unit(0.35, "lines"),
          plot.caption = element_text(size = 10))
tx_trainedv_combn_plot
ggsave(file.path("plots-transcription", "accuracy_trainedv_combined.png"), width = 8.5, height = 7.5)

#### TRANSCRIPTION BOXPLOT: ACCURACY BY TRAINED/UNTRAINED TALKERS, ALL VOWELS ####
tx_allv <- transcription %>%
    gather(key = part, value = score, c(9:12)) %>% # cols 9-12 = scores
    group_by(participant, talker_trained, trained_t, part) %>%
    summarize(score_total = sum(score), n = n()) %>%
    mutate(score_percent = score_total/n)
tx_allv$part <- factor(tx_allv$part, levels = c("whole_word", "initial", "vowel", "final"),
                       labels = c("whole\nword", "initial", "vowel", "final"))
tx_allv_segments <- tx_allv %>%
    select(-score_total, -n) %>%
    spread(key = trained_t, value = score_percent) %>%
    rename(y = untrained, yend = trained) %>%
    mutate(x = match(part, levels(part)) - 0.1,
           xend = match(part, levels(part)) + 0.1)
tx_allv$trained_t <- factor(tx_allv$trained_t, levels = c("untrained", "trained"),
                            labels = c("Untrained Talkers", "Trained Talkers"))

tx_allv_plot <- ggplot(data = tx_allv, aes(x = part, y = score_percent*100)) +
    geom_hline(aes(yintercept = mean(tx_allv$score_percent)*100, linetype = "hlinemean"),
               color = "gray75", show.legend = TRUE) +
    geom_boxplot(aes(fill = trained_t), coef = 999, position = position_dodge(.9)) +
    geom_segment(data = tx_allv_segments,
                 aes(x = x, y = y*100, xend = xend, yend = yend*100, color = participant),
                 alpha = 0.9) +
    geom_point(data = filter(tx_allv, trained_t == "Untrained Talkers"),
               aes(group = participant), alpha = 0.6, position = position_nudge(x = -.1)) +
    geom_point(data = filter(tx_allv, trained_t == "Trained Talkers"),
               aes(group = participant), alpha = 0.6, position = position_nudge(x = .1)) +
    facet_grid(~ talker_trained, labeller = as_labeller(c(`setA` = "Trained on setA Talkers",
                                                          `setB` = "Trained on setB Talkers"))) +
    scale_y_continuous(limits = c(0, 101), breaks = seq.int(0,100,10),
                       labels = function(x) paste0(x,"%"), expand = c(0,0),
                       sec.axis = sec_axis(~.*.96, name = "Words Correct\n",
                                           breaks = seq.int(0, 96, 12))) +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" mean = ",
                                            round(mean(tx_allv$score_percent)*100,
                                                  digits = 2), "%"))) +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    scale_fill_viridis(discrete = TRUE, option = "inferno", begin = .2, end = .7,
                       direction = -1) +
    labs(title = "Transcription Accuracy", subtitle = "All Vowels",
         x = "", y = "Percent Correct",
         caption = paste0("setA n = ",
                          length(unique(filter(tx_allv, talker_trained == "setA")$participant)),
                          "\nsetB n = ",
                          length(unique(filter(tx_allv, talker_trained == "setB")$participant)))) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          #axis.line.y = element_line(),
          legend.position = c(.88,.12),
          legend.title = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          axis.ticks.length = unit(0.35, "lines"),
          plot.caption = element_text(size = 10))
tx_allv_plot
ggsave(file.path("plots-transcription", "accuracy_allvowels.png"), width = 8.5, height = 7.5)

#### TRANSCRIPTION BARPLOT: ACCURACY BY TRAINED/UNTRAINED TALKERS AND VOWELS ####
tx_bar <- transcription %>%
    gather(key = part, value = score, c(9:12)) %>% # cols 9-12 = scores
    group_by(talker_trained, trained_t, trained_v, part) %>%
    summarize(score_total = sum(score), denom = n()) %>%
    mutate(pct = score_total/denom)
tx_bar$part <- factor(tx_bar$part, levels = c("whole_word", "initial", "vowel", "final"),
                      labels = c("whole\nword", "initial", "vowel", "final"))
tx_bar$trained_t <- factor(tx_bar$trained_t, levels = c("untrained", "trained"),
                           labels = c("Untrained Talkers", "Trained Talkers"))
tx_bar$trained_v <- factor(tx_bar$trained_v, levels = c("untrained", "trained"),
                           labels = c("Untrained Vowels", "Trained Vowels"))

tx_bar_plot <- ggplot(data = tx_bar, aes(x = part, y = pct*100)) +
    geom_col(aes(fill = trained_t), position = position_dodge()) +
    facet_grid(trained_v ~ talker_trained,
               labeller = as_labeller(c(`setA` = "Trained on setA Talkers",
                                        `setB` = "Trained on setB Talkers",
                                        `Untrained Vowels` = "Untrained Vowels",
                                        `Trained Vowels` = "Trained Vowels"))) +
    scale_y_continuous(limits = c(0, 100), breaks = c(seq.int(0,100,20)),
                       labels = function(x) paste0(x,"%")) +
    labs(title = "Transcription Accuracy",
         x = "", y = "Mean Percent Correct (out of 96 words)",
         caption = paste0("setA n = ",
                          length(unique(filter(tx_allv, talker_trained == "setA")$participant)),
                          "\nsetB n = ",
                          length(unique(filter(tx_allv, talker_trained == "setB")$participant)))) +
    scale_fill_viridis(discrete = TRUE, option = "inferno", begin = .2, end = .7,
                       direction = -1) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.x = unit(2, "lines"),
          legend.position = c(.03,-.13),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          plot.caption = element_text(size = 10))
tx_bar_plot
ggsave(file.path("plots-transcription", "trained_vs_untrained_vowels.png"), width = 6.25, height = 6.25)
