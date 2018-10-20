# tfta_plots.R
# Jessica Tin
# 20 Oct 2018
#
# Creates plots for TFTA.
#

#### SET SERVER PATH ####
PerrachioneLab <- "/Volumes/PerrachioneLab" # Mac
#PerrachioneLab <- "/PerrachioneLab" # Linux
#PerrachioneLab <- "R:/PerrachioneLab" # Windows
setwd(file.path(PerrachioneLab, "projects", "TFTA", "Analysis"))

#### LOAD PACKAGES ####
source(file.path(PerrachioneLab, "software", "r-scripts", "load_packages.R"))
load_packages("dplyr", "magrittr", "readr", "tidyr", "purrr", "ggplot2", "viridis",
              "scales")

#### READ IN AND PREPROCESS TRAINING DATA ####
# update and read in master CSV
source("tfta_training_master.R") # must change server path before sourcing if different
master_training <- read_csv("master_training.csv", col_types = "cciiicciiccdicc") %>%
    mutate_if(is.character, as.factor)
training <- master_training %>%
    filter(!is.na(rt)) %>% # remove skipped trials

    # specify trials with log rt within 3 standard deviations of mean log rt
    group_by(participant, day) %>%
    mutate(mean_logrt = mean(log(rt)),
           sd_logrt = sd(log(rt))) %>%
    rowwise() %>%
    mutate(rt_3SD = ifelse((log(rt) <= mean_logrt + 3*sd_logrt) &&
                               (log(rt) >= mean_logrt - 3*sd_logrt), rt, NA)) %>%
    filter(!is.na(rt_3SD)) %>%
    mutate(
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
            case_when(
                all(talker_resp == "num_1", correct == 1) ~ "hit",
                all(talker_resp == "num_1", correct == 0) ~ "miss",
                all(talker_resp == "num_2", correct == 1) ~ "crej",
                all(talker_resp == "num_2", correct == 0) ~ "fa"),
            # word condition
            case_when(
                all(word_resp == "num_1", correct == 1) ~ "hit",
                all(word_resp == "num_1", correct == 0) ~ "miss",
                all(word_resp == "num_2", correct == 1) ~ "crej",
                all(word_resp == "num_2", correct == 0) ~ "fa")
        )
    ) %>% ungroup()

# save info about which participants trained on which talkers/vowels
trained_groups <- training %>%
    group_by(participant) %>%
    summarize_at(vars(talker_trained, vowel_trained), unique) %>%
    mutate_if(is.character, as.factor)

#### READ IN AND PREPROCESS TEST DATA ####
# update and read in master CSV
source("tfta_test_master.R") # must change server path before sourcing if different
master_test <- read_csv("master_test.csv", col_types = "cciiicdiicccc") %>%
    mutate_if(is.character, as.factor)

# add carrier duration info
carriers <- read_csv(file.path("..", "Stimuli", "carrier_durations.csv"),
                     col_types = "_id") %>%
    mutate(carrier_duration = carrier_duration * 1000)
master_test %<>% left_join(carriers, by = c("talker" = "talker_number"))

test <- master_test %>%
    filter(!is.na(rt)) %>% # remove skipped trials

    # adjust rt for carrier trials
    rowwise() %>%
    mutate(rt = ifelse(carrier == "ioua", rt - carrier_duration, rt)) %>%
    ungroup() %>%
    filter(rt > 0) %>% # exclude trials answered before target word

    # specify trials with log rt within 3 standard deviations of mean log rt
    group_by(participant, test) %>%
    mutate(mean_logrt = mean(log(rt)),
           sd_logrt = sd(log(rt))) %>%
    rowwise() %>%
    mutate(rt_3SD = ifelse((log(rt) <= mean_logrt + 3*sd_logrt) &&
                               (log(rt) >= mean_logrt - 3*sd_logrt), rt, NA)) %>%
    ungroup() %>%
    filter(!is.na(rt_3SD),

           # only keep correct trials
           correct == 1) %>%

    # add trained info
    left_join(trained_groups, by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained"))

# refactor variables
test$test <- factor(test$test, levels = c("pre", "post"))
test$single_mixed <- factor(test$single_mixed, levels = c("single", "mixed"))
test$carrier <- factor(test$carrier, levels = c("none", "ioua"),
                       labels = c("No Carrier", "Carrier"))
test$trained_t <- factor(test$trained_t, levels = c("untrained", "trained"),
                         labels = c("Untrained Talkers", "Trained Talkers"))
test$trained_v <- factor(test$trained_v, levels = c("untrained", "trained"),
                         labels = c("Untrained Vowels", "Trained Vowels"))

#### COMPILE AND PREPROCESS TRANSCRIPTION DATA ####
# compile master CSV
tx_files <- list.files(file.path("transcription-scoring-completed"), full.names = TRUE)
tx_list <- vector("list", length = length(tx_files))
for (f in tx_files) {
    print(paste0("[",match(f, tx_files),"/",length(tx_files),"] ",f))
    tx_list[match(f, tx_files)] <- list(read_csv(f, col_types = "ccciiccciiii"))
}
tx_master <- suppressWarnings( # hide warning about participant factor levels
    bind_rows(tx_list) %>%
        left_join(trained_groups, by = "participant") %>%
        mutate(talker_set = ifelse(talker <= 4, "setA", "setB"),
               trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
               vowel_pair = ifelse(word_vowel %in% c("u", "o"), "pairA", "pairB"),
               trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%
        mutate_if(is.character, as.factor))

#### TRAINING PLOT: d' BY SECTION ####
dprime_section <- training %>%
    group_by(participant, talker_trained, day, section) %>%
    summarize(n = n(),
              # if hit rate = 1, adjust to 1 - 1/2N
              hit_rate = ifelse(sum(resp_signal == "hit") != sum(resp_signal %in% c("hit", "miss")),
                                sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
                                1 - 1/(2*sum(resp_signal %in% c("hit", "miss")))),
              # if false alarm rate = 0, adjust to 1/2N
              fa_rate = ifelse(sum(resp_signal == "fa") > 0,
                               sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
                               1/(2*sum(resp_signal %in% c("fa", "crej")))),
              dprime = qnorm(hit_rate) - qnorm(fa_rate)) %>%
    group_by(talker_trained, day, section) %>%
    summarize(n = n(),
              mean_hit_rate = mean(hit_rate),
              mean_fa_rate = mean(fa_rate),
              mean_dprime = mean(dprime))

dprime_section_plot <- ggplot(data = dprime_section,
                              aes(x = section, y = mean_dprime, color = talker_trained)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    scale_y_continuous(limits = c(2.35, 3.85)) +
    labs(title = "d' by Section by Day", y = "Mean d'",
         subtitle = "148 trials per section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(dprime_section, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(dprime_section, talker_trained == "setB")$n))) +
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
          legend.position = c(0.05, 0.93),
          legend.background = element_rect(fill = "transparent"))
dprime_section_plot
ggsave(file.path("plots-training","dprime_bysection.png"), width = 12, height = 6)

#### TRAINING PLOT: d' BY HALF-SECTION ####
dprime_hsection <- training %>%
    arrange(talker_trained, day, section, trial) %>%
    mutate(hsection = paste0(section,"_",(trial %/% 74) + 1)) %>%
    group_by(participant, talker_trained, day, hsection) %>%
    summarize(n = n(),
              # if hit rate = 1, adjust to 1 - 1/2N
              hit_rate = ifelse(sum(resp_signal == "hit") != sum(resp_signal %in% c("hit", "miss")),
                                sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
                                1 - 1/(2*sum(resp_signal %in% c("hit", "miss")))),
              # if false alarm rate = 0, adjust to 1/2N
              fa_rate = ifelse(sum(resp_signal == "fa") > 0,
                               sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
                               1/(2*sum(resp_signal %in% c("fa", "crej")))),
              dprime = qnorm(hit_rate) - qnorm(fa_rate)) %>%
    group_by(talker_trained, day, hsection) %>%
    summarize(n = n(),
              mean_hit_rate = mean(hit_rate),
              mean_fa_rate = mean(fa_rate),
              mean_dprime = mean(dprime))

dprime_hsection_plot <- ggplot(data = dprime_hsection,
                               aes(x = hsection, y = mean_dprime, color = talker_trained)) +
    #geom_vline(aes(xintercept = 1), color = "gray70") +
    geom_vline(aes(xintercept = 3), color = "gray80") +
    geom_vline(aes(xintercept = 5), color = "gray80") +
    geom_vline(aes(xintercept = 7), color = "gray80") +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(endsWith(x, "_1"),
                                                 paste0("Section ",substr(x,1,1)), "")) +
    scale_y_continuous(limits = c(2.35, 3.85)) +
    labs(title = "d' by Half-Section by Day", y = "Mean d'",
         subtitle = "74 trials per half-section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(dprime_hsection, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(dprime_hsection, talker_trained == "setB")$n))) +
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
          legend.position = c(0.05, 0.93),
          legend.background = element_rect(fill = "transparent"))
dprime_hsection_plot
ggsave(file.path("plots-training","dprime_byhsection.png"), width = 12, height = 6)

#### TRAINING PLOT: d' BY QUARTER-SECTION ####
dprime_qsection <- training %>%
    arrange(talker_trained, day, section, trial) %>%
    mutate(qsection = paste0(section,"_",(trial %/% 37) + 1)) %>%
    group_by(participant, talker_trained, day, qsection) %>%
    summarize(n = n(),
              # if hit rate = 1, adjust to 1 - 1/2N
              hit_rate = ifelse(sum(resp_signal == "hit") != sum(resp_signal %in% c("hit", "miss")),
                                sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
                                1 - 1/(2*sum(resp_signal %in% c("hit", "miss")))),
              # if false alarm rate = 0, adjust to 1/2N
              fa_rate = ifelse(sum(resp_signal == "fa") > 0,
                               sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
                               1/(2*sum(resp_signal %in% c("fa", "crej")))),
              dprime = qnorm(hit_rate) - qnorm(fa_rate)) %>%
    group_by(talker_trained, day, qsection) %>%
    summarize(n = n(),
              mean_hit_rate = mean(hit_rate),
              mean_fa_rate = mean(fa_rate),
              mean_dprime = mean(dprime))

dprime_qsection_plot <- ggplot(data = dprime_qsection,
                               aes(x = qsection, y = mean_dprime, color = talker_trained)) +
    #geom_vline(aes(xintercept = 1), color = "gray70") +
    geom_vline(aes(xintercept = 5), color = "gray80") +
    geom_vline(aes(xintercept = 9), color = "gray80") +
    geom_vline(aes(xintercept = 13), color = "gray80") +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(endsWith(x, "_1"),
                                                 paste0("Section ",substr(x,1,1)), "")) +
    scale_y_continuous(limits = c(2.35, 3.85)) +
    labs(title = "d' by Quarter-Section by Day", y = "Mean d'",
         subtitle = "37 trials per quarter-section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(dprime_qsection, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(dprime_qsection, talker_trained == "setB")$n))) +
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
          legend.position = c(0.05, 0.93),
          legend.background = element_rect(fill = "transparent"))
dprime_qsection_plot
ggsave(file.path("plots-training","dprime_byqsection.png"), width = 12, height = 6)

#### TRAINING PLOT: RT BY SECTION ####
rt_section <- training %>%
    group_by(participant, talker_trained, day, section) %>%
    summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
    group_by(talker_trained, day, section) %>%
    summarize(n = n(),
              mean_rt = mean(rt))

rt_section_plot <- ggplot(data = rt_section,
                          aes(x = section, y = mean_rt, color = talker_trained)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    scale_y_continuous(limits = c(800, 1450), breaks = seq.int(800, 1450, 100)) +
    labs(title = "RT by Section by Day", y = "Mean RT (ms)",
         subtitle = "148 trials per section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(rt_section, talker_trained == "setB")$n))) +
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
          legend.position = c(0.05, 0.93),
          legend.background = element_rect(fill = "transparent"))
rt_section_plot
ggsave(file.path("plots-training","rt_bysection.png"), width = 12, height = 6)

#### TRAINING PLOT: RT BY HALF-SECTION ####
rt_hsection <- training %>%
    arrange(talker_trained, day, section, trial) %>%
    mutate(hsection = paste0(section,"_",(trial %/% 74) + 1)) %>%
    group_by(participant, talker_trained, day, hsection) %>%
    summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
    group_by(talker_trained, day, hsection) %>%
    summarize(n = n(),
              mean_rt = mean(rt))

rt_hsection_plot <- ggplot(data = rt_hsection,
                           aes(x = hsection, y = mean_rt, color = talker_trained)) +
    #geom_vline(aes(xintercept = 1), color = "gray70") +
    geom_vline(aes(xintercept = 3), color = "gray80") +
    geom_vline(aes(xintercept = 5), color = "gray80") +
    geom_vline(aes(xintercept = 7), color = "gray80") +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(endsWith(x, "_1"),
                                                 paste0("Section ",substr(x,1,1)), "")) +
    scale_y_continuous(limits = c(800, 1450), breaks = seq.int(800, 1450, 100)) +
    labs(title = "RT by Half-Section by Day", y = "Mean RT (ms)",
         subtitle = "74 trials per half-section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(rt_section, talker_trained == "setB")$n))) +
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
          legend.position = c(0.05, 0.93),
          legend.background = element_rect(fill = "transparent"))
rt_hsection_plot
ggsave(file.path("plots-training","rt_byhsection.png"), width = 12, height = 6)

#### TRAINING PLOT: RT BY QUARTER-SECTION ####
rt_qsection <- training %>%
    arrange(talker_trained, day, section, trial) %>%
    mutate(qsection = paste0(section,"_",(trial %/% 37) + 1)) %>%
    group_by(participant, talker_trained, day, qsection) %>%
    summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
    group_by(talker_trained, day, qsection) %>%
    summarize(n = n(),
              mean_rt = mean(rt))

rt_qsection_plot <- ggplot(data = rt_qsection,
                           aes(x = qsection, y = mean_rt, color = talker_trained)) +
    #geom_vline(aes(xintercept = 1), color = "gray70") +
    geom_vline(aes(xintercept = 5), color = "gray80") +
    geom_vline(aes(xintercept = 9), color = "gray80") +
    geom_vline(aes(xintercept = 13), color = "gray80") +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_discrete(labels = function(x) ifelse(endsWith(x, "_1"),
                                                 paste0("Section ",substr(x,1,1)), "")) +
    scale_y_continuous(limits = c(800, 1450), breaks = seq.int(800, 1450, 100)) +
    labs(title = "RT by Quarter-Section by Day", y = "Mean RT (ms)",
         subtitle = "37 trials per quarter-section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(rt_section, talker_trained == "setB")$n))) +
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
          legend.position = c(0.05, 0.93),
          legend.background = element_rect(fill = "transparent"))
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
    scale_x_continuous(labels = function(x) paste0("Section ",x)) +
    scale_y_continuous(limits = c(600, 2500), breaks = seq.int(600, 2500, 150)) +
    labs(title = "Training RT by Section by Day", y = "Mean RT (ms)",
         subtitle = "148 trials per section, 4 sections per day\n(592 trials total)",
         caption = paste0("setA n = ", unique(filter(rt_section, talker_trained == "setA")$n),
                          "\nsetB n = ", unique(filter(rt_section, talker_trained == "setB")$n)),
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
          panel.background = element_blank())
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
         caption = paste0("setA n = ", length(unique(filter(test, talker_trained == "setA")$participant)),
                          "\nsetB n = ", length(unique(filter(test, talker_trained == "setB")$participant)))) +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" μ setA = ",
                                            round(mean(filter(test_rt_trained,
                                                              talker_trained == "setA")$mean_rt),
                                                  digits = 2), " ms\n μ setB = ",
                                            round(mean(filter( test_rt_trained,
                                                               talker_trained == "setB")$mean_rt),
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
          panel.spacing.x = unit(1, "lines"))
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
         caption = paste0("setA n = ", length(unique(filter(test, talker_trained == "setA")$participant)),
                          "\nsetB n = ", length(unique(filter(test, talker_trained == "setB")$participant)))) +
    scale_linetype_manual(values = c("dashed"),
                          labels = c(paste0(" μ setA = ",
                                            round(mean(filter(test_rt_trained,
                                                              talker_trained == "setA")$mean_rt),
                                                  digits = 2), " ms\n μ setB = ",
                                            round(mean(filter( test_rt_trained,
                                                               talker_trained == "setB")$mean_rt),
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
          panel.spacing.x = unit(1, "lines"))
test_rt_all_plot
ggsave(file.path("plots-test", "rt_allv.png"), width = 7.5, height = 6.5)

#### TRANSCRIPTION BOXPLOT: ACCURACY BY TRAINED/UNTRAINED TALKERS, TRAINED VOWELS ####
tx_trainedv <- tx_master %>%
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
    scale_y_continuous(limits = c(0, 100), breaks = seq.int(0,100,10),
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
         caption = paste0("setA n = 3", "\nsetB n = 1")) +
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
          axis.ticks.length = unit(0.35, "lines"))
tx_trainedv_plot
ggsave(file.path("plots-transcription", "accuracy_trainedvowels.png"), width = 8, height = 7.5)

#### TRANSCRIPTION BOXPLOT: ACCURACY BY TRAINED/UNTRAINED TALKERS, ALL VOWELS ####
tx_allv <- tx_master %>%
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
    scale_y_continuous(limits = c(0, 100), breaks = seq.int(0,100,10),
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
         caption = paste0("setA n = 3", "\nsetB n = 1")) +
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
          axis.ticks.length = unit(0.35, "lines"))
tx_allv_plot
ggsave(file.path("plots-transcription", "accuracy_allvowels.png"), width = 8, height = 7.5)

#### TRANSCRIPTION BARPLOT - BY TRAINED/UNTRAINED TALKERS AND VOWELS ####
tx_bar <- tx_master %>%
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
         caption = paste0("setA n = 3", "\nsetB n = 1")) +
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
          panel.spacing.x = unit(1, "lines"),
          legend.position = c(.03,-.13),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_blank())
tx_bar_plot
ggsave(file.path("plots-transcription", "trained_vs_untrained_vowels.png"), width = 6.25, height = 6.5)
