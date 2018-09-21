# tfta_plots.R
# Jessica Tin
# 21 Sept 2018
#
# Creates plots for TFTA.
#

#### LOAD PACKAGES ####
source("/Volumes/PerrachioneLab/software/r-scripts/load_packages.R")
load_packages("dplyr", "magrittr", "readr", "tidyr", "psycho", "purrr",
              "ggplot2", "viridis")

#### SET WORKING DIRECTORY ####
setwd("/Volumes/PerrachioneLab/projects/TFTA/Analysis/") # Mac
#setwd("/PerrachioneLab/projects/TFTA/Analysis/") # Linux
#setwd("R:/PerrachioneLab/projects/TFTA/Analysis/") # Windows

#### LIST EXCLUDED PARTICIPANTS ####
exclude <- c("p1513")

#### READ IN AND PREPROCESS TEST DATA ####
# update and read in master CSV
source("tfta_test_master.R")
master_test <- read_csv(max(list.files(pattern = "^master_test_")),
                        col_types = "cciiicdiicccc") %>%
    mutate_if(is.character, as.factor)

# add carrier duration info
carriers <- read_csv("../Stimuli/carrier_durations.csv", col_types = "-id") %>%
    mutate(carrier_duration = carrier_duration * 1000)
master_test %<>% left_join(carriers, by = c("talker" = "talker_number"))

test <- master_test %>%
    filter(!participant %in% exclude, # remove excluded participants
           !is.na(rt)) %>% # remove skipped trials

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
                               (log(rt) >= mean_logrt - 3*sd_logrt),
                           rt, NA)) %>%
    ungroup() %>%

    # only keep correct trials
    filter(correct == 1) %>%

    # designate talker sets
    mutate(talker_set = ifelse(talker <= 4, "setA", "setB"))

# refactor variables
test$test <- factor(test$test, levels = c("pre", "post"))
test$single_mixed <- factor(test$single_mixed, levels = c("single", "mixed"))
test$carrier <- factor(test$carrier, levels = c("none", "ioua"), labels = c("No Carrier", "Carrier"))
#test$talker_set <- factor(test$talker_set, levels = c("setB", "setA"))

#### READ IN AND PREPROCESS TRAINING DATA ####
# update and read in master CSV
source("tfta_training_master.R")
master_training <- read_csv(max(list.files(pattern = "^master_training_")),
                            col_types = "cciiicciiccdicc") %>%
    mutate_if(is.character, as.factor)
training <- master_training %>%
    filter(!participant %in% exclude, # remove excluded participants
           !is.na(rt)) %>% # remove skipped trials

    # specify trials with log rt within 3 standard deviations of mean log rt
    group_by(participant, day) %>%
    mutate(mean_logrt = mean(log(rt)),
           sd_logrt = sd(log(rt))) %>%
    rowwise() %>%
    mutate(
        rt_3SD = ifelse((log(rt) <= mean_logrt + 3*sd_logrt) &&
                            (log(rt) >= mean_logrt - 3*sd_logrt), rt, NA),

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

#### TRAINING PLOT 1: d' BY SECTION ####
p1 <- training %>%
    group_by(talker_trained, day, section) %>%
    summarize(n = n(),
              #hit_rate = sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
              #fa_rate = sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
              #dprime = dnorm(.5-hit_rate) - dnorm(.5-fa_rate),
              dprime_psycho = dprime(n_hit = sum(resp_signal == "hit"),
                                     n_fa = sum(resp_signal == "fa"),
                                     n_miss = sum(resp_signal == "miss"),
                                     n_cr = sum(resp_signal == "crej"))$dprime)

dprime_bysection <- ggplot(data = p1,
                       aes(x = section, y = dprime_psycho, color = talker_trained)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    labs(title = "d' by Section by Day", y = "d'",
         subtitle = "444 trials per section (148/participant)",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.1, 0.85))
dprime_bysection
ggsave("dprime_bysection.png", width = 8, height = 5)

#### TRAINING PLOT 2: d' BY QUARTER-SECTION ####
p2 <- training %>%
    arrange(talker_trained, day, section, trial) %>%
    mutate(qsection = paste0(section,".",(trial %/% 37) + 1)) %>%
    group_by(talker_trained, day, section, qsection) %>%
    summarize(n = n(),
              #hit_rate = sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
              #fa_rate = sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
              #dprime = dnorm(.5-hit_rate) - dnorm(.5-fa_rate),
              #dprime2 = dnorm(.5-fa_rate) - dnorm(.5-hit_rate),
              dprime_psycho = dprime(n_hit = sum(resp_signal == "hit"),
                                     n_fa = sum(resp_signal == "fa"),
                                     n_miss = sum(resp_signal == "miss"),
                                     n_cr = sum(resp_signal == "crej"))$dprime)

dprime_byqsection <- ggplot(data = p2,
                        aes(x = qsection, y = dprime_psycho, color = talker_trained)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    labs(title = "d' by Quarter-Section by Day", y = "d'",
         subtitle = "111 trials per quarter-section (37/participant)\n4 sections per day",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.05, 0.93))
dprime_byqsection
ggsave("dprime_byqsection.png", width = 12, height = 6)

#### TRAINING PLOT 3: HIT/FA RATES BY SECTION ####
p3 <- training %>%
    group_by(talker_trained, day, section) %>%
    summarize(n = n(),
              hit_rate = sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
              fa_rate = sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej"))) %>%
    gather(key = type, value = rate, -talker_trained, -day, -section, -n)
p3$type <- factor(p3$type, levels = c("hit_rate", "fa_rate"))

rates_bysection <- ggplot(data = p3,
                          aes(x = section, y = rate*100, color = type)) +
    geom_point(size = 3) +
    geom_line(aes(group = type), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    scale_y_continuous(limits = c(0,100), breaks = seq.int(0,100,25),
                       labels = function(y) paste0(y, "%")) +
    labs(title = "Hit and False Alarm Rate by Section",
         subtitle = "444 trials per section (148/participant)",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 12),
          panel.background = element_rect(fill = "gray98"),
          legend.position = c(0.08, 0.22),
          legend.title = element_blank())
rates_bysection
ggsave("rates_bysection.png", width = 8, height = 5)

#### TRAINING PLOT 4: HIT/FA RATES BY QUARTER-SECTION ####
p4 <- training %>%
    arrange(talker_trained, day, section, trial) %>%
    mutate(qsection = paste0(section,"-",(trial %/% 37) + 1)) %>%
    group_by(talker_trained, day, section, qsection) %>%
    summarize(n = n(),
              hit_rate = sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
              fa_rate = sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej"))) %>%
    gather(key = type, value = rate, -talker_trained, -day, -section, -qsection, -n)
p4$type <- factor(p4$type, levels = c("hit_rate", "fa_rate"))

rates_byqsection <- ggplot(data = p4,
                           aes(x = qsection, y = rate*100, color = type)) +
    geom_point(size = 3) +
    geom_line(aes(group = type), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_y_continuous(limits = c(0,100), breaks = seq.int(0,100,25),
                       labels = function(y) paste0(y, "%")) +
    labs(title = "Hit and False Alarm Rate by Quarter-Section by Day",
         subtitle = "111 trials per quarter-section (37/participant)\n4 sections per day",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.05, 0.23),
          legend.title = element_blank())
rates_byqsection
ggsave("rates_byqsection.png", width = 12, height = 6)

#### TRAINING PLOTS 5-7: RT BY SECTION ####
# INDIVIDUAL RTs
p5 <- training %>%
    filter(!is.na(rt_3SD)) %>%
    ungroup() %>%
    group_by(talker_trained, participant, day, section) %>%
    summarize(n = n(),
              mean_rt = mean(rt_3SD))

rt_bysection <- ggplot(data = p5,
                       aes(x = section, y = mean_rt, color = participant)) +
    geom_point(size = 3) +
    geom_line(aes(group = participant), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    labs(title = "Mean RT by Section by Day", y = "d'",
         subtitle = "436-444 trials per section (143-146/participant)",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.05, 0.88))
rt_bysection
ggsave("individual_rt_bysection.png", width = 10, height = 6)

# MEAN RTS
p6 <- training %>%
    filter(!is.na(rt_3SD)) %>%
    group_by(talker_trained, day, section) %>%
    summarize(n = n(),
              mean_rt = mean(rt_3SD))

meanrt_bysection <- ggplot(data = p6,
                           aes(x = section, y = mean_rt, color = talker_trained)) +
    geom_point(size = 3) +
    geom_line(aes(group = talker_trained), size = 1) +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    labs(title = "Mean RT by Section by Day", y = "d'",
         subtitle = "436-444 trials per section (143-146/participant)",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.08, 0.91))
meanrt_bysection
ggsave("mean_rt_bysection.png", width = 10, height = 6)

# MEAN AND INDIVIDUAL RTS
p6$participant <- "mean"
combinedrt_bysection <- ggplot(data = p5,
                               aes(x = section, y = mean_rt, color = participant)) +
    geom_point(size = 3) +
    geom_line(aes(group = participant), size = 1) +
    geom_point(data = p6, size = 4.5, color = "black") +
    geom_line(data = p6, size = 1.5, color = "black") +
    facet_grid(~ day, switch = "x", labeller = as_labeller(c(`1` = "Day 1",
                                                             `2` = "Day 2",
                                                             `3` = "Day 3"))) +
    scale_x_continuous(labels = function(x) ifelse(x == "1", paste0("Section ",x), x)) +
    labs(title = "Mean RT by Section by Day", y = "d'",
         subtitle = "436-444 trials per section (143-146/participant)",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          panel.spacing = unit(0, "lines"),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.05, 0.88))
combinedrt_bysection
ggsave("combined_rt_bysection.png", width = 10, height = 6)

#### TRAINING PLOT 8: BOXPLOT OF RT BY SECTION BY DAY ####
rt_bysection_boxplot <- ggplot(data = filter(training, !is.na(rt_3SD))) +
    geom_boxplot(aes(x = section, y = rt, fill = as.character(day),
                     group = interaction(as.character(day), section))) +
    labs(title = "RT by Section by Day", x = "Section", y = "RT (ms)",
         subtitle = "436-444 trials per day+section (143-146/participant)",
         fill = "Day",
         caption = "n = 3") +
    scale_x_continuous(labels = function(x) paste0("Section ",x)) +
    scale_fill_viridis(discrete = TRUE) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "gray96"),
          legend.position = c(0.1, 0.93),
          legend.direction = "horizontal")
rt_bysection_boxplot
ggsave("boxplots_rt_bysection.png", width = 10, height = 6)

#### TEST PLOT: PRE-TEST ####
pre <- test %>%
    filter(test == "pre") %>%
    group_by(participant, carrier, single_mixed) %>%
    mutate(mean_rt = mean(rt_3SD, na.rm = TRUE))
pre_mean <- test %>%
    filter(test == "pre") %>%
    group_by(carrier, single_mixed) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))
pre_mean$participant <- "mean"

pre_plot <- ggplot(mapping = aes(x = single_mixed, y = mean_rt, group = participant)) +
    geom_line(data = pre, aes(color = participant)) +
    geom_point(data = pre, aes(color = participant), size = 2.5) +
    geom_line(data = pre_mean, size = 1) +
    geom_point(data = pre_mean, size = 3.5) +
    scale_y_continuous(limits = c(650, 1100), breaks = c(seq.int(600, 1100, 100))) +
    facet_grid(. ~ carrier) +
    labs(title = "Pre-test, All Talkers", subtitle = "All Vowels", x = "", y = "Mean RT (ms)") +
    scale_color_viridis(discrete = TRUE, begin = .2, end = 0.9, direction = -1) +
    #geom_segment(data = pre, aes(x = 1, xend = 2, y = -Inf, yend = -Inf)) +
    #geom_segment(data = pre, aes(y = 650, yend = 1100, x = -Inf, xend = -Inf)) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 10.5, hjust = 0.5),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          #axis.line = element_blank(),
          panel.background = element_rect(fill = "gray97"))
pre_plot
ggsave(file.path("plots", "pretest.png"), width = 5, height = 4)

#### TEST PLOT: PRE-TEST BY TALKER SET ####
pre_talkers <- test %>%
    filter(test == "pre") %>%
    group_by(participant, carrier, single_mixed, talker_set) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))
pre_talkers$talker_set <- factor(pre_talkers$talker_set, levels = c("setB", "setA"))
pre_talkers_mean <- test %>%
    filter(test == "pre") %>%
    group_by(carrier, single_mixed, talker_set) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))
pre_talkers_mean$participant <- "mean"
pre_talkers_mean$talker_set <- factor(pre_talkers_mean$talker_set, levels = c("setB", "setA"))

pre_talkers_plot <- ggplot(mapping = aes(x = single_mixed, y = mean_rt, group = participant)) +
    geom_line(data = pre_talkers, aes(color = participant)) +
    geom_point(data = pre_talkers, aes(color = participant), size = 2.5) +
    geom_line(data = pre_talkers_mean, size = 1) +
    geom_point(data = pre_talkers_mean, size = 3.5) +
    #scale_y_continuous(limits = c(575, 1150), breaks = c(seq.int(600, 1200, 100))) +
    #facet_grid(carrier ~ talker_set, switch = "x") +
    facet_wrap(carrier ~ talker_set, strip.position = "top", scales = "free") +
    labs(title = "Pre-test, setA vs. setB Talkers", subtitle = "All Vowels", x = "", y = "Mean RT (ms)",
         caption = "n = 3") +
    scale_color_viridis(discrete = TRUE, begin = .2, end = 0.9, direction = -1) +
    #geom_segment(data = pre, aes(x = 1, xend = 2, y = -Inf, yend = -Inf)) +
    #geom_segment(data = pre, aes(y = 575, yend = 1150, x = -Inf, xend = -Inf)) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 10.5, hjust = 0.5),
          axis.text = element_text(size = 9.5),
          axis.title = element_text(size = 11),
          axis.line = element_line(),
          #axis.line = element_blank(),
          panel.background = element_rect(fill = "gray99"),
          panel.spacing.y = unit(2, "lines"))
pre_talkers_plot
ggsave(file.path("plots", "pretest_bytalker.png"), width = 5, height = 6.25)

#### TEST PLOT: PRE-TEST VS. POST-TEST ####
post_df <- test %>%
    rowwise() %>%
    filter((participant %in% c("p3135", "p0002") && vowel_pair == "pairA") || ((participant == "p2282") && vowel_pair == "pairB")) %>%
    group_by(participant, carrier, single_mixed, test) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))

post_plot <- ggplot(data = post_df, aes(x = single_mixed, y = mean_rt, color = test,
                                        group = interaction(participant, test))) +
    geom_line(size = 1) +
    geom_point(size = 3.5) +
    #scale_y_continuous(limits = c(700, 1100), breaks = c(seq.int(700, 1100, 100))) +
    facet_grid(participant ~ carrier) +
    labs(title = "Pre- vs. Post-test, All Talkers", subtitle = "Trained Vowels Only", x = "", y = "Mean RT (ms)") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 10.5, hjust = 0.5),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.y = unit(2, "lines"))
post_plot
ggsave(file.path("plots", "posttest.png"), width = 5, height = 5.5)


post_df <- test %>%
    rowwise() %>%
    filter((participant %in% c("p3135", "p0002") && vowel_pair == "pairA") || ((participant == "p2282") && vowel_pair == "pairB")) %>%
    group_by(carrier, single_mixed, test, talker_set) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))

post_plot <- ggplot(data = post_df, aes(x = single_mixed, y = mean_rt, color = test,
                                        group = test)) +
    geom_line(size = 1) +
    geom_point(size = 3.5) +
    scale_y_continuous(limits = c(650, 1000), breaks = c(seq.int(700, 1000, 100))) +
    facet_grid(carrier ~ talker_set) +
    labs(title = "Pre- vs. Post-test, All Talkers", x = "", y = "Mean RT (ms)") +
    scale_color_viridis(discrete = TRUE) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.y = unit(2, "lines"))
post_plot
ggsave(file.path("plots", "posttest.png"), width = 5, height = 5.5)

#### TEST PLOT: PRE- VS. POST-TEST, BY TALKER ####
post_talkers_df <- test %>%
    rowwise() %>%
    filter((participant %in% c("p3135", "p0002") && vowel_pair == "pairA") || ((participant == "p2282") && vowel_pair == "pairB")) %>%
    group_by(participant, carrier, single_mixed, test, talker_set) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))
post_talkers_mean_df <- test %>%
    rowwise() %>%
    filter((participant %in% c("p3135", "p0002") && vowel_pair == "pairA") || ((participant == "p2282") && vowel_pair == "pairB")) %>%
    group_by(carrier, single_mixed, test, talker_set) %>%
    summarize(mean_rt = mean(rt_3SD, na.rm = TRUE))
post_talkers_df$talker_set <- factor(post_talkers_df$talker_set, levels = c("setB", "setA"),
                                     labels = c("setB (Untrained)", "setA (Trained)"))
post_talkers_mean_df$talker_set <- factor(post_talkers_mean_df$talker_set, levels = c("setB", "setA"),
                                          labels = c("setB (Untrained)", "setA (Trained)"))
post_talkers_mean_df$participant <- "mean"

post_talkers_plot <- ggplot(data = post_talkers_df,
                            aes(x = single_mixed, y = mean_rt,
                                group = interaction(participant, test),
                                color = participant, alpha = test)) +
    geom_line(size = 1) +
    geom_point(aes(shape = test), size = 3.5) +
    geom_line(data = post_talkers_mean_df, size = 1.25, color = "black") +
    geom_point(data = post_talkers_mean_df, size = 4.5, color = "black", aes(shape = test)) +
    #scale_y_continuous(limits = c(600, 1150), breaks = c(seq.int(600, 1150, 100))) +
    facet_grid(carrier ~ talker_set) +
    labs(title = "Pre- vs. Post-test, setA vs. setB Talkers", subtitle = "Trained Vowels Only", x = "", y = "Mean RT (ms)") +
    scale_color_viridis(discrete = TRUE) +
    scale_alpha_discrete(range = c(0.65, .95)) +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 10.5, hjust = 0.5),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          panel.background = element_rect(fill = "gray97"),
          panel.spacing.y = unit(2, "lines"))
post_talkers_plot
ggsave(file.path("plots", "posttest_bytalker_means.png"), width = 5, height = 5.75)





