# tfta_plots.R
# Jessica Tin
# 9 Apr 2019
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

# #### UPDATE MASTER CSVs ####
# # skip this section if master CSVs are up to date
# # must change server path within each script if not /Volumes/PerrachioneLab
# source("tfta_training_master.R") # training data
# source("tfta_test_master.R") # test data
# source("tfta_transcription_master.R") # transcription data

#### READ IN AND PREPROCESS TRAINING DATA ####
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

# save training csv for stats
write_csv(training, "master_training_stats.csv")

#### SAVE TRAINING GROUPS ####
# save which participants trained on which talkers/vowels
training_groups <- training %>%
    group_by(participant) %>%
    summarize_at(vars(talker_trained, vowel_trained), unique)
write_csv(training_groups, "training_groups.csv")

#### READ IN AND PREPROCESS TEST DATA ####
# read in master CSV
master_test <- read_csv("master_test.csv", col_types = "cciicicdiicccc")

# add carrier duration info
carriers <- read_csv(file.path("..", "Stimuli", "carrier_durations.csv"),
                     col_types = "_id") %>%
    mutate(carrier_duration = carrier_duration * 1000)
master_test %<>% left_join(carriers, by = c("talker" = "talker_number"))

test <- master_test %>%
    # only plot data from participants who have finished post-test
    filter(participant %in% unique(filter(master_test, test == "post")$participant)) %>%

    # adjust rt for carrier trials
    mutate(rt_all = pmap_dbl(list(c = carrier, r = rt, d = carrier_duration),
                             function(c, r, d) ifelse(c == "ioua", r - d, r)),

           # mark NA rt for trials answered before target word
           rt = ifelse(rt_all > 0, rt_all, NA)) %>%

    # mark NA rt for trials with log rt more than 3 standard deviations from mean log rt
    group_by(participant, test) %>%
    mutate(rt = within_3SD(rt)) %>%
    ungroup() %>%

    # add training info (with training_groups participant factors as characters)
    left_join(mutate_all(training_groups, as.character), by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%

    # convert all variables except RT columns and duration to factors (+ manually
    # include paRTicipant)
    mutate_at(vars(-contains("rt"), -carrier_duration, participant), as.factor)

# save test csv for stats
write_csv(test, "master_test_stats.csv")

# refactor variables
test$test <- factor(test$test, levels = c("pre", "post"), labels = c("Pre", "Post"))
test$single_mixed <- factor(test$single_mixed, levels = c("single", "mixed"),
                            labels = c("Single", "Mixed"))
test$carrier <- factor(test$carrier, levels = c("none", "ioua"),
                       labels = c("No Carrier", "Carrier"))
test$trained_t <- factor(test$trained_t, levels = c("untrained", "trained"),
                         labels = c("Untrained Talkers", "Trained Talkers"))
test$trained_v <- factor(test$trained_v, levels = c("untrained", "trained"),
                         labels = c("Untrained Vowels", "Trained Vowels"))

#### READ IN AND PREPROCESS TRANSCRIPTION DATA ####
master_transcription <- read_csv("master_transcription.csv",
                                 col_types = "ciciiccciiiicc")

transcription <- master_transcription %>%
    # add training info (with training_groups participant factors as characters)
    left_join(mutate_all(training_groups, as.character), by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%

    # convert all variables except score columns to factors
    mutate_at(vars(-whole_word, -initial, -vowel, -final), as.factor)

# save transcription csv for stats
write_csv(transcription, "master_transcription_stats.csv")

# refactor variables
transcription$trained_t <- factor(transcription$trained_t,
                                  levels = c("untrained", "trained"),
                                  labels = c("Untrained Talkers", "Trained Talkers"))
transcription$trained_v <- factor(transcription$trained_v,
                                  levels = c("untrained", "trained"),
                                  labels = c("Untrained Vowels", "Trained Vowels"))

#### THEME TEMPLATE ####
scales_tfta <- list(
    scale_x_discrete(labels = function(x) gsub(" Talkers", "\nTalkers", x)),
    scale_y_continuous(limits = c(620, 810),
                       breaks = c(seq.int(625, 800, 25))),
    scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75),
    scale_alpha_discrete(range = c(0.5, 1)))

theme_tfta <- theme(
    # titles and labels
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10, face = "bold",
                                margin = margin(0, 6, 0, 0)),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, face = "bold", color = "black",
                               margin = margin(6, 0, 0, 0)),

    # axis lines
    axis.line = element_line(),
    axis.ticks.length = unit(0.5, "lines"),

    # legend
    legend.title = element_blank(),

    # background
    panel.background = element_blank())

theme_tfta_facets <- theme(
    # titles and labels
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10, face = "bold",
                                margin = margin(0, 6, 0, 0)),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, face = "plain",
                               margin = margin(6, 0, 3, 0)),

    # axis lines
    axis.line = element_line(),
    axis.ticks.length = unit(0.5, "lines"),

    # facets
    panel.spacing = unit(0, "lines"),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",

    # legend
    legend.title = element_blank(),

    # background
    panel.background = element_blank()
)

#### TEST PLOTS ####
summarize_test <- function(by = "test") {
    test %>% group_by_at(vars(c("participant", by))) %>%
        summarize(mean_rt = mean(rt, na.rm = TRUE)) %>%
        group_by_at(vars(by)) %>%
        summarize(se = sd(mean_rt)/sqrt(n()),
                  mean_rt = mean(mean_rt),
                  n = n())
}

#### 1-4: Pre vs. Post, No Carrier vs. Carrier, Mixed vs. Single, Untrained vs. Trained ####
test_plot_1var <- function(grouping = "test") {
    df <- summarize_test(by = grouping)
    group_title <- case_when(grouping == "test" ~ "Pre- vs. Post-Test",
                             grouping == "carrier" ~ "Carrier vs. No Carrier",
                             grouping == "single_mixed" ~ "Single- vs. Mixed-Talker",
                             grouping == "trained_t" ~ "Trained vs. Untrained Talkers")
    p <- ggplot(data = df, aes_string(x = grouping, y = "mean_rt", group = 1)) +
        geom_point(size = 3.5) +
        geom_line(size = 1) +
        geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
        labs(title = group_title,
             caption = paste0("n = ", unique(df$n)),
             y = "Mean RT (ms)") +
        scales_tfta +
        theme_tfta
    return(print(p))
}
x <- 1
for (grouping in c("test", "carrier", "single_mixed", "trained_t")) {
    print(paste0(grouping, ": ", file.path("plots-test", paste0("test_", x, ".png"))))
    test_plot_1var(grouping)
    ggsave(file.path("plots-test", paste0("test_", x, ".png")), height = 4, width = 4)
    x <- x + 1
}

#### 5: Pre vs. Post  X  Carrier vs. No Carrier ####
test5 <- summarize_test(by = c("test", "carrier"))
print(paste("test x carrier:", file.path("plots-test", "test_5.png")))
p5 <- ggplot(data = test5, aes(x = test, y = mean_rt, group = carrier)) +
    geom_point(size = 3.5) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    labs(title = "Pre- vs. Post-Test X\nCarrier vs. No Carrier",
         caption = paste0("n = ", unique(test5$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta
p5
ggsave(file.path("plots-test", "test_5.png"), height = 4, width = 4.5)

#### 6: Pre vs. Post  X  Single vs. Mixed ####
test6 <- summarize_test(by = c("test", "single_mixed"))
print(paste("test x single_mixed:", file.path("plots-test", "test_6.png")))
p6 <- ggplot(data = test6, aes(x = test, y = mean_rt, color = single_mixed,
                               shape = single_mixed, group = single_mixed)) +
    geom_point(size = 3.5) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    labs(title = "Pre- vs. Post-Test X\nSingle- vs. Mixed-Talker",
         caption = paste0("n = ", unique(test6$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta
p6
ggsave(file.path("plots-test", "test_6.png"), height = 4, width = 4.5)

#### 7: Single vs. Mixed  X  Carrier vs. No Carrier ####
test7 <- summarize_test(by = c("single_mixed", "carrier"))
print(paste("single_mixed x carrier:", file.path("plots-test", "test_7.png")))
p7 <- ggplot(data = test7, aes(x = single_mixed, y = mean_rt, color = single_mixed,
                               shape = single_mixed, group = carrier)) +
    geom_line(aes(linetype = carrier), size = 1, color = "gray35") +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    geom_point(size = 3.5) +
    labs(title = "Single- vs. Mixed-Talker X\nCarrier vs. No Carrier",
         caption = paste0("n = ", unique(test7$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(color = FALSE, shape = FALSE) +
    theme_tfta
p7
ggsave(file.path("plots-test", "test_7.png"), height = 4, width = 4.5)

#### 8: Pre vs. Post  X  Untrained vs. Trained ####
test8 <- summarize_test(by = c("test", "trained_t"))
print(paste("test x trained_t:", file.path("plots-test", "test_8_combined.png")))
p8a <- ggplot(data = test8, aes(x = test, y = mean_rt, alpha = trained_t,
                                group = trained_t)) +
    geom_point(size = 3.5) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    labs(title = "Pre- vs. Post-Test X\nUntrained vs. Trained Talkers",
         caption = paste0("n = ", unique(test8$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta
p8a
ggsave(file.path("plots-test", "test_8_combined.png"), height = 4, width = 4.5)

print(paste("test x trained_t:", file.path("plots-test", "test_8_panels.png")))
p8b <- ggplot(data = test8, aes(x = test, y = mean_rt, alpha = trained_t,
                                group = trained_t)) +
    geom_point(size = 3.5) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    facet_wrap(. ~ trained_t, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-Test X\nUntrained vs. Trained Talkers",
         caption = paste0("n = ", unique(test8$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(alpha = FALSE) +
    theme_tfta_facets
p8b
ggsave(file.path("plots-test", "test_8_panels.png"), height = 4, width = 5)

#### 9: Untrained vs. Trained  X  Carrier vs. No Carrier ####
test9 <- summarize_test(by = c("trained_t", "carrier"))
print(paste("trained_t x carrier:", file.path("plots-test", "test_9.png")))
p9 <- ggplot(data = test9, aes(x = trained_t, y = mean_rt, group = carrier)) +
    geom_line(aes(linetype = carrier), size = 1, color = "gray35") +
    geom_point(aes(alpha = trained_t), size = 3.5) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se, alpha = trained_t),
                  width = 0.1) +
    labs(title = "Untrained vs. Trained Talkers X\nCarrier vs. No Carrier",
         caption = paste0("n = ", unique(test9$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(alpha = FALSE) +
    theme_tfta
p9
ggsave(file.path("plots-test", "test_9.png"), height = 4, width = 4.5)

#### 10: Untrained vs. Trained  X Single vs. Mixed ####
test10 <- summarize_test(by = c("trained_t", "single_mixed"))
print(paste("trained_t x single_mixed:", file.path("plots-test", "test_10.png")))
p10 <- ggplot(data = test10, aes(x = trained_t, y = mean_rt, color = single_mixed,
                                 shape = single_mixed, group = single_mixed)) +
    geom_point(size = 3.5) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    labs(title = "Untrained vs. Trained Talkers X\nSingle- vs. Mixed-Talker",
         caption = paste0("n = ", unique(test10$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta
p10
ggsave(file.path("plots-test", "test_10.png"), height = 4, width = 4.5)

#### 11: Pre vs. Post  X  Carrier vs. No Carrier  X  Single vs. Mixed ####
test11 <- summarize_test(by = c("test", "carrier", "single_mixed"))
print(paste("test x carrier x single_mixed:", file.path("plots-test", "test_11_panels.png")))
p11a <- ggplot(data = test11, aes(x = test, y = mean_rt, color = single_mixed,
                                  shape = single_mixed,
                                  group = interaction(single_mixed, carrier))) +
    geom_point(size = 3.5) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    facet_wrap(~ single_mixed, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-test X Carrier vs. No Carrier\nX Single- vs. Mixed-Talker",
         caption = paste0("n = ", unique(test11$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(color = FALSE, shape = FALSE) +
    theme_tfta_facets
p11a
ggsave(file.path("plots-test", "test_11_panels.png"), height = 4, width = 6.5)

print(paste("test x carrier x single_mixed:", file.path("plots-test", "test_11_combined.png")))
p11b <- ggplot(data = test11, aes(x = test, y = mean_rt, color = single_mixed,
                                  shape = single_mixed,
                                  group = interaction(single_mixed, carrier))) +
    geom_point(size = 3.5) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    labs(title = "Pre- vs. Post-test X Carrier vs. No Carrier\nX Single- vs. Mixed-Talker",
         caption = paste0("n = ", unique(test11$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta
p11b
ggsave(file.path("plots-test", "test_11_combined.png"), height = 4, width = 4.5)

#### 12: Pre vs. Post  X  Carrier vs. No Carrier  X  Untrained vs. Trained ####
test12 <- summarize_test(by = c("test", "carrier", "trained_t"))
print(paste("test x carrier x trained_t:", file.path("plots-test", "test_12_panels.png")))
p12a <- ggplot(data = test12, aes(x = test, y = mean_rt, alpha = trained_t,
                                 group = interaction(carrier, trained_t))) +
    geom_line(aes(linetype = carrier), size = 1, color = "gray35") +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    geom_point(size = 3.5) +
    facet_wrap(~ trained_t, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-Test X Carrier vs. No Carrier\nX Untrained vs. Trained Talkers",
         y = "Mean RT (ms)",
         caption = paste0("n = ", unique(test12$n))) +
    scales_tfta +
    theme_tfta_facets
p12a
ggsave(file.path("plots-test", "test_12_panels.png"), height = 4, width = 6.5)

print(paste("test x carrier x trained_t:", file.path("plots-test", "test_12_combined.png")))
p12b <- ggplot(data = test12, aes(x = test, y = mean_rt, alpha = trained_t,
                                  group = interaction(carrier, trained_t))) +
    geom_line(aes(linetype = carrier), size = 1, color = "gray35") +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    geom_point(size = 3.5) +
    labs(title = "Pre- vs. Post-Test X Carrier vs. No Carrier\nX Untrained vs. Trained Talkers",
         y = "Mean RT (ms)",
         caption = paste0("n = ", unique(test12$n))) +
    scales_tfta +
    theme_tfta
p12b
ggsave(file.path("plots-test", "test_12_combined.png"), height = 4, width = 4.5)

#### 13: Pre vs. Post  X  Single vs. Mixed  X  Untrained vs. Trained ####
test13 <- summarize_test(by = c("test", "single_mixed", "trained_t"))
print(paste("test x single_mixed x trained_t:", file.path("plots-test", "test_13_panels.png")))
p13a <- ggplot(data = test13, aes(x = test, y = mean_rt, alpha = trained_t,
                                  color = single_mixed, shape = single_mixed,
                                  group = interaction(single_mixed, trained_t))) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    geom_point(size = 3.5) +
    facet_wrap(~ trained_t, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-Test X Single- vs. Mixed-Talker\nX Untrained vs. Trained Talkers",
         y = "Mean RT (ms)",
         caption = paste0("n = ", unique(test13$n))) +
    scales_tfta +
    guides(alpha = FALSE) +
    theme_tfta_facets
p13a
ggsave(file.path("plots-test", "test_13_panels.png"), height = 4, width = 6.5)

print(paste("test x single_mixed x trained_t:", file.path("plots-test", "test_13_combined.png")))
p13b <- ggplot(data = test13, aes(x = test, y = mean_rt, alpha = trained_t,
                                  color = single_mixed, shape = single_mixed,
                                  group = interaction(single_mixed, trained_t))) +
    geom_line( size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
    geom_point(size = 3.5) +
    labs(title = "Pre- vs. Post-Test X Single- vs. Mixed-Talker\nX Untrained vs. Trained Talkers",
         y = "Mean RT (ms)",
         caption = paste0("n = ", unique(test13$n))) +
    scales_tfta +
    theme_tfta
p13b
ggsave(file.path("plots-test", "test_13_combined.png"), height = 4, width = 4.5)

#### 14: Carrier vs. No Carrier  X  Single vs. Mixed  X  Untrained vs. Trained ####
test14 <- summarize_test(by = c("carrier", "single_mixed", "trained_t"))
print(paste("carrier x single_mixed x trained_t:", file.path("plots-test", "test_14_panels.png")))
p14a <- ggplot(data = test14, aes(x = trained_t, y = mean_rt, color = single_mixed,
                                  shape = single_mixed,
                                  group = interaction(carrier, single_mixed))) +
    geom_line(aes(linetype = carrier), size = 1, alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se, alpha = trained_t),
                  width = 0.1) +
    geom_point(aes(alpha = trained_t), size = 3.5) +
    facet_wrap(~ single_mixed, strip.position = "bottom") +
    labs(title = "Untrained vs. Trained Talkers X\nSingle- vs. Mixed-Talker X Carrier vs. No Carrier",
         caption = paste0("n = ", unique(test14$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(alpha = FALSE, color = FALSE, shape = FALSE) +
    theme_tfta_facets
p14a
ggsave(file.path("plots-test", "test_14_panels.png"), height = 4, width = 6.5)

print(paste("carrier x single_mixed x trained_t:", file.path("plots-test", "test_14_combined.png")))
p14b <- ggplot(data = test14, aes(x = trained_t, y = mean_rt, color = single_mixed,
                                  shape = single_mixed,
                                  group = interaction(carrier, single_mixed))) +
    geom_line(aes(linetype = carrier), size = 1, color = "gray35", alpha = 0.5) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se, alpha = trained_t),
                  width = 0.1) +
    geom_point(aes(alpha = trained_t), size = 3.5) +
    labs(title = "Untrained vs. Trained Talkers X\nSingle- vs. Mixed-Talker X Carrier vs. No Carrier",
         caption = paste0("n = ", unique(test14$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(alpha = FALSE) +
    theme_tfta
p14b
ggsave(file.path("plots-test", "test_14_combined.png"), height = 4, width = 4.5)

#### 15: All ####
test15 <- summarize_test(by = c("test", "carrier", "single_mixed", "trained_t"))
p15 <- ggplot(data = test15, aes(x = test, y = mean_rt, alpha = trained_t,
                                  color = single_mixed, shape = single_mixed,
                                  group = interaction(single_mixed, carrier, trained_t))) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se),
                  width = 0.1) +
    geom_point(size = 3.5) +
    labs(caption = paste0("n = ", unique(test15$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta
p15

print(paste("test x carrier x single_mixed x trained_t:", file.path("plots-test", "test_15_panels_carrier.png")))
p15a <- ggplot(data = test15, aes(x = test, y = mean_rt, alpha = trained_t,
                                  color = single_mixed, shape = single_mixed,
                                  group = interaction(single_mixed, carrier, trained_t))) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se),
                  width = 0.1) +
    geom_point(size = 3.5) +
    facet_wrap(carrier ~ trained_t, nrow = 1, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-Test X Untrained vs. Trained Talkers\nX Single- vs. Mixed-Talker X Carrier vs. No Carrier",
         caption = paste0("n = ", unique(test15$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta_facets +
    theme(strip.text = element_text(margin = margin(3,0,2,0)))
p15a
ggsave(file.path("plots-test", "test_15_panels_carrier.png"), height = 4.5, width = 8)

print(paste("test x carrier x single_mixed x trained_t:", file.path("plots-test", "test_15_panels_sm.png")))
p15b <- ggplot(data = test15, aes(x = test, y = mean_rt, alpha = trained_t,
                                  color = single_mixed, shape = single_mixed,
                                  group = interaction(single_mixed, carrier, trained_t))) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se),
                  width = 0.1) +
    geom_point(size = 3.5) +
    facet_wrap(single_mixed ~ trained_t, nrow = 1, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-Test X Untrained vs. Trained Talkers\nX Single- vs. Mixed-Talker X Carrier vs. No Carrier",
         caption = paste0("n = ", unique(test15$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    theme_tfta_facets +
    theme(strip.text = element_text(margin = margin(3,0,2,0)))
p15b
ggsave(file.path("plots-test", "test_15_panels_sm.png"), height = 4.5, width = 8)

print(paste("test x carrier x single_mixed x trained_t:", file.path("plots-test", "test_15_combined.png")))
p15c <- ggplot(data = test15, aes(x = test, y = mean_rt, alpha = trained_t,
                                  color = single_mixed, shape = single_mixed,
                                  group = interaction(single_mixed, carrier, trained_t))) +
    geom_line(aes(linetype = carrier), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se),
                  width = 0.1) +
    geom_point(size = 3.5) +
    facet_wrap(~ trained_t, strip.position = "bottom") +
    labs(title = "Pre- vs. Post-Test X Untrained vs. Trained Talkers\nX Single- vs. Mixed-Talker X Carrier vs. No Carrier",
         caption = paste0("n = ", unique(test15$n)),
         y = "Mean RT (ms)") +
    scales_tfta +
    guides(alpha = FALSE) +
    theme_tfta_facets
p15c
ggsave(file.path("plots-test", "test_15_combined.png"), height = 4.5, width = 6.5)

#### TRANSCRIPTION PLOT ####
tx_bar <- transcription %>%
    gather(key = part, value = correct, c(9:12)) %>% # cols 9-12 = scores
    group_by(participant, trained_t, part) %>%
    summarize(accuracy = sum(correct)/n()) %>%
    group_by(trained_t, part) %>%
    summarize(se_accuracy = sd(accuracy)/sqrt(n()),
              mean_accuracy = mean(accuracy),
              n = n())
tx_bar$part <- factor(tx_bar$part, levels = c("whole_word", "initial", "vowel", "final"),
                      labels = c("whole\nword", "initial", "vowel", "final"))

tx_bar_plot <- ggplot(data = tx_bar, aes(x = part, y = mean_accuracy*100, fill = trained_t)) +
    geom_col(width = .8, position = position_dodge(.8)) +
    geom_errorbar(aes(ymin = (mean_accuracy - se_accuracy)*100,
                      ymax = (mean_accuracy + se_accuracy)*100,
                      color = trained_t), width = 0.5, position = position_dodge(.8)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(seq.int(0,100,20)),
                       expand = c(0,0),
                       labels = function(x) paste0(x,"%"),
                       sec.axis = sec_axis(~ .*.96, name = "Mean Words Correct\n",
                                           breaks = seq.int(0, 96, 12))) +
    labs(title = "Transcription Accuracy",
         y = "Mean Percent Correct",
         caption = paste0("n = ", length(unique(transcription$participant)))) +
    scale_fill_manual(values = c("gray70", "gray20")) +
    scale_color_manual(values = c("gray50", "gray0")) +
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 9),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          panel.background = element_blank(),
          axis.line = element_line(),
          axis.title.x = element_blank(),
          axis.ticks.length = unit(0.3, "lines"),
          axis.ticks.x = element_blank(),
          legend.title = element_blank())
tx_bar_plot
ggsave(file.path("plots-transcription","acc_barplot.png"), height = 4, width = 5.5)

#### TRAINING PLOTS ####
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
              n = n())

#### d' by Day ####
training_dprime_plot <- ggplot(data = training_day, aes(x = day, y = mean_dprime)) +
    geom_line(aes(group = 1), size = 1) +
    geom_errorbar(aes(ymin = mean_dprime - se_dprime,
                      ymax = mean_dprime + se_dprime), width = 0.1) +
    geom_point(size = 3.5) +
    scale_y_continuous(limits = c(0, 4), expand = c(0,0)) +
    labs(title = "Training d' by Day",
         y = "Mean d'", x = "Day",
         caption = paste0("n = ", unique(training_day$n))) +
    theme_tfta +
    theme(axis.title.x = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 10, face = "plain"))
training_dprime_plot
ggsave(file.path("plots-training","dprime_byday.png"), width = 4, height = 4)

#### RT by Day ####
training_rt_plot <- ggplot(data = training_day, aes(x = day, y = mean_rt)) +
    geom_line(aes(group = 1), size = 1) +
    geom_errorbar(aes(ymin = mean_rt - se_rt,
                      ymax = mean_rt + se_rt), width = 0.1) +
    geom_point(size = 3.5) +
    scale_y_continuous(limits = c(800, 1200)) +
    labs(title = "Training RT by Day",
         y = "Mean RT (ms)", x = "Day",
         caption = paste0("n = ", unique(training_day$n))) +
    theme_tfta +
    theme(axis.title.x = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 10, face = "plain"))
training_rt_plot
ggsave(file.path("plots-training","rt_byday.png"), width = 4, height = 4)

#### ARCHIVE: TEST PLOTS ####
# ## FUNCTION: TEST RT, SINGLE VS. MIXED ##
# plot_test_rt_singlemixed <- function(all_vowels = FALSE, combined_groups = FALSE,
#                                      exclude = "none", ymin = 550, ymax = 850) {
#     if (all_vowels) {
#         df <- test
#         s <- "All Vowels"
#     } else {
#         df <- filter(test, trained_v == "Trained Vowels")
#         s <- "Trained Vowels Only"
#     }
#
#     group_by_vars <- c("participant", "carrier", "single_mixed", "test",
#                        "talker_trained", "trained_t")
#     if (combined_groups) {
#         group_by_vars <- group_by_vars[-5]
#         s <- paste0(s, ", Training Groups Combined")
#     }
#
#     df %<>% filter(!participant %in% exclude) %>%
#         group_by_at(vars(group_by_vars)) %>%
#         summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
#         group_by_at(vars(group_by_vars[-1])) %>%
#         summarize(n = n(),
#                   mean_rt = mean(mean_rt))
#
#     test_plot <- ggplot(data = df, aes(x = single_mixed, y = mean_rt, alpha = test)) +
#         geom_line(size = 1) +
#         geom_point(aes(shape = test), size = 3.5) +
#         facet_wrap(carrier ~ trained_t, scales = "free") +
#         scale_y_continuous(limits = c(ymin, ymax),
#                            breaks = c(seq.int(ymin, ymax, 50))) +
#         labs(title = "Test RT, Single-Talker vs. Mixed-Talker Blocks",
#              subtitle = s,
#              #subtitle = paste0(s, "\nParticipants excluded: ",
#              #                  paste(exclude, collapse = ", ")),
#              x = "", y = "Mean RT (ms)") +
#         scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
#         scale_alpha_discrete(range = c(0.4, 1)) + # ignore discrete alpha warning
#         theme(
#             # titles and labels
#             plot.title = element_text(hjust = 0.5, face = "bold"),
#             plot.subtitle = element_text(hjust = 0.5, face = "italic",
#                                          margin = margin(0,0,10,0,"pt")),
#             plot.caption = element_text(size = 10),
#             axis.title = element_text(size = 11),
#             axis.text = element_text(size = 11),
#
#             # facets
#             strip.placement = "outside",
#             strip.text = element_text(size = 11, face = "bold",
#                                       margin = margin(2,0,2,0,"pt")),
#             strip.background = element_blank(),
#             panel.background = element_blank(),
#             panel.spacing.x = unit(1, "lines"),
#             panel.spacing.y = unit(1.5, "lines"),
#
#             # axis lines
#             axis.line = element_line(),
#
#             # legend
#             legend.title = element_text(size = 9, face = "bold"),
#             legend.text = element_text(size = 8))
#
#     if (!combined_groups) {
#         # plot training groups separately
#         test_plot <- test_plot +
#             aes(color = talker_trained, group = interaction(test, talker_trained)) +
#             labs(caption = paste0("setA n = ",
#                                   unique(filter(df, talker_trained == "setA")$n),
#                                   "\nsetB n = ",
#                                   unique(filter(df, talker_trained == "setB")$n)))
#     } else {
#         # plot training groups together
#         test_plot <- test_plot +
#             aes(group = test) +
#             labs(caption = paste0("n = ", unique(df$n), "\n"))
#     }
#
#     print(test_plot)
# }
#
# ## PLOT SINGLE VS. MIXED RT ##
# print("Plotting test RT, single vs. mixed blocks")
# test_rt_sm_plot <- plot_test_rt_singlemixed()
# #test_rt_sm <- select(test_rt_sm_plot$data, -.group)
# ggsave(file.path("plots-test", "single_vs_mixed", "rt_trainedv.png"),
#        width = 6, height = 6.5)
# cat(paste0("plot: test_rt_sm_plot\n",
#            #"data: test_rt_sm\n"
#            "file: ", file.path("plots-test", "single_vs_mixed",
#                                "rt_trainedv.png\n")))
#
# print("Plotting test RT, single vs. mixed blocks, training groups combined")
# test_rt_sm_combn_plot <- plot_test_rt_singlemixed(combined_groups = TRUE)
# #test_rt_sm_combined <- select(test_rt_sm_combn_plot$data, -.group)
# ggsave(file.path("plots-test", "single_vs_mixed", "rt_trainedv_combined.png"),
#        width = 5.75, height = 6.5)
# cat(paste0("plot: test_rt_sm_combn_plot\n",
#            #"data: test_rt_sm_combined\n"
#            "file: ", file.path("plots-test", "single_vs_mixed",
#                                "rt_trainedv_combined.png\n")))
#
# print("Plotting test RT, single vs. mixed blocks, all vowels")
# test_rt_sm_allv_plot <- plot_test_rt_singlemixed(all_vowels = TRUE)
# #test_rt_sm_allv <- select(test_rt_sm_allv_plot$data, -.group)
# ggsave(file.path("plots-test", "single_vs_mixed", "rt_allv.png"),
#        width = 6, height = 6.5)
# cat(paste0("plot: test_rt_sm_allv_plot\n",
#            #"data: test_rt_sm_allv\n"
#            "file: ", file.path("plots-test", "single_vs_mixed", "rt_allv.png\n")))
#
# print("Plotting test RT, single vs. mixed blocks, all vowels, training groups combined")
# test_rt_sm_allv_combn_plot <- plot_test_rt_singlemixed(all_vowels = TRUE,
#                                                        combined_groups = TRUE)
# #test_rt_sm_allv_combined <- select(test_rt_sm_allv_combn_plot$data, -.group)
# ggsave(file.path("plots-test", "single_vs_mixed", "rt_allv_combined.png"),
#        width = 5.75, height = 6.5)
# cat(paste0("plot: test_rt_sm_allv_combn_plot\n",
#            #"data: test_rt_sm_allv_combined\n"
#            "file: ", file.path("plots-test", "single_vs_mixed",
#                                "rt_allv_combined.png\n")))
#
# ## FUNCTION: TEST RT, PRE VS. POST ##
# plot_test_rt_prepost <- function(all_vowels = FALSE, combined_groups = FALSE,
#                                  exclude = "none", ymin = 550, ymax = 850) {
#     if (all_vowels) {
#         df <- test
#         s <- "All Vowels"
#     } else {
#         df <- filter(test, trained_v == "Trained Vowels")
#         s <- "Trained Vowels Only"
#     }
#
#     group_by_vars <- c("participant", "carrier", "single_mixed", "test",
#                        "talker_trained", "trained_t")
#     if (combined_groups) {
#         group_by_vars <- group_by_vars[-5]
#         s <- paste0(s, ", Training Groups Combined")
#     }
#
#     df %<>% filter(!participant %in% exclude) %>%
#         group_by_at(vars(group_by_vars)) %>%
#         summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
#         group_by_at(vars(group_by_vars[-1])) %>%
#         summarize(n = n(),
#                   mean_rt = mean(mean_rt))
#
#     test_plot <- ggplot(data = df, aes(x = test, y = mean_rt, alpha = single_mixed)) +
#         geom_line(size = 1) +
#         geom_point(aes(shape = single_mixed), size = 3.5) +
#         facet_wrap(carrier ~ trained_t, scales = "free") +
#         scale_y_continuous(limits = c(ymin, ymax),
#                            breaks = c(seq.int(ymin, ymax, 50))) +
#         labs(title = "Test RT, Pre- vs. Post-Test",
#              subtitle = s,
#              #subtitle = paste0(s, "\nParticipants excluded: ",
#              #                  paste(exclude, collapse = ", ")),
#              x = "", y = "Mean RT (ms)") +
#         scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
#         scale_alpha_discrete(range = c(0.4, 1)) + # ignore discrete alpha warning
#         theme(
#             # titles and labels
#             plot.title = element_text(hjust = 0.5, face = "bold"),
#             plot.subtitle = element_text(hjust = 0.5, face = "italic",
#                                          margin = margin(0,0,10,0,"pt")),
#             plot.caption = element_text(size = 10),
#             axis.title = element_text(size = 11),
#             axis.text = element_text(size = 11),
#
#             # facets
#             strip.placement = "outside",
#             strip.text = element_text(size = 11, face = "bold",
#                                       margin = margin(2,0,2,0,"pt")),
#             strip.background = element_blank(),
#             panel.background = element_blank(),
#             panel.spacing.x = unit(1, "lines"),
#             panel.spacing.y = unit(1.5, "lines"),
#
#             # axis lines
#             axis.line = element_line(),
#
#             # legend
#             legend.title = element_text(size = 9, face = "bold"),
#             legend.text = element_text(size = 8))
#
#     if (!combined_groups) {
#         # plot training groups separately
#         test_plot <- test_plot +
#             aes(color = talker_trained, group = interaction(single_mixed, talker_trained)) +
#             labs(caption = paste0("setA n = ",
#                                   unique(filter(df, talker_trained == "setA")$n),
#                                   "\nsetB n = ",
#                                   unique(filter(df, talker_trained == "setB")$n)))
#     } else {
#         # plot training groups together
#         test_plot <- test_plot +
#             aes(group = single_mixed) +
#             labs(caption = paste0("n = ", unique(df$n), "\n"))
#     }
#
#     print(test_plot)
# }
#
# ## PLOT PRE- VS. POST-TEST RT ##
# print("Plotting test RT, pre- vs. post-test")
# test_rt_pp_plot <- plot_test_rt_prepost()
# #test_rt_pp <- select(test_rt_pp_plot$data, -.group)
# ggsave(file.path("plots-test", "pre_vs_post", "rt_trainedv.png"),
#        width = 6, height = 6.5)
# cat(paste0("plot: test_rt_pp_plot\n",
#            #"data: test_rt_pp\n"
#            "file: ", file.path("plots-test", "pre_vs_post", "rt_trainedv.png\n")))
#
# print("Plotting test RT, pre- vs. post-test, training groups combined")
# test_rt_pp_combn_plot <- plot_test_rt_prepost(combined_groups = TRUE)
# #test_rt_pp_combined <- select(test_rt_pp_combn_plot$data, -.group)
# ggsave(file.path("plots-test", "pre_vs_post", "rt_trainedv_combined.png"),
#        width = 5.75, height = 6.5)
# cat(paste0("plot: test_rt_pp_combn_plot\n",
#            #"data: test_rt_pp_combined\n"
#            "file: ", file.path("plots-test", "pre_vs_post",
#                                "rt_trainedv_combined.png\n")))
#
# print("Plotting test RT, pre- vs. post-test, all vowels")
# test_rt_pp_allv_plot <- plot_test_rt_prepost(all_vowels = TRUE)
# #test_rt_pp_allv <- select(test_rt_pp_allv_plot$data, -.group)
# ggsave(file.path("plots-test", "pre_vs_post", "rt_allv.png"),
#        width = 6, height = 6.5)
# cat(paste0("plot: test_rt_pp_allv_plot\n",
#            #"data: test_rt_pp_allv\n"
#            "file: ", file.path("plots-test", "pre_vs_post", "rt_allv.png\n")))
#
# print("Plotting test RT, pre- vs. post-test, all vowels, training groups combined")
# test_rt_pp_allv_combn_plot <- plot_test_rt_prepost(all_vowels = TRUE,
#                                                    combined_groups = TRUE)
# #test_rt_pp_allv_combined <- select(test_rt_pp_allv_combn_plot$data, -.group)
# ggsave(file.path("plots-test", "pre_vs_post", "rt_allv_combined.png"),
#        width = 5.75, height = 6.5)
# cat(paste0("plot: test_rt_pp_allv_combn_plot\n",
#            #"data: test_rt_pp_allv_combined\n"
#            "file: ", file.path("plots-test", "pre_vs_post",
#                                "rt_allv_combined.png\n")))

#### ARCHIVE: TRAINING PLOTS ####
# ## FUNCTION: TRAINING d' ##
# plot_training_dprime <- function(breakdown = "day", combined_groups = FALSE) {
#     group_by_vars <- c("participant", "talker_trained", breakdown)
#     if (breakdown != "day") assign("group_by_vars", append(group_by_vars, "day", 2))
#     if (combined_groups) assign("group_by_vars", group_by_vars[-2])
#
#     df <- training %>%
#         group_by_at(vars(group_by_vars)) %>%
#         summarize(
#             # if hit rate = 1, adjust to 1 - 1/2N
#             hit_rate = ifelse(
#                 sum(resp_signal == "hit") != sum(resp_signal %in% c("hit", "miss")),
#                 sum(resp_signal == "hit")/sum(resp_signal %in% c("hit", "miss")),
#                 1 - 1/(2*sum(resp_signal %in% c("hit", "miss")))),
#             # if false alarm rate = 0, adjust to 1/2N
#             fa_rate = ifelse(
#                 sum(resp_signal == "fa") > 0,
#                 sum(resp_signal == "fa")/sum(resp_signal %in% c("fa", "crej")),
#                 1/(2*sum(resp_signal %in% c("fa", "crej")))),
#             dprime = qnorm(hit_rate) - qnorm(fa_rate)) %>%
#         group_by_at(vars(group_by_vars[-1])) %>%
#         summarize(n = n(),
#                   mean_hit_rate = mean(hit_rate),
#                   mean_fa_rate = mean(fa_rate),
#                   mean_dprime = mean(dprime),
#                   error_dprime = sd(dprime)/sqrt(n)) %>%
#         ungroup()
#
#     dprime_plot <- ggplot(data = df, aes_string(x = breakdown, y = "mean_dprime")) +
#         scale_y_continuous(limits = c(0, 4), expand = c(0,0)) +
#         labs(title = paste("Training d' by",
#                            case_when(breakdown == "day" ~ "Day",
#                                      breakdown == "section" ~ "Section",
#                                      breakdown == "hsection" ~ "Half-Section",
#                                      breakdown == "qsection" ~ "Quarter-Section")),
#              y = "Mean d'", x = "Day") +
#         scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
#         theme(
#             # titles and labels
#             plot.title = element_text(hjust = 0.5, face = "bold"),
#             plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
#             plot.caption = element_text(size = 10),
#             axis.title.x = element_text(size = 12, face = "bold"),
#             axis.title.y = element_text(size = 12, face = "bold"),
#             axis.text = element_text(size = 12),
#
#             # facets
#             panel.background = element_blank(),
#
#             # axis lines
#             axis.line = element_line(),
#             axis.ticks.length = unit(0.5, "lines"),
#
#             # legend
#             legend.title = element_text(size = 9, face = "bold"),
#             legend.text = element_text(size = 8),
#             legend.position = c(0.13, 0.13),
#             legend.background = element_rect(fill = "transparent"),
#             legend.box.background = element_rect(fill = "white"))
#
#     if (!combined_groups) {
#         # plot training groups separately
#         df_setA <- filter(df, talker_trained == "setA")
#         df_setB <- filter(df, talker_trained == "setB")
#
#         dprime_plot <- dprime_plot +
#             aes(color = talker_trained) +
#             geom_line(aes(group = talker_trained), size = 1) +
#
#             # participants trained on setA
#             geom_errorbar(data = df_setA, aes(ymin = mean_dprime - error_dprime,
#                                               ymax = mean_dprime + error_dprime),
#                           width = 0.01*nrow(df), alpha = 0.5,
#                           position = position_nudge(x = -0.005)) +
#             geom_point(data = df_setA, size = 3,
#                        position = position_nudge(x = -0.005)) +
#
#             # participants trained on setB
#             geom_errorbar(data = df_setB, aes(ymin = mean_dprime - error_dprime,
#                                               ymax = mean_dprime + error_dprime),
#                           width = 0.01*nrow(df), alpha = 0.5,
#                           position = position_nudge(x = 0.005)) +
#             geom_point(data = df_setB, size = 3,
#                        position = position_nudge(x = 0.005)) +
#
#             labs(subtitle = "",
#                  caption = paste0("setA n = ", unique(df_setA$n), "\nsetB n = ",
#                                   unique(df_setB$n)))
#     } else {
#         # plot training groups together
#         dprime_plot <- dprime_plot +
#             # all participants
#             geom_line(aes(group = 1), size = 1) +
#             geom_errorbar(aes(ymin = mean_dprime - error_dprime,
#                               ymax = mean_dprime + error_dprime),
#                           width = 0.01*nrow(df), alpha = 0.5) +
#             geom_point(size = 3) +
#
#             labs(subtitle = "Training Groups Combined",
#                  caption = paste0("n = ", unique(df$n),"\n"))
#     }
#
#     if (breakdown != "day") {
#         # plot each day side-by-side using facets
#         dprime_plot <- dprime_plot +
#             facet_grid(~ day, switch = "x",
#                        labeller = as_labeller(c(`1` = "Day 1",
#                                                 `2` = "Day 2",
#                                                 `3` = "Day 3"))) +
#             scale_x_discrete(labels = function(x) ifelse(endsWith(x, "5"), "",
#                                                          paste0("Section ",x))) +
#             theme(axis.title.x = element_blank(),
#                   strip.placement = "outside",
#                   strip.text = element_text(size = 12),
#                   strip.background = element_blank(),
#                   panel.spacing = unit(0, "lines"),
#                   legend.position = c(0.06, 0.13))
#     }
#
#     print(dprime_plot)
# }
#
# ## PLOT d' ##
# for (b in c("day", "section", "hsection", "qsection")) {
#     # separate training groups
#     print(paste("Plotting training d' by", b))
#     p <- plot_training_dprime(b)
#     assign(paste0("dprime_",b,"_plot"), p)
#     #assign(paste0("dprime_",b), p$data)
#     ggsave(file.path("plots-training","setA_vs_setB",paste0("dprime_",b,".png")),
#            width = ifelse(b == "day", 5.5, 12), height = 6)
#     cat(paste0("plot: dprime_",b,"_plot\n",
#                #"data: dprime_",b,"\n"
#                "file: ", file.path("plots-training","setA_vs_setB",
#                                    paste0("dprime_",b,".png\n"))))
#
#     # combined training groups
#     print(paste0("Plotting training d' by ", b, ", training groups combined"))
#     p <- plot_training_dprime(b, combined_groups = TRUE)
#     assign(paste0("dprime_",b,"_combn_plot"), p)
#     #assign(paste0("dprime_",b,"_combined"), p$data)
#     ggsave(file.path("plots-training","combined",paste0("dprime_",b,"_combined.png")),
#            width = ifelse(b == "day", 5.5, 12), height = 6)
#     cat(paste0("plot: dprime_",b,"_combn_plot\n",
#                #"data: dprime_",b,"_combined\n"
#                "file: ", file.path("plots-training","combined",
#                                    paste0("dprime_",b,"_combined.png\n"))))
# }
#
# ## FUNCTION: TRAINING RT ##
# plot_training_rt <- function(breakdown = "day", combined_groups = FALSE,
#                              ymin = 650, ymax = 1350) {
#     group_by_vars <- c("participant", "talker_trained", breakdown)
#     if (breakdown != "day") assign("group_by_vars", append(group_by_vars, "day", 2))
#     if (combined_groups) assign("group_by_vars", group_by_vars[-2])
#
#     df <- training %>%
#         group_by_at(vars(group_by_vars)) %>%
#         summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
#         group_by_at(vars(group_by_vars[-1])) %>%
#         summarize(n = n(),
#                   mean_rt = mean(rt),
#                   error_rt = sd(rt)/sqrt(n)) %>%
#         ungroup()
#
#     rt_plot <- ggplot(data = df, aes_string(x = breakdown, y = "mean_rt")) +
#         labs(title = paste("Training RT by",
#                            case_when(breakdown == "day" ~ "Day",
#                                      breakdown == "section" ~ "Section",
#                                      breakdown == "hsection" ~ "Half-Section",
#                                      breakdown == "qsection" ~ "Quarter-Section")),
#              y = "Mean RT (ms)", x = "Day") +
#         scale_y_continuous(limits = c(ymin, ymax),
#                            breaks = seq.int(ymin, ymax, 100)) +
#         scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
#         theme(
#             # titles and labels
#             plot.title = element_text(hjust = 0.5, face = "bold"),
#             plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
#             plot.caption = element_text(size = 10),
#             axis.title.x = element_text(size = 12, face = "bold"),
#             axis.title.y = element_text(size = 12, face = "bold"),
#             axis.text = element_text(size = 12),
#
#             # facets
#             panel.background = element_blank(),
#
#             # axis lines
#             axis.line = element_line(),
#             axis.ticks.length = unit(0.5, "lines"),
#
#             # legend
#             legend.title = element_text(size = 9, face = "bold"),
#             legend.text = element_text(size = 8),
#             legend.background = element_rect(fill = "transparent"),
#             legend.box.background = element_rect(fill = "white"),
#             legend.position = c(0.14, 0.13))
#
#     if (!combined_groups) {
#         # plot training groups separately
#         df_setA <- filter(df, talker_trained == "setA")
#         df_setB <- filter(df, talker_trained == "setB")
#
#         rt_plot <- rt_plot +
#             aes(color = talker_trained) +
#             geom_line(aes(group = talker_trained), size = 1) +
#
#             # participants trained on setA
#             geom_errorbar(data = df_setA, aes(ymin = mean_rt - error_rt,
#                                               ymax = mean_rt + error_rt),
#                           width = 0.01*nrow(df), alpha = 0.5,
#                           position = position_nudge(x = -0.005)) +
#             geom_point(data = df_setA, size = 3,
#                        position = position_nudge(x = -0.005)) +
#
#             # participants trained on setB
#             geom_errorbar(data = df_setB, aes(ymin = mean_rt - error_rt,
#                                               ymax = mean_rt + error_rt),
#                           width = 0.01*nrow(df), alpha = 0.5,
#                           position = position_nudge(x = 0.005)) +
#             geom_point(data = df_setB, size = 3,
#                        position = position_nudge(x = 0.005)) +
#
#             labs(subtitle = "",
#                  caption = paste0("setA n = ", unique(df_setA$n), "\nsetB n = ",
#                                   unique(df_setB$n)))
#     } else {
#         # plot training groups together
#         rt_plot <- rt_plot +
#             # all participants
#             geom_line(aes(group = 1), size = 1) +
#             geom_errorbar(aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
#                           width = 0.01*nrow(df), alpha = 0.5) +
#             geom_point(size = 3) +
#
#             labs(subtitle = "Training Groups Combined",
#                  caption = paste0("n = ", unique(df$n), "\n"))
#     }
#
#     if (breakdown != "day") {
#         # plot each day side-by-side using facets
#         rt_plot <- rt_plot +
#             facet_grid(~ day, switch = "x",
#                        labeller = as_labeller(c(`1` = "Day 1",
#                                                 `2` = "Day 2",
#                                                 `3` = "Day 3"))) +
#             scale_x_discrete(labels = function(x) ifelse(endsWith(x, "5"), "",
#                                                          paste0("Section ",x))) +
#             theme(axis.title.x = element_blank(),
#                   strip.placement = "outside",
#                   strip.text = element_text(size = 12),
#                   strip.background = element_blank(),
#                   panel.spacing = unit(0, "lines"),
#                   legend.position = c(0.06, 0.13))
#     }
#
#     print(rt_plot)
# }
#
# ## PLOT RT ##
# for (b in c("day", "section", "hsection", "qsection")) {
#     # separate training groups
#     print(paste("Plotting training RT by", b))
#     p <- plot_training_rt(b)
#     assign(paste0("rt_",b,"_plot"), p)
#     #assign(paste0("rt_",b), p$data)
#     ggsave(file.path("plots-training","setA_vs_setB",paste0("rt_",b,".png")),
#            width = ifelse(b == "day", 5.5, 12), height = 6)
#     cat(paste0("plot: rt_",b,"_plot\n",
#                #"data: rt_",b,"\n"
#                "file: ", file.path("plots-training","setA_vs_setB",
#                                    paste0("rt_",b,".png\n"))))
#
#     # combined training groups
#     print(paste0("Plotting training RT by ", b, ", training groups combined"))
#     p <- plot_training_rt(b, combined_groups = TRUE)
#     assign(paste0("rt_",b,"_combn_plot"), p)
#     #assign(paste0("rt_",b,"_combined"), p$data)
#     ggsave(file.path("plots-training","combined",paste0("rt_",b,"_combined.png")),
#            width = ifelse(b == "day", 5.5, 12), height = 6)
#     cat(paste0("plot: rt_",b,"_combn_plot\n",
#                #"data: rt_",b,"_combined\n"
#                "file: ", file.path("plots-training","combined",
#                                    paste0("rt_",b,"_combined.png\n"))))
# }
#
# ## FUNCTION: RT BOXPLOTS ##
# plot_training_box <- function(exclude = "none", combined_groups = FALSE,
#                               ymin = 300, ymax = 2700) {
#     group_by_vars <- c("participant", "talker_trained", "day", "section")
#     if (combined_groups) assign("group_by_vars", group_by_vars[-2])
#
#     rt_box <- training %>%
#         filter(!participant %in% exclude) %>%
#         group_by_at(vars(group_by_vars)) %>%
#         summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
#         mutate(day_section = paste0(day, "_", section))
#
#     rt_boxplot <- ggplot(data = rt_box, aes(x = day_section, y = rt)) +
#         geom_boxplot(aes(fill = section), coef = 999) +
#         geom_point(aes(group = participant),
#                    position = position_nudge(-.25), alpha = 0.1) +
#         geom_line(aes(group = participant),
#                   position = position_nudge(-.25), alpha = 0.1) +
#
#         # labels
#         scale_x_discrete( # hacking x-axis labels...
#             # empty strings for spacing
#             limits = c("", "1_1", "1_2", "1_3", "1_4", "", "2_1", "2_2", "2_3",
#                        "2_4", "", "3_1", "3_2", "3_3", "3_4", ""),
#             # section 2 = "Day" label, section 3 = day number label
#             labels = function(x) ifelse(endsWith(x, "2"), "Day",
#                                         ifelse(endsWith(x, "3"),
#                                                substr(x, 1, 1), ""))) +
#         scale_y_continuous(limits = c(ymin, ymax),
#                            breaks = seq.int(ymin, ymax, 300),
#                            expand = c(0,0)) +
#         labs(title = "Training RT Over Time", y = "Mean RT (ms)",
#              fill = "Section") +
#
#         # formatting
#         scale_fill_viridis(discrete = TRUE,
#                            guide = guide_legend(title.position = "top",
#                                                 label.position = "bottom")) +
#         theme(
#             # titles and labels
#             plot.title = element_text(hjust = 0.5, face = "bold"),
#             plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
#             axis.title.x = element_blank(),
#             axis.text.x = element_text(size = 11, face = "bold", color = "black"),
#             axis.title.y = element_text(size = 11, face = "bold"),
#             plot.caption = element_text(size = 10),
#
#             # facets
#             strip.placement = "outside",
#             strip.text = element_text(size = 12, face = "bold"),
#             strip.background = element_blank(),
#             panel.background = element_blank(),
#
#             # axis lines
#             axis.line.y = element_line(),
#             axis.ticks.length = unit(0.5, "lines"),
#             axis.ticks.x = element_blank(),
#
#             # legend
#             legend.title = element_text(size = 9, face = "bold"),
#             legend.text = element_text(size = 8),
#             legend.position = c(0.915, 0.9),
#             legend.direction = "horizontal",
#             legend.background = element_rect(fill = "transparent"),
#             legend.box.background = element_rect(fill = "white"),
#             legend.key = element_rect(fill = "transparent"),
#             legend.spacing.x = unit(0, "lines"))
#
#     if (!combined_groups) {
#         # plot training groups separately
#         rt_boxplot <- rt_boxplot +
#             facet_wrap(~ talker_trained,
#                        labeller = as_labeller(c(`setA` = "Trained on setA Talkers",
#                                                 `setB` = "Trained on setB Talkers")),
#                        scales = "free_y") +
#             labs(#subtitle = paste("Participants excluded:",
#                 #                 paste(exclude, collapse = ", ")),
#                 caption = paste0("setA n = ",
#                                  length(unique(filter(
#                                      rt_box,
#                                      talker_trained == "setA")$participant)),
#                                  "\nsetB n = ",
#                                  length(unique(filter(
#                                      rt_box,
#                                      talker_trained == "setB")$participant))))
#     } else {
#         # plot training groups together
#         rt_boxplot <- rt_boxplot +
#             labs(subtitle = "Training Groups Combined",
#                  #paste("Training Groups Combined\nParticipants excluded:",
#                  #      paste(exclude, collapse = ", ")),
#                  caption = paste0("n = ", length(unique(rt_box$participant)))) +
#             theme(legend.position = c(0.84, 0.9))
#     }
#     print(rt_boxplot)
# }
#
# ## PLOT RT BOXPLOTS ##
# print("Plotting training RT boxplots")
# rt_boxplot <- plot_training_box()
# ggsave(file.path("plots-training","setA_vs_setB", "rt_boxplots.png"),
#        width = 8.75, height = 6)
# cat(paste0("plot: rt_boxplot\nfile: ",
#            file.path("plots-training","setA_vs_setB", "rt_boxplots.png\n")))
#
# print("Plotting training RT boxplots, training groups combined")
# rt_boxplot_combn <- plot_training_box(combined_groups = TRUE)
# ggsave(file.path("plots-training","combined","rt_boxplots_combined.png"),
#        width = 4.75, height = 5.5)
# cat(paste0("plot: rt_boxplot_combn\nfile: ",
#            file.path("plots-training","combined","rt_boxplots_combined.png\n")))

#### ARCHIVE: TRANSCRIPTION PLOTS ####
# ## FUNCTION: TRANSCRIPTION ACCURACY ##
# plot_tx <- function(all_vowels = FALSE, combined_groups = FALSE) {
#     if (all_vowels) {
#         df <- transcription
#         s <- "All Vowels"
#     } else {
#         df <- filter(transcription, trained_v == "Trained Vowels")
#         s <- "Trained Vowels Only"
#     }
#
#     group_by_vars <- c("participant", "talker_trained", "trained_t", "part")
#     if (combined_groups) {
#         group_by_vars <- group_by_vars[-2]
#         s <- paste0(s, ", Training Groups Combined\n")
#     }
#
#     df %<>% gather(key = part, value = score, c(9:12)) %>% # cols 9-12 = scores
#         group_by_at(vars(group_by_vars)) %>%
#         summarize(n = n(), score_total = sum(score)) %>%
#         mutate(score_percent = score_total/n)
#     df$part <-  factor(df$part, levels = c("whole_word", "initial", "vowel", "final"),
#                        labels = c("whole\nword", "initial", "vowel", "final"))
#
#     df_segments <- df %>%
#         select(-score_total, -n) %>%
#         spread(key = trained_t, value = score_percent) %>%
#         rename(y = "Untrained Talkers",
#                yend = "Trained Talkers") %>%
#         mutate(x = match(part, levels(part)) - 0.15,
#                xend = match(part, levels(part)) + 0.15)
#
#     tx_plot <- ggplot(data = df, aes(x = part, y = score_percent*100)) +
#         # boxplot (all participants)
#         geom_boxplot(aes(fill = trained_t), coef = 999,
#                      position = position_dodge(1)) +
#
#         # individual participant points and segments
#         geom_segment(data = df_segments, aes(x = x, y = y*100, xend = xend,
#                                              yend = yend*100),#, color = participant),
#                      alpha = 0.3) +
#         geom_point(data = filter(df, trained_t == "Untrained Talkers"),
#                    aes(group = participant), alpha = 0.3,
#                    position = position_nudge(x = -.15)) +
#         geom_point(data = filter(df, trained_t == "Trained Talkers"),
#                    aes(group = participant), alpha = 0.3,
#                    position = position_nudge(x = .15)) +
#
#         scale_x_discrete(expand = c(0.35, 0)) +
#         scale_y_continuous(limits = c(0, 101), breaks = seq.int(0,100,10),
#                            labels = function(x) paste0(x,"%"), expand = c(0,0),
#                            sec.axis = sec_axis(~.*.96, name = "Words Correct\n",
#                                                breaks = seq.int(0, 96, 12))) +
#         labs(title = "Transcription Accuracy", subtitle = s,
#              x = "", y = "Percent Correct") +
#         geom_segment(aes(x = "final", xend = "whole\nword", y = 0, yend = 0)) +
#         #scale_color_viridis(discrete = TRUE, guide = FALSE) + # individual participants
#         scale_fill_viridis(discrete = TRUE, option = "inferno",
#                            begin = .2, end = .7, direction = -1) +
#         theme(plot.title = element_text(hjust = 0.5, face = "bold"),
#               plot.subtitle = element_text(hjust = 0.5, face = "italic"),
#               plot.caption = element_text(size = 10),
#               axis.title = element_text(size = 11),
#               axis.text = element_text(size = 11),
#               panel.background = element_blank(),
#               axis.line.y = element_line(),
#               axis.ticks.length = unit(0.3, "lines"),
#               legend.title = element_blank(),
#               legend.margin = margin(c(0,0,0,0)),
#               legend.text = element_text(size = 7))
#
#     if (!combined_groups) {
#         # plot training groups separately
#         tx_plot <- tx_plot +
#             facet_wrap(~ talker_trained,
#                        labeller = as_labeller(c(`setA` = "Trained on setA Talkers\n",
#                                                 `setB` = "Trained on setB Talkers\n")),
#                        scales = "free") +
#             labs(caption = paste0("setA n = ",
#                                   length(unique(filter(
#                                       df, talker_trained == "setA")$participant)),
#                                   "\nsetB n = ",
#                                   length(unique(filter(
#                                       df, talker_trained == "setB")$participant)))) +
#             theme(strip.placement = "outside",
#                   strip.background = element_blank(),
#                   strip.text = element_text(face = "bold", size = 11),
#                   panel.spacing = unit(2, "lines"),
#                   legend.position = c(.87,.08))
#     } else {
#         tx_plot <- tx_plot +
#             labs(caption = paste0("n = ", length(unique(df$participant)), "\n")) +
#             theme(legend.position = c(.79,.08))
#     }
#
#     print(tx_plot)
# }
#
# ## PLOT TRANSCRIPTION ACCURACY ##
# print("Plotting transcription accuracy")
# tx_acc_plot <- plot_tx()
# #tx_acc <- select(tx_acc_plot$data, -.group)
# ggsave(file.path("plots-transcription", "accuracy_trainedv.png"),
#        width = 6.5, height = 5.5)
# cat(paste0("plot: tx_acc_plot\n",
#            #"data: tx_acc\n"
#            "file: ", file.path("plots-transcription", "accuracy_trainedv.png\n")))
#
# print("Plotting transcription accuracy, training groups combined")
# tx_acc_combn_plot <- plot_tx(combined_groups = TRUE)
# #tx_acc_combined <- select(tx_acc_combn_plot$data, -.group)
# ggsave(file.path("plots-transcription", "accuracy_trainedv_combined.png"),
#        width = 4, height = 5.25)
# cat(paste0("plot: tx_acc_combn_plot\n",
#            #"data: tx_acc_combined\n"
#            "file: ", file.path("plots-transcription",
#                                "accuracy_trainedv_combined.png\n")))
#
# print("Plotting transcription accuracy, all vowels")
# tx_acc_allv_plot <- plot_tx(all_vowels = TRUE)
# #tx_acc_allv <- select(tx_acc_allv_plot$data, -.group)
# ggsave(file.path("plots-transcription", "accuracy_allv.png"),
#        width = 6.5, height = 5.5)
# cat(paste0("plot: tx_acc_allv_plot\n",
#            #"data: tx_acc_allv\n"
#            "file: ", file.path("plots-transcription", "accuracy_allv.png\n")))
#
# print("Plotting transcription accuracy, all vowels, training groups combined")
# tx_acc_allv_combn_plot <- plot_tx(all_vowels = TRUE, combined_groups = TRUE)
# #tx_acc_allv_combined <- tx_acc_allv_combn_plot$data
# ggsave(file.path("plots-transcription", "accuracy_allv_combined.png"),
#        width = 4, height = 5.25)
# cat(paste0("plot: tx_acc_allv_combn_plot\n",
#            #"data: tx_acc_allv_combined\n"
#            "file: ", file.path("plots-transcription",
#                                "accuracy_allv_combined.png\n")))
