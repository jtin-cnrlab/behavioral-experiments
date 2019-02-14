# tfta_plots.R
# Jessica Tin
# 14 Feb 2019
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

    # convert all variables except RT columns to factors (+ include paRTicipant)
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
master_transcription <- read_csv("master_transcription.csv",
                                 col_types = "ciciiccciiiicc")

transcription <- master_transcription %>%
    # add training info (with training_groups participant factors as characters)
    left_join(mutate_all(training_groups, as.character), by = "participant") %>%
    mutate(trained_t = ifelse(talker_set == talker_trained, "trained", "untrained"),
           trained_v = ifelse(vowel_pair == vowel_trained, "trained", "untrained")) %>%

    # convert all variables except score columns to factors
    mutate_at(vars(-whole_word, -initial, -vowel, -final), as.factor)

# refactor variables
transcription$trained_t <- factor(transcription$trained_t,
                                  levels = c("untrained", "trained"),
                                  labels = c("Untrained Talkers", "Trained Talkers"))
transcription$trained_v <- factor(transcription$trained_v,
                                  levels = c("untrained", "trained"),
                                  labels = c("Untrained Vowels", "Trained Vowels"))

#### FUNCTION: TRAINING d' ####
plot_training_dprime <- function(breakdown = "day", combined_groups = FALSE) {
    group_by_vars <- c("participant", "talker_trained", breakdown)
    if (breakdown != "day") assign("group_by_vars", append(group_by_vars, "day", 2))
    if (combined_groups) assign("group_by_vars", group_by_vars[-2])

    df <- training %>%
        group_by_at(vars(group_by_vars)) %>%
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
            dprime = qnorm(hit_rate) - qnorm(fa_rate)) %>%
        group_by_at(vars(group_by_vars[-1])) %>%
        summarize(n = n(),
                  mean_hit_rate = mean(hit_rate),
                  mean_fa_rate = mean(fa_rate),
                  mean_dprime = mean(dprime),
                  error_dprime = sd(dprime)/sqrt(n)) %>%
        ungroup()

    dprime_plot <- ggplot(data = df, aes_string(x = breakdown, y = "mean_dprime")) +
        scale_y_continuous(limits = c(0, 4), expand = c(0,0)) +
        labs(title = paste("Training d' by",
                           case_when(breakdown == "day" ~ "Day",
                                     breakdown == "section" ~ "Section",
                                     breakdown == "hsection" ~ "Half-Section",
                                     breakdown == "qsection" ~ "Quarter-Section")),
             y = "Mean d'", x = "Day") +
        scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
        theme(
            # titles and labels
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
            plot.caption = element_text(size = 10),
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),

            # facets
            panel.background = element_blank(),

            # axis lines
            axis.line = element_line(),
            axis.ticks.length = unit(0.5, "lines"),

            # legend
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 8),
            legend.position = c(0.13, 0.13),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "white"))

    if (!combined_groups) {
        # plot training groups separately
        df_setA <- filter(df, talker_trained == "setA")
        df_setB <- filter(df, talker_trained == "setB")

        dprime_plot <- dprime_plot +
            aes(color = talker_trained) +
            geom_line(aes(group = talker_trained), size = 1) +

            # participants trained on setA
            geom_errorbar(data = df_setA, aes(ymin = mean_dprime - error_dprime,
                                              ymax = mean_dprime + error_dprime),
                          width = 0.01*nrow(df), alpha = 0.5,
                          position = position_nudge(x = -0.005)) +
            geom_point(data = df_setA, size = 3,
                       position = position_nudge(x = -0.005)) +

            # participants trained on setB
            geom_errorbar(data = df_setB, aes(ymin = mean_dprime - error_dprime,
                                              ymax = mean_dprime + error_dprime),
                          width = 0.01*nrow(df), alpha = 0.5,
                          position = position_nudge(x = 0.005)) +
            geom_point(data = df_setB, size = 3,
                       position = position_nudge(x = 0.005)) +

            labs(subtitle = "",
                 caption = paste0("setA n = ", unique(df_setA$n), "\nsetB n = ",
                                  unique(df_setB$n)))
    } else {
        # plot training groups together
        dprime_plot <- dprime_plot +
            # all participants
            geom_line(aes(group = 1), size = 1) +
            geom_errorbar(aes(ymin = mean_dprime - error_dprime,
                              ymax = mean_dprime + error_dprime),
                          width = 0.01*nrow(df), alpha = 0.5) +
            geom_point(size = 3) +

            labs(subtitle = "Training Groups Combined",
                 caption = paste0("n = ", unique(df$n),"\n"))
    }

    if (breakdown != "day") {
        # plot each day side-by-side using facets
        dprime_plot <- dprime_plot +
            facet_grid(~ day, switch = "x",
                       labeller = as_labeller(c(`1` = "Day 1",
                                                `2` = "Day 2",
                                                `3` = "Day 3"))) +
            scale_x_discrete(labels = function(x) ifelse(endsWith(x, "5"), "",
                                                         paste0("Section ",x))) +
            theme(axis.title.x = element_blank(),
                  strip.placement = "outside",
                  strip.text = element_text(size = 12),
                  strip.background = element_blank(),
                  panel.spacing = unit(0, "lines"),
                  legend.position = c(0.06, 0.13))
    }

    print(dprime_plot)
}

#### PLOT d' ####
for (b in c("day", "section", "hsection", "qsection")) {
    # separate training groups
    print(paste("Plotting d' by", b))
    p <- plot_training_dprime(b)
    assign(paste0("dprime_",b,"_plot"), p)
    assign(paste0("dprime_",b), p$data)
    ggsave(file.path("plots-training","setA_vs_setB",paste0("dprime_",b,".png")),
           width = ifelse(b == "day", 5.5, 12), height = 6)
    cat(paste0("plot: dprime_",b,"_plot\ndata: dprime_",b,"\nfile: ",
               file.path("plots-training","setA_vs_setB",
                         paste0("dprime_",b,".png\n"))))

    # combined training groups
    print(paste0("Plotting d' by ", b, ", training groups combined"))
    p <- plot_training_dprime(b, combined_groups = TRUE)
    assign(paste0("dprime_",b,"_combn_plot"), p)
    assign(paste0("dprime_",b,"_combined"), p$data)
    ggsave(file.path("plots-training","combined",paste0("dprime_",b,"_combined.png")),
           width = ifelse(b == "day", 5.5, 12), height = 6)
    cat(paste0("plot: dprime_",b,"_combn_plot\ndata: dprime_",b,"_combined\nfile: ",
               file.path("plots-training","combined",
                         paste0("dprime_",b,"_combined.png\n"))))
}

#### FUNCTION: TRAINING RT ####
plot_training_rt <- function(breakdown = "day", combined_groups = FALSE,
                             ymin = 650, ymax = 1350) {
    group_by_vars <- c("participant", "talker_trained", breakdown)
    if (breakdown != "day") assign("group_by_vars", append(group_by_vars, "day", 2))
    if (combined_groups) assign("group_by_vars", group_by_vars[-2])

    df <- training %>%
        group_by_at(vars(group_by_vars)) %>%
        summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
        group_by_at(vars(group_by_vars[-1])) %>%
        summarize(n = n(),
                  mean_rt = mean(rt),
                  error_rt = sd(rt)/sqrt(n)) %>%
        ungroup()

    rt_plot <- ggplot(data = df, aes_string(x = breakdown, y = "mean_rt")) +
        labs(title = paste("Training RT by",
                           case_when(breakdown == "day" ~ "Day",
                                     breakdown == "section" ~ "Section",
                                     breakdown == "hsection" ~ "Half-Section",
                                     breakdown == "qsection" ~ "Quarter-Section")),
             y = "Mean RT (ms)", x = "Day") +
        scale_y_continuous(limits = c(ymin, ymax),
                           breaks = seq.int(ymin, ymax, 100)) +
        scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
        theme(
            # titles and labels
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
            plot.caption = element_text(size = 10),
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),

            # facets
            panel.background = element_blank(),

            # axis lines
            axis.line = element_line(),
            axis.ticks.length = unit(0.5, "lines"),

            # legend
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 8),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "white"),
            legend.position = c(0.14, 0.13))

    if (!combined_groups) {
        # plot training groups separately
        df_setA <- filter(df, talker_trained == "setA")
        df_setB <- filter(df, talker_trained == "setB")

        rt_plot <- rt_plot +
            aes(color = talker_trained) +
            geom_line(aes(group = talker_trained), size = 1) +

            # participants trained on setA
            geom_errorbar(data = df_setA, aes(ymin = mean_rt - error_rt,
                                              ymax = mean_rt + error_rt),
                          width = 0.01*nrow(df), alpha = 0.5,
                          position = position_nudge(x = -0.005)) +
            geom_point(data = df_setA, size = 3,
                       position = position_nudge(x = -0.005)) +

            # participants trained on setB
            geom_errorbar(data = df_setB, aes(ymin = mean_rt - error_rt,
                                              ymax = mean_rt + error_rt),
                          width = 0.01*nrow(df), alpha = 0.5,
                          position = position_nudge(x = 0.005)) +
            geom_point(data = df_setB, size = 3,
                       position = position_nudge(x = 0.005)) +

            labs(subtitle = "",
                 caption = paste0("setA n = ", unique(df_setA$n), "\nsetB n = ",
                                  unique(df_setB$n)))
    } else {
        # plot training groups together
        rt_plot <- rt_plot +
            # all participants
            geom_line(aes(group = 1), size = 1) +
            geom_errorbar(aes(ymin = mean_rt - error_rt, ymax = mean_rt + error_rt),
                          width = 0.01*nrow(df), alpha = 0.5) +
            geom_point(size = 3) +

            labs(subtitle = "Training Groups Combined",
                 caption = paste0("n = ", unique(df$n), "\n"))
    }

    if (breakdown != "day") {
        # plot each day side-by-side using facets
        rt_plot <- rt_plot +
            facet_grid(~ day, switch = "x",
                       labeller = as_labeller(c(`1` = "Day 1",
                                                `2` = "Day 2",
                                                `3` = "Day 3"))) +
            scale_x_discrete(labels = function(x) ifelse(endsWith(x, "5"), "",
                                                         paste0("Section ",x))) +
            theme(axis.title.x = element_blank(),
                  strip.placement = "outside",
                  strip.text = element_text(size = 12),
                  strip.background = element_blank(),
                  panel.spacing = unit(0, "lines"),
                  legend.position = c(0.06, 0.13))
    }

    print(rt_plot)
}

#### PLOT RT ####
for (b in c("day", "section", "hsection", "qsection")) {
    # separate training groups
    print(paste("Plotting RT by", b))
    p <- plot_training_rt(b)
    assign(paste0("rt_",b,"_plot"), p)
    assign(paste0("rt_",b), p$data)
    ggsave(file.path("plots-training","setA_vs_setB",paste0("rt_",b,".png")),
           width = ifelse(b == "day", 5.5, 12), height = 6)
    cat(paste0("plot: rt_",b,"_plot\ndata: rt_",b,"\nfile: ",
               file.path("plots-training","setA_vs_setB",
                         paste0("rt_",b,".png\n"))))

    # combined training groups
    print(paste0("Plotting RT by ", b, ", training groups combined"))
    p <- plot_training_rt(b, combined_groups = TRUE)
    assign(paste0("rt_",b,"_combn_plot"), p)
    assign(paste0("rt_",b,"_combined"), p$data)
    ggsave(file.path("plots-training","combined",paste0("rt_",b,"_combined.png")),
           width = ifelse(b == "day", 5.5, 12), height = 6)
    cat(paste0("plot: rt_",b,"_combn_plot\ndata: rt_",b,"_combined\nfile: ",
               file.path("plots-training","combined",
                         paste0("rt_",b,"_combined.png\n"))))
}

#### FUNCTION: RT BOXPLOTS ####
plot_training_box <- function(exclude = "none", combined_groups = FALSE,
                              ymin = 300, ymax = 2700) {
    group_by_vars <- c("participant", "talker_trained", "day", "section")
    if (combined_groups) assign("group_by_vars", group_by_vars[-2])

    rt_box <- training %>%
        filter(!participant %in% exclude) %>%
        group_by_at(vars(group_by_vars)) %>%
        summarize(rt = mean(rt_3SD, na.rm = TRUE)) %>%
        mutate(day_section = paste0(day, "_", section))

    rt_boxplot <- ggplot(data = rt_box, aes(x = day_section, y = rt)) +
        geom_boxplot(aes(fill = section), coef = 999) +
        geom_point(aes(group = participant),
                   position = position_nudge(-.25), alpha = 0.1) +
        geom_line(aes(group = participant),
                  position = position_nudge(-.25), alpha = 0.1) +

        # labels
        scale_x_discrete( # hacking x-axis labels...
            # empty strings for spacing
            limits = c("", "1_1", "1_2", "1_3", "1_4", "", "2_1", "2_2", "2_3",
                       "2_4", "", "3_1", "3_2", "3_3", "3_4", ""),
            # section 2 = "Day" label, section 3 = day number label
            labels = function(x) ifelse(endsWith(x, "2"), "Day",
                                        ifelse(endsWith(x, "3"),
                                               substr(x, 1, 1), ""))) +
        scale_y_continuous(limits = c(ymin, ymax),
                           breaks = seq.int(ymin, ymax, 300),
                           expand = c(0,0)) +
        labs(title = "Training RT Over Time", y = "Mean RT (ms)",
             fill = "Section") +

        # formatting
        scale_fill_viridis(discrete = TRUE,
                           guide = guide_legend(title.position = "top",
                                                label.position = "bottom")) +
        theme(
            # titles and labels
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 11, face = "bold", color = "black"),
            axis.title.y = element_text(size = 11, face = "bold"),
            plot.caption = element_text(size = 10),

            # facets
            strip.placement = "outside",
            strip.text = element_text(size = 12, face = "bold"),
            strip.background = element_blank(),
            panel.background = element_blank(),

            # axis lines
            axis.line.y = element_line(),
            axis.ticks.length = unit(0.5, "lines"),
            axis.ticks.x = element_blank(),

            # legend
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 8),
            legend.position = c(0.915, 0.9),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "white"),
            legend.key = element_rect(fill = "transparent"),
            legend.spacing.x = unit(0, "lines"))

    if (!combined_groups) {
        # plot training groups separately
        rt_boxplot <- rt_boxplot +
            facet_wrap(~ talker_trained,
                       labeller = as_labeller(c(`setA` = "Trained on setA Talkers",
                                                `setB` = "Trained on setB Talkers")),
                       scales = "free_y") +
            labs(#subtitle = paste("Participants excluded:",
                 #                 paste(exclude, collapse = ", ")),
                 caption = paste0("setA n = ",
                                  length(unique(filter(
                                      rt_box,
                                      talker_trained == "setA")$participant)),
                                  "\nsetB n = ",
                                  length(unique(filter(
                                      rt_box,
                                      talker_trained == "setB")$participant))))
    } else {
        # plot training groups together
        rt_boxplot <- rt_boxplot +
            labs(subtitle = "Training Groups Combined",
                 #paste("Training Groups Combined\nParticipants excluded:",
                 #      paste(exclude, collapse = ", ")),
                 caption = paste0("n = ", length(unique(rt_box$participant)))) +
            theme(legend.position = c(0.84, 0.9))
    }
    print(rt_boxplot)
}

#### PLOT RT BOXPLOTS ####
rt_boxplot_combn <- plot_training_box(combined_groups = TRUE)
ggsave(file.path("plots-training","combined","rt_boxplots_combined.png"),
       width = 4.75, height = 5.5)

rt_boxplot <- plot_training_box()
ggsave(file.path("plots-training","setA_vs_setB", "rt_boxplots.png"),
       width = 8.75, height = 6)

#### FUNCTION: TEST RT, SINGLE VS. MIXED ####
plot_test_rt_singlemixed <- function(all_vowels = FALSE, combined_groups = FALSE,
                                     exclude = "none", ymin = 550, ymax = 850) {
    if (all_vowels) {
        df <- test
        s <- "All Vowels"
    } else {
        df <- filter(test, trained_v == "Trained Vowels")
        s <- "Trained Vowels Only"
    }

    group_by_vars <- c("participant", "carrier", "single_mixed", "test",
                       "talker_trained", "trained_t")
    if (combined_groups) {
        group_by_vars <- group_by_vars[-5]
        s <- paste0(s, ", Training Groups Combined")
    }

    df %<>% filter(!participant %in% exclude) %>%
        group_by_at(vars(group_by_vars)) %>%
        summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
        group_by_at(vars(group_by_vars[-1])) %>%
        summarize(n = n(),
                  mean_rt = mean(mean_rt))

    test_plot <- ggplot(data = df, aes(x = single_mixed, y = mean_rt, alpha = test)) +
        geom_line(size = 1) +
        geom_point(aes(shape = test), size = 3.5) +
        facet_wrap(carrier ~ trained_t, scales = "free") +
        scale_y_continuous(limits = c(ymin, ymax),
                           breaks = c(seq.int(ymin, ymax, 50))) +
        labs(title = "Test RT, Single-Talker vs. Mixed-Talker Blocks",
             subtitle = s,
             #subtitle = paste0(s, "\nParticipants excluded: ",
             #                  paste(exclude, collapse = ", ")),
             x = "", y = "Mean RT (ms)") +
        scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
        scale_alpha_discrete(range = c(0.4, 1)) + # ignore discrete alpha warning
        theme(
            # titles and labels
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic",
                                         margin = margin(0,0,10,0,"pt")),
            plot.caption = element_text(size = 10),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 11),

            # facets
            strip.placement = "outside",
            strip.text = element_text(size = 11, face = "bold",
                                      margin = margin(2,0,2,0,"pt")),
            strip.background = element_blank(),
            panel.background = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            panel.spacing.y = unit(1.5, "lines"),

            # axis lines
            axis.line = element_line(),

            # legend
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 8))

    if (!combined_groups) {
        # plot training groups separately
        test_plot <- test_plot +
            aes(color = talker_trained, group = interaction(test, talker_trained)) +
            labs(caption = paste0("setA n = ",
                                  unique(filter(df, talker_trained == "setA")$n),
                                  "\nsetB n = ",
                                  unique(filter(df, talker_trained == "setB")$n)))
    } else {
        # plot training groups together
        test_plot <- test_plot +
            aes(group = test) +
            labs(caption = paste0("n = ", unique(df$n), "\n"))
    }

    print(test_plot)
}

#### PLOT SINGLE VS. MIXED RT ####
test_rt_sm_plot <- plot_test_rt_singlemixed()
test_rt_sm <- select(test_rt_sm_plot$data, -.group)
ggsave(file.path("plots-test", "single_vs_mixed", "rt_trainedv.png"),
       width = 6, height = 6.5)

test_rt_sm_combn_plot <- plot_test_rt_singlemixed(combined_groups = TRUE)
test_rt_sm_combined <- select(test_rt_sm_combn_plot$data, -.group)
ggsave(file.path("plots-test", "single_vs_mixed", "rt_trainedv_combined.png"),
       width = 5.75, height = 6.5)

test_rt_sm_allv_plot <- plot_test_rt_singlemixed(all_vowels = TRUE)
test_rt_sm_allv <- select(test_rt_sm_allv_plot$data, -.group)
ggsave(file.path("plots-test", "single_vs_mixed", "rt_allv.png"),
       width = 6, height = 6.5)

test_rt_sm_allv_combn_plot <- plot_test_rt_singlemixed(all_vowels = TRUE,
                                                       combined_groups = TRUE)
test_rt_sm_allv_combined <- select(test_rt_sm_allv_combn_plot$data, -.group)
ggsave(file.path("plots-test", "single_vs_mixed", "rt_allv_combined.png"),
       width = 5.75, height = 6.5)

#### FUNCTION: TEST RT, PRE VS. POST ####
plot_test_rt_prepost <- function(all_vowels = FALSE, combined_groups = FALSE,
                                     exclude = "none", ymin = 550, ymax = 850) {
    if (all_vowels) {
        df <- test
        s <- "All Vowels"
    } else {
        df <- filter(test, trained_v == "Trained Vowels")
        s <- "Trained Vowels Only"
    }

    group_by_vars <- c("participant", "carrier", "single_mixed", "test",
                       "talker_trained", "trained_t")
    if (combined_groups) {
        group_by_vars <- group_by_vars[-5]
        s <- paste0(s, ", Training Groups Combined")
    }

    df %<>% filter(!participant %in% exclude) %>%
        group_by_at(vars(group_by_vars)) %>%
        summarize(mean_rt = mean(rt_3SD, na.rm = TRUE)) %>%
        group_by_at(vars(group_by_vars[-1])) %>%
        summarize(n = n(),
                  mean_rt = mean(mean_rt))

    test_plot <- ggplot(data = df, aes(x = test, y = mean_rt, alpha = single_mixed)) +
        geom_line(size = 1) +
        geom_point(aes(shape = single_mixed), size = 3.5) +
        facet_wrap(carrier ~ trained_t, scales = "free") +
        scale_y_continuous(limits = c(ymin, ymax),
                           breaks = c(seq.int(ymin, ymax, 50))) +
        labs(title = "Test RT, Pre- vs. Post-Test",
             subtitle = s,
             #subtitle = paste0(s, "\nParticipants excluded: ",
             #                  paste(exclude, collapse = ", ")),
             x = "", y = "Mean RT (ms)") +
        scale_color_viridis(discrete = TRUE, begin = 0.25, end = 0.75) +
        scale_alpha_discrete(range = c(0.4, 1)) + # ignore discrete alpha warning
        theme(
            # titles and labels
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic",
                                         margin = margin(0,0,10,0,"pt")),
            plot.caption = element_text(size = 10),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 11),

            # facets
            strip.placement = "outside",
            strip.text = element_text(size = 11, face = "bold",
                                      margin = margin(2,0,2,0,"pt")),
            strip.background = element_blank(),
            panel.background = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            panel.spacing.y = unit(1.5, "lines"),

            # axis lines
            axis.line = element_line(),

            # legend
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 8))

    if (!combined_groups) {
        # plot training groups separately
        test_plot <- test_plot +
            aes(color = talker_trained, group = interaction(single_mixed, talker_trained)) +
            labs(caption = paste0("setA n = ",
                                  unique(filter(df, talker_trained == "setA")$n),
                                  "\nsetB n = ",
                                  unique(filter(df, talker_trained == "setB")$n)))
    } else {
        # plot training groups together
        test_plot <- test_plot +
            aes(group = single_mixed) +
            labs(caption = paste0("n = ", unique(df$n), "\n"))
    }

    print(test_plot)
}

#### PLOT PRE- VS. POST-TEST RT ####
test_rt_pp_plot <- plot_test_rt_prepost()
test_rt_pp <- select(test_rt_pp_plot$data, -.group)
ggsave(file.path("plots-test", "pre_vs_post", "rt_trainedv.png"),
       width = 6, height = 6.5)

test_rt_pp_combn_plot <- plot_test_rt_prepost(combined_groups = TRUE)
test_rt_pp_combined <- select(test_rt_pp_combn_plot$data, -.group)
ggsave(file.path("plots-test", "pre_vs_post", "rt_trainedv_combined.png"),
       width = 5.75, height = 6.5)

test_rt_pp_allv_plot <- plot_test_rt_prepost(all_vowels = TRUE)
test_rt_pp_allv <- select(test_rt_pp_allv_plot$data, -.group)
ggsave(file.path("plots-test", "pre_vs_post", "rt_allv.png"),
       width = 6, height = 6.5)

test_rt_pp_allv_combn_plot <- plot_test_rt_prepost(all_vowels = TRUE,
                                                   combined_groups = TRUE)
test_rt_pp_allv_combined <- select(test_rt_pp_allv_combn_plot$data, -.group)
ggsave(file.path("plots-test", "pre_vs_post", "rt_allv_combined.png"),
       width = 5.75, height = 6.5)

#### FUNCTION: TRANSCRIPTION ACCURACY ####
plot_tx <- function(all_vowels = FALSE, combined_groups = FALSE) {
    if (all_vowels) {
        df <- transcription
        s <- "All Vowels"
    } else {
        df <- filter(transcription, trained_v == "Trained Vowels")
        s <- "Trained Vowels Only"
    }

    group_by_vars <- c("participant", "talker_trained", "trained_t", "part")
    if (combined_groups) {
        group_by_vars <- group_by_vars[-2]
        s <- paste0(s, ", Training Groups Combined\n")
    }

    df %<>% gather(key = part, value = score, c(9:12)) %>% # cols 9-12 = scores
        group_by_at(vars(group_by_vars)) %>%
        summarize(n = n(), score_total = sum(score)) %>%
        mutate(score_percent = score_total/n)
    df$part <-  factor(df$part, levels = c("whole_word", "initial", "vowel", "final"),
                       labels = c("whole\nword", "initial", "vowel", "final"))

    df_segments <- df %>%
        select(-score_total, -n) %>%
        spread(key = trained_t, value = score_percent) %>%
        rename(y = "Untrained Talkers",
               yend = "Trained Talkers") %>%
        mutate(x = match(part, levels(part)) - 0.15,
               xend = match(part, levels(part)) + 0.15)

    tx_plot <- ggplot(data = df, aes(x = part, y = score_percent*100)) +
        # boxplot (all participants)
        geom_boxplot(aes(fill = trained_t), coef = 999,
                     position = position_dodge(1)) +

        # individual participant points and segments
        geom_segment(data = df_segments, aes(x = x, y = y*100, xend = xend,
                                             yend = yend*100),#, color = participant),
                     alpha = 0.3) +
        geom_point(data = filter(df, trained_t == "Untrained Talkers"),
                   aes(group = participant), alpha = 0.3,
                   position = position_nudge(x = -.15)) +
        geom_point(data = filter(df, trained_t == "Trained Talkers"),
                   aes(group = participant), alpha = 0.3,
                   position = position_nudge(x = .15)) +

        scale_x_discrete(expand = c(0.35, 0)) +
        scale_y_continuous(limits = c(0, 101), breaks = seq.int(0,100,10),
                           labels = function(x) paste0(x,"%"), expand = c(0,0),
                           sec.axis = sec_axis(~.*.96, name = "Words Correct\n",
                                               breaks = seq.int(0, 96, 12))) +
        labs(title = "Transcription Accuracy", subtitle = s,
             x = "", y = "Percent Correct") +
        geom_segment(aes(x = "final", xend = "whole\nword", y = 0, yend = 0)) +
        #scale_color_viridis(discrete = TRUE, guide = FALSE) + # individual participants
        scale_fill_viridis(discrete = TRUE, option = "inferno",
                           begin = .2, end = .7, direction = -1) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, face = "italic"),
              plot.caption = element_text(size = 10),
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 11),
              panel.background = element_blank(),
              axis.line.y = element_line(),
              axis.ticks.length = unit(0.3, "lines"),
              legend.title = element_blank(),
              legend.margin = margin(c(0,0,0,0)),
              legend.text = element_text(size = 7))

    if (!combined_groups) {
        # plot training groups separately
        tx_plot <- tx_plot +
            facet_wrap(~ talker_trained,
                       labeller = as_labeller(c(`setA` = "Trained on setA Talkers\n",
                                                `setB` = "Trained on setB Talkers\n")),
                       scales = "free") +
            labs(caption = paste0("setA n = ",
                                  length(unique(filter(
                                      df, talker_trained == "setA")$participant)),
                                  "\nsetB n = ",
                                  length(unique(filter(
                                      df, talker_trained == "setB")$participant)))) +
            theme(strip.placement = "outside",
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold", size = 11),
                  panel.spacing = unit(2, "lines"),
                  legend.position = c(.87,.08))
    } else {
        tx_plot <- tx_plot +
            labs(caption = paste0("n = ", length(unique(df$participant)), "\n")) +
            theme(legend.position = c(.79,.08))
    }

    print(tx_plot)
}

#### PLOT TRANSCRIPTION ACCURACY ####
tx_acc_plot <- plot_tx()
tx_acc <- select(tx_acc_plot$data, -.group)
ggsave(file.path("plots-transcription", "accuracy_trainedv.png"),
       width = 6.5, height = 5.5)

tx_acc_combn_plot <- plot_tx(combined_groups = TRUE)
tx_acc_combined <- select(tx_acc_combn_plot$data, -.group)
ggsave(file.path("plots-transcription", "accuracy_trainedv_combined.png"),
       width = 4, height = 5.25)

tx_acc_allv_plot <- plot_tx(all_vowels = TRUE)
tx_acc_allv <- select(tx_acc_allv_plot$data, -.group)
ggsave(file.path("plots-transcription", "accuracy_allv.png"),
       width = 6.5, height = 5.5)

tx_acc_allv_combn_plot <- plot_tx(all_vowels = TRUE, combined_groups = TRUE)
tx_acc_allv_combined <- tx_acc_allv_combn_plot$data
ggsave(file.path("plots-transcription", "accuracy_allv_combined.png"),
       width = 4, height = 5.25)
