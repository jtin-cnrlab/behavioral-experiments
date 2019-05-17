# pavl_plots_all.R
# author: Jessica Tin
# updated: 18 Jul 2018
#
# Creates plots for PAVL.
#

#### LOAD PACKAGES ####
if(!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
if(!require("magrittr")) {install.packages("magrittr"); library(magrittr)}
if(!require("readr")) {install.packages("readr"); library(readr)}
if(!require("ggplot2")) {install.packages("ggplot2"); library(ggplot2)}
if(!require("viridis")) {install.packages("viridis"); library(viridis)}

#### SET WORKING DIRECTORY ####
setwd("/Volumes/PerrachioneLab/projects/PAVL/Analysis") # Mac
#setwd("/PerrachioneLab/projects/PAVL/Analysis") # Linux
#setwd("R:/PerrachioneLab/projects/PAVL/Analysis") # Windows

#### READ IN AND PREPROCESS DATA ####
master <- read_csv(max(list.files(pattern = "_master\\.csv$")),
                   col_types = "cicciidciidiicccc")

df <- master %>%
    filter(trial > 200) %>%
    group_by(experiment, condition, participant, block, order) %>%
    mutate(participant_block_accuracy = sum(test_accuracy) / n()) %>%
    summarize(participant_block_accuracy = unique(participant_block_accuracy)) %>%
    mutate(participant_order = paste0(participant,"_",order))
df$order <- factor(df$order, levels = c("SAME", "DIFF"))

#### PLOT ALL ACCURACY BY BLOCK BY CONDITION ####
means <- df %>%
    group_by(experiment, order, block) %>%
    summarize(mean_block_accuracy = mean_se(participant_block_accuracy)[,1],
              se_min = mean_se(participant_block_accuracy)[,2],
              se_max = mean_se(participant_block_accuracy)[,3])

plot_all <- ggplot(data = df, aes(x = block)) +
    # individual participants
    geom_line(aes(y = participant_block_accuracy, color = participant,
                  linetype = order, group = participant_order), alpha = 0.35) +
    geom_point(aes(y = participant_block_accuracy, color = participant,
                   group = participant_order), alpha = 0.35) +

    # means
    geom_line(data = filter(means, order == "DIFF"),
              aes(x = block, y = mean_block_accuracy, linetype = order),
              color = "red3", size = 1.5) +
    geom_errorbar(data = filter(means, order == "DIFF"),
                  aes(x = block, ymin = se_min, ymax = se_max),
                  color = "red3", width = 0.15) +
    geom_point(data = filter(means, order == "DIFF"),
               aes(x = block, y = mean_block_accuracy),
               color = "red3", size = 3) +
    geom_line(data = filter(means, order == "SAME"),
              aes(x = block, y = mean_block_accuracy, linetype = order),
              color = "green3", size = 1.5) +
    geom_errorbar(data = filter(means, order == "SAME"),
                  aes(x = block, ymin = se_min, ymax = se_max),
                  color = "green", width = 0.15) +
    geom_point(data = filter(means, order == "SAME"),
               aes(x = block, y = mean_block_accuracy),
               color = "green3", size = 3) +

    # chance
    geom_hline(aes(yintercept = 0.2), linetype = "dashed", color = "darkgray", size = 1.25) +

    # facet by experiment (_wrap with "free_y" vs. _grid to repeat y-axis labels)
    facet_wrap(~ experiment, labeller = as_labeller(c(`A` = "Experiment A",
                                                      `B` = "Experiment B",
                                                      `C` = "Experiment C")),
               scales = "free_y") +

    # formatting
    scale_x_continuous(limits = c(.9, 5.1), breaks = 1:5) +
    scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.1))) +
    geom_segment(aes(x = 1, xend = 5, y = -Inf, yend = -Inf)) +
    geom_segment(aes(y = 0, yend = 1, x = -Inf, xend = -Inf)) +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    labs(title = "Average Test Accuracy",
         caption = paste0("ExpA n=", length(unique(df$participant[df$experiment == "A"])),
                          "\nExpB n=", length(unique(df$participant[df$experiment == "B"])),
                          "\nExpC n=",length(unique(df$participant[df$experiment == "C"]))),
         x = "Block", y = "Accuracy", color = "Condition") +
    theme(panel.background = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12),
          strip.background = element_blank(),
          axis.ticks.length = unit(0.5, "lines"))
plot_all

#### PLOT DIFFERENCE IN ACCURACY BY ORDER BY CONDITION ####
diff <- df %>%
    group_by(experiment, participant, order) %>%
    mutate(mean_order_accuracy = mean_se(participant_block_accuracy)[,1],
           se_min = mean_se(participant_block_accuracy)[,2],
           se_max = mean_se(participant_block_accuracy)[,3]) %>%
    select(experiment, participant, order, condition, mean_order_accuracy, se_min, se_max)
diff$order <- factor(diff$order, levels = c("DIFF", "SAME"))

# determine whether participants completed SAME or DIFF condition first
first <- count(master, participant, order, date) %>%
    arrange(participant, date) %>%
    mutate(first = "")
for(p in unique(first$participant)) {
    first$first[first$participant == p] <- first$order[match(p, first$participant)]
}
first %<>% group_by(participant) %>%
    summarize(first = unique(first))
diff %<>% left_join(first, by = "participant")

diff_means <- diff %>%
    group_by(experiment, order, first) %>%
    summarize_at(vars(mean_order_accuracy, se_min, se_max), mean)

diff_means_exp <- diff_means %>%
    group_by(experiment, order) %>%
    summarize_at(vars(mean_order_accuracy, se_min, se_max), mean)

plot_diff <- ggplot(data = diff, aes(x = order, y = mean_order_accuracy, color = first)) +
    # individual participants
    geom_line(aes(group = participant), color = "lightgray", alpha = 0.35) +
    geom_point(aes(group = participant), size = 1.5, alpha = 0.35) +

    # means by first
    geom_line(data = diff_means, aes(group = first), size = 1.1) +
    geom_point(data = diff_means, aes(group = first), size = 2.5) +

    # means by experiment
    geom_line(data = diff_means_exp, aes(group = experiment), color = "black",
              size = 1.1) +
    geom_errorbar(data = diff_means_exp, aes(ymin = se_min, ymax = se_max),
                  color = "black", width = 0.15) +
    geom_point(data = diff_means_exp, shape = 21, color = "black", fill = "white",
               size = 4) +

    # chance
    geom_hline(aes(yintercept = 0.2), linetype = "dashed", color = "darkgray", size = 1.25) +

    # facet by experiment (_wrap with "free_y" vs. _grid to repeat y-axis labels)
    facet_wrap(~ experiment, labeller = as_labeller(c(`A` = "Experiment A",
                                                      `B` = "Experiment B",
                                                      `C` = "Experiment C")),
               scales = "free_y") +

    # formatting
    scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.1))) +
    geom_segment(aes(x = 1, xend = 2, y = -Inf, yend = -Inf), color = "black") +
    geom_segment(aes(y = 0, yend = 1, x = -Inf, xend = -Inf), color = "black") +
    scale_color_viridis(discrete = TRUE, name = "Condition\nreceived\nfirst") +
    labs(title = "Average Test Accuracy",
         caption = paste0("ExpA n=", length(unique(diff$participant[diff$experiment == "A"])),
                          "\nExpB n=", length(unique(diff$participant[diff$experiment == "B"])),
                          "\nExpC n=",length(unique(diff$participant[diff$experiment == "C"]))),
         x = "Order of Conditions", y = "Accuracy") +
    theme(panel.background = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12),
          strip.background = element_blank(),
          axis.ticks.length = unit(0.5, "lines"))
plot_diff

#### SAVE PLOTS ####
timestamp <- format(Sys.time(), tz = "EST5EDT", format = "%m-%d-%y_%H%M")
ggsave(file.path("plots", paste0(timestamp,"_all.png")),
       plot = plot_all, width = 8, height = 5)
ggsave(file.path("plots", paste0(timestamp,"_diff.png")),
       plot = plot_diff, width = 8, height = 5)
