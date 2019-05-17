summarize_test <- function(by = "test", p) {
    test %>% filter(participant == p) %>%
        group_by_at(vars(c(by))) %>%
        summarize(mean_rt = mean(rt, na.rm = TRUE),
                  se = sd(rt, na.rm = TRUE)/sqrt(n()),
                  n = n())
}

for (z in unique(test$participant)) {
    print(z)
    test12 <- summarize_test(by = c("test", "carrier", "trained_t"), p = z)
    p12a <- ggplot(data = test12,
                   aes(x = test, y = mean_rt, alpha = trained_t,
                       group = interaction(carrier, trained_t))) +
        geom_line(aes(linetype = carrier), size = 1, color = "gray35") +
        geom_errorbar(aes(ymin = mean_rt - se, ymax = mean_rt + se), width = 0.1) +
        geom_point(size = 3.5) +
        facet_wrap(~ trained_t, strip.position = "bottom") +
        labs(title = z,
             y = "Mean RT (ms)") +
        scales_tfta +
        scale_y_continuous(limits = c(450, 1050), breaks = seq.int(450, 1050, 100)) +
        theme_tfta_facets
    p12a
    ggsave(file.path("plots-test", "individual", paste0(z,".png")), height = 4, width = 6.5)
}
