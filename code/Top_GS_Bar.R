top_GS <- function(finals){

    GS <- finals %>% filter(tourney_level == "G") %>% group_by(winner_name) %>%
        summarize(winner_count = n()) %>%
        top_n(10, winner_count) %>%
        arrange(desc(winner_count))

    g <- GS %>% ggplot() +
        geom_bar(aes(x = winner_name, y = winner_count, fill = winner_name), stat = "identity") +
        theme_bw() +
        labs(title = "Top 10 Grand Slam Winners", x = "Player", y = "Number of Grand Slams") +
        theme(legend.position = "none") +
        scale_fill_brewer(palette = "Set3") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1))
    g



}