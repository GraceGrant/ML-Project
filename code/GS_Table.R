current_winners_GS <- function(finals){

    df <- finals %>% filter(year(date) >= 2000) %>% filter(tourney_level == "G") %>%
        select(c("date", "tourney_name", "winner_name"))

    df$year <- year(as.Date(df$date))

    pivot_table <- df %>% group_by (year) %>% pivot_wider(
        names_from = tourney_name,
        values_from = winner_name
    )

    pivot_table$date <- NULL

    pivot_table <- pivot_table[, -ncol(pivot_table)]
    pivot_table$year <- as.character(pivot_table$year)

    filterW <- pivot_table %>% filter(!is.na(Wimbledon)) %>% select(c(Wimbledon))

    filterR <- pivot_table %>% filter(!is.na(`Roland Garros`)) %>% select(c(`Roland Garros`))

    filterUS <- pivot_table %>% filter(!is.na(`US Open`)) %>% select(c(`US Open`))

    filterA <- pivot_table %>% filter(!is.na(`Australian Open`)) %>% select(c(`Australian Open`))

    merged_df <- merge(merge(merge(filterA, filterR, by = "year"), filterW, by = "year"), filterUS, by = "year")

    table <- kable(merged_df, row.names = TRUE,
                   caption = "Grandslam Winners since 2000", booktabs = T) %>%
        kable_styling(full_width = T,
                      latex_options = c("striped", "scale_down", "HOLD_position"),
                      font_size = 10)

    table


}