current_winners <- function(finals){

    df <- finals %>% filter(year(date) >= 2000) %>%
        select(c("date", "tourney_name", "winner_name"))

    table <- kable(df, row.names = TRUE,
                   caption = "Grandslam Winners since 2000", booktabs = T) %>%
        kable_styling(full_width = T,
                      latex_options = c("striped", "scale_down", "HOLD_position"),
                      font_size = 10)

    table


}