percentage_bar <- function(Result){

    df <- Result %>% filter(grepl("Top 5", Type)) %>%
        select(Player, Win_Percentage, Aces_Perc, BreakPoints_Saved)

    df_long <- pivot_longer(df, cols = c(Win_Percentage, Aces_Perc, BreakPoints_Saved), names_to = "Variable", values_to = "Value")

    # Create a grouped bar graph
    graph <- df_long %>% ggplot() +
        geom_bar(aes(x = Player, y = Value, fill = Variable), stat = "identity", position = position_dodge()) +
        labs(title = "Big 3 Statistics Against Top 5 Players", y = "Percentage", x = NULL, fill = "Statistic") +
        scale_fill_brewer(palette = "Set2") +
        theme_bw()
    graph



}