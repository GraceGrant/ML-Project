model_prep <- function(result_Large_events){

    df <- result_Large_events %>% filter(year(date) >= 2003) %>% select(-winner_rank_points, -loser_rank_points)
    df


}