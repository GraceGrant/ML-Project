model_prep_attempt <- function(result_Large_events){

    filter <- result_Large_events %>% filter(lubridate::year(date) >= 2003) %>%
        select(-winner_rank_points, -loser_rank_points, -winner_hand, -loser_hand, -winner_entry, -loser_entry,
               -winner_seed, -loser_seed, -tourney_date) %>% filter(!is.na(minutes)) %>% na.omit()

    df_50 <- filter %>%
        count(winner_name) %>%
        top_n(50, n) %>%
        select(winner_name)

    filter <- filter %>% filter(winner_name %in% df_50$winner_name)


    set.seed(123)

    # create a random binary variable
    df <- filter %>%
        mutate(random_bin = rbinom(nrow(filter), 1, 0.5))

    # assign winner and loser to player 1 and player 2 based on the random binary variable
    df <- df %>%
        mutate(
            Player1 = ifelse(random_bin == 1, winner_id, loser_id),
            Player1_Rank = ifelse(random_bin == 1, winner_rank, loser_rank),
            Player2 = ifelse(random_bin == 1, loser_id, winner_id),
            Player2_Rank = ifelse(random_bin == 1, loser_rank, winner_rank),
            Player1_ht = ifelse(random_bin == 1, winner_ht, loser_ht),
            Player2_ht = ifelse(random_bin == 1, loser_ht, winner_ht),
            Player1_age = ifelse(random_bin == 1, winner_age, loser_age),
            Player2_age = ifelse(random_bin == 1, loser_age, winner_age),
            Player1_1stin = ifelse(random_bin == 1, w_1stIn, l_1stIn),
            Player2_1stin = ifelse(random_bin == 1, l_1stIn, w_1stIn),
            Player1_ace = ifelse(random_bin == 1, w_ace, l_ace),
            Player2_ace = ifelse(random_bin == 1, l_ace, w_ace),
            Player1_bpSaved = ifelse(random_bin == 1, w_bpSaved, l_bpSaved),
            Player2_bpSaved = ifelse(random_bin == 1, l_bpSaved, w_bpSaved),
            Player1_bpFaced = ifelse(random_bin == 1, w_bpFaced, l_bpFaced),
            Player2_bpFaced = ifelse(random_bin == 1, l_bpFaced, w_bpFaced),
            Match_Winner = ifelse(random_bin == 1, 1, 0)
        )

    df <- df %>% select(-date, -draw_size, -tourney_level, -match_num, -winner_ioc, -loser_ioc,
                        -score, -best_of, -round, -w_df, -l_df, -w_svpt, -l_svpt, -w_1stWon, -l_1stWon, -w_2ndWon, -l_2ndWon,
                        -w_SvGms, -l_SvGms)

    df



}