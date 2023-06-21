Ranking_Prep_Function <- function(Lst_rankings){

    read_file_func <- function(Lst_rankings){
        # listchoose <- Lst[[10]]
        listchoose <- Lst_rankings

        shhcsv <- function(x, ...) {
            sjrd <- purrr::quietly(read_csv)
            df <- sjrd(x, ... )
            df$result
        }
        result <- suppressWarnings(shhcsv(listchoose))
        result <-
            result %>%
            mutate(across(.cols = c(contains("rank"), contains("player"), contains("points")),
                          .fns = ~as.numeric(.)))
    }

    result <- Lst_rankings %>% map_df(~read_file_func(.))

    # Tons of data, might require some filtering.



    # result$tourney_date %>% unique

    result <-
        result %>% mutate(date = ymd(ranking_date)) %>%
        select(date, rank, player) %>% filter(rank < 21, month(date) == 12)
    result

}