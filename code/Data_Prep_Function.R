Data_Prep_Function <- function(Lst_matches, Tournament_Inputs = c("G", "M", "F")){

    read_file_func <- function(Lst_matches){

        listchoose <- Lst_matches

        shhcsv <- function(x, ...) {
            sjrd <- purrr::quietly(read_csv)
            df <- sjrd(x, ... )
            df$result
        }
        result <- suppressWarnings(shhcsv(listchoose))
        result <-
            result %>%
            mutate(across(.cols = -c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.numeric(.))) %>%
            mutate(across(.cols = c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.character(.)))
    }

    result <- Lst_matches %>% map_df(~read_file_func(.))



    result <-
        result %>% mutate(date = ymd(tourney_date)) %>%
        select(date, everything())


    result_Large_events <-
        result %>% mutate(date = ymd(tourney_date)) %>% select(date, everything()) %>%
        filter(tourney_level %in% Tournament_Inputs)

    result_Large_events

}