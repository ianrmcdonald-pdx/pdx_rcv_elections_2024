install.packages("vote")


library(tidyverse)
library(vote)


precinct_reference <- read_csv("data/Precinct_ID_Reference.csv",
                               show_col_types = FALSE) |> 
                      mutate(id_number = str_trim(id_number), 
                             precinct = str_trim(precinct))


read_pdx_file <- function(pdx_csv_file) {
  
    read_csv(pdx_csv_file, show_col_types = FALSE) |> 
      select(recordID = "Record Id", precinct = Precinct, 
           rank_1 = "Rank 1",
           rank_2 = "Rank 2",
           rank_3 = "Rank 3",
           rank_4 = "Rank 4",
           rank_5 = "Rank 5",
           rank_6 = "Rank 6")
}

mayor_2024 <- read_pdx_file("data/pdx_2024_mayor.csv")
council_d1_2024 <- read_pdx_file("data/pdx_2024_council_d1.csv")
council_d2_2024 <- read_pdx_file("data/pdx_2024_council_d2.csv")
council_d3_2024 <- read_pdx_file("data/pdx_2024_council_d3.csv")
council_d4_2024 <- read_pdx_file("data/pdx_2024_council_d4.csv")


compute_stv <- function(office_inp, seats_num = 1, quota_const = TRUE) {
 
   office <- office_inp |> 
    pivot_longer(cols = !c(recordID, precinct), 
               names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    filter(choice != "undervote") |> 
    filter(!str_detect(choice, "\n")) |> 
    group_by(recordID, precinct, choice) |> 
    slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    group_by(recordID, precinct) |> 
    mutate(choice_num = rank(choice_num)) |> 
    ungroup()


  office <- office |> 
    pivot_wider(id_cols = c("recordID", "precinct"),
              names_from = choice,
              values_from = choice_num) 

  office |> 
    select(!c(recordID, precinct)) |> 
    stv(nseats=seats_num, constant.quota = quota_const)

}

mayor_stv <- compute_stv(mayor_2024, quota_const = FALSE)
summary_mayor_stv <- summary(mayor_stv)

d1_stv <- compute_stv(council_d1_2024, seats_num = 3)
summary_d1_stv <- summary(d1_stv)

d2_stv <- compute_stv(council_d2_2024, seats_num = 3)
summary_d2_stv <- summary(d1_stv)

d3_stv <- compute_stv(council_d3_2024, seats_num = 3)
summary_d3_stv <- summary(d1_stv)

d4_stv <- compute_stv(council_d4_2024, seats_num = 3)
summary_d4_stv <- summary(d4_stv)



        



         










