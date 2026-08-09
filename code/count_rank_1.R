library(tidyverse)

count_rank_1 <- function(table_name) {
  
  if(!("rank_1" %in% colnames(table_name))) 
  {
    return("No column named rank_1")
  }
  
  table_name |> 
    filter(!str_detect(rank_1, "\n")) |>  #eliminate overvotes
    filter(!str_detect(rank_1, "undervote")) |>  #eliminate undervotes
    
    group_by(rank_1) |> #group by candidate name
    count(sort = TRUE) |> #count and sort
    ungroup() 
  
}


count_rank_1(mayor_2024)
count_rank_1(council_d1_2024)

council_d1_2024 |> 
  filter(precinct == 1) |> 
  count_rank_1()
