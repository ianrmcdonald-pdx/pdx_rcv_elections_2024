library(devtools)
install_github('jayemerson/STV')

library(STV)

load("~/Library/Mobile Documents/com~apple~CloudDocs/PSU Data Science Spring 2026/260511/process_cvr_objects.RData")

rm(list=setdiff(ls(), "council_d4_2024"))

office_inp <- council_d4_2024
eliminated <- ""

compute_stv_wide <- function(office_inp, eliminated = "") {
  
  office <- office_inp |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    filter(choice != "undervote" & 
             !str_detect(choice, "\n") & 
             choice != eliminated) |> 
    
    group_by(recordID, precinct, choice) |> 
      slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
      mutate(choice_num = rank(choice_num)) |> 
    ungroup() 
  
  
  office |> 
    pivot_wider(id_cols = c(recordID, precinct),
                names_from = choice,
                values_from = choice_num) 
  
}

x <- compute_stv_wide(council_d4_2024) 
x <- x |> 
  select(!c(recordID, precinct))
x1 <- cleanBallots(as.data.frame(x))



y <- STV::stv(x1, seats = 3, surplusMethod = "Fractional", quotaFloor = TRUE)
z <- vote::stv(x1, nseats = 3, constant.quota = TRUE)

