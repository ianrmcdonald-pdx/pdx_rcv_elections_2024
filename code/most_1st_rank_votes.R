d1_first <- colSums(council_d1 == 1, na.rm = TRUE) 

d2_first <- colSums(council_d2 == 1, na.rm = TRUE)

d3_first <- colSums(council_d3 == 1, na.rm = TRUE)

d4_first <- colSums(council_d4 == 1, na.rm = TRUE)

mayor_first <- colSums(mayor == 1, na.rm = TRUE)

d1_win_nrcv <- sort(d1_first, decreasing = TRUE) |>
  head(3)

d2_win_nrcv <- sort(d2_first, decreasing = TRUE) |> 
  head(3)

d3_win_nrcv <- sort(d3_first, decreasing = TRUE) |>
  head(3)

d4_win_nrcv <- sort(d4_first, decreasing = TRUE) |>
  head(3)

mayor_nrcv <- sort(mayor_first, decreasing = TRUE) |>
  head(1)
