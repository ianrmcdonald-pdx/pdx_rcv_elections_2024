

x <- dlist[[30]]  #chad lykins eliminated

cl_eliminated <- x$cvr |> 
  filter(rank_1 == "Chad Lykins") |> 
  count(rank_2)

