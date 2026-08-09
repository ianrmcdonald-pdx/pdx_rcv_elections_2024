library(purrr)

mm <- list(a = list(1:2), b = list(3:4), d = list(5:6))
nn <- list(a = list(7:8), b = list(9:10), d = list(11:12))
kk <- list(mm, nn)

kk1 <- kk |> purrr::map(~c(pluck(.,2),pluck(.,3))) 
str(kk1)
#> List of 2
#>  $ :List of 2
#>   ..$ : int [1:2] 3 4
#>   ..$ : int [1:2] 5 6
#>  $ :List of 2
#>   ..$ : int [1:2] 9 10
#>   ..$ : int [1:2] 11 12
#Created on 2021-09-15 by the reprex package (v2.0.0)

dlistx <- list(dlist0, dlist1, dlist2, dlist3, dlist4, dlist5)


library(purrr)

del <- mtcars |> 
  split(mtcars$cyl) |>  # from base R
  map(\(df) lm(mpg ~ wt, data = df)) |> 
  map(summary) |>
  map_dbl("r.squared")
