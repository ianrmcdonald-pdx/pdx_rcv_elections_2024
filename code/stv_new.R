stv_new <- function (x, seats = 1, file = "", surplusMethod = "Cambridge", 
          quotaMethod = "Droop", quotaFloor = TRUE, quotaFixed = FALSE) 
{
  if (is.null(surplusMethod) | is.null(quotaMethod) | is.null(quotaFloor)) 
    stop("surplusMethod, quotaMethod, and quotaFloor must all be specified.")
  if (c(!surplusMethod %in% c("Cambridge", "Fractional") | 
        length(surplusMethod) != 1)[1]) 
    stop("Please set surplusMethod = 'Cambridge' or 'Fractional'. These are currently the only supported methods.")
  if (c(!quotaMethod %in% c("Droop", "Hare") | length(quotaMethod) != 
        1)[1]) 
    stop("Please set quotaMethod = 'Droop' or 'Hare'. These are currently the only supported methods.")
  if (c(!quotaFloor %in% c(TRUE, FALSE) | length(quotaFloor) != 
        1)[1]) 
    stop("Please set quotaFloor = TRUE or FALSE.")
  junk <- validateBallots(x)
  if (seats > ncol(x)) 
    stop("Number of seats must be less than or equal to the number of candidates.")
  elim <- c()
  elect <- c()
  if (surplusMethod == "Cambridge") {
    included.ballots <- rep(TRUE, nrow(x))
  }
  if (surplusMethod == "Fractional") {
    ballot.weights <- rep(1, nrow(x))
  }
  unfilled <- seats
  Nround <- 0
  res <- data.frame(matrix(NA, ncol = 9 + ncol(x), nrow = 0))
  names(res) <- c("ballots", "seats.to.fill", "quota", "max.vote.count", 
                  "min.vote.count", "elim.cand", "tied.for.elim", "elect.cand", 
                  "surplus", names(x))
  while (unfilled > 0) {
    Nround <- Nround + 1
    res[Nround, ] <- rep(NA, ncol(res))
    res$seats.to.fill[Nround] <- unfilled
    curr.candidates <- setdiff(names(x), c(elim, elect))
    if (unfilled == length(curr.candidates)) {
      elect <- c(elect, curr.candidates)
      res$elect.cand[Nround] <- paste(curr.candidates, 
                                      collapse = "; ")
      res$seats.to.fill[Nround] <- 0
      res[Nround, elect] <- "Elected"
      res[Nround, elim] <- "Eliminated"
      res$quota[Nround] <- "Won by elim"
      if (file != "") {
        if (substr(file, nchar(file) - 3, nchar(file)) != 
            ".csv") 
          warning("File name provided does not include .csv extention")
        write.table(res, file = file, sep = ",", row.names = FALSE)
      }
      return(list(elected = elect, details = res))
    }
    if (surplusMethod == "Cambridge") {
      curr.ballots <- (rowSums(!is.na(x[, curr.candidates])) > 
                         0) & included.ballots
      ballot.size <- sum(curr.ballots)
    }
    if (surplusMethod == "Fractional") {
      curr.ballots <- rowSums(!is.na(x[, curr.candidates])) > 
        0
      ballot.size <- floor(sum(curr.ballots * ballot.weights))
    }
    res$ballots[Nround] <- ballot.size
    if (quotaFloor) {
      if (quotaMethod == "Droop") 
        quota <- floor(ballot.size/(unfilled + 1)) + 
          1
      if (quotaMethod == "Hare") 
        quota <- floor(ballot.size/unfilled)
    
    } else {
      if (quotaMethod == "Droop") 
        quota <- ballot.size/(unfilled + 1) + 1
      if (quotaMethod == "Hare") 
        quota <- ballot.size/unfilled
    }
    
    
    res$quota[Nround] <- round(quota, 2)
    
    #if(Nround > 1 & quotaFixed == TRUE) {
      res$quota[Nround] <- 21129 #res$quota[1]
    #}
    
    top.choice <- apply(x[, curr.candidates], 1, function(i.row) names(x[, 
                                                                         curr.candidates])[which.min(i.row)])
    top.choice[!curr.ballots] <- NA
    vote.counts <- table(factor(top.choice, levels = curr.candidates))
    if (surplusMethod == "Fractional") {
      for (i in 1:length(vote.counts)) {
        weighted.votes <- (names(vote.counts[i]) == top.choice) * 
          ballot.weights
        vote.counts[i] <- sum(weighted.votes, na.rm = TRUE)
      }
    }
    res[Nround, names(vote.counts)] <- round(vote.counts, 
                                             2)
    res$max.vote.count[Nround] <- round(max(vote.counts), 
                                        2)
    res$min.vote.count[Nround] <- round(min(vote.counts), 
                                        2)
    res[Nround, elect] <- "Elected"
    res[Nround, elim] <- "Eliminated"
    if (any(vote.counts >= quota)) {
      curr.elected <- vote.counts[vote.counts >= quota]
      if (length(curr.elected) > unfilled) {
        warning(paste0("In round ", Nround, ", more candidates met the quota than can be elected to open seats. Ties were broken randomly."))
        curr.elected <- sample(curr.elected, unfilled)
      }
      elect <- c(elect, names(curr.elected))
      unfilled <- seats - length(elect)
      res$elect.cand[Nround] <- paste(names(curr.elected), 
                                      collapse = "; ")
      res$surplus[Nround] <- paste(round(curr.elected - 
                                           quota, 2), collapse = "; ")
      for (i in names(curr.elected)) {
        cand.ballots <- which(top.choice == i)
        if (surplusMethod == "Cambridge") {
          included.ballots[sample(cand.ballots, quota)] <- FALSE
        }
        if (surplusMethod == "Fractional") {
          ballot.weights[cand.ballots] <- ballot.weights[cand.ballots] * 
            (vote.counts[i] - quota)/vote.counts[i]
        }
      }
    }
    else {
      curr.elim <- names(which(vote.counts == min(vote.counts)))
      if (length(curr.elim) > 1) 
        res$tied.for.elim[Nround] <- paste("Yes: ", length(curr.elim), 
                                           sep = "")
      elim <- c(elim, sample(curr.elim, 1))
      res$elim.cand[Nround] <- tail(elim, 1)
    }
  }
  if (file != "") {
    if (substr(file, nchar(file) - 3, nchar(file)) != ".csv") 
      warning("File name provided does not include .csv extention")
    write.table(res, file = file, sep = ",", row.names = FALSE)
  }
  return(list(elected = elect, details = res))
}
