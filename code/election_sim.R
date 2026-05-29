# assign data for whatever race you wanna run sim for to vote_df (avoids function being hard coded)
vote_df <- 
  
# single transferable vote rank choice voting function
rcv_sim <- function(vote_df, precinct_col = "precinct", exclude_precints = NULL,
                    seats = 3) {
  
  # update vote_df to only include precincts NOT excluded 
  if (!is.null(exclude_precints)) {
    vote_df <- vote_df[!vote_df[[precinct_col]] %in% exclude_precints, ]}
  
  candidates     <- setdiff(names(vote_df), precinct_col) # get candidates
  ballot_matrix  <- as.matrix(vote_df[candidates])
# baseline ballot weights 
   ballot_weights <- rep(1.0, nrow(ballot_matrix))
# empty vectors to store winners can eliminated candidates 
  eliminated     <- c()
  winners        <- c()

# function to get top choices 
  get_top_choices <- function(mat, active) {
    mask <- apply(mat, 2, as.numeric) # change columns to numeric 
    colnames(mask) <- colnames(mat)
    inactive <- setdiff(colnames(mask), active) # get candidiates not in active vector 
    # note: Inf is used in place of NA because NA values can cause issues, Inf accomplishes the same thing in this 
    # context without the risks posed by NAs
    if (length(inactive) > 0) mask[, inactive] <- Inf # use Inf to remove candidate from contention
    mask[is.na(mask)] <- Inf # handles unranked candidates on ballots 
    all_inf <- rowSums(is.finite(mask)) == 0 # get exhausted ballots 
    best_col <- max.col(-mask, ties.method = "first") # make "smallest" rank the largest, returns vector of column indecies 
    result <- colnames(mask)[best_col] # converts column indecies into candidate names 
    result[all_inf] <- NA # overwrites exhausted ballots 
    return(result) # returns vector of top results with NAs for exhausted ballots 
  }

# while loop for how long to continue stv process 
  while (length(winners) < seats && length(eliminated) < length(candidates)) {

    active      <- setdiff(candidates, c(eliminated, winners)) # get active candidates 
    top_choices <- get_top_choices(ballot_matrix, active) # get top choices
    

    choice_factor <- factor(top_choices, levels = candidates) # make candidates into factors 
    votes <- tapply(ballot_weights, choice_factor, sum, default = 0) # apply function to factor vector 
    
    # threshold using only ballots with active top choice
    total     <- sum(ballot_weights[!is.na(top_choices)])
    threshold <- floor(total / (seats + 1)) + 1
    
    # save new winners 
    new_winners <- names(votes[votes >= threshold & !names(votes) %in% winners])
    
    if (length(new_winners) > 0) {
      # snapshot weights so simultaneous winners calculate from the same base
      weights_snapshot <- ballot_weights
      
      
      for (winner in new_winners) {
        winner_votes   <- votes[winner] # get winner votes 
        winner_ballots <- which(top_choices == winner) # get winner ballots 
        
        if (winner_votes > threshold) {
          # truncate surplus fraction to 4 decimal places (per city spec)
          surplus_fraction <- trunc((winner_votes - threshold) / winner_votes * 1e4) / 1e4
          ballot_weights[winner_ballots] <- weights_snapshot[winner_ballots] * surplus_fraction
        } else {
          # exact threshold — no surplus, exhaust these ballots
          ballot_weights[winner_ballots] <- 0
        }
      }
      
      winners <- c(winners, new_winners) # combine winners with new winners 
      
    } else {
      # eliminate candidate with fewest weighted votes among still-active
      active_votes <- votes[names(votes) %in% active]
      if (length(active_votes) == 0) break
      eliminated <- c(eliminated, names(active_votes)[which.min(active_votes)])
    }
  }
  
  return(winners[1:min(seats, length(winners))]) # return results 
}
