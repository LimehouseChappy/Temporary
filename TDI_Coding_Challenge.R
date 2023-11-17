# PROGRAM: TDI_Test_Code.R
# AUTHOR: Jamie Daubenspeck 
################################################################################

library(tidyverse)
rm(list = ls())

# read in playlists and store in data frame
p1s1_path <- "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/playlist_1_station_1.txt"
p1s2_path <- "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/playlist_1_station_2.txt"
p2s1_path <- "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/playlist_2_station_1.txt"
p2s2_path <- "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/playlist_2_station_2.txt"

intake_playlist <- function(path) {
  # creates a vector of length(duration playlist) that has the enjoyment
  # for each minute the song is playing
      
  df <- read.table(path, header = F)
  colnames(df) <- c("d", "e")
  df$song_id <- 1:nrow(df)
  
  df_play <- data.frame("song_id" = rep(df$song_id, df$d))
  df_play <- left_join(df_play, df, by = c("song_id"))
  
  e_vec <- df_play$e
  
  return(e_vec)
}

e1 <- intake_playlist(p1s1_path)
e2 <- intake_playlist(p1s2_path)

#### QUESTION 1: SWITCHING STRATEGY #####

# given a vector of 0, 1 the same length in duration the playlists, calculate total enjoyment from listening

e_mat <- cbind(e1, e2)
total_mins <- nrow(e_mat)

write.csv(e_mat, "C:/Users/jamie/OneDrive/Documents/Data Science/DataIncubator/q1_matrix.csv")
print("Q1. Utility 88") # calculated in excel

# given a vector of switching and listening utility matrix, calculate total utility
calc_utility <- function(vec, e_mat) {
  
  # count number of switches in vec (count switching the station as a number of changes)
  num_changes <- sum(abs(diff(vec))) + abs(vec[1] - 1)
  switch_penalty = 3
  
  # sum total utility
  total_u <- sum(e_mat[cbind(1:length(vec), vec)])
  
  # penalize for switching
  net_u <- total_u - num_changes*switch_penalty
  
  return(net_u)
}

# function to calculate optimal path
calc_optimal_path_payoff <- function(e_mat) {
  
  total_mins = nrow(e_mat)
  i = total_mins
  
  p2_payoff <- numeric(total_mins)
  p1_payoff <- numeric(total_mins)
  
  p2_payoff[i] <- max(e_mat[i,2], e_mat[i,1] - 3)
  p1_payoff[i] <- max(e_mat[i,1], e_mat[i,2] - 3)
  
  for (i in (nrow(e_mat) - 1):1) {
    p2_payoff[i] <- max(e_mat[i,2] + p2_payoff[i+1], e_mat[i,1] - 3 + p1_payoff[i+1])
    p1_payoff[i] <- max(e_mat[i,1] + p1_payoff[i+1], e_mat[i,2] - 3 + p2_payoff[i+1])
  }
  
  df <- data.frame(p1_payoff = p1_payoff, p2_payoff = p2_payoff)
  
  return(df)
}

df_opt <- calc_optimal_path_payoff(e_mat)
max_col <- apply(df_opt, MARGIN=1, FUN=function(x) which.max(x))
u_check <- calc_utility(max_col, e_mat)

##### Question 2: Max Overall Utility #####
print(paste("Q2. Max Utility ", max(df_opt)))

# calculate all possible listening paths
df_vec <- expand.grid(rep(list(c(1,2)), total_mins))

##### QUestions 3 Average Utility #####
all_u <- apply(df_vec, MARGIN = 1, FUN = calc_utility, e_mat = e_mat)

max_u = max(all_u)
avg_u = mean(all_u)

print(paste("Q2. Max utility (check):", max_u))
print(paste("Q3. Avg utility:", avg_u))

##### Questions 4: Max Utility on bigger dataframe #####

e1_big <- intake_playlist(p2s1_path)
e2_big <- intake_playlist(p2s2_path)
e_mat_big <- cbind(e1_big, e2_big)

df_opt_big <- calc_optimal_path_payoff(e_mat_big)

# check accuracy
max_col_big <- apply(df_opt_big, MARGIN=1, FUN=function(x) which.max(x))

print(paste("Q4. Max utility", max(df_opt_big)))

##### Questions 5,6: Use MonteCarlo  to calc Avg and SD ######
total_mins <- length(e1_big)

num_sims = 10^4*10^7

sum_val <- 0
sum_sq <- 0
max_val <- 0
n <- 0

new_avg <- 0
new_var <- 0
new_sd <- 0

cat("|")
for (i in 1:num_sims) {
  vec <- sample(c(1, 2), total_mins, replace = TRUE)
  u <- calc_utility(vec, e_mat_big)
  
  # update the accumulated values
  sum_val <- sum_val + u
  sum_sq <- sum_sq + u^2
  max_val <- max(max_val, u)
  n <- n + 1
  
  # every 10000 iterations, csee if average is converging
  if(n %% 10000 == 0) {
    cat("-")
    old_avg <- new_avg
    old_var <- new_var
    old_sd <- new_sd
    
    new_avg <- sum_val / n
    new_var <- (sum_sq - (sum_val^2)/n)/(n-1) 
    new_sd <- sqrt(new_var)
    
    if(abs(new_avg - old_avg) < .001 & abs(new_sd - old_sd) < .00001) {
      print("\n")
      print("Average has converged")
      break
    }
    
  }
}
cat("| Monte Carlo Complete \n")

avg_mc <- sum_val / n
var_mc <- (sum_sq - (sum_val^2)/n)/(n-1)  # variance
sd_mc <- sqrt(var_mc)  # standard deviation

print(paste("Q5. Average utility:", avg_mc))
print(paste("Q6. SD Utility:", sd_mc))
