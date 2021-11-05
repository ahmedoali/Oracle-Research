
# Oracle optimisation -----------------------------------------------------


# Simulation --------------------------------------------------------------



#
# A very naive simulation of an oracle procedure. After
# each iteration the deposits of each feed provider updates
# based on their reward and penalty. Each feed provider is assigned
# a weight based on their proximity to the median result.
#

rm(list = ls())

N = 5 # Number of inputs for each exchange
N_feeds <- 10 # Number of feed providers, can simulate random number of feeds if needed
deposits <- runif(N_feeds,1000,30000) # Simulating initial deposits for each feed provider
results <- list() # Final results will be stored in a list
for(j in 1:N){
  
  # New inputs
  feed_input <- runif(n = N_feeds, min = 1000, max = 1500)
  
  # Initial deposit
  Initial_deposit <- deposits
  
  # Weighting
  med <- median(feed_input)
  med_abd <- mad(feed_input)
  weight <- (med - feed_input)^2
  weight_scaled <- 1 - (weight - min(weight))/(max(weight) - min(weight))
  
  for(i in 1:N_feeds){
    TVL <- sum(deposits)
    min_deposit <- 0.05*TVL
  
  # Punish low deposits
  if(deposits[i] < min_deposit){
    deposits[i] = deposits[i] - log(TVL)
  }
  
  # Penalty
  Penalty <- (1-weight_scaled)*0.01*deposits
  
  # Reward
  Reward <- weight_scaled*0.1*deposits
  
  # Adjust deposit
  deposits[i] <- deposits[i] + Reward[i] - Penalty[i]
  
  # Names 
  Names <- paste0("Provider"," ",seq(1,N_feeds,1))
  
  }
  # Store results
  results[[j]] <- data.frame("Providers" = Names,"Feed input" = feed_input, "Median input" = rep(med,N_feeds)
                             ,"MAD" = rep(med_abd,N_feeds)
                             ,"Min Deposit" = rep(min_deposit,N_feeds), "Initial_deposit" = Initial_deposit
                             ,"Final Deposit"= deposits
                             ,"Weight"=weight_scaled,"Reward" = Reward,"Penalty" = Penalty)
}

results
