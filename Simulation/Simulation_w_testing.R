# Simulation of oracle testing process ------------------------------------

rm(list = ls()) # Clean environment

N <- 100 # Time points - assume 1 year has 1*e^5 data pulls
# Using 100 for now due to computational limitations
N_feeds <- 60 # Number of feed providers (can simulate random number of feeds if needed
# but for this experiment we will be monitoring the same feeds over time to look at annual return)
deposits <- rnorm(N_feeds, mean = 10000, sd = 1000) # Simulating initial deposits for each feed provider
TVL <- 0 # Starting total value locked
feed_spread <- 5 # Standard deviation of feed inputs
results <- list() # Final results will be stored in a list
library(utils)
pb <- txtProgressBar(min = 0, max = N, initial = 0) 
for(j in 1:N){
  
  # Number of smart contracts pulling data
  N_smart <- rbinom(1,1000,0.6)
  
  # Fees pool for iteration j
  sc_fees <- 30*N_smart
  
  # Record if oracle tests ring alarm
  oracle_test <-c(rep(0,60)) 
  
  # Initial deposit
  Initial_deposit <- deposits
  
  # New inputs
  feed_input <- rnorm(n = N_feeds, mean = 1500, sd = feed_spread)
  med <- median(feed_input)
  med_abd <- mad(feed_input)
  
  # Test 1 - MAD
  # MAD of inputs
  for(i in 1:N_feeds){
    if(feed_input[i] > med + med_abd*3 | feed_input[i] < med - med_abd*3){
      oracle_test[i] <- 1
    } else {
      oracle_test[i] <- oracle_test[i]
    }
  }
  
  # Test 2 - coeff of var
  c_var_MAD <- 100*mad(feed_input)/median(feed_input)
  c_var_SD <- 100*sd(feed_input)/mean(feed_input)
  
  for(i in 1:N_feeds){
    if(c_var_MAD > 0.5 | c_var_SD > 0.5){
      oracle_test[i] <- 1 + oracle_test[i]
    } else {
      oracle_test[i] <- oracle_test[i]
    }
  }
  
  if(sum(oracle_test) == 2*N_feeds){
    Reward == c(rep(0,N_feeds))
    for(i in 1:N_feeds){
      Penalty[i] == 0.3*deposits[i]
    }
  } else {
    Reward <- c(rep(1,N_feeds))
    Penalty <- c(rep(0,N_feeds))
  }
  
  if(sum(oracle_test) == 2*N_feeds){
    break 
  } else {
    
    # Test 3 - Deposit test
    if(j > 1){
      for(i in 1:N_feeds){
        if(feed_input[i] < min_dep){
          Penalty[i] <- 0.1*deposits[i]
        } else {
          Penalty[i] <- Penalty[i]
        }
      }
    }

    # Test 4 - per of 0s
    # Bootstrap MAD and apply tests
    bstraps <- c(rep(NA,10000))
    for(i in 1:10000){
      samp <- sample(1:N_feeds,floor(0.4*N_feeds), replace = TRUE)
      bsample <- feed_input[samp]
      bstraps[i] <- mad(bsample)
    }
    
    min_count <- 100*sum(ifelse(bstraps == 0,1,0))/10000
    
    for(i in 1:N_feeds){
      if(min_count <= 10){
        oracle_test[i] <- oracle_test[i] + 1
      } else {
        oracle_test[i] <- oracle_test[i]
      }
    }
    
    # Test 5 - Standardised scores
    std_scores <- c(rep(NA,N_feeds))
    for(i in 1:N_feeds){
      std_scores[i] <- (feed_input[i] - mean(feed_input))/sd(feed_input)
      if(std_scores[i] > 5){
        oracle_test[i] <- oracle_test[i] + 1
      } else {
        oracle_test[i] <- oracle_test[i]
      }
    }
    
    for(i in 1:N_feeds){
      # Penalty
      if(oracle_test[i] > 3){
        Penalty[i] <- Penalty[i] + ifelse(Penalty[i] == 0,0.3*deposits[i],0)
        Reward[i] <- 0
      } else {
        Penalty[i] <- Penalty[i]
        Reward[i] <- Reward[i]
      }
    }
    
    # Total penalty pool
    pen_pool <- sum(Penalty)
    
    # Total revenue
    revenue <- sum(pen_pool,sc_fees)
    
    for(i in 1:N_feeds){
      # Reward
      if(sum(Reward) == 0){
        Reward[i] <- 0
      } else {
        Reward[i] <- revenue/length(Reward[Reward > 0])
      }
    }
    
    # Adjust deposit
    deposits <- deposits + Reward - Penalty
    
    TVL <- max(deposits) + revenue
    min_dep <- TVL/N_feeds
    
    # Names 
    Names <- paste0("Provider"," ",seq(1,N_feeds,1))
    Rounds <- paste0("Round",j)
    
    # Store results
    results[[j]] <- data.frame("Providers" = Names,"Rounds" = Rounds,"Feed input" = feed_input, "Median input" = rep(med,N_feeds)
                               ,"MAD" = rep(med_abd,N_feeds),"TVL" = rep(TVL,N_feeds),"min_dep" = rep(min_dep,N_feeds) ,"Revenue" = rep(revenue,N_feeds)
                               , "Initial deposit" = Initial_deposit, "std_scores" = std_scores
                               ,"Final Deposit"= deposits,"Reward" = Reward,"Penalty" = Penalty,"Coeff var MAD" = rep(c_var_MAD,N_feeds)
                               ,"Coeff var SD" = rep(c_var_SD,N_feeds),"test" = oracle_test)
    oracle_data <- do.call(rbind.data.frame, results)
    setTxtProgressBar(pb,j)
  }
}
close(pb)

ar <- (oracle_data$Final.Deposit[oracle_data$Rounds == paste0("Round",j-1)] - oracle_data$Initial.deposit[oracle_data$Rounds == paste0("Round",1)])/oracle_data$Initial.deposit[oracle_data$Rounds == paste0("Round",1)]
ar

rm(list= ls()[!(ls() %in% c('oracle_data','ar'))])

  
