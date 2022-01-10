

oracle_simulation <- function(N_feeds,deposit_average,deposit_spread,
                              feed_average,feed_spread,acc_MAD,
                              sc_fee){
  N <- 100
  deposits <- rnorm(N_feeds, mean = deposit_average, sd = deposit_spread) # Simulating initial deposits for each feed provider
  TVL <- 0 # Starting total value locked
  min_dep <- 0 # Initialise minimum deposit
  results <- list() # Final results will be stored in a list
  
  for(j in 1:N){
    
    # Number of smart contracts pulling data
    N_smart <- rbinom(1,1000,0.6)
    
    # Fees pool for iteration j
    sc_fees <- sc_fee*N_smart
    
    # Record if oracle tests ring alarm
    oracle_test <- c(rep(0,N_feeds)) 
    
    # Initial deposit
    Initial_deposit <- deposits
    
    # Test 1 - Deposit test
    deposit_test <- c(rep(0,N_feeds))
    if(j > 1){
      for(i in 1:N_feeds){
        if(feed_input[i] < min_dep){
          deposit_test[i] <- 1
        } else {
          deposit_test[i] <- 0
        }
      }
    }
    
    # New inputs
    feed_input <- rnorm(n = N_feeds, mean = feed_average, sd = feed_spread)
    med <- median(feed_input)
    med_abd <- mad(feed_input)
    
    # Test 2 - MAD
    # MAD of inputs
    for(i in 1:N_feeds){
      if(med_abd >= acc_MAD){
        MAD_test <- 1
      } else {
        MAD_test <- 0
      }
    }
    
    # Test 3 - coeff of var
    c_var_MAD <- 100*mad(feed_input)/median(feed_input)
    c_var_SD <- 100*sd(feed_input)/mean(feed_input)
    
    for(i in 1:N_feeds){
      if(c_var_MAD > 1 | c_var_SD > 1){
        var_test <- 1
      } else {
        var_test <- 0
      }
    }
    
    # Test 4 - Standardised scores
    std_scores <- c(rep(NA,N_feeds))
    std_test <- c(rep(NA,N_feeds))
    for(i in 1:N_feeds){
      std_scores[i] <- (feed_input[i] - mean(feed_input))/sd(feed_input)
      if(abs(std_scores[i]) > 5){
        std_test[i] <- 1
      } else {
        std_test[i] <- 0
      }
    }
    
    # Total penalty pool
    oracle_test <- deposit_test + c(rep(MAD_test,N_feeds)) 
    + c(rep(var_test,N_feeds)) + std_test
    Penalty <- c(rep(NA,N_feeds))
    for(i in 1:N_feeds){
      if(oracle_test[i] < 3){
        Penalty[i] = 0
      } else {
        Penalty[i] = 0.3*deposits[i]
      }
    }
    pen_pool <- sum(Penalty)
    
    # Total revenue
    revenue <- sum(pen_pool,sc_fees)
    Reward <- c(rep(NA,N_feeds))
    for(i in 1:N_feeds){
      # Reward
      if(Penalty[i] > 0){
        Reward[i] <- 0
      } else {
        Reward[i] <- (0.1*revenue)/length(Penalty[Penalty == 0])
      }
    }
    
    # Adjust deposit
    deposits <- deposits + Reward - Penalty
    
    TVL <- max(deposits) + revenue
    min_dep <- TVL/N_feeds
    
    # Final aggregated choice
    dat <- c(rep(NA,N_feeds))
    for(i in 1:N_feeds){
      if(oracle_test[i] < 3){
        dat[i] <- feed_input[i]
      } else {
        dat[i] <- 0
      }
    }
    
    agg_dat <- median(dat[dat > 0])
    
    # Names 
    Names <- paste0("Provider"," ",seq(1,N_feeds,1))
    Rounds <- paste0("Round",j)
    
    # Store results
    results[[j]] <- data.frame("Providers" = Names,"Rounds" = Rounds,"Feed input" = feed_input, "Median input" = rep(med,N_feeds)
                               ,"MAD" = rep(med_abd,N_feeds),"TVL" = rep(TVL,N_feeds),"min_dep" = rep(min_dep,N_feeds) ,"Revenue" = rep(revenue,N_feeds)
                               , "Initial deposit" = Initial_deposit, "std_scores" = std_scores
                               ,"Final Deposit"= deposits,"Reward" = Reward,"Penalty" = Penalty,"Coeff var MAD" = rep(c_var_MAD,N_feeds)
                               ,"Coeff var SD" = rep(c_var_SD,N_feeds),"Oracle_tests" = oracle_test ,"Oracle_data" = rep(agg_dat,N_feeds))
    oracle_data <- do.call(rbind.data.frame, results)
    
  }
  ar <- 100*(oracle_data$Final.Deposit[oracle_data$Rounds == paste0("Round",j)] - oracle_data$Initial.deposit[oracle_data$Rounds == paste0("Round",1)])/oracle_data$Initial.deposit[oracle_data$Rounds == paste0("Round",1)]
  ar
  
return(data.frame(Names,ar))  
}

oracle_simulation(N_feeds = 100,deposit_average = 10000
                  ,deposit_spread = 1000,feed_average = 1500
                  ,feed_spread = 100,acc_MAD = 100
                  ,sc_fee = 30)
