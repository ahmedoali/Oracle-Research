
# Updated Oracle simulation -----------------------------------------------

#
# Included more complex MAD calculation, coefficient of variation and
# Smart contract fees
#

rm(list = ls())
N <- 10 # Time points
N_feeds <- 10 # Number of feed providers, can simulate random number of feeds if needed
deposits <- rnorm(N_feeds, mean = 10000, sd = 1000) # Simulating initial deposits for each feed provider
results <- list() # Final results will be stored in a list
for(j in 1:N){
  
  # Number of smart contracts pulling data
  N_smart <- rbinom(1,1000,0.6)
  
  # Fees pool for iteration j
  sc_fees <- 30*N_smart
  
  # New inputs
  feed_input <- rnorm(n = N_feeds, mean = 1500, sd = 200)
  
  # Initial deposit
  Initial_deposit <- deposits
  
  # Weighting
  med <- median(feed_input)
  med_abd <- mad(feed_input)
  diff_sq <- (med - feed_input)^2
  weight <- 1 - (diff_sq - min(diff_sq))/(max(diff_sq) - min(diff_sq))
  
  # Test - coeff of var
  c_var_MAD <- 100*mad(feed_input)/median(feed_input)
  c_var_SD <- 100*sd(feed_input)/mean(feed_input)
  
  # Bootstrap MAD and apply tests
  bstraps <- c(rep(NA,10000))
  for(i in 1:10000){
    samp <- sample(1:N_feeds,floor(0.6*N_feeds), replace = TRUE)
    bsample <- feed_input[samp]
    bstraps[i] <- mad(bsample)
  }
  
  min_count <- 100*sum(ifelse(bstraps == 0,1,0))/10000
  
  library(Hmisc)
  
  removeOutliers <- function(x) {
    hdmedian <- function(u) as.numeric(hdquantile(u, 0.5))
    
    x <- x[!is.na(x)]
    m <- hdmedian(x)
    deviations <- abs(x - m)
    lowerMAD <- 1.4826 * hdmedian(deviations[x <= m])
    upperMAD <- 1.4826 * hdmedian(deviations[x >= m])
    
    return(list("m"=m,"LM"=lowerMAD,"UM" = upperMAD,"Outliers" = ifelse(x >= m - 3*lowerMAD & x <= m + 3*upperMAD,1,0)))
  }
  
  RO <- removeOutliers(feed_input)
  
  doub_mad <- RO$Outliers
  
  Penalty <- c(rep(0,N_feeds))
  Reward <- c(rep(0,N_feeds))
  for(i in 1:N_feeds){
    # Penalty
    if(feed_input[i] >= RO$m + 3*RO$UM | feed_input[i] <= RO$m - 3*RO$LM){
      Penalty[i] <- 0.1*deposits[i]
    } else {
      Penalty[i] = 0
    }
  }
  
  # Total penalty pool
  pen_pool <- sum(Penalty)
  
  # Total revenue
  revenue <- sum(pen_pool,sc_fees)
  
  for(i in 1:N_feeds){
    # Reward
    Reward[i] <- revenue * weight[i]/sum(weight)
  }
  
  # Adjust deposit
  deposits <- deposits + Reward - Penalty
  
  # Names 
  Names <- paste0("Provider"," ",seq(1,N_feeds,1))
  Rounds <- paste0("Round",j)
  
  # Store results
  results[[j]] <- data.frame("Providers" = Names,"Rounds" = Rounds,"Feed input" = feed_input, "Median input" = rep(med,N_feeds)
                             ,"MAD" = rep(med_abd,N_feeds), "Revenue" = rep(revenue,N_feeds)
                             , "Initial deposit" = Initial_deposit
                             ,"Final Deposit"= deposits
                             ,"Weight"=weight,"Reward" = Reward,"Penalty" = Penalty,"Coeff var MAD" = rep(c_var_MAD,N_feeds)
                             ,"Coeff var SD" = rep(c_var_SD,N_feeds),"DoubMAD" =doub_mad,"Count0" = min_count)
  oracle_data <- do.call(rbind.data.frame, results)
}
