# Bootstrapping LTC MAD data  -----------------------------------------

library(readxl)
exchange_data <- read_excel("LTC exchange data.xlsx")

exchange_data$median_ex <- apply(exchange_data[,c(5:17)],1,median)
exchange_data$mad_ex <- apply(exchange_data[,c(5:17)],1,mad)

# Sampling random time points in the matrix and any number of random exchanges
# in the data to calculate MAD and each iteration is replaced.
bstraps <- c()
for(i in 1:10000){
  rows <- sample(nrow(exchange_data),size = 1)
  N_feed <- rbinom(1,ncol(exchange_data),0.75)
  cols <- sample(5:17,N_feed,replace = TRUE)
  bsample <- exchange_data[rows,cols]
  bstraps <- c(mad(as.matrix(bsample)),bstraps)
}

# Summary statistics and uncertainty interval - right skewed 
# as observed before
mean(bstraps)
sd(bstraps)
sort(bstraps)[c(250,9750)]

# Plot raw MAD data, bootstrapped MAD data and simulated values
# from the lognormal distribution
par(mfrow = c(2,2))
hist(bstraps, freq = FALSE, main = "Distribution of bootstrapped MAD")
hist(as.matrix(exchange_data[,19]), xlab = "MAD data" 
     ,freq = FALSE, main = "MAD distribution")
