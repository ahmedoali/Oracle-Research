

# Bootstrapping Bitcoin MAD data  -----------------------------------------

library(readxl)
exchange_data <- read_excel("exchange_data.xlsx")

exchange_data$median_ex <- apply(exchange_data[,c(5:30)],1,median)
exchange_data$mad_ex <- apply(exchange_data[,c(5:30)],1,mad)

# Convert data.frame to matrix
exchange_data <- as.matrix(exchange_data)

# Sampling random time points in the matrix and 15 random exchanges
# to calculate MAD and each iteration is replaced.
bstraps <- c()
for(i in 1:10000){
  rows <- sample(nrow(exchange_data),size = 1)
  cols <- sample(5:30,15,replace = TRUE)
  bsample <- exchange_data[rows,cols]
  bstraps <- c(mad(bsample),bstraps)
}

# Summary statistics and uncertainty interval - right skewed 
# as observed before
mean(bstraps)
sd(bstraps)
sort(bstraps)[c(250,9750)]

# Plot raw MAD data, bootstrapped MAD data and a simulated values
# from the lognormal distribution
par(mfrow = c(2,2))
hist(bstraps, freq = FALSE, main = "Distribution of bootstrapped MAD")
hist(exchange_data[,32], freq = FALSE, main = "MAD distribution")
hist(rlnorm(10000,meanlog = 3.86,sdlog = 0.7),freq = FALSE)

