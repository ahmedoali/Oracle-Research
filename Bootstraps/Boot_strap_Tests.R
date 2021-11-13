
# Oracle set of tests for feed data ---------------------------------------

# Read in Bitcoin data
library(readxl)
exchange_data <- read_excel("exchange_data.xlsx")

# Make into matrix
dat <- as.matrix(exchange_data[,c(5:30)])

#
# Test 1 - Deciding whether the cryptocurrency
# prices are suffering from market inefficiency 
# or are they just bad feed data. 
#
# Test 2 - Deciding whether the spread of the
# price data are similar to a good provider
# distribution or is malice hidden within by
# looking at minimum and maximum
#
# Test 3 - Resample feed data many times to
# obtain the feed spread distribution and observe 
# test statistics such as IQR, MAD, kurtosis and
# skew
#
#

# Test 1 - anything above 0.15% indicates malicious providers and
# not a result of market inefficiency. Between 0.08% and 0.15% could
# be due to market inefficiency.

apply(dat,1,summary)

apply(dat,1,function(x) round(100*((max(x)/min(x))-1),2))
apply(dat,1,function(x) round(100*((max(x)/quantile(x,0.75))-1),2))
apply(dat,1,function(x) round(100*((quantile(x,0.25)/min(x))-1),2))

# Test 2
# Bootstrap each row many times and collect
# Kurtosis, skewness, mean > median
N <- ncol(dat)
M <- 10000
p <- 0.6
bstraps_MAD <- c(rep(NA,M))
bstraps_IQR <- c(rep(NA,M))
s_skew <- c(rep(NA,N))
s_kur <- c(rep(NA,N))
c_rskew <- c(rep(NA,N))
s_IQR <- c(rep(NA,N))
MAD_save <- matrix(data = NA, nrow = M, ncol = N)
IQR_save <- matrix(data = NA, nrow = M, ncol = N)

for(i in 1:N){
  for(j in 1:M){
    rows <- i
    cols <- sample(1:ncol(dat),floor(p*ncol(dat)),replace = TRUE)
    bsample <- dat[rows,cols]
    bstraps_MAD[j] <- mad(bsample)
    bstraps_IQR[j] <- quantile(bsample,0.75) - quantile(bsample,0.25)
  }
  MAD_save[,i] <- bstraps_MAD 
  IQR_save[,i] <- bstraps_IQR 
  s_skew[i] <- skewness(bstraps_MAD)
  s_kur[i] <- kurtosis(bstraps_MAD)
  c_rskew[i] <- ifelse(mean(bstraps_MAD) > median(bstraps_MAD),1,0)
}


apply(MAD_save,2,summary) # If min not exactly 0 then it is 
# difficult to get a subsample dominated by identical values. A sign of
# inefficiency perhaps.
apply(IQR_save,2,summary)
s_skew
s_kur
c_rskew


# Example -----------------------------------------------------------------

# Add malicious values to random rows and random exchanges
N <- ncol(dat)
M <- nrow(dat)
p <- 0.6
for(i in 1:M){
  rows <- i
  for(j in 1:floor(p*N)){
    cols <- sample(1:N,floor(p*N),replace = TRUE)
    dat[i,cols] <- runif(1, min = -1500, max = 1500) + dat[i,cols]
  }
}


# Test 1
apply(dat,1,summary)

apply(dat,1,function(x) round(100*((max(x)/min(x))-1),2))
apply(dat,1,function(x) round(100*((max(x)/quantile(x,0.75))-1),2))
apply(dat,1,function(x) round(100*((quantile(x,0.25)/min(x))-1),2))


# Test 2
N <- ncol(dat)
M <- 10000
p <- 0.6
bstraps_MAD <- c(rep(NA,M))
bstraps_IQR <- c(rep(NA,M))
s_skew <- c(rep(NA,N))
s_kur <- c(rep(NA,N))
c_rskew <- c(rep(NA,N))
s_IQR <- c(rep(NA,N))
MAD_save <- matrix(data = NA, nrow = M, ncol = N)
IQR_save <- matrix(data = NA, nrow = M, ncol = N)

for(i in 1:N){
  for(j in 1:M){
    rows <- i
    cols <- sample(1:ncol(dat),floor(p*ncol(dat)),replace = TRUE)
    bsample <- dat[rows,cols]
    bstraps_MAD[j] <- mad(bsample)
    bstraps_IQR[j] <- quantile(bsample,0.75) - quantile(bsample,0.25)
  }
  MAD_save[,i] <- bstraps_MAD 
  IQR_save[,i] <- bstraps_IQR 
  s_skew[i] <- skewness(bstraps_MAD)
  s_kur[i] <- kurtosis(bstraps_MAD)
  c_rskew[i] <- ifelse(mean(bstraps_MAD) > median(bstraps_MAD),1,0)
}


apply(MAD_save,2,summary)
apply(IQR_save,2,summary)
s_skew
s_kur
c_rskew

