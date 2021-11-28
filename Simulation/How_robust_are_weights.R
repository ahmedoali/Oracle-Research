
# How robust are weights --------------------------------------------------

# Bitcoin

# Read in Bitcoin data
library(readxl)
exchange_data <- read_excel("exchange_data.xlsx")

# Make into matrix
dat <- as.matrix(exchange_data[,c(5:30)])

# Weighting
apply(dat,1,median)
apply(dat,1,mad)
diff_sq <- t(apply(dat,1,function(s) (median(s) - s)^2))
weight <- t(apply(diff_sq,1,function(diff_sq) (diff_sq - max(diff_sq))/(min(diff_sq) - max(diff_sq))*(1-0.2)+0.2))

# Add malicious values to random rows and random exchanges
N <- ncol(dat)
M <- nrow(dat)
p <- 0.1
dat_m <- dat
for(i in 1:M){
  rows <- i
  cols <- sample(1:N,floor(p*N),replace = TRUE)
  for(j in 1:length(cols)){
    dat_m[i,cols] <- runif(1, min = -5000, max = 5000) + dat[i,cols]
  }
}

# Weighting
apply(dat_m,1,median)
apply(dat_m,1,mad)
diff_sq_m <- t(apply(dat_m,1,function(s) (median(s) - s)^2))
weight_m <- t(apply(diff_sq_m,1,function(diff_sq) (diff_sq - max(diff_sq))/(min(diff_sq) - max(diff_sq))*(1-0.2)+0.2))

# Exchange analysis
results <- data.frame("Good" = apply(weight,2,function(x) 100*sum(ifelse(x >0.9,1,0))/63),
           "Bad" = apply(weight_m,2,function(x) 100*sum(ifelse(x >0.9,1,0))/63))

# Litecoin

rm(list = ls())

# Read in Litecoin data
library(readxl)
exchange_data <- read_excel("LTC exchange data.xlsx")

# Make into matrix
dat <- as.matrix(exchange_data[,c(5:17)])

# Weighting
apply(dat,1,median)
apply(dat,1,mad)
diff_sq <- t(apply(dat,1,function(s) (median(s) - s)^2))
weight <- t(apply(diff_sq,1,function(diff_sq) (diff_sq - max(diff_sq))/(min(diff_sq) - max(diff_sq))*(1-0.2)+0.2))

# Add malicious values to random rows and random exchanges
N <- ncol(dat)
M <- nrow(dat)
p <- 0.1
dat_m <- dat
emname <- matrix(NA,nrow = nrow(dat),ncol = floor(p*N))
for(i in 1:M){
  rows <- i
  cols <- sample(1:N,floor(p*N),replace = TRUE)
  for(j in 1:length(cols)){
    dat_m[i,cols] <- runif(1, min = -10, max = 10) + dat[i,cols]
    emname[i,] <- colnames(dat)[cols]
  }
}

table(emname)

# Weighting
apply(dat_m,1,median)
apply(dat_m,1,mad)
diff_sq_m <- t(apply(dat_m,1,function(s) (median(s) - s)^2))
weight_m <- t(apply(diff_sq_m,1,function(diff_sq) (diff_sq - max(diff_sq))/(min(diff_sq) - max(diff_sq))*(1-0.2)+0.2))

# Exchange analysis
results <- data.frame("Good" = apply(weight,2,function(x) 100*sum(ifelse(x >0.9,1,0))/502),
                      "Bad" = apply(weight_m,2,function(x) 100*sum(ifelse(x >0.9,1,0))/502),
                      "Malicious count" = table(emname))
