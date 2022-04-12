# Committee size simulation -----------------------------------------------


# Read data ---------------------------------------------------------------

library(readxl)
dat <- read_xlsx("Final survey data v3.xlsx")
dat <- cbind("Unique_ID" = seq(1,nrow(dat),1),dat)
dat <- dat[dat$`Which of the following do you intend on becoming?` == "Node runner",]
dat <- dat[,c(1,19)]


# Simulation --------------------------------------------------------------

N <- 1000000
results <- list()
library(utils)
pb <- txtProgressBar(min = 0, max = N, initial = 0)
for(j in 1:N){
    hash_max <- 2^512
    hash <- runif(nrow(dat), min = 0, max = hash_max)
    r <- hash/hash_max
    tau <- 10000
    t <- round(runif(1,min = 100, max = 500),0)
    N <- dat[sample(nrow(dat), t), ] # Introduce weighted sampling?
    # Assumption being - more stake, more requests and better tech
    N_votes <- c(rep(0,nrow(N)))
    vote <- c(rep(NA,nrow(N)))
    for(i in 1:nrow(N)){
        if(N[i,2] == 0) next
        prob <- cumsum(dbinom(0:round(N[i,2],0),round(N[i,2],0),tau/sum(N[,2])))
        N_votes[i] <- findInterval(r[i],prob)
        # Voting behaviour simulation - assuming all tokens behave the
        # same from a node runner/validator
        vote[i] <- rbinom(1,1,0.6)
    }
    temp <- data.frame("ID" = N[,1], "Tokens" = N[,2],
                       "N_Votes" = N_votes, "Vote" = vote)
    temp <- na.omit(temp)
    # Weighted votes
    crit <- ifelse(sum(temp$N_Votes[temp$Vote == 1]) > sum(temp$N_Votes[temp$Vote == 0]),1,0)
    rewards <- c(rep(0,nrow(temp)))
    # Assign rewards to majority
    for(k in 1:nrow(temp)){
        if(temp$Vote[k] == crit){
            # Weighted reward
            rewards[k] = 8*temp$N_Votes[k]/sum(temp$N_Votes[temp$Vote == crit])
        }
    }
    results[[j]] <- cbind(temp,"Rewards" = rewards)
    setTxtProgressBar(pb,j)
}
close(pb)

# Create dataframe
library(data.table)
dat_v2 <- rbindlist(results)
dat_v3 <- setorder(dat_v2,ID)

# Merge dataframes
library(tidyverse)
dat_v4 <- left_join(dat,rbindlist(results)[, sum(Rewards), by = ID], by = c("Unique_ID" = "ID"))

N <- dat_v3 %>% 
    group_by(ID) %>% 
    count()

dat_v5 <- left_join(dat_v4, N, by = c("Unique_ID" = "ID"))

# Remove missing
dat_v6 <- na.omit(dat_v5)

# Save dataframe
library(openxlsx)
write.xlsx(dat_v6, 'results.xlsx')

# Plots -------------------------------------------------------------------

# Plot of N_tokens vs Rewards

jpeg("rplot1.jpg", width = 350, height = 350)

P <- ggplot(data = dat_v6, aes(x = N_tokens, y = Rewards)) + 
    geom_jitter(width = 0.5, height = 0.5)

require(scales)
P + scale_x_continuous(labels = comma) + 
    xlab("Number of tokens (GORA)") + 
    ylab("Reward (GORA)")

dev.off()

# Plot of N_tokens vs ROI

jpeg("rplot2.jpg", width = 350, height = 350)

dat_v6$ROI <- 100*dat_v6$Rewards/dat_v6$N_tokens
head(dat_v6$ROI)

Q <- ggplot(data = dat_v6, aes(x = N_tokens, y = ROI)) + 
    geom_jitter(width = 0.5)

require(scales)
Q + scale_x_continuous(labels = comma) + 
    xlab("Number of tokens (GORA)") + 
    ylab("ROI")

dev.off()
