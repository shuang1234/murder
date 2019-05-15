# Task 1: Choose a dataset and research question
# Dataset: murders
# Researsh question: Is the rate of murders in South the same 
#   as the rate in North Central?

# Explore the data preliminarily
library(dslabs)

data("murders")
head(murders)
summary(murders)
summary(murders$region)

# Create a new column: rate - gun murders per 1000000
murders$rate <- murders$total/murders$population * 1000000

# Calculate the mean and sd of rate for each region
### This is the Table1 in the report
library(dplyr)
murders_group <- murders %>%
  group_by(region) %>%
  summarise(rate_mean = mean(rate), rate_sd = sd(rate))

View(murders_group)

# Visualise the data
### This is the Figure1 in the report
library(ggplot2)
ggplot(murders, aes(x = region, y = rate)) +
  geom_boxplot()


# Task 2: Choose statistical tests
# Statistical tests: 
#     Parametric: t-test
#     Non-parametric: Wilcoxon rank-sum Test (also called Mann-Whitney U Test)


# Task 3: Generate and analyze simulated data

size_and_power <- function(n, effect, round){
  # Purpose: Write a function to 1)simulate data under different scenarios
  #         2) apply t-test and Wilcoxon rank-sum test
  #         3) calculate the size and power of these statistical tests
  # Input: n - sample size
  #        effect - effect size(different between means)
  #        round - measurement error(number of decimal places)
  # Output: t_size - size for t-test
  #         wil_size - size for Wilcoxon rank-sum test 
  #         t_power - power for t-test
  #         wil_power - power for Wilcoxon rank-sum test 
  
  # Set seed for reproducibility
  set.seed(180009169)
  
  t_pvalues <- numeric(1000)
  wil_pvalues <- numeric(1000)
  
  # Simulate data 1000 times
  for(i in 1:1000){

   sim_murders <- data.frame(region = rep(c("South", "North Central"), each = n),
                             rate = c(round(rnorm(n, murders_group[[2,2]], 
                                                  murders_group[[2,3]]), round), 
                                      round(rnorm(n, murders_group[[2,2]] - effect, 
                                                  murders_group[[3,3]]), round)))
   
   # Calculate p-value
   t_pvalues[i] <- t.test(rate ~ region, data = sim_murders)[[3]]
   wil_pvalues[i] <- wilcox.test(rate ~ region, data = sim_murders)[[3]]
  }
   
   # Calculate size and power 
   if (effect == 0) {
     
     t_size <- sum(t_pvalues <= 0.05)/1000
     wil_size <- sum(wil_pvalues <= 0.05)/1000
     
     return(list(paste("The size for t-test is", t_size), 
                 paste("The size for Wilcoxon rank-sum test is", wil_size)))
   }
   else{
     
     t_power <- sum(t_pvalues <= 0.05)/1000
     wil_power <- sum(wil_pvalues <= 0.05)/1000
     
     return(list(paste("The power for t-test is", t_power), 
                 paste("The power for Wilcoxon rank-sum test is", wil_power)))
   }
}

# When the effect size is zero, calculate the sizes under different sample sizes
#   and measurement errors(numbers of decimal places)
lapply(c(2, 0, -1), function(x){
  size_and_power(n = 10, effect = 0, round = x)})

lapply(c(2, 0, -1), function(x){
  size_and_power(n = 100, effect = 0, round = x)})
  
sapply(c(2, 0, -1), function(x){
  size_and_power(n = 1000, effect = 0, round = x)})  


# When the number of decimal is fixed, calculate the powers under different 
#   sample sizes and effect sizes
sapply(c(10, 20, 30), function(x){
  size_and_power(n = 10, effect = x, round = 2)})

sapply(c(10, 20, 30), function(x){
  size_and_power(n = 100, effect = x, round = 2)})

sapply(c(10, 20, 30), function(x){
  size_and_power(n = 1000, effect = x, round = 2)})


# Report and visualise the sizes for t-test and Wilcoxon rank-sum test
### This is the Figure2 in the report
ttest_size <- data.frame(sampleSize = rep(c(10, 100, 1000), each = 3), 
                         decimalPlace = rep(c(-1, 0, 2),3), 
                         size = c(0.043, 0.037, 0.038, 0.047, 0.048, 0.048, 
                                  0.057, 0.056, 0.058))
ttest_size$sampleSize <- as.factor(ttest_size$sampleSize)
ttest_size$decimalPlace <- as.factor(ttest_size$decimalPlace)
ggplot(data = ttest_size, aes(x = decimalPlace, y = size, color = sampleSize, 
                              group = sampleSize)) +
  geom_point() +
  geom_line() +
  labs(title = " Sizes for t-test ") +
  theme(plot.title=element_text(hjust=0.5))


wilcoxon_size <- data.frame(sampleSize = rep(c(10, 100, 1000), each = 3), 
                            decimalPlace = rep(c(-1, 0, 2),3), 
                            size = c(0.046, 0.043, 0.042, 0.067, 0.070, 0.070, 
                                     0.072, 0.078, 0.077))
                         
wilcoxon_size$sampleSize <- as.factor(wilcoxon_size$sampleSize)
wilcoxon_size$decimalPlace <- as.factor(wilcoxon_size$decimalPlace)

ggplot(data = wilcoxon_size, aes(x = decimalPlace, y = size, color = sampleSize, 
                              group = sampleSize)) +
  geom_point() +
  geom_line() +
  labs(title = " Sizes for Wilcoxon rank-sum test ") +
  theme(plot.title=element_text(hjust=0.5))  

# Report and visualise the powers for t-test and Wilcoxon rank-sum test
### This is the Figure3 in the report
ttest_power <- data.frame(sampleSize = rep(c(10, 100, 1000), each = 3), 
                          effectSize = rep(c(10, 15, 20),3), 
                          power = c(0.123, 0.229, 0.364, 0.768, 0.979, 1.000, 
                                   1.000, 1.000, 1.000))
                          
ttest_power$sampleSize <- as.factor(ttest_power$sampleSize)
ttest_power$effectSize <- as.factor(ttest_power$effectSize)

ggplot(data = ttest_power, aes(x = effectSize, y = power, color = sampleSize, 
                                 group = sampleSize)) +
  geom_point() +
  geom_line() +
  labs(title = " Powers for t-test ") +
  theme(plot.title=element_text(hjust=0.5)) 
                
              

wilcoxon_power <- data.frame(sampleSize = rep(c(10, 100, 1000), each = 3), 
                          effectSize = rep(c(10, 15, 20),3), 
                          power = c(0.118, 0.216, 0.349, 0.731, 0.978, 0.999, 
                                    1.000, 1.000, 1.000))

wilcoxon_power$sampleSize <- as.factor(wilcoxon_power$sampleSize)
wilcoxon_power$effectSize <- as.factor(wilcoxon_power$effectSize)

ggplot(data = wilcoxon_power, aes(x = effectSize, y = power, color = sampleSize, 
                               group = sampleSize)) +
  geom_point() +
  geom_line() +
  labs(title = " Powers for Wilcoxon rank-sum test ") +
  theme(plot.title=element_text(hjust=0.5)) 
    
