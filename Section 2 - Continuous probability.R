library(tidyverse)
library(dslabs)
data(heights)

###### Section 2.1 Continuous probability assessment ######

  #Q1: Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a 
      #standard deviation of 3 inches. If we pick a female at random, what is the probability that she is 5 feet(60in) or shorter?

pnorm(60, 64, 3)

  #Q2: Using the same distribution as in 1, if we pick a female at random, what is the probability that she is 6 feet(72in) or taller?

1-pnorm(72, 64, 3)

  #Q3:If we pick a female at random, what is the probability that she is between 61 and 67 inches?

    #should be p of being 67 or less minus p of being 61 or less

pnorm(67, 64, 3) - pnorm(61, 64, 3)

  #Q4: Repeat the previous calculation using pnorm to define the probability that a randomly chosen woman will have a height between
      #61 and 67 inches, converted to centimeters by multiplying each value by 2.54.

pnorm(67*2.54, 64*2.54, 3*2.54) - pnorm(61*2.54, 64*2.54, 3*2.54)

  #Q5: Compute the probability that the height of a randomly chosen female is within 1 SD from the average height.

pnorm (64+3, 64, 3) - pnorm (64-3, 64, 3)

  #Q6: Imagine the distribution of male adults is approximately normal with an average of 69 inches and a standard deviation of 
      #3 inches. How tall is a male in the 99th percentile?

qnorm(0.99, 69, 3)

  #Q7: The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. 
      #Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people   
      #are born each year. Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest
      #IQ scores.

set.seed(1)
B <- 1000
highestIQs <- replicate(B, {
  IQs <- rnorm(10000, 100, 15)
  max(IQs)
})

hist(highestIQs)

###### Section 2.2 Continuous probability assessment ######

#The ACT is a standardized college admissions test used in the United States. For the three year period 2016-2018, ACT standardized
#test scores were approximately normally distributed with a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are 
#integers between 1 and 36, but we will ignore this detail and use continuous values instead.)
 
#Set the seed to 16, then use rnorm to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of
#5.7. Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
 
 #Questions 1 and 2: ACT scores, part 1

  #1a: What is the mean of act_scores?

mean(act_scores)

  #1b: What is the standard deviation of act_scores?

sd(act_scores)

  #1c: A perfect score is 36 or greater. In act_scores, how many perfect scores are there out of 10,000 simulated tests?

sum(act_scores >= 36)

  #1d: In act_scores, what is the probability of an ACT score greater than 30?

mean(act_scores > 30)

  #1e: In act_scores, what is the probability of an ACT score less than or equal to 10?

mean(act_scores <= 10)

  #2: Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function over x 
     #given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

 #Questions 3 and 4: ACT scores, part 2
  #Convert act_scores to Z-scores. Use the mean and standard deviation of act_scores, not the original values used to generate 
  #random test scores.

z_scores <- (act_scores - mean(act_scores))/sd(act_scores)

  #3a: What is the probability of a Z-score greater than 2?

mean(z_scores > 2)
  
  #3b: What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?

2 * sd(act_scores) + mean(act_scores)

  #3c: A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm to determine the 97.5th percentile of normally
      #distributed data with the mean and standard deviation observed in act_scores. What is the 97.5th percentile of act_scores?

qnorm(0.975, mean(act_scores), sd(act_scores))

  #4a: What is the minimum integer score such that the probability of that score or lower is at least .95?

  #My answer: I know from my stats knowledge that the z-score corresponding to a p of .95 one-tail is 1.65. The act score 
              #corresponding to that z-score is 30.20. Thirty cannot be the answer because the p of getting a score of 30
              #would be just below .95. Therefore, the logical solution would be the next integer, 31. We can double-check this
              #anwer by computing the p of getting a score of 31.

1.65 * sd(act_scores) + mean(act_scores)
mean(act_scores <= 31)

  #course answer

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))

  #4b: Use qnorm to determine the expected 95th percentile, the value for which the probability of receiving that score or 
      #lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7. What is the 95th percentile of act_scores?

qnorm(0.95, 20.9, 5.7)

  #4c: Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores
      #data. Save these as sample_quantiles. In what percentile is a score of 26?

  #My answer:
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- qnorm(p, mean(act_scores), sd(act_scores))

  #Course answer:
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

  #4d: Make a corresponding set of theoretical quantiles using qnorm over the interval p <- seq(0.01, 0.99, 0.01) with 
      #mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing 
      #sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles, sample_quantiles)