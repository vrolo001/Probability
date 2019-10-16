###### Section 4.1: The big short assessment ######
library(tidyverse)

  #Say you manage a bank that gives out 10,000 loans. The default rate is 0.03 and you lose $200,000 in each foreclosure.
  
p <- 0.03
loss_per_foreclosure <- -200000
n <- 10000

  #Q1: Create a random variable S that contains the earnings of your bank. Calculate the total amount of money lost in this scenario.

set.seed(1)
defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1-p, p))
S <- sum(defaults)*loss_per_foreclosure

  #Q2: Run a Monte Carlo simulation with 10,000 outcomes for S, the sum of losses over 10,000 loans. Make a histogram of the results.

set.seed(1)
B <- 10000
 sum_losses <- replicate(B, {
   defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1-p, p))
   S <- sum(defaults)*loss_per_foreclosure
 })
 
hist(sum_losses)

  #Q3: Assuming the bank makes no money if the loan is paid, What is the expected value of S, the sum of losses over 10,000 loans?

mu_n <- n * (loss_per_foreclosure*p + 0*(1-p))
  
  #Q4: What is the standard error of S?

se <- sqrt(n) * abs(0 - loss_per_foreclosure) * sqrt(p*(1-p)) #shouldn't it be - instead of + like the datacamp answered suggested?

  #Q5: Assume we give out loans for $180,000. How much money do we need to make when people pay their loans so that our net loss
      #is $0? In other words, what interest rate do we need to charge in order to not lose money?

          # we want loss_per_foreclosure*p + interest*(1-p) = 0 therefore,

interest <- -(loss_per_foreclosure*p)/(1-p)
interest_rate <- interest/180000

  #Q6: What should the interest rate be so that the chance of losing money is 1 in 20? In math notation, what should the
      #interest rate be so that Pr(S<0)=0.05?

x <- (-loss_per_foreclosure*(n*p - qnorm(0.05)*sqrt(n*p*(1-p)))) / (n*(1-p) + qnorm(0.05)*sqrt(n*p*(1-p)))

rate <- x/180000

  #Q7: The bank wants to minimize the probability of losing money. Which of the following achieves their goal without making
      #interest rates go up?
      #A smaller pool of loans
      #A larger probability of default
      #A reduced default rate (correct)
      #A larger cost per loan default
