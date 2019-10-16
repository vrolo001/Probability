###### Section 4.2: The Big Short, verified learners ######

options(digits = 3)
library(tidyverse)
library(dslabs)

  #Just as banks must decide how much to charge as interest on loans based on estimates of loan defaults, insurance companies
  #must decide how much to charge as premiums for policies given estimates of the probability that an individual will collect
  #on that policy. We will use data from 2015 US Period Life Tables. The needed data from dslabs is provided below
data(death_prob)
head(death_prob)

  #Questions 1 and 2: Insurance rates, part 1

  #An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within one year.
  #The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event of a claim, the 
  #company forfeits the premium and loses a total of $150,000, and if there is no claim the company gains the premium amount
  #of $1,150. The company plans to sell 1,000 policies to this demographic.

n <- 1000
claim <- -150000
no_claim <- 1150

  #1a: Use death_prob to determine the death probability of a 50 year old female, p.

p <- death_prob %>%
  filter(sex == "Female" & age == 50) %>%
  pull(prob)

  #1b: What is the expected value of the company's net profit on one policy for a 50 year old female?

mu <- claim * p + no_claim * (1-p)

  #1c: Calculate the standard error of the profit on one policy for a 50 year old female.

sigma <- abs(no_claim-claim) * sqrt(p*(1-p))

  #1d: What is the expected value of the company's profit over all 1,000 policies for 50 year old females?

mu_n <- mu * n

  #1e: What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?  

sigma_n <- sigma * sqrt(n)

  #1f: Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of
      #1,000 policies.

pnorm(n, mu_n, sigma_n)

  #50 year old males have a different probability of death than 50 year old females. We will calculate a profitable
  #premium for 50 year old males in the following four-part question.  

  #2a: Use death_prob to determine the probability of death within one year for a 50 year old male.

p_male <- death_prob %>%
  filter(sex == "Male" & age == 50) %>%
  pull(prob)

  #2b:Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to
     #be $700,000. Use the formula for expected value of the sum of draws and solve for the preimum
     #premium  b :
     
     #We want n*(ap+b(1???p)) to be 700,000, and we want to calculate b.

mu_s <- 700000
premium <- ((mu_s/n)-claim*p_male)/(1-p_male)

  #2c: Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.

sigma_n_males <- sqrt(n) * abs(premium-claim) * sqrt(p_male*(1-p_male))

  #2d: What is the probability of losing money on a series of 1,000 policies to 50 year old males?

pnorm(0, mu_s, sigma_n_males)

  #Questions 3 and 4: Insurance rates, part 2

  #Life insurance rates are calculated using mortality statistics from the recent past. They are priced such that companies
  #are almost assured to profit as long as the probability of death remains similar. If an event occurs that changes the 
  #probability of death in a given age group, the company risks significant losses. We'll look at a scenario in which a 
  #lethal pandemic disease increases the probability of death within 1 year for a 50 year old to .015. Unable to predict 
  #the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.

p_pandemic <- 0.015
claim 
no_claim
n

  #3a: What is the expected value of the company's profits over 1,000 policies?

pandemic_mu_n <- n * (claim*p_pandemic + no_claim*(1-p_pandemic))
  
  #3b: What is the standard error of the expected value of the company's profits over 1,000 policies?

pandemic_sigma <- sqrt(n) * abs(no_claim - claim) * sqrt(p_pandemic*(1-p_pandemic))

  #3c: What is the probability of the company losing money?

pnorm(0, pandemic_mu_n, pandemic_sigma)

  #3d: Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out
      #of business. What is the probability of losing more than $1 million?

pnorm(-1000000, pandemic_mu_n, pandemic_sigma)

  #3e: Investigate death probabilities p <- seq(.01, .03, .001). What is the lowest death probability for which the 
  #chance of losing money exceeds 90%?

p <- seq(.01, .03, .001)

chance_loss <- function(p){
  claim <- -150000
  no_claim <- 1150
  n <- 1000
  pandemic_mu_n <- n * (claim*p + no_claim*(1-p))
  pandemic_sigma <- sqrt(n) * abs(no_claim - claim) * sqrt(p_pandemic*(1-p_pandemic))
  pnorm(0, pandemic_mu_n, pandemic_sigma)
}

p_loss <- sapply(p, chance_loss)
all_ps <- data.frame(p, p_loss) 
all_ps %>%
  filter(p_loss > 0.90)

  #3f: Investigate death probabilities p <- seq(.01, .03, .0025). What is the lowest death probability for which the
      #chance of losing over $1 million exceeds 90%?

p <- seq(.01, .03, .0025)

chance_loss <- function(p){
  claim <- -150000
  no_claim <- 1150
  n <- 1000
  pandemic_mu_n <- n * (claim*p + no_claim*(1-p))
  pandemic_sigma <- sqrt(n) * abs(no_claim - claim) * sqrt(p_pandemic*(1-p_pandemic))
  pnorm(-1000000, pandemic_mu_n, pandemic_sigma)
}

p_loss <- sapply(p, chance_loss)
all_ps <- data.frame(p, p_loss) 
all_ps %>%
  filter(p_loss > 0.90)

  #Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, 
  #loss of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25, then run the model once. 

set.seed(25, sample.kind = "Rounding")

n <- 1000
p_loss <- 0.015
a <- -150000
b <- 1150

profit <- sample(c(a,b), n, replace = TRUE, prob = c(p_loss, 1-p_loss))
sum(profit)

  #4a: What is the reported profit (or loss) in millions (that is, divided by 10^6 )?

sum(profit)/1*10^6

  #4b: Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate the
      #range of profits/losses over 1,000 loans. What is the observed probability of losing $1 million or more?

set.seed(27, sample.kind = "Rounding")
B <- 10000

prob_losing <- replicate(B, {
  profit <- sample(c(a,b), n, replace = TRUE, prob = c(p_loss, 1-p_loss))
  sum(profit)
})

mean(prob_losing < -1*10^6) #be careful with negatives, losing more than 1mil means you want numbers SMALLER than -1*10^6

  #Questions 5 and 6: Insurance rates, part 3

  #Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a premium 
  #cost for which the probability of losing money is under 5%, assuming the death rate stays stable at  p=0.015 .

p_death <- 0.015
claim <- -150000
p_losing <- 0.05
n <- 1000

  #5a: Calculate the premium required for a 5% chance of losing money given  n=1000  loans, probability of death  p=0.015,
      #and loss per claim  l=???150000 . Save this premium as x for use in further questions.

x <- -claim*((n*p_death)-(qnorm(0.05)*sqrt(n*p_death*(1-p_death))))/(n*(1-p_death)+qnorm(0.05)*sqrt(n*p_death*(1-p_death)))

  #5b: What is the expected profit per policy at this rate?

mu <- claim*p_death + x*(1-p_death)

  #5c: What is the expected profit over 1,000 policies?

mu_n <- n*mu

  #5d: Run a Monte Carlo simulation with B=10000 to determine the probability of losing money on 1,000 policies given the
      #new premium x, loss on a claim of $150,000, and probability of claim  p=.015 . Set the seed to 28 before running 
      #your simulation. What is the probability of losing money here?

set.seed(28, sample.kind = "Rounding")
B <- 10000
profit <- replicate(B, {
  cases <-sample(c(claim, x), 1000, replace = TRUE, prob = c(p_death, 1-p_death))
  sum(cases)
}) 

mean(profit < 0)

  #The company cannot predict whether the pandemic death rate will stay stable. Set the seed to 29, then write a Monte 
  #Carlo simulation that for each of  B=10000  iterations:
  #1)randomly changes p by adding a value between -0.01 and 0.01 with sample(seq(-0.01, 0.01, length = 100), 1)
  #2)uses the new random p to generate a sample of n=1,000  policies with premium x and loss per claim  l=???150000
  #3)returns the profit over  n  policies (sum of random variable)

set.seed(29, sample.kind = "Rounding")
B <- 10000
profit <- replicate(B, {
  p_death <- 0.015
  change_p <- sample(seq(-0.01, 0.01, length = 100), 1)
  new_p <- p_death + change_p
  cases <-sample(c(claim, x), 1000, replace = TRUE, prob = c(new_p, 1-new_p))
  sum(cases)
}) 

  #6a: What is the expected value over 1,000 policies?

mean(profit)

  #6b: What is the probability of losing money?

mean(profit < 0)

  #6c: What is the probability of losing more than $1 million?

mean(profit < -1*10^6)
