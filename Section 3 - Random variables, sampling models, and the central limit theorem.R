###### Section 3.1: Random variables and sampling models ######

  #An American roulette wheel has 18 red, 18 black, and 2 green pockets. Each red and black pocket is associated with a number from
  #1 to 36. The two remaining green slots feature "0" and "00". Players place bets on which pocket they think a ball will land in after
  #the wheel is spun. Players can bet on a specific number (0, 00, 1-36) or color (red, black, or green).
  
  #Q1: What are the chances that the ball lands in a green pocket?

green <- 2
black <- 18
red <- 18
p_green <- 2/(green + black + red)
p_green

  #Q2: In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17
      #as a prize. Create a model to predict your winnings from betting on green one time.

set.seed(1)
X <- sample(c(17, -1), 1, replace = TRUE, prob = c(1/19, 18/19))
X

  #Q3: Compute the expected value of X, the random variable you generated previously
      #note: the expected value of a random variable is ap+b(1-p) where a and b are the outcomes (in this case, winning $17 and
      #losing $1)

EX <- (17*1/19) + (-1*18/19)

  #Q4: Compute the standard error of the random variable X from exercise 2
      #note: the standard error of one draw of a random variable is the standard deviation of the values in the urn |b-a|???(p(1-p))

SE <- abs(-1 - 17) * sqrt(1/19 * 18/19)

  #Q5: Create a random variable S that sums your winnings after betting on green 1,000 times.

set.seed(1)
X <- sample(c(17, -1), 1000, replace = TRUE, prob = c(1/19, 18/19))
S <- sum(X)
S

  #Q6: What is the expected value of S?

ES <- 1000*EX
  
  #Q7: What is the standard error of S?
      #note that standard error of the sum of n draws of a random variable is ???n*|b-a|???(p(1-p))

sqrt(1000)* abs(-1 - 17) * sqrt(1/19 * 18/19)

###### Section 3.2: The central limit theorem conitnued ######

  #Q1: Using the variables from the previous section, and the CLT with expected value "avg" and standard error "se", compute
      #the probability that you win money betting on green 100 times.

avg <- 100 * EX
se <- sqrt(100) * abs(17 - -1) * sqrt(p_green* (1-p_green))
1 - pnorm(0, avg, se)

  #Q2: Create a Monte Carlo simulation that generates 10,000 outcomes of S, the sum of 100 bets. Compute the average and standard
      #deviation of the resulting list and compare them to the expected value (-5.263158) and standard error (40.19344) for S that
      #you calculated previously.

set.seed(1)
B <- 10000
list <- replicate(B, {
  X <- sample(c(17, -1), 100, replace = TRUE, prob = c(1/19, 18/19))
  S <- sum(X)
 })

mean(list)
sd(list)

  #Q3: Calculate the probability of winning money from the Monte Carlo simulation. 

mean(list > 0)

  #Q4: The Monte Carlo result and the CLT approximation for the probability of losing money after 100 bets are close, but not that
      #close. What could account for this?

        #The CLT does not work as well when the probability of success is small.

  #Q5: Now create a random variable Y that contains your average winnings per bet after betting on green 10,000 times.

set.seed(1)
X <- sample(c(17, -1), 10000, replace = TRUE, prob = c(1/19, 18/19))
Y <- mean(X)

  #Q6: What is the expected value of Y, the average outcome per bet after betting on green 10,000 times?

EY <- 17*1/19 + -1*18/19

  #Q7: What is the standard error of Y, the average result of 10,000 spins?

SEY <- abs(-1-17) * sqrt(1/19*(1-1/19))/sqrt(10000)

  #Q8: What is the probability that your winnings are positive after betting on green 10,000 times?

1-pnorm(0,EY,SEY)

  #Q9: Create a Monte Carlo simulation that generates 10,000 outcomes of S, the average outcome from 10,000 bets on green.Compute 
      #the average and standard deviation of the resulting list to confirm the results from previous exercises using the Central
      #Limit Theorem.

set.seed(1)
B <- 10000
list <- replicate(B, {
  X <- sample(c(17, -1), 10000, replace = TRUE, prob = c(1/19, 18/19))
  S <- mean(X)
})

mean(list)
sd(list)

  #Q10: What is the probability of winning more than $0 as estimated by your Monte Carlo simulation? 

mean(list > 0)

  #Q11: The Monte Carlo result and the CLT approximation are now much closer than when we calculated the probability of winning for 
      #100 bets on green. What could account for this difference?

        #The CLT works better when the sample size is larger.

###### Section 3.3: Random variables, sampling models, and the central limit theorem ######
options(digits = 3)

  #An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a 
  #correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student 
  #chooses answers by guessing for all questions on the test.

  #Questions 1 and 2: SAT testing

  #1a: What is the probability of guessing correctly for one question?

p_correct <- 1/5

  #1b: What is the expected value of points for guessing on one question?

a <- 1
b <- -0.25
mu <- a*p_correct + b*(1-p_correct)

  #1c: What is the expected score of guessing on all 44 questions?

n <- 44
n * (a*p_correct + b*(1-p_correct))

  #1d: What is the standard error of guessing on all 44 questions?

se <- sqrt(n) * abs(b-a) * sqrt(p_correct * (1-p_correct))

  #1e: Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.

1-pnorm(8, mu, se)

  #1f: Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test. What is the probability that 
      #a guessing student scores 8 points or higher?

set.seed(21, sample.kind = "Rounding")

B <- 10000
score <- replicate(B, {
  X <- sample(c(1, -0.25), 44, replace = TRUE, prob = c(p_correct, 1-p_correct))
  sum(X)
})
mean(score >= 8)
  
  #Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question
  #gives a score of 0.

a <- 1
b <- 0
p_a <- 1/4
n <- 44

  #2a: What is the expected value of the score when guessing on this new test?

n * (a*p_a + b*(1-p_a))

  #2b: What is the probability of scoring over 30 when guessing?

mu <- a*p_a + b*(1-p_a)
se <- sqrt(n) * abs(b-a) * sqrt(p_a * (1-p_a))
1-pnorm(30, mu*n, se)

  #2c: Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
      #What is the lowest p such that the probability of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)
sample_data <- function(p){
  a <- 1
  b <- 0
  n <- 44
  mu <- n * (a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  higher_35 <- 1-pnorm(35, mu, se)
}

probs <- sapply(p, sample_data)

min(p[which(probs > 0.8)])

  #Question 3: Betting on roulette

  #A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet 
  #pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of 
  #losing money if he places 500 bets on the roulette House Special.

a <- 6
b <- -1
p_a <- 5/38

  #3a: What is the expected value of the payout for one bet?

EX <- a*p_a + b*(1-p_a)

  #3b: What is the standard error of the payout for one bet?

sigma <- abs(b-a) * sqrt(p_a*(1-p_a))

  #3c: What is the expected value of the average payout over 500 bets?

EX  #The expected value of average of multiple draws from an urn is the expected value of the urn ( ?? ).

  #3d: What is the standard error of the average payout over 500 bets?

se <- sigma/sqrt(500)

  #3e: What is the expected value of the sum of 500 bets?

500*EX

  #3f: What is the standard error of the sum of 500 bets?

sqrt(500) *sigma

  #3g: Use pnorm with the expected value of the sum and standard error of the sum to calculate the probability of losing money over
      #500 bets,  Pr(X???0) .

pnorm (0, 500*EX, sqrt(500) *sigma)
