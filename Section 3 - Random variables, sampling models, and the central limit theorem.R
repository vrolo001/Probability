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

abs(-1 - 17) * sqrt(1/19 * 18/19)

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

