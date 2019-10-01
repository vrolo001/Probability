###### Section 1.1 Introduction to Discrete Probability ######

### Assessment: Intro to Discrete Probability (on edx)

  #Imagine a box containing 3 cyan balls, 5 magenta balls, and 7 yellow balls. 

balls <- rep(c("cyan", "magenta", "yellow"), times = c(3,5,7))

  #Q1: What is the probability of drawing one cyan ball? 0.2

    mean(balls == "cyan")
  
  #Q2: What is the probability that the ball will not be cyan? 0.8
    mean(!balls == "cyan")
    
  #Q3: Consider sampling without replacement. What is the probability that the first draw is cyan and the second draw is not cyan?
    (3/15)*(12/14)
  
  #Q4: COnsider sampling w/replacement. What is the probability that the first draw is cyan and that the second draw is not cyan?
    mean(balls == "cyan") * mean(!balls == "cyan")
    3/15 * 12/15 #alternative
  
#### Assessment: Intro to Discrete Probability (datacamp; same problems as edx only solved differently)
    
    cyan <- 3
    magenta <- 5
    yellow <- 7
    
  #Q1: What is the probability of drawing one cyan ball?
    p <- cyan/(cyan + magenta + yellow)
  #Q2: Using the probability of choosing a cyan ball, p, calculate the probability of choosing any other ball.
    1-p
  #Q3: Consider sampling without replacement. What is the probability that the first draw is cyan and the second draw is not cyan?
      #Calculate the conditional probability p_2 of choosing a ball that is not cyan after one cyan ball has been removed from the box.
      #Calculate the joint probability of both choosing a cyan ball on the first draw and a ball that is not cyan on the second draw using
      #p_1 and p_2
    p_2 <- 12/14
    p_1 <- cyan/(cyan + magenta + yellow)
    p_1 * p_2
  #Q4: COnsider sampling w/replacement. What is the probability that the first draw is cyan and that the second draw is not cyan?
      #Calculate the probability p_2 of choosing a ball that is not cyan on the second draw, with replacement. Next, use p_1 and p_2 to 
      #calculate the probability of choosing a cyan ball on the first draw and a ball that is not cyan on the second draw (after replacing
      #the first ball).
    p_1 <- cyan / (cyan + magenta + yellow)
    p_2 <- (magenta + yellow)/(cyan + magenta + yellow)
    p_1 * p_2
    
###### Section 1.2: Combinations and permutations ######
library(gtools)  
    
  #Q2: You've drawn 5 balls from a box that has 3 cyan balls, 5 magenta balls, and 7 yellow balls, with replacement, and all have been yellow.
      #What is the probability that the next one is yellow? (events are independent, therefore the Pr of a yellow draw remains the same 
      #across n trials)

balls <- rep(c("cyan", "magenta", "yellow"), times = c(3,5,7))
p_yellow <- mean(balls == "yellow")

 #course answer
cyan <- 3
magenta <- 5
yellow <- 7

p_yellow <- yellow/(cyan+magenta+yellow)

  #Q3: If you roll a 6-sided die once, what is the probability of not seeing a 6? If you roll a 6-sided die six times, what is the 
      #probability of not seeing a 6 on any roll?

p_no6 <- 5/6
p_no6^6

  #Q4: What is the probability that the Celtics win at least one of seven games if the Cavs have a 60% chance of winning each game?
      #Remember that the Celtics must win one of the first four games, or the series will be over!      
      #Calculate the probability that the Cavs will win the first four games of the series.
      #Calculate the probability that the Celtics win at least one game in the first four games of the series.
  
p_cavs_win4 <- 0.60^4
1-p_cavs_win4

  #Q5: Create a Monte Carlo simulation to confirm your answer to the previous problem by estimating how frequently the Celtics win
      #at least 1 of 4 games. Use 10000 simulations. The provided sample code simulates a single series of four random games, simulated_games

B <- 10000
set.seed(1)
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win")
})
mean(celtic_wins)

###### Section 1.3: Addition rule and Monty Hall ######

  #Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. 
  #The teams are equally good, so they each have a 50-50 chance of winning each game.

  #Q1: If the Cavs lose the first game, what is the probability that they win the series?

  #Initial answer
n<-6
outcomes<-c(0,1)
l<-list(outcomes)
l<-rep(l, times = n)
l<-expand.grid(l)
mean(rowSums(l) >= 4)

  #alternative answer I thought of while attempting Q2
outcomes<-c(0,1)
games<-permutations(2,6, v = outcomes, repeats.allowed = TRUE)
mean(rowSums(games) >= 4)

  #Q2: Confirm the results of the previous question with a Monte Carlo simulation to estimate the probability of the Cavs winning the series
      #after losing the first game.

B<-10000
set.seed(1)
results<-replicate(B,{
  outcomes<-c(0,1)
  games<-sample(outcomes,6, replace = TRUE)
  sum(games) >= 4
  })
mean(results)

  #Q3: Teams A and B are playing a seven series game series. Team A is better than team B and has p > .5 chance of winning each game.
      #Assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)
      #Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
      #Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win.
Pr <- sapply(p, prob_win)

      #Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

  #Q4:Repeat the previous exercise, but keep the probability that team A wins fixed at p <- 0.75 and compute the probability for 
      #different series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.

N<-seq(1,25,2)

prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

Pr <- sapply(N, prob_win)
plot(N,Pr)
