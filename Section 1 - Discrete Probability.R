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

###### Section 1.4: Assessment: Discrete Probability
library(gtools)
library(tidyverse)
options(digits = 3)

### Question 1: Olympic running

#In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of 8 runners
#were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).

  #1a:How many different ways can the 3 medals be distributed across 8 runners?

nrow(permutations(8,3))

  #1b: How many different ways can the three medals be distributed among the 3 runners from Jamaica?

nrow(permutations(3,3))

  #1c:What is the probability that all 3 medals are won by Jamaica?

Pr <- nrow(permutations(3,3))/nrow(permutations(8,3))

  #1d: Run a Monte Carlo simulation on the vector below representing the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

    #For each iteration of the Monte Carlo simulation, within a replicate loop, select 3 runners representing the 3 medalists and 
    #check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
    #Calculate the probability that all the runners are from Jamaica.

set.seed(1)
B <- 10000
results <- replicate(B,{
  runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
  medalists <- sample(runners, 3)
  all(medalists == "Jamaica")
  })
mean(results)

### Question 2: Restaurant management

#A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year.
#He wants to change his special if needed to allow at least 365 choices. A meal at the restaurant includes 1 entree, 2 sides, and 1 drink.
#He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 
#1 drink from a list of 2 options.

  #2a: How many meal combinations are possible with the current menu?

entree <- 6
side<- combinations(6,2)
drink <- 2

entree * nrow(side) * drink

  #2b: The manager has one additional drink he could add.How many combinations are possible if he expands the special to 3 drink options?

entree <- 6
side<- combinations(6,2)
drink <- 3

entree * nrow(side) * drink

  #2c: The manager decides to add the third drink but needs to expand the number of options. He would prefer not to change his menu further and 
  #wants to know if he can meet his goal by letting customers choose more sides. How many meal combinations are there if customers can choose
  #from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?

entree <- 6
side<- combinations(6,3)
drink <- 3

entree * nrow(side) * drink

  #2d: The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices
  #instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to
  #offer in order to meet his goal.
  #Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree
  #options, 3 drink choices, and a selection of 2 sides from 6 options. Use sapply to apply the function to entree option counts ranging
  #from 1 to 12. What is the minimum number of entree options required in order to generate more than 365 combinations?

n <- 1:12
meals <- function(n){
  n * nrow(combinations(6,2)) * 3
}

total_meals <- sapply(n, meals)

data.frame(entrees = n, meals = total_meals) %>%
  filter(meals > 365) %>%
  min(.$entrees)

  #2e: The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to 
  #expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
  #Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 
  #3 drink choices, and a selection of 2 sides from the specified number of side choices. Use sapply to apply the function to side counts
  #ranging from 2 to 12. What is the minimum number of side options required in order to generate more than 365 combinations?

n <- 2:12
meals <- function(n){
  6 * nrow(combinations(n,2)) * 3
}

total_meals <- sapply(n, meals)

data.frame(sides = n, meals = total_meals) %>%
  filter(meals > 365) %>%
  min(.$sides)

### Question 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1

#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. 
#The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer 
#(cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a 
#variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake
#in grams per day (tobgp) across cases and controls grouped by age range (agegp).

  #3a: How many groups are in the study?

agegp <- nlevels(esoph$agegp) #while the possible number of combinations is the multiplication of the levels of all factor variables,
alcgp <- nlevels(esoph$alcgp) #the dataset does not have data points for all of these possible cobinations. Upon observing the
tobgp <- nlevels(esoph$tobgp) #data with commands such as view(esoph), it becomes clear there is only one datum point for only some
                              #combinations. This, using the nrow command would show the number of possible combinations for which we
groups <- agegp*alcgp*tobgp   #do have at least one datum.
nrow(esoph)

  #3b: How many cases are there? Save this value as all_cases

all_cases <- sum(esoph$ncontrols)

  #4a: What is the probability that a subject in the highest alcohol consumption group is a cancer case?

  #My answer

esoph_high <- esoph %>%
  filter(alcgp == "120+")
sum(esoph_high$ncases)/sum (sum(esoph_high$ncases), sum(esoph_high$ncontrols))

  #Course answer

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

  #4b: What is the probability that a subject in the lowest alcohol consumption group is a cancer case?

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

  #4c: Given that a person is a case, what is the probability that they smoke 10g or more a day?

esoph %>%
  filter(!ncases == 0) %>%
  summarize(tobgp_10 = sum(ncases[!tobgp == "0-9g/day"]),
            tobgp = sum(ncases)) %>%
  mutate(p_smoke = tobgp_10/tobgp) %>%
  pull(p_smoke)

  #4d: Given that a person is a control, what is the probability that they smoke 10g or more a day?

esoph %>%
  summarize(tobgp_10 = sum(ncontrols[!tobgp == "0-9g/day"]),
            tobgp = sum(ncontrols)) %>%
  mutate(p_smoke = tobgp_10/tobgp) %>%
  pull(p_smoke)

### Question 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2

  #5a: For cases, what is the probability of being in the highest alcohol group?

  #My answer
p_top_alc <- esoph %>%
  summarize(top_al = sum(ncases[alcgp == "120+"]),
            tot_al = sum(ncases)) %>%
  mutate(prob_top_al = top_al/tot_al) %>%
  pull(prob_top_al)
  
  #course answer
high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alc <- high_alc_cases/all_cases

  #5b: For cases, what is the probability of being in the highest tobacco group?

p_top_tob <- esoph %>%
  summarize(top_tob = sum(ncases[tobgp == "30+"]),
            tot_tob = sum(ncases)) %>%
  mutate(prob_top_tob = top_tob/tot_tob) %>%
  pull(prob_top_tob)

  #5c: For cases, what is the probability of being in the highest alcohol group AND the highest tobacco group?
        #Probability of both highest alcohol and highest tobacco divided by total ncases

cases_both <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(cases_both = sum(ncases)) %>%
  pull(cases_both)

p_alc_tob <- cases_both/sum(esoph$ncases)

  #5d: For cases, what is the probability of being in the highest alcohol group OR the highest tobacco group?
        #Pr(A or B) = Pr(A)+Pr(B)-Pr(A & B)

p_either <- p_top_alc + p_top_tob - p_alc_tob

  #6a: For controls, what is the probability of being in the highest alcohol group?

p_top_alc_control <- esoph %>%
  summarize(top_al = sum(ncontrols[alcgp == "120+"]),
            tot_al = sum(ncontrols)) %>%
  mutate(prob_top_al = top_al/tot_al) %>%
  pull(prob_top_al)

  #6b: How many times more likely are cases than controls to be in the highest alcohol group?

p_top_alc/p_top_alc_control

  #6c: For controls, what is the probability of being in the highest tobacco group?

p_top_tob_control <- esoph %>%
  summarize(top_tob = sum(ncontrols[tobgp == "30+"]),
            tot_tob = sum(ncontrols)) %>%
  mutate(prob_top_tob = top_tob/tot_tob) %>%
  pull(prob_top_tob)

  #6d: For controls, what is the probability of being in the highest alcohol group AND the highest tobacco group?

cases_both_control <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(cases_both = sum(ncontrols)) %>%
  pull(cases_both)

p_alc_tob_control <- cases_both_control/sum(esoph$ncontrols)

  #6e: For controls, what is the probability of being in the highest alcohol group OR the highest tobacco group?

p_either_control <- p_top_alc_control + p_top_tob_control - p_alc_tob_control

  #6f: How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?

p_either/p_either_control
