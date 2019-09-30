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
      #Calculate the joint probability of both choosing a cyan ball on the first draw and a ball that is not cyan on the second draw using p_1 and p_2
    p_2 <- 12/14
    p_1 <- cyan/(cyan + magenta + yellow)
    p_1 * p_2
  #Q4: COnsider sampling w/replacement. What is the probability that the first draw is cyan and that the second draw is not cyan?
      #Calculate the probability p_2 of choosing a ball that is not cyan on the second draw, with replacement. Next, use p_1 and p_2 to 
      #calculate the probability of choosing a cyan ball on the first draw and a ball that is not cyan on the second draw (after replacing the first ball).
    p_1 <- cyan / (cyan + magenta + yellow)
    p_2 <- (magenta + yellow)/(cyan + magenta + yellow)
    p_1 * p_2
    
  ###### Section 1.2: Combinations and permutations ######
    
    
    

    