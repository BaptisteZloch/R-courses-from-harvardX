beads <- rep(c("red", "blue"), times = c(2, 3)) # create an urn with 2 red, 3 blue
beads # view beads object
sample(beads, 1) # sample 1 bead at random
B <- 10000 # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1)) # draw 1 bead, B times
tab <- table(events) # make a table of outcome counts
tab # view count table
prop.table(tab) # view table of outcome proportions

sample(beads, 5) #extract 5 beads in a random ordre without remplacement
#sample(beads,6) #gives an error because it's not possible to take 6 beads form an urn with 5 beads
events <- sample(beads, B, replace = TRUE) #do the same thing than replicate
prop.table(table(events)) #create a table with the same result
mean(beads == "blue") # compute the absolute probability of getting blue

paste("hello", "world") #concat the two strings
expand.grid(pants = c("blue", "black"), shirt = c("whitr", "grey", "plaid")) #create a vector contining all the combinaions between the two lists.

#creating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

#probability of getting a king
kings <- paste("King", suits)
mean(deck %in% kings)
library(gtools)
permutations(5, 2) # ways to choose 2 numbers in order from 1:5 no repetition
permutations(3, 2) # order matters
combinations(3, 2) # order does not matter


all_phone_numbers <- permutations(10, 7, v = 0:9) #choose 7 numbers each between 0 and 9 generate all phone numbers
n <- nrow(all_phone_numbers) #count of phone numbers
index <- sample(n, 5) #pick 5 among all numbers
all_phone_numbers[index,] #print it into the console


hands <- permutations(52, 2, v = deck) #choose 2 cards into the deck which contain 52 cards
first_card <- hands[, 1]
second_card <- hands[, 2]
sum(first_card %in% kings) #having a card in the first hand

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings) #having a card in the second hand

#black jack natural 21
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v = deck) # all possible hands
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[, 1] %in% aces & hands[, 2] %in% facecard)
# probability of a natural 21 checking for both ace first and ace second
mean((hands[, 1] %in% aces & hands[, 2] %in% facecard) | (hands[, 2] %in% aces & hands[, 1] %in% facecard))

#monte-carlo method does the same thing
# code for one hand of blackjack
hand <- sample(deck, 2)
hand
# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)



# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n) {
  prob_unique <- seq(365, 365 - n + 1) / 365 # vector of fractions for mult. rule
  1 - prod(prob_unique) # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob) # plot Monte Carlo results
lines(n, eprob, col = "red") # add line for exact prob

# Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat")) # puts prizes in random order
  prize_door <- doors[prize == "car"] # note which door has prize
  my_pick <- sample(doors, 1) # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1) # open door with no prize that isn't chosen
  stick <- my_pick # stick with original door
  stick == prize_door # test whether the original door has the prize
})
mean(stick) # probability of choosing prize door when sticking

# Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat")) # puts prizes in random order
  prize_door <- doors[prize == "car"] # note which door has prize
  my_pick <- sample(doors, 1) # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1) # open door with no prize that isn't chosen
  switch <- doors[!doors %in% c(my_pick, show)] # switch to the door that wasn't chosen first or opened
  switch == prize_door # test whether the switched door has the prize
})
mean(switch) # probability of choosing prize door when switching

#calculating probability using Monte-carlo simulation 
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s) # generate 800 normally distributed random heights
  max(simulated_data) # determine the tallest height
})
mean(tallest >= 7 * 12) # proportion of times that tallest person exceeded 7 feet (84 inches)

#density
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
    ggplot(aes(x, f)) +
    geom_line()
#CDF
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = pnorm(x)) %>%
    ggplot(aes(x, f)) +
    geom_line()

#monte carlo roulette
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]
# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19)) # 1000 independent draws
S <- sum(x) # total winnings = sum of draws

n <- 1000 # number of roulette players
B <- 10000 # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19)) # simulate 1000 spins
  sum(X) # determine total profit
})
mean(S < 0) # probability of the casino losing money
s <- seq(min(S), max(S), length = 100) # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame(S = S) %>% # make data frame of S for histogram
ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

