ptions(digits = 3) # report 3 significant digits
library(tidyverse)
library(titanic)
library(dslabs)
library(gtools)


titanic <- titanic_train %>%
    select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
    mutate(Survived = factor(Survived),
           Pclass = factor(Pclass),
           Sex = factor(Sex))

titanic %>% group_by(Sex) %>% ggplot(aes(x = Age, fill = Sex)) + geom_histogram(binwidth = 1)

#count the number of each sex group
titanic %>%
    ggplot(aes(Age, y = ..count.., fill = Sex)) +
    geom_density(alpha = 0.2, position = "stack")

#density diagram 
titanic %>%
    ggplot(aes(Age, fill = Sex)) +
    geom_density(alpha = 0.2)
#show the general shap of the 2 distributions 
titanic %>%
    ggplot(aes(Age, fill = Sex)) +
    geom_density(alpha = 0.2) +
    facet_grid(Sex ~ .)

#creating a qq plot
#create parameter
params <- titanic %>%
    filter(!is.na(Age)) %>%
    summarize(mean = mean(Age), sd = sd(Age))
#draw the qq plot
titanic %>%
    filter(!is.na(Age)) %>%
    ggplot(aes(sample = Age)) +
    geom_qq(dparams = params) +
    geom_abline() #theorical line


#plot 1 - survival filled by sex
titanic %>%
    ggplot(aes(Survived, fill = Sex)) +
    geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
    ggplot(aes(Survived, fill = Sex)) +
    geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
    ggplot(aes(Sex, fill = Survived)) +
    geom_bar()

titanic %>%
    ggplot(aes(Age, fill = Survived)) +
    geom_density(alpha = 0.2)

#drax the number of death versus ages
titanic %>%
    ggplot(aes(Age, y = ..count.., fill = Survived)) +
    geom_density(alpha = 0.2, position = "stack")

titanic %>%
    filter(Fare > 0) %>%
    ggplot(aes(Survived, Fare)) +
    geom_boxplot() +
    scale_y_continuous(trans = "log2") +
    geom_jitter(alpha = 0.2)

titanic %>%
    ggplot(aes(Survived, fill = Pclass)) +
    geom_bar(position = position_dodge())
titanic %>%
    ggplot(aes(Survived, fill = Pclass)) +
    geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
    geom_density(alpha = 0.2) + facet_grid(Sex ~ Pclass)
titanic %>%
    ggplot(aes(Age, y = ..count.., fill = Survived)) +
    geom_density(position = "stack") +
    facet_grid(Sex ~ Pclass)
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)
entree_choices <- function(x) {
  6 * nrow(combinations(x, 2)) * 3
}

combos <- sapply(2:12, entree_choices)

data.frame(entrees = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

data(esoph)

esoph %>%
  filter(ncases != 0 & tobgp != "0-9g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()
all_cases <- sum(esoph$ncontrols)
tob_cases / all_cases
#highest alcohol group
high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alc <- high_alc_cases / all_cases
p_case_high_alc
#highest tabac group
high_tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
p_case_high_tob <- high_tob_cases / all_cases
p_case_high_tob
#intersect
high_alc_tob_cases <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc_tob <- high_alc_tob_cases / all_cases
p_case_high_alc_tob
#union
p_case_either_highest <- p_case_high_alc + p_case_high_tob - p_case_high_alc_tob
p_case_either_highest


high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
p_case_high_alc <- high_alc_cases / all_cases
p_case_high_alc

#ACT
set.seed(16)
act_avg <- 20.9
act_sd <- 5.7
act_score <- rnorm(10000, act_avg, act_sd)
mean(act_score <= 10)
sum(act_score >= 36)
normalized_act_score <- (act_score - mean(act_score)) / sd(act_score)
mean(normalized_act_score > 2)
qnorm(0.975, mean(act_score), sd(act_score))
prob <- function(x) {
  mean(act_score <= x)
}
x <- 1:36
cdf <- sapply(x, prob)
df <- data.frame(x = x, CDF = cdf)
df %>% ggplot(aes(x = x, y = CDF)) + geom_line()
min(which(cdf >= .95))
qnorm(0.95, act_avg, act_sd)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_score, p)
theorical_quantiles <- qnorm(p, 20.9, 5.7)

quantiles <- data.frame(samples = sample_quantiles, theorical = theorical_quantiles)
quantiles %>% ggplot(aes(x = samples, y = theorical)) + geom_point() + geom_abline()

#SAT Test
#44 question
#5 choices 
#-0.25 penality
#+1 good anwser
penality <- 0
graded <- 1
nb_questions <- 44
good_guess <- 1 / 4
bad_guess <- 1 - good_guess
expected_value <- penality * bad_guess + graded * good_guess
standard_error <- abs(penality - graded) * sqrt(bad_guess * good_guess) * sqrt(nb_questions)
1 - pnorm(8, expected_value, standard_error)
set.seed(21)
B <- 10000
S <- replicate(B, {
  x <- sample(c(graded, penality), nb_questions, replace = TRUE, prob = c(good_guess, bad_guess))
  sum(x)
})
mean(S >= 8) #score equal or grater than 8
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x) {
  mu <- expected_value*nb_questions
  sigma <- standard_error
  1 - pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

#Q3 casino
win <- 5/38
not_win <- 1-win
bets <- 500
gain <- 6
loss<- -1
ev <- loss*not_win+win*gain
ev
se <- abs(loss-gain)*sqrt(not_win*win)
se
pnorm(0,bets*ev,se*sqrt(bets))