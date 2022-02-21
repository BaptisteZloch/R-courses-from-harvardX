#CDF ploting
a <- seq(min(my_data), max(my_data), length = 100) # define range of values spanning the dataset
cdf_function <- function(x) {
  # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

stdDev <- sqrt(sum((x - mean(x)) ^ 2 / length(x)))

z <- scale(x) #standard unit N(0,1)
pnorm(a, avg, s) #Loi normale

mean(x <= 72) - mean(x <= 69) #proportion of the population between 72 and 69
pnorm(72, avg, stdev) - pnorm(72, avg, stdev) # does the same thing
qnorm(p, mu, sigma) #give the theoretical value of a quantile with probability p

pnorm(qnorm(0.025)) = 0.025 #inverse functions
theoretical_quantiles <- qnorm(p, 69, 3) #theorical quantile
quantile(x, vector_pourcent) #give the quantile of our datas
#plot the relation between theorical and real quantile 
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles) #callaed qq plot
abline(0, 1)
#Several types of
murder
geom_point(aes(x = population / 10 ^ 6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population / 10 ^ 6, total))

# add text layer to scatterplot
p + geom_point(aes(population / 10 ^ 6, total)) +
    geom_text(aes(population / 10 ^ 6, total, lab))

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population / 10 ^ 6, total, label = abb))
p + geom_point(size = 3) +
    geom_text(nudge_x = 1.5)


s <- heights %>% filter(sex == "Male") %>% summarize(average = mean(height), Standar_deviation = sd(height)) #equiv view in SQL
#produce summarize table 
#only function that return single value

heights %>% group_by(sex) %>% summarize(average = mean(height), stdev = sd(height)) #return the average and stddev of male and female as SQL would do
#order by
murders %>% arrange(population) %>% head() #order the state by population in ascending way
murders %>% arrange(desc(population)) %>% head() #order the state by population in descending way
murders %>% arrange(region, population) %>% head() #order by region then by population
murders %>% top_n(10, murder_rate) #return the top 10 here

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
    ggplot(aes(x, y, color = col)) +
    geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

#slope chart 
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
    filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10 ^ 7)

dat %>%
    mutate(location = ifelse(year == 2010, 1, 2),
           location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                             location + 0.22, location),
           hjust = ifelse(year == 2010, 1, 0)) %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
    xlab("") +
    ylab("Life Expectancy")
#Altman-Bland plot
library(ggrepel)
dat %>%
    mutate(year = paste0("life_expectancy_", year)) %>%
    select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
    mutate(average = (life_expectancy_2015 + life_expectancy_2010) / 2,
                difference = life_expectancy_2015 - life_expectancy_2010) %>%
    ggplot(aes(average, difference, label = country)) +
    geom_point() +
    geom_text_repel() +
    geom_abline(lty = 2) +
    xlab("Average of 2010 and 2015") +
    ylab("Difference between 2015 and 2010")

# 3 dimensionnal plot
# Tile plot of measles rate by year and state
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
    filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
    mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
    mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
    ggplot(aes(year, rate)) +
    geom_line() +
    ylab("Cases per 10,000") +
    geom_vline(xintercept = 1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill = rate)) +
    geom_tile(color = "grey50") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
    geom_vline(xintercept = 1963, col = "blue") +
    theme_minimal() + theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")

#Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
    filter(disease == the_disease) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm = TRUE) / sum(population, na.rm = TRUE) * 10000)

# make line plot of measles rate by year by state
dat %>%
    filter(!is.na(rate)) %>%
    ggplot() +
    geom_line(aes(year, rate, group = state), color = "grey50",
        show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") +
    geom_text(data = data.frame(x = 1955, y = 50),
        mapping = aes(x, y, label = "US average"), color = "black") +
    geom_vline(xintercept = 1963, col = "blue")
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
    ggplot(aes(x_hat)) +
    geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
    ggplot(aes(sample = x_hat)) +
    stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
    geom_abline() +
    ylab("X_hat") +
    xlab("Theoretical normal")
grid.arrange(p1, p2, nrow = 1)
