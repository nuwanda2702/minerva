install.packages("arm")

##Review the given code
# To accompany the reading for Lesson 3.1
# especially the 2-3 pages or so on simulation for regression.

# Just familiarize yourself with this, you are not expected to master
# or fully understand it...

# There is more on this topic later in the text (not assigned) on page 143, 144, etc.
# for those who are interested...
# install.packages("arm") # if you have never done this before..
library(arm)
library(Matching)
data(lalonde)

lm1 <- lm(lalonde$re78 ~ lalonde$age)

lm1$coef

# (Intercept) lalonde$age 
# 3946.18432    53.39136 

library(arm)

set.seed(123)

sim_results <- sim(lm1, n.sims = 20)

#An object of class "sim"
#Slot "coef":
#  (Intercept) lalonde$age
#[1,]    2529.596  125.747146
#[2,]    4068.453   50.264868
#[3,]    2459.907  128.231652
#[4,]    3388.157   45.228051
#[5,]    3673.356   75.663980
#[6,]    3021.909   87.822194
#[7,]    5227.719    9.208761
#[8,]    4049.076   36.388712
#[9,]    3766.603   73.614147
#[10,]    3468.195   74.248542
#[11,]    2949.824   80.297123
#[12,]    3313.671   77.240036
#[13,]    5386.850   -4.525489
#[14,]    4132.172   32.138917
#[15,]    4432.393   41.367413
#[16,]    4040.994   46.977430
#[17,]    2509.447  102.622745
#[18,]    5821.639  -22.415580
#[19,]    3694.763   58.069571
#[20,]    1391.671  162.997176

#Slot "sigma":
# [1] 6763.101 6370.051 6595.001 6278.686 6515.286 6358.705 6815.413 6872.779 6668.219 6367.108 6441.676 6485.399 6704.606 6461.708
#[15] 6850.913 6465.988 6641.988 6878.283 6608.084 6749.646

set.seed(232)

# 20 sims is too few to get reliable results
sim_results2 <- sim(lm1, n.sims = 10000)
mean(sim_results2@coef[,1])

# 3953 -- compare to 3946.18432 (when n.sims is big, e.g., 100K it's a better estimate)

mean(sim_results2@coef[,2])
# 53 -- compare to 53.39 (ditto re large number of simulations)

# take the std devs of the simulated coefficients and compare to the standard errors (use summary)

# use the quantile function to get 95% conf intervals of coefficients and compare to confint() estimates
# (?confint you don't know how to use it)
# you'll need something like quantile(sim_results2@coef[,1], probs = c(0.025, 0.975))

# you can use these simulated coefs on their own to produce a prediction (a predicted y) 
# for every simulated coefficient you've got...
# but that wouldn't incorporate the estimate of irreducible error built into your model.
# to get a full prediction interval, you need to produce a predicted y for every 
# row (i.e., for every simulated coefficient you've got) that ALSO INCLUDES a simulated sigma...
# like this:

first_set_of_simulated_ys_including_irreducible_error <- 
  sim_results2@coef[1,1]*rep(1, length(lalonde$age)) + 
  sim_results2@coef[1,2]*lalonde$age + 
  rnorm(length(lalonde$age), 0, sim_results2@sigma[1])

second_set_of_simulated_ys_including_irreducible_error <- 
  sim_results2@coef[2,1]*rep(1, length(lalonde$age)) + 
  sim_results2@coef[2,2]*lalonde$age + 
  rnorm(length(lalonde$age), 0, sim_results2@sigma[2])

# obviously, in practice, the above would be done via something like a loop
# and you could do it for each of your simulated estimates (in this case, 10K)
# and once you had these simulated ys, you could estimate prediction intervals of y
# for every x in your data set...

# We will discuss all this in 3.1

##Work on your partner data set.
rmse = function(m, o){
  sqrt(mean((m - o)^2))
}
mod <- lm(sameer$Luminosity~sameer$Attraction, data = sameer)
predictions <- predict(mod,test)
rmse_test <-rmse(test$Luminosity,predictions)
rmse_train <- rmse(sameer$Luminosity,mod)