###############################
####### REPLICATION ###########
###############################
###REPLICATION FOR FIGURE 3
##Libraries
library(arm)
library(foreign)
library(sp)
library(Matching)

#---------------------------------------------
##Dataset
load("MainDataset.RData")

#this polygon dataframe has two parts:
#crd for communities before 1999 
#and pol for communities after 1999

#the figures for replication only 
#deals with data before 1999
attach(crd@data)

#---------------------------------------------
##Create a dataframe with the necessary variables 
#for figure 3
foo <- cbind(Dwel88, distTreb,blacha88)
foo <- data.frame(foo)
#clean the data: leave out the missing entries
foo <- foo[complete.cases(foo),]
detach(crd@data)

#---------------------------------------------
#Calculate the y-axis: proportion of 
#dwelling with metal roofs
proportion <- rep(0, nrow(foo))
for(i in (1:nrow(foo))){
  proportion[i] <- foo$blacha88[i]/foo$Dwel88[i]
}
#add to the data frame 
foo["proportion"] <- proportion

#---------------------------------------------
#The figure only considers the distance within 50 km
#so we subset a smaller dataset
foo_used <- foo[which(foo$distTreb <= 50),]

#---------------------------------------------
#Logit regression
logit <- glm(foo_used$proportion ~ log(foo_used$distTreb))
summary(logit)

#Simulation
iterations = 10000
sim.logit <- sim(logit,n.sims=iterations)

#Predict the proportion for different distance values
distances = seq(5,50,0.5)
len = length(distances)
#create an empty matrix to store the predicted values
ys <- matrix(rep(0,len*iterations),nrow=len,ncol=10000)

#fill in the matrix:
i = 0
#like the original paper, 
#for every distance, we calculate the log of the distance
#and multiply it with the coefficient from the logit model
#we adjust the column and row in the dlog matrix
#as well as transposing the coeficient matrix of the logit
for (d in distances) {
  i = i + 1
  dlog <- matrix(c(1,log(d)),nrow=1,ncol= 2)
  ys[i,] <- dlog %*% t(sim.logit@coef)
}

#---------------------------------------------
#Calculate the 95% confidence interval
#and the regression line
#Calculate the bound for 95% CI
lbound = rep(0,len)
mid = rep(0,len)
ubound = rep(0,len)

for (i in (1:len)) {
  lbound[i] = quantile(ys[i,],0.025)
  mid[i] = quantile(ys[i,],0.5)
  ubound[i] = quantile(ys[i,],0.975)
}

#---------------------------------------------
#Plot the figure
plot(x = c(1:50),
     xlim=c(5,50),
     ylim=c(0.15,0.75),
     xlab = "Distance to Treblinka, km",
     ylab = "Proportion",
     main = "Dwellings with metal roofs (1988)")

#add the line segments for each distance values
i = 0 
for (d in distances) {
  i = i + 1
  segments(
    #lower bound value
    x0 = d,y0 = lbound[i],
    #upper bound value
    x1 = d,y1 = ubound[i],
    col="blue"
  )
}
#add the regression line
lines(x = distances,y = mid,type="l",lwd=2)

###############################
####### EXTENSION #############
###############################

###PROPENSITY SCORE MATCHING

attach(crd@data)
##Create a dataframe with the necessary variables 
foo_m <- cbind(Dwel88, distTreb,blacha88,CityDistKm,
               Trade82,distRail45,Pop1988,Handicr82s)
foo_m <- data.frame(foo_m)
#clean the data: leave out the missing entries
foo_m <- foo_m[complete.cases(foo_m),]

#---------------------------------------------
#Calculate the y-axis: proportion of 
#dwelling with metal roofs
proportion <- rep(0, nrow(foo_m))
for(i in (1:nrow(foo_m))){
  proportion[i] <- foo_m$blacha88[i]/foo_m$Dwel88[i]
}
#add to the data frame 
foo_m["proportion"] <- proportion
detach(crd@data)

#---------------------------------------------
##Create the treatment and control group
which.treat <- which(foo_m$distTreb <= 50)
which.control <- which(foo_m$distTreb >= 50)
treat <- rep(0, nrow(foo_m))
treat[which.treat] <- 1
#add to the dataframe
foo_m["treat"] <- treat
head(foo_m)

#---------------------------------------------
##Matching
#Calculate the propensity scores
lm.pscore <- glm(treat~ CityDistKm + Trade82 +distRail45+
                   Pop1988+Handicr82s,
                 data=foo_m, family = "binomial")
summary(lm.pscore)
X <- lm.pscore$fitted.values
Y=foo_m$proportion

#Treatment effect
mout_pscore <-  Match(Y=Y, Tr = treat, X=X,
                      replace = TRUE)
summary(mout_pscore) 

#Balance check
mb_mout_pscore <- MatchBalance(treat~ CityDistKm +Trade82+distRail45+
                                 Pop1988+Handicr82s,
                               data=foo_m,
                               match.out = mout_pscore,
                               nboots = 5000)
#before min p: 0.006
#after min p: 0.0506


#---------------------------------------------
###GENETIC MATCHING WITH PROPENSITY SCORE
#Match on all the variables
x <- cbind(lm.pscore$fitted.values,foo_m$CityDistKm,
           foo_m$Trade82, foo_m$distRail45,
             foo_m$Pop1988, foo_m$Handicr82s)
genout <- GenMatch(Tr = treat, X=X, 
                   pop.size=200, wait.generations=25)

matchout.gen <- Match(Y=Y, X = X, Tr = treat, 
                      Weight.matrix=genout)
mb.out <- MatchBalance(treat~ CityDistKm +Trade82+distRail45+
                         Pop1988+Handicr82s,
                       data=foo_m, match.out = matchout.gen,
                       nboots=1000)
#before p min: 0.005
#after p min: 0.014
