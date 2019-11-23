#Question 2
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo)
dim(foo)

#Build the two models:
#The original model:
glm1 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4,
            data = foo, family = binomial)

#The new model
glm.new <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
                     trnsfcap + develop + exp + decade + treaty + untype4 + exp*untype4 + wardur*logcost, 
                   data = foo, family = binomial)

#Hold the predictors at their means:
mean.wartype <- mean(foo$wartype)
mean.logcost <- mean(foo$logcost)
mean.factnum <- mean(foo$factnum)
mean.factnum2 <- mean(foo$factnum2)
mean.trnsfcap <- mean(foo$trnsfcap)
mean.develop <- mean(foo$develop)
mean.exp <- mean(foo$exp)
mean.decade <- mean(foo$decade)
mean.treaty <- mean(foo$treaty)

#Obtain logistic regression predictions using the general function:
get_logit <- function(X, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*X)
  return(exp(logit) / (1 + exp(logit)))
}

##THE ORGINAL MODEL:
#We need 2 vectors to store the predicted values of the original model:
storage.original.treat <- rep(NA, 315)
storage.original.control <- rep(NA, 315)

#Calculate the prediction using the orignal model:
for (wardur in 1:315) {
  
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0)
  
  storage.original.treat[wardur]  <- get_logit(X.treat, coef(glm1))
  storage.original.control[wardur]  <- get_logit(X.control, coef(glm1))
}

#Calculate the treatment effect from this model:
original_y <- storage.original.treat - storage.original.control

##THE NEW MODEL
#We need 2 vectors to store the predicted values of the new model:
storage.new.treat <- rep(NA, 315)
storage.new.control <- rep(NA, 315)

#Calculate the prediction using the new model:
for (wardur in 1:315) {
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1, mean.exp*1,wardur*mean.logcost)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0, mean.exp*0,wardur*mean.logcost)
  storage.new.treat[wardur]  <- get_logit(X.treat, coef(glm.new))
  storage.new.control[wardur]  <- get_logit(X.control, coef(glm.new))
}

#Calculate the treatment effect from this model:
new_y <- storage.new.treat - storage.new.control

#Plotting
plot(1:315, original_y, ylim=c(0,1), type='l', 
     lty='dotted', xlab='Duration of wars in months',
     ylab='Marginal effects of UN peacekeeping operations')
lines(1:315, new_y, ylim=c(0,1))
text(60, .85, "Model with interaction term", cex=.8)
text(60, .6, "Dotted: Original model", cex=.8)

##Question 4
##PROCESS DATA
#import data again
foo1 <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

#extract the relevant column as in question 1, but adding uncint (predictors)
#pbs21 and pbs51 as outcome variables
foo1 <- foo1[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 52, 34, 35)]

#check to see if we have NA and remove NA
which(is.na(foo1$uncint)==TRUE) # no na in uncint
which(is.na(foo1$pbs2l)==TRUE)
which(is.na(foo1$pbs5l)==TRUE) #NA in 4 16 84 93 98
foo1 <- foo1[c(-19, -47, -4, -16, -84, -93, -98), ]
which(is.na(foo1$pbs5l)==TRUE)

#set the treatment and variables to a number for later calculation:
which.none <- which(foo1$uncint == 'None')
which.observer <- which(foo1$uncint == 'Observer')
which.pko <- which(foo1$uncint == 'PKO')
which.enforcement <- which(foo1$uncint == 'Enforcement')

#create a vector for treatment indication, set everything to 0
treat <- rep(0,length(foo1$uncint))
#add the treated
treat[which.observer] <- 1
treat[which.pko] <- 1
treat[which.enforcement] <- 1

#replace the original uncint column with the new one indicating treatment
foo1$uncint <- treat

#-----------------------------------------------------------
##LOGISTIC REGRESSION
#matching
library(Matching)
mb.logit <- MatchBalance(treat ~ wartype + logcost + wardur + factnum + 
                           factnum2 + trnsfcap + develop + 
                           exp + decade + treaty, data=foo1, nboots=500,)
#p-value: 0.0001
##effect in 2 yr (pbs21)
lm.fit.2yr <- glm(pbs2l~treat+wartype+logcost+wardur+factnum+factnum2+
                    trnsfcap+develop+exp+decade+treaty,
                  data=foo1, family='binomial')
summary(lm.fit.2yr)

#treatment effect:
#build a function to calculate logistic regression, taking input as
#the vector of variables we want to consider,
#and the coefficients
#return the predicted value using the logit model
get_logit <- function(coefs, vec) {
  num_vec <- append(vec, 1, after=0)
  logit <- sum(coefs*num_vec)
  prob <- 1/(1+exp(-logit))
  return(prob)
}

#calculate treatment effect by subtracting the predicted value
#for the control from the treated
#return the average treatment effect
get_treatment_effect <- function(coefs){
  storage_treatment_effect <- c()
  for (i in 1:nrow(foo1)){
    row <- foo[i,]
    vec.tr <- c(1, row$wartype, row$logcost, row$wardur, row$factnum,
                   row$factnum2,row$trnsfcap, row$develop, row$exp, row$decade, row$treaty)
    vec.ctrl <- c(0, row$wartype, row$logcost, row$wardur, row$factnum,
                     row$factnum2,row$trnsfcap, row$develop, row$exp, row$decade, row$treaty)
    treat_effect <- get_logit(coefs, vec.tr) - get_logit(coefs, vec.ctrl)
    storage_treatment_effect[i] <- treat_effect
  }
  return(sum(storage_treatment_effect)/length(storage_treatment_effect))
}
coefs_2yr <-lm.fit.2yr$coefficients
tr_effect_logit_2yr <- get_treatment_effect(coefs_2yr)
tr_effect_logit_2yr #result: 0.09684871

##effect in 5 yr (pbs51)
lm.fit.5yr <- glm(pbs5l~treat+wartype+logcost+wardur+factnum+factnum2+
                    trnsfcap+develop+exp+decade+treaty,
                  data=foo1, family='binomial')
summary(lm.fit.5yr)
#treatment effect, using the function built above to calculate
coefs_5yr <- lm.fit.5yr$coefficients
tr_effect_logit_5yr <- get_treatment_effect(coefs_5yr)
tr_effect_logit_5yr #result: 0.1327706

#------------------------------------------------------------
#PROPENSITY SCORE MATCHING
#Calculate the propensity scores
lm.pscore <- glm(treat~wartype+logcost+wardur+factnum+factnum2+
                   trnsfcap+develop+exp+decade+treaty+I(wardur*wartype),
                 data=foo1, family = "binomial")
summary(lm.pscore)
X <- lm.pscore$fitted.values
Y1=foo1$pbs2l
Y2=foo1$pbs5l

#treatment effect calculation:
#2 years:
mout_pscore_2yr <- Match(Y=Y1, Tr = treat, X=X,replace = TRUE,
                         estimand = "ATT", ties = TRUE)

summary(mout_pscore_2yr) # 0.060606 ,ste: 0.17277

#5 years:
mout_pscore_5yr <- Match(Y=foo1$pbs5l, Tr = treat, X=X,replace = TRUE,
                         estimand = "ATT", ties = TRUE)
summary(mout_pscore_5yr)  # 0.090909 ,ste: 0.17682 

#Balance check:
mb.pscore_2yr <- MatchBalance(treat ~ wartype+logcost+wardur+factnum+factnum2+
                            trnsfcap+develop+exp+decade+treaty+I(wardur*wartype),
                          data=foo1, match.out = mout_pscore_2yr, nboots=5000)
#p-value: 0.113 
mb.pscore_5yr <- MatchBalance(treat ~ wartype+logcost+wardur+factnum+factnum2+
                                trnsfcap+develop+exp+decade+treaty+I(wardur*wartype),
                              data=foo1, match.out = mout_pscore_5yr, nboots=5000)
#p-value: 0.1164

#------------------------------------------------------
##GENETIC MATCHING WITH PROPENSITY SCORE
#Match on all variables:
X <- cbind(lm.pscore$fitted.values,foo1$wartype, foo1$logcost, foo1$wardur, foo1$factnum, foo1$factnum2,
           foo1$trnsfcap, foo1$develop, foo1$exp, foo1$decade, foo1$treaty)
genout <- GenMatch(Tr = treat, X=X, pop.size=200, wait.generations=25)
# 2.867543e-01 
#2 YEARs
matchout.gen <- Match(Y=Y1, X = X, Tr = treat, Weight.matrix=genout)
mb.out <- MatchBalance(treat ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+
                         develop+exp+decade+treaty,
                       data=foo1, match.out = matchout.gen, nboots=1000)
#after matching min p-value:  0.251
summary(matchout.gen) #0.15152 # 0.13688 

#5 YEARs
matchout.gen1 <- Match(Y=Y2, X = X, Tr = treat, Weight.matrix=genout)
mb.out1 <- MatchBalance(treat ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+
                         develop+exp+decade+treaty,
                       data=foo1, match.out = matchout.gen1, nboots=1000)
#after matching min p-value:  0.221  
summary(matchout.gen1) #0.18182 0.12921 

#--------------------------------------------------------------------
##GENETIC MATCHING (W/O PROPENSITY SCORE)
#Match on all variables:
X <- cbind(foo1$wartype, foo1$logcost, foo1$wardur, foo1$factnum, foo1$factnum2,
           foo1$trnsfcap, foo1$develop, foo1$exp, foo1$decade, foo1$treaty)
genout2 <- GenMatch(Tr = treat, X=X, pop.size=200, wait.generations=25)
#2.610180e-01
#2 YEARs
matchout.gen2 <- Match(Y=Y1, X = X, Tr = treat, Weight.matrix=genout2)
#2.867543e-01
mb.out2 <- MatchBalance(treat ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+
                         develop+exp+decade+treaty,
                       data=foo1, match.out = matchout.gen2, nboots=1000)
#after matching min p-value:  0.238 
summary(matchout.gen2) #0.15152 #STD:0.13181 

#5 YEARs
matchout.gen3 <- Match(Y=Y2, X = X, Tr = treat, Weight.matrix=genout2)
mb.out3 <- MatchBalance(treat ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+
                         develop+exp+decade+treaty,
                       data=foo1, match.out = matchout.gen3, nboots=1000)
#after matching min p-value:  0.246  
summary(matchout.gen3) #0.18182 #STD: 0.12443
