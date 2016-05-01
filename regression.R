library(data.table)
library(barvs)
library(BMA)
library(BayesLogit)
library(dplyr)

data = clayton_all
# make vector of 1's and 0's for binary regression
fast = c()
sliders = c()
#pitches = data$pitch_type
pitches = clayton

for (i in 1:length(pitches)){
	if (any(pitches[i] == (c("FA", "FF", "FT", "FC")))){
		fast[i] = 1

	} else {fast[i] = 0}

	if (pitches[i] == "SL"){

		sliders[i] = 1
	} else {sliders[i] = 0}

}

# number of unique pitches
J = nlevels(pitches)

# contingency tables: some exploratory data analysis
pitches = factor(data$pitch_type)
outs_table = table(factor(data$pre_outs), fast)
balls_table = table(factor(data$pre_balls), fast)



# design matrices for input variables
# var1: outs
outs = factor(clayton_all$pre_outs)
X1.all = model.matrix(~ outs - 1)
X1    = X1.all[, -3]

# var2: balls
balls = factor(clayton_all$pre_balls)
X2.all = model.matrix(~ balls - 1)
X2    = X2.all[, -4]

# var3: strikes
strikes = factor(clayton_all$pre_strikes)
X3.all = model.matrix(~ strikes - 1)
X3    = X3.all[, -3]


# testing variable selection from BMA package
# all data for binary regression
data_reg = data.frame(y, X1, X2, X3)
output = bic.glm(y ~ outs0 + outs1 + balls0 + balls1 + balls2 + strikes0 + strikes1,
 		glm.family="binomial", 
 		data = data_reg)
summary(output)


# BAYESIAN BINARY LOGISTIC REGRESSION  

# X design matrix: colnames are the variables
X = cbind(1, X1, X2, X3)

# number of parameters
P = ncol(X)

# run model
model1 = logit(sliders, X, n = rep(1, length(sliders)),
samp = 2000, burn = 500)

# mean of posterior beta
post_beta = colMeans(model1$beta)

# predict new probability for new_x
new_x = c(0, 0, 0, 0, 0, 0, 0) 
prob = (1 + exp(-new_x %*% post_beta))^(-1)



# MULTINOMIAL LOGISTIC REGRESSION
P = ncol(X)
X = cbind(1, X1, X2, X3)
J = length(unique(clayton))

# design matrix for y's
Y.all = model.matrix(~ clayton - 1)
Y = Y.all[, -J]

# run model
multi_out = mlogit(Y, X, n = rep(1, length(clayton)), samp = 1000, burn = 100)

betas = multi_out$beta
post_betas = matrix(0, nrow = J, ncol = P)

# get posterior means and put in formula
for (j in 1:J-1){
	post_betas[j,] = colMeans(betas[,,j])
}

post_betas[J] = 0

new_x = c(1,1,0,0,0,0,1,0)
probs = c()
for (j in 1:J){
	probs[j] = exp(new_x %*% post_betas[j,])
}

# print probability vector
prob_vec = probs / sum(probs)
prob_vec

pitches_names = names(pitches)
next_pitch = sample(x = pitches_names, size = 1, replace = TRUE, prob = next_probs)
# model cross validation 

# test on training, testing 

# model checking


# compare 3 things for same pitcher:
# the regression model, the markov model
# the naive transition matrix

# compare: regresssion model for 1 pitcher vs all pitchers
# accuracy results 