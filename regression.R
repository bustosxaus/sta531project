library(data.table)
library(barvs)
library(BMA)
library(BayesLogit)

data = fread("Pitchfx.csv")
data = data[which(data$pitch_type != "")]

# sample data
data = data[1:50000,]

# make vector of 1's and 0's for binary regression
fast = c()
sliders = c()
pitches = data$pitch_type

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



# design matrices for variables
# var1: outs
outs = factor(data$pre_outs)
X.all = model.matrix(~ outs - 1)
X1    = X.all[, -3]

# var2: balls
balls = factor(data$pre_balls)
X.all = model.matrix(~ balls - 1)
X2    = X.all[, -4]

# var3: strikes
strikes = factor(data$pre_strikes)
X.all = model.matrix(~ strikes - 1)
X3    = X.all[, -3]


# testing variable selection from BMA package
# all data for binary regression
data_reg = data.frame(y, X1, X2, X3)

output = bic.glm(y ~ outs0 + outs1 + balls0 + balls1 + balls2 + strikes0 + strikes1,
 		glm.family="binomial", 
 		data = data_reg)
summary(output)


# BAYESIAN BINARY LOGISTIC REGRESSION  

# X design matrix: colnames are the variables
X = cbind(X1, X2, X3)

# number of parameters
P = ncol(X)

# run model
model1 = logit(sliders, X, n = rep(1, length(sliders)),
samp = 2000, burn = 500)

# mean of posterior beta
post_beta = colMeans(model1$beta)

# predict new probability for new_x
new_x = c(1, 0, 0, 0, 0, 1, 0) 
prob = (1 + exp(-new_x %*% post_beta))^(-1)



# MULTINOMIAL LOGISTIC REGRESSION
J = length(unique(pitches))

# design matrix for y's
Y.all = model.matrix(~ pitches - 1)
Y = Y.all[, -J]

# run model
multi_out = mlogit(Y, X, n = rep(1, length(pitches)), samp = 1000, burn = 100)

betas = multi_out$beta
post_betas = matrix(0, nrow = J-1, ncol = P)

# get posterior means and put in formula
for (j in 1:J-1){
	post_betas[j,] = colMeans(betas[,,j])
}

probs = c()
for (j in 1:J-1){
	probs[j] = exp(new_x %*% post_betas[j,])
}

# print probability vector
prob_vec = probs / sum(probs)
prob_vec

# model cross validation 

# test on training, testing 

# model checking