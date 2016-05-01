library(data.table)
library(BayesLogit)
library(dplyr)
library(mlogitBMA)

source("kershaw.R")


#data = fread("Pitchfx.csv") %>%
#  	   mutate(game_date = dmy(game_date))

#data = data[which(data$pitch_type != "")]
#clayton_all = dplyr::filter(data, pitcher_id == "477132")  %>%
#              dplyr::filter(pitch_type != "IN") %>%
# 		      arrange(game_date, at_bat_number)
#clayton = clayton_all$pitch_type 



# contingency tables: some exploratory data analysis
#pitches = factor(data$pitch_type)
#outs_table = table(factor(data$pre_outs), fast)
#balls_table = table(factor(data$pre_balls), fast)




# design matrices for input variables
# var1: outs
outs = factor(kershaw$pre_outs)
X1.all = model.matrix(~ outs - 1)
X1    = X1.all[, -3]

# var2: balls
balls = factor(kershaw$pre_balls)
X2.all = model.matrix(~ balls - 1)
X2    = X2.all[, -4]

# var3: strikes
strikes = factor(kershaw$pre_strikes)
X3.all = model.matrix(~ strikes - 1)
X3    = X3.all[, -3]



# testing variable selection from BMA package
# all data for binary regression
#data_reg = data.frame(fast, X1, X2, X3)
#output = bic.glm(fast ~ outs0 + outs1 + balls0 + balls1 + balls2 + strikes0 + strikes1,
# 		glm.family="binomial", 
# 		data = data_reg)
#summary(output)


# BAYESIAN BINARY LOGISTIC REGRESSION  

# X design matrix: colnames are the variables
#X = cbind(1, X1, X2, X3)

# number of parameters
#P = ncol(X)

# run model
#model1 = logit(sliders, X, n = rep(1, length(sliders)),
#samp = 2000, burn = 500)

# mean of posterior beta
#post_beta = colMeans(model1$beta)

# predict new probability for new_x
#new_x = c(0, 0, 0, 0, 0, 0, 0) 
#prob = (1 + exp(-new_x %*% post_beta))^(-1)



# MULTINOMIAL LOGISTIC REGRESSION
X = cbind(1, X1, X2, X3)
P = ncol(X)
J = length(unique(kershaw$pitch_type))

# design matrix for y's
Y.all = model.matrix(~ kershaw$pitch_type - 1)
Y = Y.all[, -J]
colnames(Y) = c("CH", "CU", "FF")

# run model
multi_out = mlogit(Y, X, n = rep(1, length(kershaw$pitch_type)), samp = 1000, burn = 100)
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

pitches_names = sort(unique(kershaw$pitch_type))
next_pitch = sample(x = pitches_names, size = 1, replace = TRUE, prob = prob_vec)


# MULTINOMIAL VARIABLE SELECTION
# with package

# cross validation: accuracy of pitches 
