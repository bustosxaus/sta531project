library(data.table)
library(BayesLogit)
library(dplyr)
library(mlogitBMA)
library(caret)

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


# testing variable selection from BMA package
# all data for binary regression
#data_reg = data.frame(fast, X1, X2, X3)
#output = bic.glm(fast ~ outs0 + outs1 + balls0 + 
#  balls1 + balls2 + strikes0 + strikes1,
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
# design matrices for input variables
kershaw$pre_outs = factor(kershaw$pre_outs)
kershaw$pre_balls = factor(kershaw$pre_balls)
kershaw$pre_strikes = factor(kershaw$pre_strikes)
#kershaw$previous_event_type = factor(kershaw$previous_event_type)
kershaw$previous_pitch_type[1] = "FF"
kershaw$previous_pitch_type = factor(kershaw$previous_pitch_type)


J = length(unique(kershaw$pitch_type))
pitches_names = sort(unique(kershaw$pitch_type))


# design matrix for Y's
Y.all = model.matrix(~ pitch_type - 1, data = kershaw)
Y = Y.all[, -J]

# design matrix for X
X = model.matrix(~ pre_outs + pre_balls + pre_strikes + 
    pitch_number + runners +
    pitch_count + top_inning_sw + bat_side + 
    inning + previous_pitch_type,
    data = kershaw)
P = ncol(X)

# RUN MODEL
multi_out = mlogit(Y, X, n = rep(1, nrow(Y)), samp = 2000, burn = 500)
betas = multi_out$beta
# get posterior means and put in formula
post_betas = matrix(0, nrow = J, ncol = P)
for (j in 1:J-1){
	post_betas[j,] = colMeans(betas[,,j])
}
post_betas[J] = 0


# PREDICT PITCHES 
# sample for testing 
X_test = X

probs = matrix(0, nrow = nrow(X_test), ncol = J)
for (i in 1:nrow(X_test)){
  new_x = X_test[i,]
  for (j in 1:J){
    probs[i,j] = exp(new_x %*% post_betas[j,])
  }
}

# probability matrix from results of model
prob_mat = probs / rowSums(probs)

# find average accuracy of model
accuracy = c()
for (m in 1:10){
  pred_pitches = c()

  for (i in 1:nrow(prob_mat)){
    prob_vec = prob_mat[i,]
    pred_pitches[i] = sample(x = pitches_names, 
        size = 1, replace = TRUE, prob = prob_vec)
  }

  accuracy[m] = mean(pred_pitches == kershaw$pitch_type)
}
mean(accuracy)

# function to pull out variables from data frame
# predict pitch based on model 

new_x = c(1, rep(0, P-1))






# MULTINOMIAL VARIABLE SELECTION
# with package

######################################### CROSS VALIDATION #########################################

# Number of folds
k = 35

# number of observations
n = nrow(kershaw)

# Getting our folds -> these are indices for the rows that are the folds
folds = createFolds(1:n, k = k)

# Running k-fold CV
holdout = c()

for(i in 1:k)
{
  # Testing set
  test = kershaw[folds[[i]], ]
  # Training set
  train = kershaw[-folds[[i]], ]
  
  # design matrices for input variables
  
  
  # run model
  
  
  # get posterior means and put in formula
  for (j in 1:J-1){
    post_betas[j,] = colMeans(betas[,,j])
  }
  
  post_betas[J] = 0
  
  ### SECTION WHERE WE HAVE TO CODE IN THE VECTORS FOR ALL OF THE TRAINING DATA ###
  
  # GONNA NEED A FOR LOOP I IMAGINE
  new_x = c(1,1,0,0,0,0,1,0)
  
  #################################################################################
  
  probs = c()
  for (j in 1:J){
    probs[j] = exp(new_x %*% post_betas[j,])
  }
  
  # print probability vector
  prob_vec = probs / sum(probs)
  
}


