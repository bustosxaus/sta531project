library(data.table)
library(BayesLogit)
library(dplyr)
library(mlogitBMA)
library(caret)

source("kershaw.R")





formula_maker = function(vars)
{
  form = vars[1]

  if (length(vars) > 1){
    for(t in 2:length(vars))
    {
      form = paste(form, vars[t], sep = "+")
    }
  }
  FORMULA = as.formula(paste("~", form))

  return(FORMULA)
}


# MULTINOMIAL LOGISTIC REGRESSION 

######################################### CROSS VALIDATION #########################################
CV = function(k, pitcher, vars){
  
  # Unique pitchers for our pitcher of interest
  J = length(unique(pitcher$new_pitch_type))
  # Names of pitches
  pitches_names = sort(unique(pitcher$new_pitch_type))
  
  # Building our formula for model.matrix
  formula = formula_maker(vars)


  # number of observations
  n = nrow(pitcher)

  # Getting our folds -> these are indices for the rows that are the folds
  folds = createFolds(1:n, k = k)

  # Running k-fold CV
  holdout = c()

  for(i in 1:k)
  {
    # Testing set
    test = pitcher[folds[[i]], ]
    # Training set
    train = pitcher[-folds[[i]], ]
    
    # design matrix for pitch_type
    # design matrix for Y's
    Y.all = model.matrix(~ new_pitch_type - 1, data = train)
    Y = Y.all[, -J]
    
    X_train = model.matrix(formula, data = train)
    P = ncol(X_train)
    
    # run model
    multi_out = mlogit(Y, X_train, n = rep(1, nrow(Y)), samp = 1000, burn = 200)
    betas = multi_out$beta
    
    # get posterior means
    post_betas = matrix(0, nrow = J, ncol = P)
    for (j in 1:J-1){
      post_betas[j,] = colMeans(betas[,,j])
    }
    post_betas[J] = 0
    
    
    # design matrix for test set, X_test
    X_test = model.matrix(formula,
                            data = test)
    P = ncol(X_test)
    
    # Finding multinomial probabilities associated with each test observation
    probs = matrix(0, nrow = nrow(X_test), ncol = J)
    for (w in 1:nrow(X_test)){
      new_x = X_test[w,]
      for (j in 1:J){
        probs[w,j] = exp(new_x %*% post_betas[j,])
      }
    }
    
    # probability matrix from results of model
    prob_mat = probs / rowSums(probs)
    
    # find average accuracy of model
    accuracy = c()
    for (m in 1:100){
      pred_pitches = c()
      
      for (g in 1:nrow(prob_mat)){
        prob_vec = prob_mat[g,]
        pred_pitches[g] = sample(x = pitches_names, 
                                 size = 1, replace = TRUE, prob = prob_vec)
      } 
      accuracy[m] = mean(pred_pitches == test$new_pitch_type)
    }
    
    holdout[i] = mean(accuracy)
  }


  return(mean(holdout))

}




# Number of folds
k = 2

# Variables we wish to use
vars = c("pre_outs", "pre_strikes", "pre_balls",
  "inning")

vars = c("pre_outs", "count", "pitch_number", "runners",
  "pitch_count", "top_inning_sw", "bat_side", "inning", 
  "prev_pitch_type")

CV(k, kershaw, vars)


# accuracy without each variable
without = matrix(0, nrow = 1, ncol = length(vars))
colnames(without) = vars

# Iteratively removing one variable and performing CV
for(d in 1:length(vars))
{
  # removing one variable at a time
  vars_remove = vars[-d]
  # Performing CV on the model missing variable d
  without[1, d] = CV(k = k, pitcher = kershaw, vars = vars_remove)
}

vars = c("pre_outs", "count", "pitch_number", "runners_count",
  "top_inning_sw", "bat_side", "inning", 
  "prev_pitch_type")
i = 2

difference = 5
max_accuracy = c(0)
current_vars = c()
while (difference > 0){

  add = matrix(0, nrow = 1, ncol = length(vars))
  colnames(add) = vars


  # Iteratively add one variable and performing CV
  for(d in 1:length(vars))
  {
    vars_add = vars[d]
    add[1, d] = CV(k = k, pitcher = kershaw, vars = c(current_vars, vars_add))
  }

  max_accuracy[i] = max(add[1,])
  difference = max_accuracy[i] - max_accuracy[i-1]

  new_var = colnames(add)[which.max(add[1,])]
  current_vars = c(current_vars, new_var)

  vars = vars[-which(vars == new_var)]

  i = i + 1
}

# testing variable selection from BMA package
# all data for binary regression
data_reg = data.frame(fast, X1, X2, X3)
output = bic.glm(fast ~ outs0 + outs1 + balls0 + 
  balls1 + balls2 + strikes0 + strikes1,
     glm.family="binomial", 
     data = data_reg)
summary(output)