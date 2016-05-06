library(data.table)
library(BayesLogit)
library(dplyr)
library(mlogitBMA)
library(caret)
library(nnet)

source("kershaw.R")

####### FORMULA MAKER #########
formula_maker = function(vars)
{
  form = vars[1]

  if (length(vars) > 1){
    for(t in 2:length(vars))
    {
      form = paste(form, vars[t], sep = "+")
    }
  }
  FORMULA = as.formula(paste("~", form, "-1"))

  return(FORMULA)
}


# MULTINOMIAL LOGISTIC REGRESSION 

######################################### CROSS VALIDATION #########################################
CV = function(k, pitcher, vars){
  
  # Unique pitchers for our pitcher of interest
  J = length(unique(pitcher$pitch_type))
  # Names of pitches
  pitches_names = sort(unique(pitcher$pitch_type))
  
  # Building our formula for model.matrix
  formula = formula_maker(vars)


  # number of observations
  n = nrow(pitcher)

  # Getting our folds -> these are indices for the rows that are the folds
  folds = createFolds(1:n, k = k)

  # Running k-fold CV
  holdout = c()

  accuracy_matrix = matrix(0, nrow = k, ncol = J)
  error_matrix = matrix(0, nrow = k, ncol = J)

  for(i in 1:k)
  {
    # Testing set
    test = pitcher[folds[[i]], ]
    # Training set
    train = pitcher[-folds[[i]], ]
    
    # design matrix for pitch_type
    # design matrix for Y's
    Y.all = model.matrix(~ pitch_type - 1, data = train)
    Y = Y.all[, -J]
    
    X_train = model.matrix(formula, data = train)
    P = ncol(X_train)
    
    # run model
    # multi_out = mlogit(Y, X_train, n = rep(1, nrow(Y)), samp = 1000, burn = 200)
    # betas = multi_out$beta
  
    multi_out = multinom(train$pitch_type ~ X_train)
    post_betas = matrix(0, nrow = J, ncol = P+1)
    post_betas[2:J,] = coefficients(multi_out)
    
    
    # get posterior means
    #post_betas = matrix(0, nrow = J, ncol = P)
    #for (j in 1:J-1){
    #  post_betas[j,] = colMeans(betas[,,j])
    #}
    #post_betas[J] = 0
    
    
    # design matrix for test set, X_test
    X_test = model.matrix(formula, data = test)
    P = ncol(X_test)
    X_test = cbind(1, X_test)
    
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
    #for (m in 1:100){
      pred_pitches = c()
      
      for (g in 1:nrow(prob_mat)){
        prob_vec = prob_mat[g,]
        pred_pitches[g] = sample(x = pitches_names, 
                                 size = 1, replace = TRUE, prob = prob_vec)
        }       
      accuracy[m] = mean(pred_pitches == test$pitch_type)
      #}
    

      true_pitches = test$pitch_type
      accuracy_pitch = c()
      for (b in 1:length(pitches_names)){
        subset = which(true_pitches == pitches_names[b])
          accuracy_pitch[b] = mean(true_pitches[subset] ==  pred_pitches[subset])
      }

      not_accuracy_pitch = c()
      for (c in 1:length(pitches_names)){
        subset = which(true_pitches == pitches_names[c])
          not_accuracy_pitch[c] = mean(true_pitches[subset] !=  pred_pitches[subset])
      }

    accuracy_matrix[i,] = accuracy_pitch
    error_matrix[i,] = not_accuracy_pitch
    holdout[i] = mean(accuracy)
  }

  return(list(mean(holdout), colMeans(accuracy_matrix), colMeans(error_matrix)))

}


# RUN 3 DIFFERENT MODELS WITH CROSS VALIDATION 

vars1 = c("count")
vars2 = c("count", "pre_outs", "inning")
vars3 = c("pre_outs", "count", "pitch_number", "runners_count",
  "pitch_count", "top_inning_sw", "bat_side", "inning", 
  "prev_pitch_type")

vars1_list = CV(5, kershaw, vars1)
vars2_list = CV(5, kershaw, vars2)
vars3_list = CV(5, kershaw, vars3)

results = matrix(c(vars1_list[[2]], vars2_list[[2]], vars3_list[[2]]), byrow = TRUE, 
  nrow = 3, ncol = 4)

results = cbind(results, c(vars1_list[[1]], vars2_list[[1]], 
  vars3_list[[1]]))

library(xtable)
rownames(results) = c("LR1", "LR2", "LR3")
colnames(results) = c(pitches_names, "Total")


############ TEST MODEL IN SAMPLE #########
# Unique pitchers for our pitcher of interest
J = length(unique(kershaw$pitch_type))

# Names of pitches
pitches_names = sort(unique(kershaw$pitch_type))
  
# Building our formula for model.matrix
formula = formula_maker(vars3)

# number of observations
n = nrow(kershaw)
X = model.matrix(formula, data = kershaw)
    
multi_out = multinom(kershaw$pitch_type ~ X)   
pred_pitches = predict(multi_out) 
total_accuracy = mean(kershaw$pitch_type == pred_pitches)


true_pitches = kershaw$pitch_type
accuracy_pitch = c()
for (i in 1:length(pitches_names)){
  subset = which(pred_pitches == pitches_names[i])
  accuracy_pitch[i] = mean(true_pitches[subset] ==  pred_pitches[subset])
}

ALL_RESULTS3 = c(accuracy_pitch, total_accuracy)



###### VARIABLE SELECTION #######

vars = c("pre_outs", "count", "pitch_number", "runners_count",
  "top_inning_sw", "bat_side", "inning", 
  "prev_pitch_type")
i = 2

k = 5
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
