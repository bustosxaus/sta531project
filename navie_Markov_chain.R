library(data.table)
library(dplyr)
library(lubridate)

source("kershaw.R")
J = length(unique(kershaw$pitch_type))
pitches_names = sort(unique(kershaw$pitch_type))


# NAIVE TRANSITION MATRIX PREDICTION 
# create naive transition matrix to predict next pitch
trans_matrix = function(sequence){
	n = length(sequence)

	pairs = data.frame(From = character(n-1),
					   To = character(n-1),
					   stringsAsFactors = FALSE)

	for (i in 1:(n-1)){
		pairs[i,] = c(sequence[i], sequence[i+1])
	}
	T = table(pairs$From, pairs$To)
	T = T / rowSums(T)
	return(T)
}

init_vec = function(sequence){
	n = length(sequence)
	freqs = table(sequence)
	probs = freqs / sum(freqs)
	return(probs)
}

next_pitch = function(prev_pitch, T){
	next_probs = T[prev_pitch, ]
	pitches = colnames(T)
	next_pitch = sample(x = pitches, size = 1, replace = TRUE, prob = next_probs)
	#next_pitch = names(which.max(next_probs))
	return(next_pitch)
}

model_game = function(T, init, true_pitches){
	pred_pitches = c()
	pitches = colnames(T)
	pred_pitches[1] = sample(x = pitches, size = 1, prob = init)				  

	for (i in 2:length(true_pitches)){
		pred_pitches[i] = next_pitch(true_pitches[i-1], T)

	}
	return(pred_pitches)
}



#### CROSS VALIDATION ####

# Number of folds
k = 5

# number of observations
n = nrow(kershaw)

# Getting our folds -> these are indices for the rows that are the folds
folds = ntile(1:n, k)

# Running k-fold CV
holdout = c()


accuracy_matrix = matrix(0, nrow = k, ncol = J)

for(i in 1:k)
{

	# Testing set
 	 test = kershaw$pitch_type[which(folds == i)]
 	 # Training set
 	 train = kershaw$pitch_type[-which(folds == i)]


	# trained model
	T = trans_matrix(train)
	pitches = colnames(T)
	init = init_vec(train)

	# run on held out set
	true_pitches = test


	# accuracy for naive method
	accuracy = c()
	for (m in 1:100){
		predicted_pitches = model_game(T, init, true_pitches)
		accuracy[m] = mean(predicted_pitches == true_pitches)

	}

      accuracy_pitch = c()
      for (b in 1:length(pitches_names)){
        subset = which(true_pitches == pitches_names[b])
          accuracy_pitch[b] = mean(true_pitches[subset] ==  predicted_pitches[subset])
      }

    accuracy_matrix[i,] = accuracy_pitch

	holdout[i] = mean(accuracy)

}

mean(holdout)


### TEST ON IN SAMPLE #####
results1 = c(colMeans(accuracy_matrix), mean(holdout))



	# trained model
	T = trans_matrix(kershaw$pitch_type)
	pitches = colnames(T)
	init = init_vec(train)

	# run on held out set
	true_pitches = kershaw$pitch_type


	# accuracy for naive method
	accuracy = c()
	for (m in 1:100){
		predicted_pitches = model_game(T, init, true_pitches)
		accuracy[m] = mean(predicted_pitches == true_pitches)

	}

      accuracy_pitch = c()
      for (b in 1:length(pitches_names)){
        subset = which(true_pitches == pitches_names[b])
          accuracy_pitch[b] = mean(true_pitches[subset] ==  predicted_pitches[subset])
      }


RESULTS1 = c(accuracy_pitch, mean(accuracy))



##### SUPER NAIVE ######
pred_pitches_snaive = sample(x = pitches_names, size = length(kershaw$pitch_type), replace = TRUE, prob = init) 
total_accuracy = mean(kershaw$pitch_type == pred_pitches_snaive)


## CROSS VALIDATION ##

# Number of folds
k = 5

# number of observations
n = nrow(kershaw)

# Getting our folds -> these are indices for the rows that are the folds
folds = createFolds(1:n, k = k)

# Running k-fold CV
holdout = c()

for(i in 1:k)
{
	# Testing set
 	test = kershaw[folds[[i]], ]$pitch_type
 	# Training set
 	train = kershaw[-folds[[i]], ]$pitch_type

 	true_pitches = test

 	init = init_vec(train)

 	accuracy = c()
 	for (m in 1:100){
 		pred_pitches = sample(x = pitches_names, size = length(test), replace = TRUE, prob = init) 
		accuracy[m] = mean(test == pred_pitches)
	}

	accuracy_pitch = c()
      for (b in 1:length(pitches_names)){
        subset = which(true_pitches == pitches_names[b])
          accuracy_pitch[b] = mean(true_pitches[subset] ==  pred_pitches[subset])
      }

    accuracy_matrix[i,] = accuracy_pitch
	holdout[i] = mean(accuracy)
}

mean(holdout)


#### TEST ON IN SAMPLE ####
results3 = c(colMeans(accuracy_matrix), mean(holdout))

 	init = init_vec(kershaw$pitch_type)

 	accuracy = c()
 	for (m in 1:100){
 		pred_pitches = sample(x = pitches_names, size = length(kershaw$pitch_type), replace = TRUE, prob = init) 
		accuracy[m] = mean(kershaw$pitch_type == pred_pitches)
	}

	accuracy_pitch = c()
      for (b in 1:length(pitches_names)){
        subset = which(true_pitches == pitches_names[b])
          accuracy_pitch[b] = mean(true_pitches[subset] ==  pred_pitches[subset])
      }


RESULTS2 = c(accuracy_pitch, mean(accuracy))

