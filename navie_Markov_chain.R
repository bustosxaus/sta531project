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



# plot for path of pithces, predicted and true
#plot(c(1:50), factor(true_pitches[1:50]), type = "p")
#lines(c(1:50), factor(predicted_pitches[1:50]), col = "red")

#### CROSS VALIDATION ####

# Number of folds
k = 10

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


	# trained model
	T = trans_matrix(train)
	pitches = colnames(T)
	init = init_vec(train)

	# run on held out set
	true_pitches = test


	# accuracy for naive method
	accuracy = c()
	for (m in 1:10){
		predicted_pitches = model_game(T, init, true_pitches)
		accuracy[m] = mean(predicted_pitches == true_pitches)

	}

	holdout[i] = mean(accuracy)

}

mean(holdout)




##### SUPER NAIVE ######
pred_pitches_snaive = sample(x = pitches_names, size = length(kershaw$pitch_type), replace = TRUE, prob = init) 
total_accuracy = mean(kershaw$pitch_type == pred_pitches_snaive)


## CROSS VALIDATION ##

# Number of folds
k = 10

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


 	init = init_vec(train)

 	accuracy = c()
 	for (m in 1:10){
 		pred_pitches = sample(x = pitches_names, size = length(test), replace = TRUE, prob = init) 
		accuracy[m] = mean(test == pred_pitches)
	}
	holdout[i] = mean(accuracy)
}

mean(holdout)
