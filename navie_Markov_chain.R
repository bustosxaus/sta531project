library(data.table)
library(dplyr)
library(lubridate)

data = fread("Pitchfx.csv") %>%
  	   mutate(game_date = dmy(game_date))

data = data[which(data$pitch_type != "")]
clayton_all = dplyr::filter(data, pitcher_id == "477132")  %>%
              dplyr::filter(pitch_type != "IN") %>%
 		      arrange(game_date, at_bat_number)
clayton = clayton_all$pitch_type 

felix_all = dplyr::filter(data, pitcher_id == "433587")  %>%
 		      arrange(game_date, at_bat_number)
felix = felix_all$pitch_type 



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




# trained model
T = trans_matrix(clayton)
pitches = colnames(T)
init = init_vec(kershaw$pitch_type)

# run on held out set
true_pitches = clayton


# accuracy for naive method
accuracy = c()
for (i in 1:1){
	predicted_pitches = model_game(T, init, true_pitches)
	accuracy[i] = mean(predicted_pitches == true_pitches)

}

total_accuracy_naive = mean(accuracy)

# accuracy per pitch
accuracy_pitch = c()
for (i in 1:length(pitches)){
	subset = which(true_pitches == pitches[i])
	accuracy_pitch[i] = mean(true_pitches[subset] ==  predicted_pitches[subset])
}


# plot for path of pithces, predicted and true
plot(c(1:50), factor(true_pitches[1:50]), type = "p")
lines(c(1:50), factor(predicted_pitches[1:50]), col = "red")

# plot for accuracies per pitch
barplot(c(accuracy_pitch, total_accuracy))

#SUPER NAIVE
pred_pitches_snaive = sample(x = pitches_names, size = length(kershaw$pitch_type), replace = TRUE, prob = init) 
total_accuracy = mean(kershaw$pitch_type == pred_pitches_snaive)

# 3-D TRANSITION MATRIX PREDICTION
# create naive but based on n-1 and n-2

#trans_matrix_3D = function(sequence){
#	n = length(sequence)
#	triples = data.frame(From = character(n-1),
#					   Middle = character(n-1),
#					   To = character(n-1),
#					   stringsAsFactors = FALSE)
#
#	for (i in 1:(n-2)){
#		triples[i,] = c(sequence[i], sequence[i+1], sequence[i+2])
#	}
#	T = table(triples$From, triples$Middle, triples$To)
#
#	for (z in 1:dim(T)[3]){
#		T[,,z] = T[,,z] / colsums(T)
#
#	}
#	return(T)
#}

#T = trans_matrix_3D(data$pitch_type)

