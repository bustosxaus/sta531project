library(data.table)
library(dplyr)

data = fread("Pitchfx.csv")
data = data[which(data$pitch_type != "")]

# sample data
data = data[1:1000,]

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


T = trans_matrix(data$pitch_type)
previous = 5
next_predction = names(which.max(T[previous,]))


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

