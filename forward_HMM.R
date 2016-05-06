library(HMM)

source("kershaw.R")
J = length(unique(kershaw$pitch_type))
pitches_names = sort(unique(kershaw$pitch_type))


m = 10
syms = pitches_names
states = c(1:m)

T = as.matrix(read.table("trans_clayton.txt"))
init = c(read.table("start_clayton.txt"))$V1
emis = as.matrix(read.table("emis_clayton.txt"))
colnames(emis) = pitches_names
#hmm = initHMM(states, syms, startProbs = init, 
	#transProbs = T, emissionProbs = emis)

x = kershaw$pitch_type[1:10]
n = length(x)

#forward_prob = HMM::forward(hmm, x)

forward = function(init, T, emis, x, m){
     # G matrix instead of S with use of logs
     n = length(x)
     G = matrix(NA, nrow = n, ncol = m)

     # first row of G matrix
     # for each state
     for (j in 1:m){
          
          G[1, j] = log(init[j]) + log(emis[j, x[1]])
     }

     # 2,..,n rows of G
     for (i in 2:n){

          for (j in 1:m){

               A = G[i-1,] + log(T[,j]) + log(emis[j, x[i]])
               b = max(A)

               G[i, j] = b + log(sum(exp(A - b)))
          }
     }

     return(G)

}

forward_probs = forward(init, T, emis, x, 10)
 

#probs = forward_prob[, n] %*% T %*% emis
#probs = probs / sum(probs)
total_sum = 0

for (i in 1:10){
	for (j in 1:10){

		esto = forward_probs[10,i] + log(T)[i,j] + log(emis)[j,]
		total_sum = total_sum + esto
	}

}

exp(total_sum)