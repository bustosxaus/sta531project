library(HMM)
library(data.table)
data = fread("Pitchfx.csv")

source("kershaw.R")
J = length(unique(kershaw$pitch_type))
pitches_names = sort(unique(kershaw$pitch_type))


m = 10
syms = pitches_names
states = c(1:m)

T = as.matrix(read.table("trans_clayton.txt"))
init = c(read.table("start_clayton.txt"))$V1
emis = as.matrix(read.table("emis_clayton.txt"))

hmm = initHMM(states, syms, startProbs = init, transProbs = T, emissionProbs = emis)

x = kershaw$pitch_type[1:10]
n = length(x)

forward_prob = HMM::forward(hmm, x)
 
probs = forward_prob[, n] %*% T %*% emis
probs = probs / sum(probs)

predicted = 

