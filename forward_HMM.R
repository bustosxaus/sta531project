library(HMM)
library(data.table)
data = fread("Pitchfx.csv")

source("kershaw.R")
J = length(unique(kershaw$pitch_type))
pitches_names = sort(unique(kershaw$pitch_type))


m = 10
k = pitches_names
states = c(1:m)
syms = c(1:k)

T = read.table("trans_clayton.txt")
init = read.table()
emis = read.table("emis_clayton.txt")

hmm = initHMM(states, syms, startProbs = init, transProbs = T, emissionProbs = emis)

x = kershaw$pitch_type
n = length(x)

forward_prob = HMM::forward(hmm, x)
 
probs = exp(forward_prob[, n]) %*% T %*% emis
probs = probs / sum(probs)

predicted = 

