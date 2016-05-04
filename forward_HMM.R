library(data.table)
data = fread("Pitchfx.csv")


library(HMM)

m = 3
k = 5
states = c(1:m)
syms = c(1:k)
T = matrix( c(0.45820796,  0.05941577,  0.48237626,
        0.36935183,  0.49966634,  0.13098183,
        0.39179294,  0.25688283,  0.35132423), 
		nrow = 3, ncol = 3, byrow = TRUE)

init = c(2.74224948e-85,   9.99760158e-01,   2.39842461e-04)

emis = matrix(c(8.18511248e-09,   2.28131933e-01,   3.21444984e-01,
          1.27978246e-27,   4.50423075e-01,
         6.11359725e-02,   8.06767692e-02,   7.87946792e-01,
          2.51432015e-05,   7.02153236e-02,
         1.10627891e-09,   1.06907766e-01,   7.15588273e-01,
          1.48347882e-03,   1.76020481e-01), nrow = 3, ncol = 5)

hmm = initHMM(states, syms, startProbs = init, transProbs = T, emissionProbs = emis)

x = c(1,2,3,1,1,5,5,5)
n = length(x)

forward_prob = HMM::forward(hmm, x)
 
probs = exp(forward_prob[, n]) %*% T %*% emis
probs = probs / sum(probs)

predicted = which.max(probs)

