numbers = c(0.4177, 0.4183, 0, 0.4913, 0.4952, 0.5010)

results = matrix(numbers, nrow = length(numbers), ncol = 1)
rownames(results) = c("Sampling", "Markov", "HMM", "LR1", "LR2", "LR3")
colnames(results) = "Accuracy"
results

