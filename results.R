


vars1 = c("count")
vars2 = c("count", "pre_outs", "inning")
vars3 = c("pre_outs", "count", "pitch_number", "runners_count",
  "pitch_count", "top_inning_sw", "bat_side", "inning", 
  "prev_pitch_type")



tab = rbind(results3, results1, results)

rownames(tab) = c("Sampling", "Markov", "LR1", "LR2", "LR3")
colnames(tab) = c(pitches_names, "Total")

print(xtable(tab, digits = 4, caption = "Percent Accuracies from Cross Validation"), file = "ALL4_CV.tex")


tab2 = rbind(RESULTS2, RESULTS1, ALL_RESULTS1, ALL_RESULTS2, ALL_RESULTS3)

rownames(tab2) = c("Sampling", "Markov", "LR1", "LR2", "LR3")
colnames(tab2) = c(pitches_names, "Total")

print(xtable(tab2, digits = 4, caption = "Percent Accuracies from In-Sample Testing"), 
	file = "ALL4_NOTCV.tex")


barplot((tab), beside = TRUE)

pdf("NOT_CV.pdf", height = 6, width = 8)
barplot((tab2), beside = TRUE, ylab = "Percent Accuracy",
	xlab = "Type of Pitches", legend = TRUE,
	args.legend = list(x = "topleft"),
	ylim = c(0,0.9), main = "In Sampling Testing")
dev.off()

pdf("CV.pdf", height = 6, width = 8)
barplot((tab), beside = TRUE, ylab = "Percent Accuracy",
	xlab = "Type of Pitches", legend = TRUE,
	args.legend = list(x = "topleft"),
	ylim = c(0,0.9), main = "Results from Cross Validation")
dev.off()