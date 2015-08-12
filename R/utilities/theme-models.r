##
##		Functions to help with theme models
##		12 May 2014
##		Andreas Beger
##

# Model summary stats
evalfit <- function(model, data) {
	# Calculate Brier score, AUC, percent correct, and percent reduction in 
	# error relative to an all-0 model.
	# Inputs: a spdur object ("model") and the data in which to create 
	#	      predictions ("data"). Observed values are assumed to be "failure"
	#		  in "data".
	# Output: Named 1x4 matrix with summary fit statistics

  if (class(model)[1]=="spdur") {
    ch <- predict(model, newdata=data, type="conditional hazard")
  } else if (class(model)[1]=="glm") {
    ch <- predict(model, newdata=data, type="response")
  }

	aucroc <- auc_roc(data[, "failure"], ch)
	aucpr <- auc_pr(data[, "failure"], ch)

	# Find optimum cutpoint
	max_f_cut <- function(obs, pred) {
	  # Wrapper for ROCR that returns maximum F score as numeric
	  rocr.df <- prediction(pred, obs)
	  perf <- performance(rocr.df, "f")
	  res  <- perf@x.values[[1]][which(perf@y.values[[1]]==max(perf@y.values[[1]], na.rm=T))]
	  return(res)
	}
  
	# Find cutpoint for desired recall
	recall_cut <- function(obs, pred, rec = 0.5) {
	  rocr_df <- prediction(pred, obs)
	  perf <- performance(rocr.df, "recall")
    # which(abs(x-your.number)==min(abs(x-your.number)))
	  res  <- perf@x.values[[1]][which(perf@y.values[[1]]==max(perf@y.values[[1]], na.rm=T))]
	  return(res)
	}
  
	optim.t <- max_f_cut(data[, "failure"], ch)

	# Binary predictions
	yhat <- as.numeric(ch >= optim.t)
	#yhat <- as.numeric(ch>0.5)

	# Classifier categories
	n  <- nrow(data)
	tp <- sum(yhat==1 & data[, "failure"]==1)
	fp <- sum(yhat==1 & data[, "failure"]==0)
	tn <- sum(yhat==0 & data[, "failure"]==0)
	fn <- sum(yhat==0 & data[, "failure"]==1)

	pC   <- (tp + tn)/n  # aka Accuracy
	pC.null <- sum(data[, "failure"]==0)/nrow(data)
	pre  <- (pC-pC.null)/pC.null

	# Recall and precision
	recall <- tp / (tp + fn)
	prec   <- tp / (tp + fp)

	# Balanced accuracy
	specificity <- tn / (fp + tn)
	f2 <- 0.5*recall + 0.5*specificity

	res <- matrix(c(optim.t, brier(data[, "failure"], ch), aucroc, aucpr, pC, 
		pre, recall, prec, f2), nrow=1)
	colnames(res) <- c("Cut point", "Brier", "AUC", "AUC-PR", "Accuracy", "pre", "Recall", 
		"Precision", "F2")
	row.names(res) <- c(substitute(data))
	res
}

evalfit_all <- function(model) {
	# Calculate training and test period fit for a theme model
	res <- rbind(
		evalfit(model, train),
		evalfit(model, calib),
		evalfit(model, test)
		)
	res
}
