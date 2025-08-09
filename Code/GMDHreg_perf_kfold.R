predGMDH = function(M,data) # the PRED function
{ return (predict(M,as.matrix(data[,1:m.var]))) }

#==========================================
start_time <- Sys.time()

library('GMDHreg')
library('Metrics')
library(caret)

setwd("~/Manuscripts/105ScourDepth/Code")

datafile = "Culvert.csv"
# datafile = "Sluice.csv"

#====================
a <- read.csv(paste0("../Data/", datafile))
n <- nrow(a)
m <- ncol(a)
m.var = m - 1

folds <- 4
fold_index <- createFolds(a[[m]], k = folds, list = TRUE)

if(datafile=="Culvert.csv"){
  G = 2 # polynomial degree: 0=linear, 1=interaction, 2=quadratic
}else{
  G = 1
}


metrics_all = data.frame()

for (i in 1:folds) {
  cat("Fold", i, "\n")
  test_idx = fold_index[[i]]
  a_test = a[test_idx, ]
  a_train = a[-test_idx, ]
  
  x_train = a_train[, 1:m.var]
  y_train = a_train[, m]
  
  model = gmdh.combi(X = as.matrix(x_train),
                     y = y_train,
                     G = G,
                     criteria = "PRESS")
  
  pred_train = predict(model, as.matrix(a_train[, 1:m.var]))
  pred_test = predict(model, as.matrix(a_test[, 1:m.var]))
  
  # Training Metrics
  rmse.train = rmse(a_train[, m], pred_train)
  mae.train = mae(a_train[, m], pred_train)
  mape.train = mape(a_train[, m], pred_train)
  r2.train = 1 - sum((a_train[, m] - pred_train)^2) / sum((a_train[, m] - mean(a_train[, m]))^2)
  cor.train = cor(a_train[, m], pred_train)
  
  # Test Metrics
  rmse.test = rmse(a_test[, m], pred_test)
  mae.test = mae(a_test[, m], pred_test)
  mape.test = mape(a_test[, m], pred_test)
  r2.test = 1 - sum((a_test[, m] - pred_test)^2) / sum((a_test[, m] - mean(a_test[, m]))^2)
  cor.test = cor(a_test[, m], pred_test)
  
  metrics_all = rbind(metrics_all, data.frame(
    fold = i,
    rmse.train, mae.train, mape.train, r2.train, cor.train,
    rmse.test, mae.test, mape.test, r2.test, cor.test
  ))
}

end_time <- Sys.time()
time = difftime(end_time, start_time, units = "secs")
metrics_all$time = rep(time, nrow(metrics_all))

# Save full metrics for all folds
if (G == 1) {
  Method = "GMDHreg_G1"
} else {
  Method = "GMDHreg_G2"
}

summaryfile = paste0("../Results/", Method, "_perf_cv_summary_metrics_", datafile)
write.csv(metrics_all, summaryfile, row.names = FALSE, quote = FALSE)

# ============================
# Summary statistics for test metrics
summary_stats <- function(metric_column) {
  avg = round(mean(metric_column), 3)
  sdv = round(sd(metric_column), 3)
  sprintf("%.3f (±%.3f)", avg, sdv)
}

test_summary = sapply(metrics_all[, c("rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")], summary_stats)
test_summary = as.data.frame(t(test_summary))
rownames(test_summary) = NULL

# Save test summary
summary.stats.file = paste0("../Results/", Method, "_perf_cv_summary_metrics_test_", datafile)
write.csv(test_summary, summary.stats.file, row.names = FALSE, quote = FALSE)
