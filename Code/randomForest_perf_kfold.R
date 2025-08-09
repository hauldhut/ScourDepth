#======================================
predrandomForest = function(M,data) # the PRED function
{ return (predict(M,as.matrix(data[,1:m.var]))) }

#==========================================
start_time <- Sys.time()

library('randomForest')
library('Metrics')
library('caret')
library('rminer') # for variable importance

setwd("~/Manuscripts/105ScourDepth/Code")

datafile = "Culvert.csv"
# datafile = "Sluice.csv"

if (datafile == "Culvert.csv"){
  ntree = 300
  nodesize = 1
} else {
  ntree = 500
  nodesize = 3
}

a <- read.csv(paste0("../Data/",datafile))

n <- nrow(a)
m <- ncol(a)
m.var = m - 1

folds <- createFolds(a[, m], k = 4, list = TRUE, returnTrain = FALSE)

results <- list()

for (i in 1:4) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(1:n, test_idx)
  
  a_train <- a[train_idx, ]
  a_test <- a[test_idx, ]
  
  model = randomForest(
    x = a_train[, 1:m.var],
    y = a_train[, m],
    ntree = ntree,
    mtry = 3,
    nodesize = nodesize
  )
  
  imp = Importance(model, data = a_train)
  
  pred_train = predict(model, as.matrix(a_train[, 1:m.var]))
  pred_test = predict(model, as.matrix(a_test[, 1:m.var]))
  
  # Metrics for training
  rmse.train = rmse(a_train[, m], pred_train)
  mae.train = mae(a_train[, m], pred_train)
  mape.train = mape(a_train[, m], pred_train)
  r2.train = 1 - sum((a_train[, m] - pred_train)^2) / sum((a_train[, m] - mean(a_train[, m]))^2)
  cor.train = cor(a_train[, m], pred_train)
  
  # Metrics for testing
  rmse.test = rmse(a_test[, m], pred_test)
  mae.test = mae(a_test[, m], pred_test)
  mape.test = mape(a_test[, m], pred_test)
  r2.test = 1 - sum((a_test[, m] - pred_test)^2) / sum((a_test[, m] - mean(a_test[, m]))^2)
  cor.test = cor(a_test[, m], pred_test)
  
  summary = data.frame(
    fold = i,
    rmse.train, mae.train, mape.train, r2.train, cor.train,
    rmse.test, mae.test, mape.test, r2.test, cor.test
  )
  
  results[[i]] <- summary
}

#==============================
# Post-processing
#==============================

df.summary <- do.call(rbind, results)

end_time <- Sys.time()
time = difftime(end_time, start_time, units = "secs")
df.summary$time = rep(time, nrow(df.summary))

summaryfile = paste0("../Results/randomForest_perf_cv_summary_metrics_", datafile)
write.csv(df.summary, summaryfile, row.names = FALSE, quote = FALSE)

#==============================
# Summary statistics for test metrics
#==============================

test_metrics = df.summary[, c("rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")]

summary.stats = sapply(test_metrics, function(x) {
  avg = round(mean(x), 3)
  sdv = round(sd(x), 3)
  sprintf("%.3f (±%.3f)", avg, sdv)
})

summary.stats = as.data.frame(t(summary.stats))
rownames(summary.stats) = NULL

# Save summary stats to file
summary.stats.file = paste0("../Results/randomForest_perf_cv_summary_metrics_test_", datafile)
write.csv(summary.stats, summary.stats.file, row.names = FALSE, quote = FALSE)
