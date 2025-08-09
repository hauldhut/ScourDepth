start_time <- Sys.time()

library('kernlab')
library('Metrics')
library(foreach)
library('doParallel')
library('caret')

setwd("~/Manuscripts/105ScourDepth/Code")

datafile = "Culvert.csv"
# datafile = "Sluice.csv"

if (datafile == "Culvert.csv"){
  C = 1
} else {
  C = 5
}

a <- read.csv(paste0("../Data/", datafile))

n <- nrow(a)
m <- ncol(a)
m.var = m - 1

no_cores <- 4
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# Create 4-folds for cross-validation
folds <- createFolds(a$dsa, k = 4, list = TRUE, returnTrain = FALSE)

all_metrics <- data.frame()

for (i in 1:4) {
  test_idx <- folds[[i]]
  a_train <- a[-test_idx, ]
  a_test <- a[test_idx, ]

  model <- ksvm(dsa ~ ., 
                data = a_train,
                C = C,
                epsilon = 0.1,
                kernel = 'rbfdot')

  y_train = a_train[, m]
  y_test = a_test[, m]

  pred_train = predict(model, a_train[, 1:m.var])
  pred_test = predict(model, a_test[, 1:m.var])

  # Train metrics
  rmse.train = rmse(y_train, pred_train)
  mae.train = mae(y_train, pred_train)
  mape.train = mape(y_train, pred_train)
  r2.train = 1 - sum((y_train - pred_train)^2) / sum((y_train - mean(y_train))^2)
  cor.train = cor(y_train, pred_train)

  # Test metrics
  rmse.test = rmse(y_test, pred_test)
  mae.test = mae(y_test, pred_test)
  mape.test = mape(y_test, pred_test)
  r2.test = 1 - sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2)
  cor.test = cor(y_test, pred_test)

  all_metrics <- rbind(all_metrics, 
    data.frame(rmse.train, mae.train, mape.train, r2.train, cor.train,
               rmse.test, mae.test, mape.test, r2.test, cor.test))
}

stopCluster(cl)

# Add timing
end_time <- Sys.time()
time = difftime(end_time, start_time, units = "secs")
all_metrics$time = rep(time, 4)

# Save raw results
summaryfile = paste0("../Results/ksvm_kernlab_perf_cv_summary_metrics_", datafile)
write.csv(all_metrics, summaryfile, row.names = FALSE, quote = FALSE)

# Create test metric summary (mean ± stdev)
test_metrics = all_metrics[, c("rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")]

summary.stats = sapply(test_metrics, function(x) {
  avg = round(mean(x), 3)
  sdv = round(sd(x), 3)
  sprintf("%.3f (±%.3f)", avg, sdv)
})

summary.stats = as.data.frame(t(summary.stats))
rownames(summary.stats) = NULL

# Save to file
summary.stats.file = paste0("../Results/ksvm_kernlab_perf_cv_test_metrics_summary_", datafile)
write.csv(summary.stats, summary.stats.file, row.names = FALSE, quote = FALSE)
