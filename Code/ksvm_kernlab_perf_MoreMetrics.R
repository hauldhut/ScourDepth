# ==========================================
start_time <- Sys.time()

library('kernlab')
library('Metrics')
library(foreach)
library(doParallel)

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

n_train <- round(n * 0.75)
ntrial = 100

no_cores <- 15
cl <- makeCluster(no_cores)
registerDoParallel(cl)

t = 1

res <- foreach(t = 1:ntrial, .combine = rbind) %dopar% {
  
  library('kernlab')
  library('Metrics')
  
  picked = sample(seq_len(n), size = n_train)
  a_train = a[picked, ]
  a_test = a[-picked, ]
  
  model <- ksvm(dsa ~ .,
                data = a_train,
                C = C,
                epsilon = 0.1,
                kernel = 'rbfdot')
  
  # Predict and calculate metrics
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
  
  summary <- c(rmse.train, mae.train, mape.train, r2.train, cor.train,
               rmse.test, mae.test, mape.test, r2.test, cor.test)
  
  detail.train <- cbind(y_train, pred_train)
  detail.test <- cbind(y_test, pred_test)
  
  list(summary, detail.train, detail.test)
}

stopCluster(cl)

# Collect results
df.summary = NULL
df.detail.train = NULL
df.detail.test = NULL

for (t in 1:ntrial) {
  df.summary = rbind(df.summary, res[t, 1][[1]])
  df.detail.train = cbind(df.detail.train, res[t, 2][[1]])
  df.detail.test = cbind(df.detail.test, res[t, 3][[1]])
}

# Summary data frame
df.summary = as.data.frame(df.summary)
colnames(df.summary) = c("rmse.train", "mae.train", "mape.train", "r2.train", "cor.train",
                         "rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")
rownames(df.summary) = NULL

# Add timing
end_time <- Sys.time()
time = difftime(end_time, start_time, units = "secs")
df.summary$time = rep(time, nrow(df.summary))

# Save raw trial summary
summaryfile = paste0("../Results/ksvm_kernlab_perf_summary_metrics_", datafile)
write.csv(df.summary, summaryfile, row.names = FALSE, quote = FALSE)

# ==========================================
# Create and save test metric summary (mean ± stdev)

test_metrics = df.summary[, c("rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")]

summary.stats = sapply(test_metrics, function(x) {
  avg = round(mean(x), 3)
  sdv = round(sd(x), 3)
  sprintf("%.3f (±%.3f)", avg, sdv)
})

summary.stats = as.data.frame(t(summary.stats))
rownames(summary.stats) = NULL

# Save to file
summary.stats.file = paste0("../Results/ksvm_kernlab_perf_test_metrics_summary_", datafile)
write.csv(summary.stats, summary.stats.file, row.names = FALSE, quote = FALSE)

# ==========================================
# Optional: Save detail files (commented by default)

df.detail.train = as.data.frame(df.detail.train)
colnames(df.detail.train) = NULL
detail.train.file = paste0("../Results/ksvm_kernlab_perf_detail.train_", datafile)
write.csv(df.detail.train, detail.train.file, row.names = FALSE, quote = FALSE)

df.detail.test = as.data.frame(df.detail.test)
colnames(df.detail.test) = NULL
detail.test.file = paste0("../Results/ksvm_kernlab_perf_detail.test_", datafile)
write.csv(df.detail.test, detail.test.file, row.names = FALSE, quote = FALSE)
