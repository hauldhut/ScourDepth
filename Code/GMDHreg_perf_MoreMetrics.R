#======================================

predGMDH = function(M,data) # the PRED function
{ return (predict(M,as.matrix(data[,1:m.var]))) }

#==========================================
start_time <- Sys.time()

library('GMDHreg')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")

datafile = "Culvert.csv"
# datafile = "Sluice.csv"


a <- read.csv(paste0("../Data/", datafile))

n <- nrow(a)
m <- ncol(a)
m.var = m - 1

n_train <- round(n * 0.75)
ntrial = 100

no_cores <- 15
cl <- makeCluster(no_cores)
registerDoParallel(cl)

if(datafile=="Culvert.csv"){
  G = 2 # polynomial degree: 0=linear, 1=interaction, 2=quadratic
}else{
  G = 1
}

res <- foreach(t = 1:ntrial, .combine = rbind) %dopar% {
  library('GMDHreg')
  library('Metrics')
  library('rminer')
  
  print(paste0("t = ", t, "\n"))
  
  picked = sample(seq_len(n), size = n_train)
  a_train = a[picked, ]
  a_test = a[-picked, ]
  
  x_train = a_train[, 1:m.var]
  y_train = a_train[, m]
  
  model = gmdh.combi(X = as.matrix(x_train),
                     y = y_train,
                     G = G,
                     criteria = "PRESS")
  
  imp = Importance(model, data = a_train, PRED = predGMDH, outindex = m)
  
  pred_train = predict(model, as.matrix(a_train[, 1:m.var]))
  pred_test = predict(model, as.matrix(a_test[, 1:m.var]))
  
  # --- Training Metrics ---
  rmse.train = rmse(a_train[, m], pred_train)
  mae.train = mae(a_train[, m], pred_train)
  mape.train = mape(a_train[, m], pred_train)
  r2.train = 1 - sum((a_train[, m] - pred_train)^2) / sum((a_train[, m] - mean(a_train[, m]))^2)
  cor.train = cor(a_train[, m], pred_train)
  
  # --- Test Metrics ---
  rmse.test = rmse(a_test[, m], pred_test)
  mae.test = mae(a_test[, m], pred_test)
  mape.test = mape(a_test[, m], pred_test)
  r2.test = 1 - sum((a_test[, m] - pred_test)^2) / sum((a_test[, m] - mean(a_test[, m]))^2)
  cor.test = cor(a_test[, m], pred_test)
  
  summary <- c(rmse.train, mae.train, mape.train, r2.train, cor.train,
               rmse.test, mae.test, mape.test, r2.test, cor.test)
  
  detail.train <- cbind(a_train[, m], pred_train)
  detail.test <- cbind(a_test[, m], pred_test)
  
  res <- list(summary, detail.train, detail.test, imp)
}
stopCluster(cl)

# ============================
# Aggregate Results
df.summary = NULL
df.detail.train = NULL
df.detail.test = NULL
for (t in 1:ntrial) {
  df.summary = rbind(df.summary, res[t, 1][[1]])
  df.detail.train = cbind(df.detail.train, res[t, 2][[1]])
  df.detail.test = cbind(df.detail.test, res[t, 3][[1]])
}

df.summary = as.data.frame(df.summary)
colnames(df.summary) = c("rmse.train", "mae.train", "mape.train", "r2.train", "cor.train",
                         "rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")
rownames(df.summary) = NULL

end_time <- Sys.time()
time = difftime(end_time, start_time, units = "secs")
df.summary$time = rep(time, nrow(df.summary))

# ============================
# Save Results
if (G == 1) {
  Method = "GMDHreg_G1"
} else {
  Method = "GMDHreg_G2"
}

summaryfile = paste0("../Results/", Method, "_perf_summary_metrics_", datafile)
write.csv(df.summary, summaryfile, row.names = FALSE, quote = FALSE)

# Optional: save train/test prediction details
df.detail.train = as.data.frame(df.detail.train)
colnames(df.detail.train) = NULL
detail.train.file = paste0("../Results/", Method, "_perf_detail.train_", datafile)
write.csv(df.detail.train, detail.train.file, row.names = FALSE, quote = FALSE)

df.detail.test = as.data.frame(df.detail.test)
colnames(df.detail.test) = NULL
detail.test.file = paste0("../Results/", Method, "_perf_detail.test_", datafile)
write.csv(df.detail.test, detail.test.file, row.names = FALSE, quote = FALSE)

# ============================
# Summary statistics for test metrics

test_metrics = df.summary[, c("rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")]

summary.stats = sapply(test_metrics, function(x) {
  avg = round(mean(x), 3)
  sdv = round(sd(x), 3)
  sprintf("%.3f (±%.3f)", avg, sdv)
})

summary.stats = as.data.frame(t(summary.stats))
rownames(summary.stats) = NULL

# Save summary stats to file
summary.stats.file = paste0("../Results/", Method, "_perf_summary_metrics_test_", datafile)
write.csv(summary.stats, summary.stats.file, row.names = FALSE, quote = FALSE)
