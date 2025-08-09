#======================================
predrandomForest = function(M,data) # the PRED function
{ return (predict(M,as.matrix(data[,1:m.var]))) }

#==========================================
start_time <- Sys.time()

library('randomForest')
library('Metrics')
library('foreach')
library('doParallel')
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

n_train <- round(n * 0.75)
ntrial <- 100
no_cores <- 15

cl <- makeCluster(no_cores)
registerDoParallel(cl)

t = 1
res <- foreach(t = 1:ntrial, .combine = rbind) %dopar% {
  library('randomForest')
  library('Metrics')
  library('rminer')
  
  print(paste0("t = ", t, "\n"))
  
  picked = sample(seq_len(n), size = n_train)
  a_train = a[picked, ]
  a_test = a[-picked, ]
  
  x_train = a_train[, 1:m.var]
  y_train = a_train[, m]
  
  model = randomForest(
    x = x_train,
    y = y_train,
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
  
  summary = c(rmse.train, mae.train, mape.train, r2.train, cor.train,
              rmse.test, mae.test, mape.test, r2.test, cor.test)
  
  detail.train = cbind(a_train[, m], pred_train)
  detail.test = cbind(a_test[, m], pred_test)
  
  list(summary, detail.train, detail.test, model, imp)
}

stopCluster(cl)

#==============================
# Post-processing
#==============================
df.summary = NULL
df.detail.train = NULL
df.detail.test = NULL

for(t in 1:ntrial){
  df.summary = rbind(df.summary, res[t,1][[1]])
  df.detail.train = cbind(df.detail.train, res[t,2][[1]])
  df.detail.test = cbind(df.detail.test, res[t,3][[1]])
}

df.summary = as.data.frame(df.summary)
colnames(df.summary) = c(
  "rmse.train", "mae.train", "mape.train", "r2.train", "cor.train",
  "rmse.test", "mae.test", "mape.test", "r2.test", "cor.test"
)

end_time <- Sys.time()
time = difftime(end_time, start_time, units = "secs")
df.summary$time = rep(time, nrow(df.summary))

summaryfile = paste0("../Results/randomForest_perf_summary_metrics_", datafile)
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
summary.stats.file = paste0("../Results/randomForest_perf_summary_metrics_test_", datafile)
write.csv(summary.stats, summary.stats.file, row.names = FALSE, quote = FALSE)

# Optional: Write detailed predictions (commented)
df.detail.train = as.data.frame(df.detail.train)
colnames(df.detail.train) <- NULL
detail.train.file = paste0("../Results/randomForest_perf", "_detail.train_", datafile)
write.csv(df.detail.train, detail.train.file, row.names = FALSE, quote = FALSE)

df.detail.test = as.data.frame(df.detail.test)
colnames(df.detail.test) <- NULL
detail.test.file = paste0("../Results/randomForest_perf", "_detail.test_", datafile)
write.csv(df.detail.test, detail.test.file, row.names = FALSE, quote = FALSE)
