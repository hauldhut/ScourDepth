# ==========================================
start_time <- Sys.time()

library('nnet')
library('Metrics')
library(caret)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")

# datafile <- "Culvert.csv"  # hoặc 
datafile <- "Sluice.csv"

# Thiết lập số hidden units dựa theo dataset
if (datafile == "Culvert.csv") {
  size <- 2
} else {
  size <- 1
}

a <- read.csv(paste0("../Data/", datafile))
max.dsa <- max(a$dsa)

n <- nrow(a)
m <- ncol(a)
m.var <- m - 1

no_cores <- 4
cl <- makeCluster(no_cores)
registerDoParallel(cl)

method <- "mlp_nnet"
cat("Method:", method, "\n")

# Chuẩn bị 4-fold cross-validation
set.seed(123)
a$fold <- createFolds(a[[m]], k = 4, list = FALSE)

results <- foreach(k = 1:4, .combine = rbind) %dopar% {
  library('nnet')
  library('Metrics')
  
  train_set <- a[a$fold != k, 1:m]
  test_set  <- a[a$fold == k, 1:m]
  
  x_train <- train_set[, 1:m.var]
  y_train <- train_set[, m]
  x_test  <- test_set[, 1:m.var]
  y_test  <- test_set[, m]
  
  model <- nnet(x = x_train,
                y = y_train / max.dsa,
                size = size,
                decay = 0.01,
              )
  
  pred_train <- predict(model, x_train) * max.dsa
  pred_test <- predict(model, x_test) * max.dsa
  
  rmse.train <- rmse(y_train, pred_train)
  mae.train <- mae(y_train, pred_train)
  mape.train <- mape(y_train, pred_train)
  r2.train <- 1 - sum((y_train - pred_train)^2) / sum((y_train - mean(y_train))^2)
  cor.train <- cor(y_train, pred_train)
  
  rmse.test <- rmse(y_test, pred_test)
  mae.test <- mae(y_test, pred_test)
  mape.test <- mape(y_test, pred_test)
  r2.test <- 1 - sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2)
  cor.test <- cor(y_test, pred_test)
  
  c(rmse.train, mae.train, mape.train, r2.train, cor.train,
    rmse.test, mae.test, mape.test, r2.test, cor.test)
}

stopCluster(cl)

# ==========================================
# Tổng hợp kết quả

df.summary <- as.data.frame(results)
colnames(df.summary) <- c("rmse.train", "mae.train", "mape.train", "r2.train", "cor.train",
                          "rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")

# Ghi thời gian
end_time <- Sys.time()
time <- difftime(end_time, start_time, units = "secs")
df.summary$time <- rep(time, nrow(df.summary))

# Lưu kết quả từng fold
summaryfile <- paste0("../Results/", method, "_perf_cv_summary_metrics_", datafile)
write.csv(df.summary, summaryfile, row.names = FALSE, quote = FALSE)

# ==========================================
# Tạo bảng tổng hợp test metrics (mean ± sd)

test_metrics <- df.summary[, c("rmse.test", "mae.test", "mape.test", "r2.test", "cor.test")]

summary.stats <- sapply(test_metrics, function(x) {
  avg <- round(mean(x), 3)
  sdv <- round(sd(x), 3)
  sprintf("%.3f (±%.3f)", avg, sdv)
})

summary.stats <- as.data.frame(t(summary.stats))
rownames(summary.stats) <- NULL

# Lưu file summary test metrics
summary.stats.file <- paste0("../Results/", method, "_perf_cv_summary_metrics_test_", datafile)
write.csv(summary.stats, summary.stats.file, row.names = FALSE, quote = FALSE)
