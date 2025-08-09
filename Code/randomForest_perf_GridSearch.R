start_time <- Sys.time()

library('randomForest')
library('Metrics')
library('foreach')
library('doParallel')

setwd("~/Manuscripts/105ScourDepth/Code")

datafile <- "Culvert.csv"
# datafile <- "Sluice.csv"

a <- read.csv(paste0("../Data/", datafile))

n <- nrow(a)
m <- ncol(a)
m.var <- m - 1

n_train <- round(n * 0.75)
ntrial <- 100

# Define hyperparameter grid
ntree.grid <- c(100, 300, 500)
mtry.grid <- c(1, 2, 3)
nodesize.grid <- c(1, 3, 5, 10)
grid <- expand.grid(ntree = ntree.grid, mtry = mtry.grid, nodesize = nodesize.grid)

no_cores <- 15
cl <- makeCluster(no_cores)
registerDoParallel(cl)

results.grid <- list()

for (g in 1:nrow(grid)) {
  this.ntree <- grid$ntree[g]
  this.mtry <- grid$mtry[g]
  this.nodesize <- grid$nodesize[g]
  
  cat("Running grid: ntree =", this.ntree, ", mtry =", this.mtry, ", nodesize =", this.nodesize, "\n")
  
  res <- foreach(t = 1:ntrial, .combine = rbind, .packages = c("randomForest", "Metrics")) %dopar% {
    set.seed(t)
    picked <- sample(seq_len(n), size = n_train)
    a_train <- a[picked, ]
    a_test <- a[-picked, ]
    
    x_train <- a_train[, 1:m.var]
    y_train <- a_train[, m]
    x_test <- a_test[, 1:m.var]
    y_test <- a_test[, m]
    
    model <- randomForest(x = x_train,
                          y = y_train,
                          ntree = this.ntree,
                          mtry = this.mtry,
                          nodesize = this.nodesize)
    
    pred_train <- predict(model, x_train)
    pred_test <- predict(model, x_test)
    
    rmse.train <- rmse(y_train, pred_train)
    cor.train <- cor(y_train, pred_train)
    rmse.test <- rmse(y_test, pred_test)
    cor.test <- cor(y_test, pred_test)
    
    c(rmse.train, cor.train, rmse.test, cor.test)
  }
  
  colnames(res) <- c("rmse.train", "cor.train", "rmse.test", "cor.test")
  results.grid[[g]] <- list(params = grid[g, ], summary = res)
}

stopCluster(cl)

# Identify optimal hyperparameters
avg.rmse <- sapply(results.grid, function(r) mean(r$summary[, "rmse.test"], na.rm = TRUE))
best.index <- which.min(avg.rmse)
best.config <- results.grid[[best.index]]$params
best.summary <- results.grid[[best.index]]$summary

cat("✅ Best hyperparameters:\n")
print(best.config)

# Save best results
summaryfile <- paste0("../Results/randomForest_grid_summary_", datafile)
write.csv(best.summary, summaryfile, row.names = FALSE, quote = FALSE)

paramsfile <- paste0("../Results/randomForest_grid_bestparams_", datafile)
write.csv(best.config, paramsfile, row.names = FALSE, quote = FALSE)

end_time <- Sys.time()
cat("⏱ Total runtime:", difftime(end_time, start_time, units = "mins"), "\n")
