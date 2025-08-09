start_time <- Sys.time()

library('nnet')
library('Metrics')
library('foreach')
library('doParallel')

setwd("~/Manuscripts/105ScourDepth/Code")

datafile <- "Culvert.csv"
# datafile <- "Sluice.csv"

a <- read.csv(paste0("../Data/", datafile))
max.dsa <- max(a$dsa)

n <- nrow(a)
m <- ncol(a)
m.var <- m - 1

n_train <- round(n * 0.75)
ntrial <- 100

# Grid
size.grid <- c(1, 2, 3, 4, 5)
decay.grid <- c(0, 0.0001, 0.001, 0.01, 0.1, 1)
grid <- expand.grid(size = size.grid, decay = decay.grid)

no_cores <- 15
cl <- makeCluster(no_cores)
registerDoParallel(cl)

results.grid <- list()

for (g in 1:nrow(grid)) {
  this.size <- grid$size[g]
  this.decay <- grid$decay[g]
  
  cat("Running grid: size =", this.size, ", decay =", this.decay, "\n")
  
  res <- foreach(t = 1:ntrial, .combine = rbind, .packages = c("nnet", "Metrics")) %dopar% {
    set.seed(t)
    picked <- sample(seq_len(n), size = n_train)
    a_train <- a[picked, ]
    a_test <- a[-picked, ]
    
    x_train <- a_train[, 1:m.var]
    y_train <- a_train[, m]
    x_test <- a_test[, 1:m.var]
    y_test <- a_test[, m]
    
    model <- tryCatch({
      nnet(x = x_train,
           y = y_train / max.dsa,
           size = this.size,
           decay = this.decay,
           maxit = 200,
           linout = TRUE,
           trace = FALSE,
           rang = 0.1)
    }, error = function(e) return(NULL))
    
    if (is.null(model)) {
      return(rep(NA, 4))
    }
    
    pred_train <- predict(model, x_train) * max.dsa
    pred_test <- predict(model, x_test) * max.dsa
    
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

# Find optimal configuration
avg.rmse <- sapply(results.grid, function(r) mean(r$summary[, "rmse.test"], na.rm = TRUE))
best.index <- which.min(avg.rmse)
best.config <- results.grid[[best.index]]$params
best.summary <- results.grid[[best.index]]$summary

cat("✅ Best hyperparameters:\n")
print(best.config)

# Set output name using best parameters
method <- paste0("mlp_nnet_size", best.config$size, "_decay", best.config$decay)

# Save summary
summaryfile <- paste0("../Results/", method, "_summary_", datafile)
write.csv(best.summary, summaryfile, row.names = FALSE, quote = FALSE)

# Save best parameter configuration
paramsfile <- paste0("../Results/", method, "_bestparams_", datafile)
write.csv(best.config, paramsfile, row.names = FALSE, quote = FALSE)

# Detail train/test
# Optional: regenerate 100 models with best config if you need predicted values
# Let me know if you want to include those predictions.

end_time <- Sys.time()
cat("⏱ Total runtime:", difftime(end_time, start_time, units = "mins"), "\n")
