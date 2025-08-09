start_time <- Sys.time()

library('kernlab')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")

datafile = "Sluice.csv"
# datafile = "Culvert.csv"

a <- read.csv(paste0("../Data/", datafile))

n <- nrow(a)
m <- ncol(a)
m.var <- m - 1
n_train <- round(n * 0.75)
ntrial <- 100

# Hyperparameter grid
C.grid <- c(0.01, 0.1, 1, 5, 10)
epsilon.grid <- c(0.01, 0.1, 1)
kernel.grid <- c("rbfdot", "polydot", "vanilladot")

grid <- expand.grid(C = C.grid, epsilon = epsilon.grid, kernel = kernel.grid)

no_cores <- 15
cl <- makeCluster(no_cores)
registerDoParallel(cl)

results.grid <- list()

for (g in 1:nrow(grid)) {
  this.C <- grid$C[g]
  this.eps <- grid$epsilon[g]
  this.kernel <- as.character(grid$kernel[g])
  
  cat("Running grid: kernel =", this.kernel, ", C =", this.C, ", epsilon =", this.eps, "\n")
  
  res <- foreach(t = 1:ntrial, .combine = rbind, .packages = c("kernlab", "Metrics")) %dopar% {
    set.seed(t)  # Ensure reproducibility
    picked <- sample(seq_len(n), size = n_train)
    a_train <- a[picked, ]
    a_test <- a[-picked, ]
    
    model <- tryCatch({
      ksvm(dsa ~ ., data = a_train,
           kernel = this.kernel,
           epsilon = this.eps,
           C = this.C)
    }, error = function(e) return(NULL))
    
    if (is.null(model)) {
      return(rep(NA, 4))
    }
    
    pred_test <- predict(model, a_test[, 1:m.var])
    rmse.test <- rmse(a_test[, m], pred_test)
    cor.test <- cor(a_test[, m], pred_test)
    
    pred_train <- predict(model, a_train[, 1:m.var])
    rmse.train <- rmse(a_train[, m], pred_train)
    cor.train <- cor(a_train[, m], pred_train)
    
    c(rmse.train, cor.train, rmse.test, cor.test)
  }
  
  colnames(res) <- c("rmse.train", "cor.train", "rmse.test", "cor.test")
  results.grid[[g]] <- list(grid = grid[g, ], summary = res)
}

stopCluster(cl)

# Find optimal configuration
avg.rmse <- sapply(results.grid, function(r) mean(r$summary[, "rmse.test"], na.rm = TRUE))
best.index <- which.min(avg.rmse)
best.config <- results.grid[[best.index]]$grid
best.summary <- results.grid[[best.index]]$summary

cat("Best parameters:\n")
print(best.config)

# Save results
summaryfile <- paste0("../Results/ksvm_grid_summary_", datafile)
write.csv(best.summary, summaryfile, row.names = FALSE, quote = FALSE)

# Save optimal hyperparameters
paramsfile <- paste0("../Results/ksvm_grid_bestparams_", datafile)
write.csv(best.config, paramsfile, row.names = FALSE, quote = FALSE)

end_time <- Sys.time()
cat("Total runtime:", difftime(end_time, start_time, units = "mins"), "\n")
