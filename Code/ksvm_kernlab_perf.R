#==========================================
start_time <- Sys.time()


library('kernlab')
# library('clusterSim')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")

# datafile = "Culvert.csv"
datafile = "Sluice.csv"

a <- read.csv(paste0("../Data/",datafile))

n <- nrow(a)
m <- ncol(a)
m.var = m-1

# a <-data.Normalization(a,type="n4",normalization="column")

n_train <- round(n*0.75)

ntrial=100

no_cores <- 4
cl <- makeCluster(no_cores)
registerDoParallel(cl)

t=1

res <- foreach(t = 1:ntrial, .combine = rbind) %dopar% {

  library('kernlab')
  library('Metrics')
  
  print(paste0("t = ", t ,"\n"))
  
  # # Fixed split
  # a_train <-a[1:n_train,]
  # a_test <-a[(n_train+1):n,]
  
  # Random split
  # set.seed(777)
  
  # randomly split data in r
  picked = sample(seq_len(n),size = n_train)
  a_train =a[picked,]
  a_test =a[-picked,]
  
  x_train = a_train[,1:m.var]
  y_train = a_train[,m]
  
  model <- ksvm(dsa~.,
                     data = a_train,
                     kernel = 'rbfdot',#Radial Basis kernel "Gaussian"
                     kpar=list(sigma=0.05),#para for the selected kernel
                     C=5,
                     cross=3)
  
  pred_train = predict(model,as.matrix(a_train[,1:m.var]))
  rmse.train = rmse(a_train[,m],pred_train)
  cor.train = cor(a_train[,m],pred_train)
  
  pred_test = predict(model,as.matrix(a_test[,1:m.var]))
  rmse.test =rmse(a_test[,m],pred_test)
  cor.test =cor(a_test[,m],pred_test)
  
  summary <-c(rmse.train, cor.train, rmse.test,cor.test)
  detail.train <-cbind(a_train[,m],pred_train)
  detail.test <-cbind(a_test[,m],pred_test)
  
  res <-list(summary, detail.train, detail.test)
}
stopCluster(cl)
dim(res)

df.summary = NULL
df.detail.train = NULL
df.detail.test = NULL
for(t in 1:ntrial){
  df.summary = rbind(df.summary,res[t,1][[1]])
  df.detail.train = cbind(df.detail.train, res[t,2][[1]])
  df.detail.test = cbind(df.detail.test, res[t,3][[1]])
}

#Summary
df.summary = as.data.frame(df.summary)
colnames(df.summary) = c("rmse.train","cor.train","rmse.test","cor.test")
rownames(df.summary) = NULL

end_time <- Sys.time()
time=difftime(end_time, start_time, units = "secs")

df.summary$time = rep(time,nrow(df.summary))

summaryfile = paste0("../Results/ksvm_kernlab_perf","_summary_",datafile)
write.csv(df.summary,summaryfile, row.names = FALSE, quote = FALSE)

# detail.train
df.detail.train = as.data.frame(df.detail.train)
colnames(df.detail.train) = NULL
detail.train.file = paste0("../Results/ksvm_kernlab_perf","_detail.train_",datafile)
write.csv(df.detail.train,detail.train.file, row.names = FALSE, quote = FALSE)

# detail.test
df.detail.test = as.data.frame(df.detail.test)
colnames(df.detail.test) = NULL
detail.test.file = paste0("../Results/ksvm_kernlab_perf","_detail.test_",datafile)
write.csv(df.detail.test,detail.test.file, row.names = FALSE, quote = FALSE)

