
ImportanceBarPlot <- function(df, maintitle, colname){
  #Draw by barplot
  max = ceiling(max(df['mean']))
  base_r_barplot <- barplot(df$mean ~ var,  # Draw and store Base R barplot
                            df,
                            main = maintitle,
                            xlab = "Variables", 
                            ylab = "Importance",
                            ylim = c(0, max),
                            names = colname)
  #add error bar
  arrows(x0 = base_r_barplot,                           # Add error bars
         y0 = df$mean + df$se,
         y1 = df$mean - df$se,
         angle = 90,
         code = 3,#1: ceiling, 2: floor, 3: both
         length = 0.05)#Length of error bar 
}

#======================================

start_time <- Sys.time()


library('nnet')
# library('clusterSim')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")


# datafile = "Culvert.csv"
datafile = "Sluice.csv"


a <- read.csv(paste0("../Data/",datafile))
max.dsa = max(a$dsa)
n <- nrow(a)
m <- ncol(a)
m.var = m-1

# a <-data.Normalization(a,type="n4",normalization="column")

n_train <- round(n*0.75)

ntrial=100

no_cores <- 8
cl <- makeCluster(no_cores)
registerDoParallel(cl)

t=1

res <- foreach(t = 1:ntrial, .combine = rbind) %dopar% {

  library('nnet')
  library('Metrics')
  
  print(paste0("t = ", t ,"\n"))
  
  
  # randomly split data in r
  picked = sample(seq_len(n),size = n_train)
  a_train =a[picked,]
  a_test =a[-picked,]
  
  x_train = a_train[,1:m.var]
  y_train = a_train[,m]
  
  model <- nnet(x = x_train,
               y= y_train/max.dsa,
               size = 2, #number of units in the hidden layer. Can be zero if there are skip-layer units.
               rang = 0.1, #Initial random weights on [-rang, rang]. Value about 0.5 unless the inputs are large, in which case it should be chosen so that rang * max(|x|) is about 1.
               decay = 5e-4, #parameter for weight decay. Default 0.
               maxit = 200) #maximum number of iterations. Default 100.
  
  pred_train = predict(model,as.matrix(a_train[,1:m.var]))*max.dsa
  rmse.train = rmse(a_train[,m],pred_train)
  cor.train = cor(a_train[,m],pred_train)
  
  pred_test = predict(model,as.matrix(a_test[,1:m.var]))*max.dsa
  rmse.test =rmse(a_test[,m],pred_test)
  cor.test =cor(a_test[,m],pred_test)
  
  summary <-c(rmse.train, cor.train, rmse.test,cor.test)
  detail.train <-cbind(a_train[,m],pred_train)
  detail.test <-cbind(a_test[,m],pred_test)
  
  res <-list(summary, detail.train, detail.test, model)
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

summaryfile = paste0("../Results/mlp_nnet_perf","_summary_",datafile)
write.csv(df.summary,summaryfile, row.names = FALSE, quote = FALSE)

# detail.train
df.detail.train = as.data.frame(df.detail.train)
colnames(df.detail.train) = NULL
detail.train.file = paste0("../Results/mlp_nnet_perf","_detail.train_",datafile)
write.csv(df.detail.train,detail.train.file, row.names = FALSE, quote = FALSE)

# detail.test
df.detail.test = as.data.frame(df.detail.test)
colnames(df.detail.test) = NULL
detail.test.file = paste0("../Results/mlp_nnet_perf","_detail.test_",datafile)
write.csv(df.detail.test,detail.test.file, row.names = FALSE, quote = FALSE)
