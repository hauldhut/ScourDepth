summarizeSA <- function(res){
  ntrial = (ncol(res)-1)/2
  cn <- c("var", "rmse.mean", "rmse.se","r.mean", "r.se")
  
  res.summary = data.frame(matrix(nrow = m.var, ncol = length(cn)))
  
  colnames(res.summary) <- cn
  res.summary$var <- res$var
  for(i in 1:m.var){
    # nna.i = which(!is.na(res[i,]))#not NA indices
    
    rmse.i = seq(2,ntrial*2,2)
    # rmse.i = intersect(nna.i,rmse.i)
    
    r.i = seq(3,ntrial*2+1,2)
    # r.i = intersect(nna.i,r.i)
    
    rmse.vec = res[i,rmse.i][!is.na(res[i,rmse.i])]
    r.vec = res[i,r.i][!is.na(res[i,r.i])]
    
    print(length(rmse.vec))
    print(length(r.vec))
    
    res.summary[i,2] = mean(rmse.vec)
    res.summary[i,3] = sd(rmse.vec)/sqrt(length(rmse.vec))
    res.summary[i,4] = mean(r.vec)
    res.summary[i,5] = sd(r.vec)/sqrt(length(r.vec))
  }
  return(res.summary)
}

#======================================
start_time <- Sys.time()

library('randomForest')
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
m.var <- m-1

# a <-data.Normalization(a,type="n4",normalization="column")

n_train <- round(n*0.75)

ntrial=100

no_cores <- 8
cl <- makeCluster(no_cores)
registerDoParallel(cl)


res <- foreach(t = 1:ntrial, .combine = cbind) %dopar% {
  
  # t=10
  library('randomForest')
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
  
  sa.df <- NULL
  
  for(v in 1:m.var) {
    
    training = a_train[,-c(v)]
    testing = a_test[,-c(v)]
    
    
    x_train = training[,1:(m.var-1)]
    y_train = training[,m.var]

  
    model = randomForest(x = x_train,
                                 y = y_train,
                                 ntree = 500, mtry = sqrt(ncol(x_train)))

 
    pred_test = predict(model,as.matrix(testing[,1:(m.var-1)]))
    rmse =Metrics::rmse(testing[,m.var],pred_test)
    r =cor(testing[,m.var],pred_test)
    # rsq = cc*cc; #R squared
    
    sa.df <- rbind(sa.df, data.frame(
      rmse = rmse,
      r=r))
  }
  res <- sa.df
}
stopCluster(cl)

res

res <- cbind(var=colnames(a[,1:m.var]), res)

# if(any(is.na(res))){
#   stop("Results contain NA/Inf...!")
# }

res.summary = summarizeSA(res)
res.summary

filename = paste0("../Results/randomForest_DCI_",datafile)
write.csv(res.summary,filename)

