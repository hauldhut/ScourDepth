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
predrandomForest = function(M,data) # the PRED function
{ return (predict(M,as.matrix(data[,1:m.var]))) }

#==========================================
start_time <- Sys.time()

library('randomForest')
# library('clusterSim')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")

# datafile = "Culvert.csv"
datafile = "Sluice.csv"

if (datafile == "Culvert.csv"){
  ntree = 300
  nodesize = 1
}else{
  ntree = 500
  nodesize = 3
}

a <- read.csv(paste0("../Data/",datafile))

# #Remove F
# a = a[,c(1,3,4,5,6)]

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
  
  x_train = a_train[,1:m.var]
  y_train = a_train[,m]
  
  # model = anfis(a_train,num.labels, max.iter = 30, step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX",
  #               type.implication.func = "ZADEH")
  
  model = randomForest(x = x_train,
                              y = y_train,
                              ntree = ntree, 
                              mtry = 3,
                              nodesize = nodesize,
                       )
  
  library('rminer')
  imp = Importance(model, data = a_train)
  
  pred_train = predict(model,as.matrix(a_train[,1:m.var]))
  rmse.train = rmse(a_train[,m],pred_train)
  cor.train = cor(a_train[,m],pred_train)
  
  pred_test = predict(model,as.matrix(a_test[,1:m.var]))
  rmse.test =rmse(a_test[,m],pred_test)
  cor.test =cor(a_test[,m],pred_test)
  
  summary <-c(rmse.train, cor.train, rmse.test,cor.test)
  detail.train <-cbind(a_train[,m],pred_train)
  detail.test <-cbind(a_test[,m],pred_test)
  
  res <-list(summary, detail.train, detail.test, model, imp)
}
stopCluster(cl)
dim(res)

# df.summary = NULL
# df.detail.train = NULL
# df.detail.test = NULL
# for(t in 1:ntrial){
#   df.summary = rbind(df.summary,res[t,1][[1]])
#   df.detail.train = cbind(df.detail.train, res[t,2][[1]])
#   df.detail.test = cbind(df.detail.test, res[t,3][[1]])
# }
# 
# #Summary
# df.summary = as.data.frame(df.summary)
# colnames(df.summary) = c("rmse.train","cor.train","rmse.test","cor.test")
# rownames(df.summary) = NULL
# 
# end_time <- Sys.time()
# time=difftime(end_time, start_time, units = "secs")
# 
# df.summary$time = rep(time,nrow(df.summary))
# 
# summaryfile = paste0("../Results/randomForest_perf","_summary_",datafile)
# write.csv(df.summary,summaryfile, row.names = FALSE, quote = FALSE)
# 
# # detail.train
# df.detail.train = as.data.frame(df.detail.train)
# colnames(df.detail.train) = NULL
# detail.train.file = paste0("../Results/randomForest_perf","_detail.train_",datafile)
# write.csv(df.detail.train,detail.train.file, row.names = FALSE, quote = FALSE)
# 
# # detail.test
# df.detail.test = as.data.frame(df.detail.test)
# colnames(df.detail.test) = NULL
# detail.test.file = paste0("../Results/randomForest_perf","_detail.test_",datafile)
# write.csv(df.detail.test,detail.test.file, row.names = FALSE, quote = FALSE)


#===================CALCULATE & DRAW=======================
par(mfrow=c(1,3))


#Calculate the importance of variables returned from Importance function of rminer 
nrow = m-1
ncol=ntrial
impmat <-matrix(nrow = nrow,ncol=ncol)
i = 1
for(i in 1:ntrial){
  impmat[,i] = res[i,5][[1]]$imp[1:nrow] #the last element is for dependent variable/output (dsa), it is also equal to 0
}
rownames(impmat) <-colnames(a)[1:nrow] 

#Normalize 
for(i in 1:ncol){
  impmat[,i] <-impmat[,i]/colSums(impmat)[i]
}

impmat_avg = matrix(nrow = nrow, ncol = 2)
for(i in 1:nrow(impmat)){
  impmat_avg[i,1] = mean(impmat[i,])
  impmat_avg[i,2] = sd(impmat[i,])/sqrt(ntrial)
}
rownames(impmat_avg) <- rownames(impmat)
colnames(impmat_avg) <- c("mean","se")

# impmat_avg

df.impmat_avg <- as.data.frame(impmat_avg)
df.impmat_avg$var <-rownames(df.impmat_avg)
df.impmat_avg <- df.impmat_avg[order(df.impmat_avg$var),]

df.impmat_avg


summaryfile = paste0("../Results/randomForest_VBSA_",datafile)
write.csv(df.impmat_avg,summaryfile, row.names = FALSE, quote = FALSE)

maintitle = paste0("RF by rminer.Importance - ",datafile)
colname = gsub("a","/a",rownames(df.impmat_avg))
ImportanceBarPlot(df.impmat_avg, maintitle, colname)

#COMPARE TO
#Use importance of RF
impmat <-matrix(nrow = m.var,ncol=ntrial)
for(i in 1:ntrial){
  model = res[i,4][[1]]
  impmat[,i] <-model$importance
}
rownames(impmat) <-rownames(res[1,4][[1]]$importance) 

impmat_avg = matrix(nrow = m.var, ncol = 2)
for(i in 1:nrow(impmat)){
  impmat_avg[i,1] = mean(impmat[i,])
  impmat_avg[i,2] = sd(impmat[i,])/sqrt(ntrial)
}
rownames(impmat_avg) <- rownames(impmat)
colnames(impmat_avg) <- c("mean","se")

# impmat_avg

df.impmat_avg <- as.data.frame(impmat_avg)
df.impmat_avg$var <-rownames(df.impmat_avg)
df.impmat_avg <- df.impmat_avg[order(df.impmat_avg$var),]

df.impmat_avg

maintitle = paste0("RF by RF.Importance - ",datafile)
colname = gsub("a","/a",rownames(df.impmat_avg))
ImportanceBarPlot(df.impmat_avg, maintitle, colname)


#COMPARE TO
#use varImp function in caret to Calculate the importance of variables
library('caret')
# methods(varImp)
#caret implements RandomForest, so it understands the returned RandomForest object (model) 

impmat_caret <-matrix(nrow = m.var,ncol=ntrial)
i = 1
for(i in 1:ntrial){
  model = res[i,4][[1]]
  impmat_caret[,i] = as.matrix(varImp(model)) 
}
rownames(impmat_caret) <-colnames(a)[1:m.var] 

#Normalize 
for(i in 1:ntrial){
  impmat_caret[,i] <-impmat_caret[,i]/colSums(impmat_caret)[i]
}
# impmat_caret

impmat_caret_avg = matrix(nrow = m.var, ncol = 2)
for(i in 1:nrow(impmat_caret)){
  impmat_caret_avg[i,1] = mean(impmat_caret[i,])
  impmat_caret_avg[i,2] = sd(impmat_caret[i,])/sqrt(ntrial)
}
rownames(impmat_caret_avg) <- rownames(impmat_caret)
colnames(impmat_caret_avg) <- c("mean","se")

# impmat_caret_avg

df.impmat_caret_avg <- as.data.frame(impmat_caret_avg)
df.impmat_caret_avg$var <-rownames(df.impmat_caret_avg)
df.impmat_caret_avg <- df.impmat_caret_avg[order(df.impmat_caret_avg$var),]

df.impmat_caret_avg


maintitle = paste0("RF by caret.varImp - ",datafile)
colname = gsub("a","/a",rownames(df.impmat_caret_avg))
ImportanceBarPlot(df.impmat_caret_avg, maintitle, colname)






