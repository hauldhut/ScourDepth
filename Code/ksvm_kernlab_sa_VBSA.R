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


library('kernlab')
# library('clusterSim')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")

datafile = "Culvert.csv"
# datafile = "Sluice.csv"

if (datafile == "Culvert.csv"){
  C = 1
}else{#all hyperparameters are the same as submitted
  C = 5
}

a <- read.csv(paste0("../Data/",datafile))

# #Remove F
# a = a[,c(1,3,4,5,6)]

n <- nrow(a)
m <- ncol(a)
m.var <- m-1

# a <-data.Normalization(a,type="n4",normalization="column")

n_train <- round(n*0.75)

ntrial=100

no_cores <- 8
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
  
  # model <- ksvm(dsa~.,
  #               data = a_train,
  #               kernel = 'rbfdot',#Radial Basis kernel "Gaussian"
  #               kpar=list(sigma=0.05),#para for the selected kernel
  #               C=5)
  
  library('rminer') #ksvm or svm – support vector machine (classification and regression, uses ksvm from kernlab package)
  #Thus, we used the same settings
  model <- fit(dsa~., 
                data=a_train, 
                model="ksvm", 
                 epsilon = 0.1,
                 kernel = 'rbfdot',
                C=C)
  imp = Importance(model, data = a_train)
  
  pred_train = predict(model,as.matrix(a_train[,1:m.var]))
  res_train = rmse(a_train[,m],pred_train)
  
  pred_test = predict(model,as.matrix(a_test[,1:m.var]))
  res_test =rmse(a_test[,m],pred_test)
  
  res <-list(res_train, res_test, imp)
}
stopCluster(cl)



#===================CALCULATE & DRAW=======================
# par(mfrow=c(1,2))

#Calculate the importance of variables returned from Importance function of rminer 
nrow = m.var
ncol=ntrial
impmat <-matrix(nrow = nrow,ncol=ncol)
i = 1
for(i in 1:ntrial){
  impmat[,i] = res[i,3][[1]]$imp[1:nrow] #the last element is for dependent variable/output (dsa), it is also equal to 0
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

impmat_avg

df.impmat_avg <- as.data.frame(impmat_avg)
df.impmat_avg$var <-rownames(df.impmat_avg)
df.impmat_avg <- df.impmat_avg[order(df.impmat_avg$var),]

df.impmat_avg

summaryfile = paste0("../Results/ksvm_kernlab_VBSA_",datafile)
write.csv(df.impmat_avg,summaryfile, row.names = FALSE, quote = FALSE)

maintitle = paste0("SVM by rminer.Importance - ",datafile)
colname = gsub("a","/a",rownames(df.impmat_avg))
ImportanceBarPlot(df.impmat_avg, maintitle, colname)


end_time <- Sys.time()
time=difftime(end_time, start_time, units = "secs")

