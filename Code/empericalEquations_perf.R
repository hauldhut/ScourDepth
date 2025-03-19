summarizePerf <- function(res){
  rmse.train.mean = mean(as.numeric(res[,1]))
  rmse.train.sd = sd(as.numeric(res[,1]))
  rmse.train.min = min(as.numeric(res[,1]))
  
  rmse.test.mean = mean(as.numeric(res[,2]))
  rmse.test.sd = sd(as.numeric(res[,2]))
  rmse.test.min = min(as.numeric(res[,2]))
  
  perf.summary.df = NULL
  perf.summary.df <-rbind(perf.summary.df, data.frame(stage = "train", mean = rmse.train.mean, sd = rmse.train.sd, min = rmse.train.min))
  perf.summary.df <-rbind(perf.summary.df, data.frame(stage = "test", mean = rmse.test.mean, sd = rmse.test.sd, min = rmse.test.min))
  
  return(perf.summary.df)
}

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

library("SciViews")
# library('clusterSim')
library('Metrics')
library(foreach)
library(doParallel)

setwd("~/Manuscripts/105ScourDepth/Code")


# datafile =  "Culvert.csv"
datafile = "Sluice.csv"
a <- read.csv(paste0("../Data/",datafile))

n <- nrow(a)
m <- ncol(a)
m.var = m-1

ntrial = 100

if(datafile=="Sluice.csv"){
  pred_Sarkar2005 = 0.42*(a$Fd)^0.49*(a$La)^(-0.36)*(a$dta)^1.08
  pred_Dey2006 = 2.59*(a$Fd)^0.94*(a$La)^(-0.37)*(a$dta)^0.16*(a$D50a)^0.25
  pred_Chatterjee1994 = 0.775*a$F
  pred_Hopfinger2004 = 0.43*((a$D50a)^0.11)*(a$Fd)^1.1 - 0.2 
  
  res_Sarkar2005.rmse = rmse(a[,m],pred_Sarkar2005)
  res_Dey2006.rmse = rmse(a[,m],pred_Dey2006)
  res_Chatterjee1994.rmse = rmse(a[,m],pred_Chatterjee1994)
  res_Hopfinger2004.rmse = rmse(a[,m],pred_Hopfinger2004)
  
  res_Sarkar2005.r = cor(a[,m],pred_Sarkar2005)
  res_Dey2006.r = cor(a[,m],pred_Dey2006)
  res_Chatterjee1994.r = cor(a[,m],pred_Chatterjee1994)
  res_Hopfinger2004.r = cor(a[,m],pred_Hopfinger2004)
  
  for(Method in c("res_Chatterjee1994","res_Hopfinger2004","res_Sarkar2005","res_Dey2006")){
  # for(Method in c("res_Chatterjee1994")){
    if(Method=="res_Sarkar2005"){
      rmse = res_Sarkar2005.rmse 
      r = res_Sarkar2005.r
    }else if(Method=="res_Dey2006"){
      rmse = res_Dey2006.rmse 
      r = res_Dey2006.r
    }else if(Method=="res_Hopfinger2004"){
      rmse = res_Hopfinger2004.rmse 
      r = res_Hopfinger2004.r
    }else{
      rmse = res_Chatterjee1994.rmse 
      r = res_Chatterjee1994.r
    }
    # df.summary = data.frame(rmse.train=rep(rmse,ntrial),cor.train=rep(r,ntrial),rmse.test=rep(rmse,ntrial),cor.test=rep(r,ntrial),time=rep(0,ntrial))    
    df.summary = data.frame(rmse.train=rmse,cor.train=r,rmse.test=rmse,cor.test=r)    
    summaryfile = paste0("../Results/",Method,"_perf","_summary_",datafile)
    write.csv(df.summary,summaryfile, row.names = FALSE, quote = FALSE)
  }
}else{
  a_s0 = a[a$s==0,]#Circular
  pred_Lim1995 = 0.45*a_s0$Fd
  pred_Abt1985 = 3.67*(a_s0$Fd)^0.57*(1.2)^0.4*(1.2)^(-0.4)
  pred_Emami2010 = (0.6*a_s0$dta+1.8)*ln(a_s0$Fd) + 1.23*a_s0$dta-2.25
  
  res_Lim1995.rmse = rmse(a_s0[,m],pred_Lim1995)
  res_Abt1985.rmse = rmse(a_s0[,m],pred_Abt1985)
  res_Emami2010.rmse = rmse(a_s0[,m],pred_Emami2010)
  
  res_Lim1995.r = cor(a_s0[,m],pred_Lim1995)
  res_Abt1985.r = cor(a_s0[,m],pred_Abt1985)
  res_Emami2010.r = cor(a_s0[,m],pred_Emami2010)
  
  a_s1 = a[a$s==1,]#Square
  pred_Taha2020 = 0.56*a_s1$Fd + 0.45*a_s1$dta - 1.05
  pred_Abida1991 = (exp((a_s1$Fd-2)/2.03) - 0.373)*(1.2/153)^(-0.275)
  
  res_Taha2020.rmse = rmse(a_s1[,m],pred_Taha2020)
  res_Abida1991.rmse = rmse(a_s1[,m],pred_Abida1991)
  
  res_Taha2020.r = cor(a_s1[,m],pred_Taha2020)
  res_Abida1991.r = cor(a_s1[,m],pred_Abida1991)
  
  for(Method in c("res_Lim1995","res_Abt1985","res_Emami2010","res_Taha2020")){
    if(Method=="res_Lim1995"){
      rmse = res_Lim1995.rmse 
      r = res_Lim1995.r
    }else if(Method=="res_Abt1985"){
      rmse = res_Abt1985.rmse 
      r = res_Abt1985.r
    }else if(Method=="res_Emami2010"){
      rmse = res_Emami2010.rmse 
      r = res_Emami2010.r
    }else if(Method=="res_Taha2020"){
      rmse = res_Taha2020.rmse 
      r = res_Taha2020.r
    }else{
      rmse = res_Abida1991.rmse 
      r = res_Abida1991.r
    }
    # df.summary = data.frame(rmse.train=rep(rmse,ntrial),cor.train=rep(r,ntrial),rmse.test=rep(rmse,ntrial),cor.test=rep(r,ntrial),time=rep(0,ntrial))    
    df.summary = data.frame(rmse.train=rmse,cor.train=r,rmse.test=rmse,cor.test=r)    
    summaryfile = paste0("../Results/",Method,"_perf","_summary_",datafile)
    write.csv(df.summary,summaryfile, row.names = FALSE, quote = FALSE)
  }
}

