################## SELECT DATASET ##################

# datafiles.Com = c("Culvert.csv")
datafiles.Com = c("Sluice.csv")

##################
summarizePerf <- function(df.summary){
  df.summary[is.na(df.summary)]=0
  
  ntrial = nrow(df.summary)
  time = df.summary$time[1]
  
  rmse.train.mean = mean(as.numeric(df.summary$rmse.train))
  rmse.train.sd = sd(as.numeric(df.summary$rmse.train))
  rmse.train.min = min(as.numeric(df.summary$rmse.train))
  
  cor.train.mean = mean(as.numeric(df.summary$cor.train))
  cor.train.sd = sd(as.numeric(df.summary$cor.train))
  cor.train.max = max(as.numeric(df.summary$cor.train))
  
  
  rmse.test.mean = mean(as.numeric(df.summary$rmse.test))
  rmse.test.sd = sd(as.numeric(df.summary$rmse.test))
  rmse.test.min = min(as.numeric(df.summary$rmse.test))
  rmse.test.min.idx = which(df.summary$rmse.test==rmse.test.min)
  
  cor.test.mean = mean(as.numeric(df.summary$cor.test))
  cor.test.sd = sd(as.numeric(df.summary$cor.test))
  cor.test.max = max(as.numeric(df.summary$cor.test))
  cor.test.max.idx = which(df.summary$cor.test==cor.test.max)
  
  
  df.sum = NULL
  df.sum <-rbind(df.sum, data.frame(stage = "train", rmse.mean = rmse.train.mean, rmse.sd = rmse.train.sd, rmse.min = rmse.train.min,cor.mean = cor.train.mean, cor.sd = cor.train.sd, cor.max = cor.train.max, ntrial = ntrial, time=time, rmse.test.min.idx=rmse.test.min.idx, cor.test.max.idx=cor.test.max.idx))
  df.sum <-rbind(df.sum, data.frame(stage = "test", rmse.mean = rmse.test.mean, rmse.sd = rmse.test.sd, rmse.min = rmse.test.min, cor.mean = cor.test.mean, cor.sd = cor.test.sd, cor.max = cor.test.max,ntrial = ntrial, time=time, rmse.test.min.idx=rmse.test.min.idx, cor.test.max.idx=cor.test.max.idx))
  
  return(df.sum)
}


library(hash)
library(ggplot2)
library(RColorBrewer)
library("patchwork")
library("Metrics")

setwd("~/Manuscripts/105ScourDepth/Code")


Methods = c("GMDHreg_G1","ksvm_kernlab", "mlp_nnet", "randomForest")
Methods.Short = c("GMDH","SVR", "ANN", "RF")

datafiles = c("Sluice.csv","Culvert.csv")

h.config2Res = hash()
for(i in 1:length(Methods)){
  Method = Methods[i]
  Method.Short = Methods.Short[i]
  for(datafile in datafiles){
    config = paste0(Method,"_perf_summary_",datafile)
    summaryfile = paste0("../Results/",config)
    print(summaryfile)
    if(file.exists(paste0(summaryfile))){
      df.summary <- read.csv(summaryfile, header = TRUE) 
      df.Res = summarizePerf(df.summary)
      df.Res$method = rep(Method.Short, nrow(df.Res))
      df.Res$datafile = rep(datafile, nrow(df.Res))
      h.config2Res[[config]] = df.Res
    }else{
      print("File is not existing...!")
    }
    
  }
}
length(h.config2Res)
keys(h.config2Res)


################## AI-BASED COMPARISON ##################
#Compare among AI methods
Methods.Com = c("GMDHreg_G1","ksvm_kernlab", "mlp_nnet", "randomForest")


df.Res.Com = NULL
for(Method in Methods.Com){
  for(datafile in datafiles.Com){
    config = paste0(Method,"_perf_summary_",datafile)
    df.Res = h.config2Res[[config]]
    print(config)
    print(df.Res)
    df.Res.Com = rbind(df.Res.Com, df.Res)
  }
}

dim(df.Res.Com)
df.Res.Com

FigureType = paste0("../Figure_Final/Figure_Perf_Compare")

ntrial = df.Res.Com$ntrial[1]

h.Res2Plot = hash()
yValues = c("RMSE","Cor","Time")
for(yValue in yValues){
  if(yValue == "RMSE"){
    df.Res.Com$yValue = df.Res.Com$rmse.mean
    df.Res.Com$yValue.se = df.Res.Com$rmse.sd/sqrt(ntrial)
    y = "RMSE\n"
  }else if(yValue == "Cor"){
    df.Res.Com$yValue = df.Res.Com$cor.mean
    df.Res.Com$yValue.se = df.Res.Com$cor.sd/sqrt(ntrial)
    y = "CC\n"
  }else{
    df.Res.Com$yValue = df.Res.Com$time  
    df.Res.Com$yValue.se = 0
    y = "Time (s)\n"
  }
  
  fontsize = 12
  p = ggplot(data=df.Res.Com, aes(x=method, y=yValue, fill=stage)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=yValue-yValue.se, ymax=yValue+yValue.se), width=.2,
                  position=position_dodge(.9))
  
  # title = paste0(paste0(datafiles.Com,collapse = "-"),"_by_",yValue)
  title = ""#Performance Comparison
  p = p + labs(title=title, x="\n", y = y) +
    # scale_fill_brewer(palette="Blues") +
    theme_light() +
    theme(
      text = element_text(size=fontsize),# All font sizes
      plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size=fontsize),
      # legend.title=element_blank(),#Remove legend title (Network)
      axis.text = element_text(size = fontsize),
      axis.title = element_text(size = fontsize),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
      legend.position = "top"  # Move legend to bottom
    )
  h.Res2Plot[[yValue]] = p
}

################## EMPIRICAL EQUATION-BASED COMPARISON ##################
#Compare to imperical methods

if("Sluice.csv" %in% datafiles.Com){
  Methods.Emperical = c("Chatterjee1994","Hopfinger2004","Sarkar2005","Dey2006")  
}else{
  # Methods.Emperical = c("Lim1995","Abt1985","Emami2010","Taha2020","Abida1991")
  Methods.Emperical = c("Lim1995","Abt1985","Emami2010","Taha2020")
}


df.Res.Emp = NULL
for(Method in Methods.Emperical){
  for(datafile in datafiles.Com){
    config = paste0("res_",Method,"_perf_summary_",datafile)
    summaryfile = paste0("../Results/",config)
    print(summaryfile)
    if(file.exists(paste0(summaryfile))){
      df.summary <- read.csv(summaryfile, header = TRUE) 
      # df.Res = summarizePerf(df.summary)
      # df.Res$method = rep(Method.Short, nrow(df.Res))
      # df.Res$datafile = rep(datafile, nrow(df.Res))
      # h.config2Res[[config]] = df.Res
      df.summary$method = Method
      df.summary$datafile = datafile
      df.Res.Emp = rbind(df.Res.Emp, df.summary)
    }else{
      print("File is not existing...!")
    }
    
  }
}

dim(df.Res.Emp)
df.Res.Emp

FigureType = paste0("../Figure_Final/Figure_Perf_Compare")

ntrial = df.Res.Com$ntrial[1]

h.ResEmp2Plot = hash()
yValues = c("RMSE","Cor")
for(yValue in yValues){
  if(yValue == "RMSE"){
    df.Res.Emp$yValue = df.Res.Emp$rmse.train
    df.Res.Emp$yValue.se = 0
    y = "RMSE\n"
  }else if(yValue == "Cor"){
    df.Res.Emp$yValue = df.Res.Emp$cor.train
    df.Res.Emp$yValue.se = 0
    y = "CC\n"
  }else{
    df.Res.Com$yValue = df.Res.Emp$time  
    df.Res.Com$yValue.se = 0
    y = "Time (s)\n"
  }
  
  fontsize = 12
  p = ggplot(data=df.Res.Emp, aes(x=method, y=yValue), alpha=0.5) +
    geom_bar(stat="identity",position=position_dodge(), fill="#FF7F50")
    # geom_errorbar(aes(ymin=yValue, ymax=yValue), width=.2,
    #               position=position_dodge(.9))
  
  # title = paste0(paste0(datafiles.Com,collapse = "-"),"_by_",yValue)
  title = ""#Performance Comparison
  p = p + labs(title=title, x="\n", y = y) +
    # scale_fill_brewer(palette="Blues") +
    theme_light() +
    theme(
      text = element_text(size=fontsize),# All font sizes
      plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size=fontsize),
      # legend.title=element_blank(),#Remove legend title (Network)
      axis.text = element_text(size = fontsize),
      axis.title = element_text(size = fontsize),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)
    )
  h.ResEmp2Plot[[yValue]] = p
}

  
library('cowplot')
plot_grid(h.Res2Plot[["RMSE"]], h.Res2Plot[["Cor"]], h.ResEmp2Plot[["RMSE"]], h.ResEmp2Plot[["Cor"]], labels=c("A", "B", "C","D"), ncol = 2, nrow = 2)

summaryfile = paste0(FigureType,"_",paste0(Methods.Short,collapse = "-"),"_",paste0(datafiles.Com,collapse = "-"))
ggsave(paste0(summaryfile,".pdf"), width = 10, height = 10)

df.Res.Com$rmse_combined <- paste(round(df.Res.Com$rmse.mean, 3), 
                          " (±", 
                          round(df.Res.Com$rmse.sd, 3), 
                          ")", 
                          sep = "")
df.Res.Com$cor_combined <- paste(round(df.Res.Com$cor.mean, 3), 
                                  " (±", 
                                  round(df.Res.Com$cor.sd, 3), 
                                  ")", 
                                  sep = "")
write.csv(df.Res.Com, paste0(summaryfile,"_ML.csv"))
write.csv(df.Res.Emp, paste0(summaryfile,"_EM.csv"))

################## AI-BASED CORRELATION ##################
#Draw scatter plot for correlation for trials having maximum Cor for each method

h.Method2ObsPred = hash()
for(i in 1:length(Methods)){
  Method = Methods[i]
  Method.Short = Methods.Short[i]
  trial.idx = df.Res.Com[df.Res.Com$method==Method.Short,]$cor.test.max.idx[2]
  cor.test.max = df.Res.Com[df.Res.Com$method==Method.Short,]$cor.max[2]
  cat(trial.idx, cor.test.max,"\n")
  
  for(datafile in datafiles.Com){
    config = paste0(Method,"_perf_detail.test_",datafile)
    detail.test.file = paste0("../Results/",config)
    print(detail.test.file)
    if(file.exists(paste0(detail.test.file))){
      df.detail.test <- read.csv(detail.test.file, header = FALSE) 
      obs = df.detail.test[,(trial.idx-1)*2+1]
      pred = df.detail.test[,(trial.idx-1)*2+2]
      
      print(cor(obs, pred))
      
      h.Method2ObsPred[[Method.Short]] = data.frame(obs, pred)
    }else{
      print("File is not existing...!")
    }
  }  
}
length(h.Method2ObsPred)

FigureType = paste0("../Figure_Final/Figure_Cor")

fontsize = 12
h.Method2CorPlot = hash()
pi=0
for(Method.Short in keys(h.Method2ObsPred)){
  
  print(paste0("Processing plot for ", Method.Short))
  df.ObsPred = h.Method2ObsPred[[Method.Short]]
  
  cor = round(cor(df.ObsPred$obs, df.ObsPred$pred),3)
  rmse = round(rmse(df.ObsPred$obs, df.ObsPred$pred),3)
  cat(cor,rmse,"\n")
  
  df.ObsPred$legend = rep(paste0("CC = ",cor,"\n","RMSE = ", rmse),nrow(df.ObsPred))
  
  p <- ggplot(df.ObsPred, aes(x=obs, y=pred, color = legend)) +
    geom_point() +
    geom_smooth(method=lm) + #add linear trend line
    # geom_line(size=1) +
    # ggtitle(paste0(Method.Short,"_",paste0(datafiles.Com,collapse = "-"))) + #Set plot title
    ggtitle(Method.Short) + #Set plot title
    
    xlab("\nObservation") + #Set x label
    ylab("Prediction\n") + #Set y label
    theme_light() +
    theme(
      text = element_text(size=fontsize),# All font sizes
      plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size=fontsize),
      legend.position = c(0.7, 0.15),
      legend.title=element_blank(),#Remove legend title (Network)
      axis.text = element_text(size = fontsize),
      axis.title = element_text(size = fontsize)
    )
  h.Method2CorPlot[[Method.Short]] = p
  
}
library('cowplot')
#Methods.Short = c("ANFIS", "GMDH","SVM", "ANN", "RF")
plot_grid(h.Method2CorPlot[["GMDH"]], h.Method2CorPlot[["SVR"]],h.Method2CorPlot[["ANN"]],h.Method2CorPlot[["RF"]], labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

ggsave(paste0(FigureType,"_",paste0(Methods.Short,collapse = "-"),"_",paste0(datafiles.Com,collapse = "-"),".pdf"), width = 10, height = 10)

