library(hash)
library(ggplot2)
library(RColorBrewer)
library("patchwork")

################# LOAD RESULTS ##################
setwd("~/Manuscripts/105ScourDepth/Code")


Methods = c("GMDHreg_G1","ksvm_kernlab", "mlp_nnet", "randomForest")
Methods.Short = c("GMDH","SVR", "ANN", "RF")

datafiles = c("Sluice.csv","Culvert.csv")

h.config2SA = hash()
for(i in 1:length(Methods)){
  Method = Methods[i]
  Method.Short = Methods.Short[i]
  for(datafile in datafiles){
    config = paste0(Method,"_sa_",datafile)
    SAfile = paste0("../Results/",config)
    print(SAfile)
    if(file.exists(paste0(SAfile))){
      df.SA <- read.csv(SAfile, header = TRUE) 
      df.SA$param = gsub("a","/a",df.SA$var)
      df.SA$method = rep(Method.Short, nrow(df.SA))
      h.config2SA[[config]] = df.SA
    }else{
      print("File is not existing...!")
    }
    
  }
}
length(h.config2SA)
keys(h.config2SA)


################# DCI-BASED COMPARE ##################
#Compare
Methods.Com = c("GMDHreg_G1","ksvm_kernlab", "mlp_nnet", "randomForest")

#Run 2 times for two datasets

# datafiles.Com = c("Culvert.csv") #Figure (A), (B)
datafiles.Com = c("Sluice.csv") #Figure (C), (D)

df.SA.Com = NULL
for(Method in Methods.Com){
  for(datafile in datafiles.Com){
  # for(datafile in datafiles){
    config = paste0(Method,"_sa_",datafile)
    
    df.SA = h.config2SA[[config]]
    print(config)
    print(df.SA)
    df.SA.Com = rbind(df.SA.Com, df.SA)
  }
}
dim(df.SA.Com)
df.SA.Com
df.SA.Com[df.SA.Com$method=="ANN",]$rmse.mean

FigureType = paste0("../Figure_Final/Figure_DCI_Compare")

ntrial = 100

h.SA2Plot = hash()
yValues = c("RMSE","R")
for(yValue in yValues){
  if(yValue == "RMSE"){
    df.SA.Com$yValue = df.SA.Com$rmse.mean
    df.SA.Com$yValue.se = df.SA.Com$rmse.se
    y = "RMSE\n"
  }else{
    df.SA.Com$yValue = df.SA.Com$r.mean
    df.SA.Com$yValue.se = df.SA.Com$r.se
    y = "CC\n"#Correlation Coefficient (CC)
  }
  
  if(datafile=="Culvert.csv"){
    df.SA.Com$param <- factor(df.SA.Com$param, levels = c("dt/a", "Fd", "s", "ww", "blk"))
  }else{
    df.SA.Com$param <- factor(df.SA.Com$param, levels = c("dt/a", "Fd", "L/a", "D50/a", "F"))
  }
  
  fontsize = 12
  p = ggplot(data=df.SA.Com, aes(x=method, y=yValue, fill=param, by=param)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=yValue-yValue.se, ymax=yValue+yValue.se), width=.2,
                  position=position_dodge(.9))
  
  # title = paste0(paste0(datafiles.Com,collapse = "-"),"_SA_by_",yValue)
  title = ""#Sensitivity analysis
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
  h.SA2Plot[[yValue]] = p
}
  
  
library('cowplot')
if (datafiles.Com == c("Culvert.csv")){
  plot_grid(h.SA2Plot[["RMSE"]], h.SA2Plot[["R"]], labels=c("A", "B"), ncol = 2, nrow = 1)  
}else{
  plot_grid(h.SA2Plot[["RMSE"]], h.SA2Plot[["R"]], labels=c("C", "D"), ncol = 2, nrow = 1)
}


ggsave(paste0(FigureType,"_",paste0(Methods.Short,collapse = "-"),"_",paste0(datafiles.Com,collapse = "-"),".pdf"), width = 10, height = 4)
