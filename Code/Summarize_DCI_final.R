library(hash)
library(ggplot2)
library(RColorBrewer)
library("patchwork")

################# LOAD RESULTS ##################
setwd("~/Manuscripts/105ScourDepth/Code")


Methods = c("GMDHreg_G2","ksvm_kernlab", "mlp_nnet", "randomForest")
Methods.Short = c("GMDH","SVR", "ANN", "RF")

datafiles = c("Sluice.csv","Culvert.csv")

h.config2Imp = hash()
for(datafile in datafiles){
  for(i in 1:length(Methods)){
    if(i==1){
      if(datafile=="Culvert.csv"){
        Method = "GMDHreg_G2"
      }else{
        Method = "GMDHreg_G1"
      }
      Method.Short = "GMDH"
    }else{
      Method = Methods[i]
      Method.Short = Methods.Short[i]  
    }
  
    config = paste0(Method,"_DCI_",datafile)
    Impfile = paste0("../Results/",config)
    print(Impfile)
    if(file.exists(paste0(Impfile))){
      df.Imp <- read.csv(Impfile, header = TRUE) 
      df.Imp$param = gsub("a","/a",df.Imp$var)
      df.Imp$method = rep(Method.Short, nrow(df.Imp))
      h.config2Imp[[config]] = df.Imp
    }else{
      print("File is not existing...!")
    }
    
  }
}
length(h.config2Imp)
keys(h.config2Imp)


################# DCI-BASED COMPARE ##################
#Compare


#Run 2 times for two datasets

datafiles.Com = c("Culvert.csv") #Figure (A), (B)
# datafiles.Com = c("Sluice.csv") #Figure (C), (D)

df.Imp.Com = NULL
for(datafile in datafiles.Com){
  for(i in 1:length(Methods)){
    if(i==1){
      if(datafile=="Culvert.csv"){
        Method = "GMDHreg_G2"
      }else{
        Method = "GMDHreg_G1"
      }
      # Method.Short = "GMDH"
    }else{
      Method = Methods[i]
      # Method.Short = Methods.Short[i]  
    }
  # for(datafile in datafiles){
    config = paste0(Method,"_DCI_",datafile)
    
    df.Imp = h.config2Imp[[config]]
    print(config)
    print(df.Imp)
    df.Imp.Com = rbind(df.Imp.Com, df.Imp)
  }
}
dim(df.Imp.Com)
df.Imp.Com
df.Imp.Com[df.Imp.Com$method=="ANN",]$rmse.mean

FigureType = paste0("../Figure_Final/Figure_DCI_Compare")

ntrial = 100

h.Imp2Plot = hash()
yValues = c("RMSE","R")
for(yValue in yValues){
  if(yValue == "RMSE"){
    df.Imp.Com$yValue = df.Imp.Com$rmse.mean
    df.Imp.Com$yValue.se = df.Imp.Com$rmse.se
    y = "RMSE\n"
  }else{
    df.Imp.Com$yValue = df.Imp.Com$r.mean
    df.Imp.Com$yValue.se = df.Imp.Com$r.se
    y = "CC\n"#Correlation Coefficient (CC)
  }
  
  if(datafile=="Culvert.csv"){
    df.Imp.Com$param <- factor(df.Imp.Com$param, levels = c("dt/a", "Fd", "s", "ww", "blk"))
  }else{
    df.Imp.Com$param <- factor(df.Imp.Com$param, levels = c("dt/a", "Fd", "L/a", "D50/a", "F"))
  }
  
  fontsize = 12
  p = ggplot(data=df.Imp.Com, aes(x=method, y=yValue, fill=param, by=param))+
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=yValue-yValue.se, ymax=yValue+yValue.se), width=.2,position=position_dodge(.9))
  
  if(datafile=="Culvert.csv"){
    p = p +scale_fill_manual(labels = c(expression(italic(paste("d"[t],"/a"))), 
                                        expression(italic(paste("F"[d]))),
                                        expression(italic("s")), 
                                        expression(italic("ww")), 
                                        expression(italic("blk"))),
                             values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"))
  }else{
    p = p +scale_fill_manual(labels = c(expression(italic(paste("d"[t],"/a"))), 
                                        expression(italic(paste("F"[d]))),
                                        expression(italic("L/a")), 
                                        expression(italic(paste("D"[50],"/a"))), 
                                        expression(italic("F"))),
                             values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"))
  }
    
    
  # title = paste0(paste0(datafiles.Com,collapse = "-"),"_DCI_by_",yValue)
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
  h.Imp2Plot[[yValue]] = p
}
  
  
library('cowplot')
if (datafiles.Com == c("Culvert.csv")){
  plot_grid(h.Imp2Plot[["RMSE"]], h.Imp2Plot[["R"]], labels=c("A", "B"), ncol = 2, nrow = 1)  
}else{
  plot_grid(h.Imp2Plot[["RMSE"]], h.Imp2Plot[["R"]], labels=c("C", "D"), ncol = 2, nrow = 1)
}


ggsave(paste0(FigureType,"_",paste0(Methods.Short,collapse = "-"),"_",paste0(datafiles.Com,collapse = "-"),".pdf"), width = 10, height = 4)
