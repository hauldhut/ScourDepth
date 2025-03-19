################# VBSA-BASED COMPARE ##################
library(hash)
library(ggplot2)
library(RColorBrewer)
library("patchwork")

setwd("~/Manuscripts/105ScourDepth/Code")
#Draw Importance for each method

Methods = c("GMDHreg_G1","ksvm_kernlab", "mlp_nnet", "randomForest")
Methods.Short = c("GMDH","SVR", "ANN", "RF")

#Run only one time for two dataset
datafiles = c("Sluice.csv","Culvert.csv")

h.config2Imp = hash()
for(i in 1:length(Methods)){
  Method = Methods[i]
  Method.Short = Methods.Short[i]
  for(datafile in datafiles){
    config = paste0(Method,"_VBSA_",datafile)
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


###################################
#Compare
Methods.Com = c("GMDHreg_G1","ksvm_kernlab", "mlp_nnet", "randomForest")
# datafiles.Com = c("Sluice.csv")
# datafiles.Com = c("Culvert.csv")

datafiles.Com = c("Culvert.csv","Sluice.csv")


h.datafile2Imp = hash()
for(datafile in datafiles.Com){
  df.Imp.Com = NULL
  for(Method in Methods.Com){
    config = paste0(Method,"_VBSA_",datafile)
    df.Imp = h.config2Imp[[config]]
    print(config)
    print(df.Imp)
    df.Imp.Com = rbind(df.Imp.Com, df.Imp)
  }
  h.datafile2Imp[[datafile]] = df.Imp.Com
}
length(h.datafile2Imp)
h.datafile2Imp[["Culvert.csv"]]

FigureType = paste0("../Figure_Final/Figure_VBSA_Compare")

ntrial = 100

h.datafile2ImpPlot = hash()
for(datafile in datafiles.Com){
  df.Imp.Com = h.datafile2Imp[[datafile]]
  
  if(datafile=="Culvert.csv"){
    df.Imp.Com$param <- factor(df.Imp.Com$param, levels = c("dt/a", "Fd", "s", "ww", "blk"))
  }else{
    df.Imp.Com$param <- factor(df.Imp.Com$param, levels = c("dt/a", "Fd", "L/a", "D50/a", "F"))
  }
  
  fontsize = 12
  p = ggplot(data=df.Imp.Com, aes(x=method, y=mean, fill=param, by=param)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                  position=position_dodge(.9))
  
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
  
  # title = "Parameter Importance"
  p = p + labs(title=title, x="\n", y = "Importance\n") +
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
  h.datafile2ImpPlot[[datafile]] = p
}

library('cowplot')
plot_grid(h.datafile2ImpPlot[["Culvert.csv"]], h.datafile2ImpPlot[["Sluice.csv"]], labels=c("A", "B"), ncol = 2, nrow = 1)

ggsave(paste0(FigureType,"_",paste0(Methods.Short,collapse = "-"),".pdf"), width = 10, height = 4)

