user_wd='...' #user should set name of directory where tabualted data and map files from stage 5 are located (this is also where the plots will be saved) 

library(ggplot2)
library(grid)
library(rgdal)
library(mapproj)
library(munsell)

rm(list=ls())
setwd(user_wd)
windowsFonts(Times=windowsFont("TT Times New Roman"))
#load map shapefile data
#requires "Longhurst_world_v4_2010" and "ne_110m_land" which can be sourced from https://www.naturalearthdata.com/downloads/
long<-readOGR(dsn=user_wd, layer="Longhurst_world_v4_2010")
long_df <- fortify(long)
wmap<-readOGR(dsn=user_wd, layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

####create violin plots

#sm
#load data and create dataframe
setwd(user_wd)
load("sm_Results_tables.Rdata")
disc1 <- trend_betap2_no
disc2 <- trend_betap2_weatherhead

  Trend <- c(rbind(disc1,disc2))
disc2 <- disc_betap2_weatherhead
  discmag <- c(rbind(disc2,disc2))
  disc2 <- disc2_betap2_weatherhead
  discmag2 <- c(rbind(disc2,disc2))
  disc <- rep(c(rep("w/o",each=40000),rep("sw+m",each=40000)),times=23)
  

  Region <- rep(c(1:23),each=80000)
 
  daf <- cbind(Region,Trend,disc,discmag,discmag2)
  daf <- as.data.frame(daf)
  daf[,2] <- Trend#weird error converting to data frame ledas to factorisation
  daf[,4] <- discmag
    daf[,5] <- discmag2
  daf$Region <- as.factor(Region)
  daf$disc <- as.factor(daf$disc)   


   ResultsTab_comb <- rbind(ResultsTab_no,ResultsTab_weatherhead)
    disc <- c(rep("w/o",each=23),rep("sw+m",each=23))
     ResultsTab_comb_old <- ResultsTab_comb
    ResultsTab_comb <-cbind(ResultsTab_comb,disc)
     colnames(ResultsTab_comb)[4] <- "upper"
    colnames(ResultsTab_comb)[3] <- "lower"
    colnames(ResultsTab_comb)[2] <- "trend"
ResultsTab_comb<- as.data.frame(ResultsTab_comb)
ResultsTab_comb[,3] <- ResultsTab_comb_old[,3]
ResultsTab_comb[,4] <- ResultsTab_comb_old[,4]
ResultsTab_comb[,2] <- ResultsTab_comb_old[,2]
ResultsTab_comb[,6] <- ResultsTab_comb_old[,6]
ResultsTab_comb[,7] <- ResultsTab_comb_old[,7]
ResultsTab_comb[,8] <- ResultsTab_comb_old[,8]
ResultsTab_comb[,9] <- ResultsTab_comb_old[,9]
ResultsTab_comb[,10] <- ResultsTab_comb_old[,10]
ResultsTab_comb[,11] <- ResultsTab_comb_old[,11]
       ResultsTab_comb$Region <- as.factor(ResultsTab_comb$Region)
  ResultsTab_comb$disc <- as.factor(ResultsTab_comb$disc) 
  
  

#create plot 
      psm1 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("orange","black"),guide=guide_legend(title="Scenario"),labels=c("SeaWiFS & MERIS/MODIS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-5,5,2.5),minor_breaks = seq(-5,5,1.25),limits=c(-5,5)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0))  +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("orange","black"))+ggtitle("(a)")
      
          psm9 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("orange","black"),guide=guide_legend(title="Scenario"),labels=c("SeaWiFS & MERIS/MODIS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-5,5,1),minor_breaks = seq(-5,5,0.5),limits=c(-5,5)) +theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))  +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("orange","black"))+ggtitle("(a)")
      # theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))+
      #scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))

  daf <- daf[rep(c(F,T),each=40000),]
ResultsTab_comb <- ResultsTab_comb[24:46,]
          psm2 <- ggplot(daf,aes(x=Region,y=discmag))+geom_violin(adjust=2,scale="width",fill="orange")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=round(seq(-0.6,0.6,0.2),1),minor_breaks = round(seq(-0.6,0.6,0.2),1),limits=c(-0.61,0.61)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag1,ymin=ldm1,ymax=udm1),position = "dodge",colour="orange",size=1.2)+ggtitle("(b)")
     
                  psm3 <- ggplot(daf,aes(x=Region,y=discmag2))+geom_violin(adjust=2,scale="width",fill="orange")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=round(seq(-0.6,0.6,0.2),1),minor_breaks = round(seq(-0.6,0.6,0.2),1),limits=c(-0.61,0.61)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag2,ymin=ldm2,ymax=udm2),position = "dodge",colour="orange",size=1.2)+ggtitle("(c)")
     

  #sw
#load data and create dataframe

setwd(user_wd)
load("sw_Results_tables.Rdata")
disc1 <- trend_betap2_no
disc2 <- trend_betap2_weatherhead

  Trend <- c(rbind(disc1,disc2))
  disc2 <- disc_betap2_weatherhead
  discmag <- c(rbind(disc2,disc2))
  disc <- rep(c(rep("w/o",each=40000),rep("sw+m",each=40000)),times=23)
  

  Region <- rep(c(1:23),each=80000)
 
  daf <- cbind(Region,Trend,disc,discmag)
  daf <- as.data.frame(daf)
    daf[,2] <- Trend#weird error converting to data frame ledas to factorisation
  daf[,4] <- discmag
  daf$Region <- as.factor(Region)
  daf$disc <- as.factor(daf$disc)   

    ResultsTab_comb <- rbind(ResultsTab_no,ResultsTab_weatherhead)
    disc <- c(rep("w/o",each=23),rep("sw+m",each=23))
     ResultsTab_comb_old <- ResultsTab_comb
    ResultsTab_comb <-cbind(ResultsTab_comb,disc)
     colnames(ResultsTab_comb)[4] <- "upper"
    colnames(ResultsTab_comb)[3] <- "lower"
    colnames(ResultsTab_comb)[2] <- "trend"
ResultsTab_comb<- as.data.frame(ResultsTab_comb)
ResultsTab_comb[,3] <- ResultsTab_comb_old[,3]
ResultsTab_comb[,4] <- ResultsTab_comb_old[,4]
ResultsTab_comb[,2] <- ResultsTab_comb_old[,2]
ResultsTab_comb[,6] <- ResultsTab_comb_old[,6]
ResultsTab_comb[,7] <- ResultsTab_comb_old[,7]
ResultsTab_comb[,8] <- ResultsTab_comb_old[,8]
       ResultsTab_comb$Region <- as.factor(ResultsTab_comb$Region)
  ResultsTab_comb$disc <- as.factor(ResultsTab_comb$disc) 
#create plot
      psw1 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("violetred3","black"),guide=guide_legend(title="Scenario"),labels=c("SeaWiFS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-4.5,4.5,1.5),minor_breaks = seq(-4.5,4.5,0.75),limits=c(-4.6,4.6)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0))  +  
         geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("violetred3","black"))+ggtitle("(a)")
         
       psw9 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("violetred3","black"),guide=guide_legend(title="Scenario"),labels=c("SeaWiFS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-5,5,1),minor_breaks = seq(-5,5,0.5),limits=c(-5.1,5.1)) +theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))  +  
         geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("violetred3","black"))+ggtitle("(a)")
      # theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))+
      #scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))
 daf <- daf[rep(c(F,T),each=40000),]
 ResultsTab_comb <- ResultsTab_comb[24:46,]
          psw2 <- ggplot(daf,aes(x=Region,y=discmag))+geom_violin(adjust=2,scale="width",fill="violetred3")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
    scale_y_continuous(breaks=seq(-0.5,0.5,0.25),minor_breaks = seq(-0.5,0.5,0.125),limits=c(-0.6,0.6))  +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0))    +
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag1,ymin=ldm1,ymax=udm1),position = "dodge",colour="violetred3",size=1.2)+ggtitle("(b)")
     
    
  #m
#load data and create dataframe
setwd(user_wd)
load("m_Results_tables.Rdata")
disc1 <- trend_betap2_no
disc2 <- trend_betap2_weatherhead

  Trend <- c(rbind(disc1,disc2))
  disc2 <- disc_betap2_weatherhead
  discmag <- c(rbind(disc2,disc2))
  disc <- rep(c(rep("w/o",each=40000),rep("m",each=40000)),times=23)
  

  Region <- rep(c(1:23),each=80000)
 
  daf <- cbind(Region,Trend,disc,discmag)
  daf <- as.data.frame(daf)
    daf[,2] <- Trend#weird error converting to data frame ledas to factorisation
  daf[,4] <- discmag
  daf$Region <- as.factor(Region)
  daf$disc <- as.factor(daf$disc)   

    ResultsTab_comb <- rbind(ResultsTab_no,ResultsTab_weatherhead)
    disc <- c(rep("w/o",each=23),rep("m",each=23))
     ResultsTab_comb_old <- ResultsTab_comb
    ResultsTab_comb <-cbind(ResultsTab_comb,disc)
     colnames(ResultsTab_comb)[4] <- "upper"
    colnames(ResultsTab_comb)[3] <- "lower"
    colnames(ResultsTab_comb)[2] <- "trend"
ResultsTab_comb<- as.data.frame(ResultsTab_comb)
ResultsTab_comb[,3] <- ResultsTab_comb_old[,3]
ResultsTab_comb[,4] <- ResultsTab_comb_old[,4]
ResultsTab_comb[,2] <- ResultsTab_comb_old[,2]
ResultsTab_comb[,6] <- ResultsTab_comb_old[,6]
ResultsTab_comb[,7] <- ResultsTab_comb_old[,7]
ResultsTab_comb[,8] <- ResultsTab_comb_old[,8]
       ResultsTab_comb$Region <- as.factor(ResultsTab_comb$Region)
  ResultsTab_comb$disc <- as.factor(ResultsTab_comb$disc) 
 #create plot
      pm1 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("aquamarine3","black"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-3,3,1),minor_breaks = seq(-3,3,0.5),limits=c(-3.1,3.1)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("aquamarine3","black"))+ggtitle("(a)")
       #for legend
            pm9 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("aquamarine3","black"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + 
      scale_y_continuous(breaks=seq(-4,4,1),minor_breaks = seq(-3.5,3.5,0.5),limits=c(-3.6,3.6)) +theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0)) +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("aquamarine3","black"))+ggtitle("(a)")
       daf <- daf[rep(c(F,T),each=40000),]
       ResultsTab_comb <- ResultsTab_comb[24:46,]
          pm2 <- ggplot(daf,aes(x=Region,y=discmag))+geom_violin(adjust=2,scale="width",fill="aquamarine3")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
   scale_y_continuous(breaks=seq(-0.4,0.4,0.2),minor_breaks = seq(-0.4,0.4,0.1),limits=c(-0.41,0.41)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0))  +  
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag1,ymin=ldm1,ymax=udm1),position = "dodge",colour="aquamarine3",size=1.2)+ggtitle("(b)")
     
  #mv
#load data and create dataframe
setwd(user_wd)
load("mv_Results_tables.Rdata")
disc1 <- trend_betap2_no
disc2 <- trend_betap2_weatherhead

  Trend <- c(rbind(disc1,disc2))
disc2 <- disc_betap2_weatherhead
  discmag <- c(rbind(disc2,disc2))
  disc2 <- disc2_betap2_weatherhead
  discmag2 <- c(rbind(disc2,disc2))
  disc <- rep(c(rep("w/o",each=40000),rep("mv",each=40000)),times=23)
  

  Region <- rep(c(1:23),each=80000)
 
  daf <- cbind(Region,Trend,disc,discmag,discmag2)
  daf <- as.data.frame(daf)
  daf[,2] <- Trend#weird error converting to data frame ledas to factorisation
  daf[,4] <- discmag
    daf[,5] <- discmag2
  daf$Region <- as.factor(Region)
  daf$disc <- as.factor(daf$disc)   


   ResultsTab_comb <- rbind(ResultsTab_no,ResultsTab_weatherhead)
    disc <- c(rep("w/o",each=23),rep("mv",each=23))
     ResultsTab_comb_old <- ResultsTab_comb
    ResultsTab_comb <-cbind(ResultsTab_comb,disc)
     colnames(ResultsTab_comb)[4] <- "upper"
    colnames(ResultsTab_comb)[3] <- "lower"
    colnames(ResultsTab_comb)[2] <- "trend"
ResultsTab_comb<- as.data.frame(ResultsTab_comb)
ResultsTab_comb[,3] <- ResultsTab_comb_old[,3]
ResultsTab_comb[,4] <- ResultsTab_comb_old[,4]
ResultsTab_comb[,2] <- ResultsTab_comb_old[,2]
ResultsTab_comb[,6] <- ResultsTab_comb_old[,6]
ResultsTab_comb[,7] <- ResultsTab_comb_old[,7]
ResultsTab_comb[,8] <- ResultsTab_comb_old[,8]
ResultsTab_comb[,9] <- ResultsTab_comb_old[,9]
ResultsTab_comb[,10] <- ResultsTab_comb_old[,10]
ResultsTab_comb[,11] <- ResultsTab_comb_old[,11]
       ResultsTab_comb$Region <- as.factor(ResultsTab_comb$Region)
  ResultsTab_comb$disc <- as.factor(ResultsTab_comb$disc) 
  

 #Create plot
      pmv1 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("yellow2","black"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS & VIIRS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-3,3,1),minor_breaks = seq(-3.5,3.5,0.5),limits=c(-4,4)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0))  +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("yellow2","black"))+ggtitle("(a)")
      
            pmv9 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("yellow2","black"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS & VIIRS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-3,3,1),minor_breaks = seq(-3.5,3.5,0.5),limits=c(-4,4)) +theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))  +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("yellow2","black"))+ggtitle("(a)")
      # theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))+
      #scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))

  daf <- daf[rep(c(F,T),each=40000),]
ResultsTab_comb <- ResultsTab_comb[24:46,]
          pmv2 <- ggplot(daf,aes(x=Region,y=discmag))+geom_violin(adjust=2,scale="width",fill="yellow2")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-0.8,0.8,0.4),minor_breaks = seq(-0.8,0.8,0.2),limits=c(-0.81,0.81)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag1,ymin=ldm1,ymax=udm1),position = "dodge",colour="yellow2",size=1.2)+ggtitle("(b)")
     
                  pmv3 <- ggplot(daf,aes(x=Region,y=discmag2))+geom_violin(adjust=2,scale="width",fill="yellow2")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-0.8,0.8,0.4),minor_breaks = seq(-0.8,0.8,0.2),limits=c(-0.81,0.81)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag2,ymin=ldm2,ymax=udm2),position = "dodge",colour="yellow2",size=1.2)+ggtitle("(c)")
     #msv
#load data and create dataframe
setwd(user_wd)
load("msv_Results_tables.Rdata")
disc1 <- trend_betap2_no
disc2 <- trend_betap2_weatherhead

  Trend <- c(rbind(disc1,disc2))
disc2 <- disc_betap2_weatherhead
  discmag <- c(rbind(disc2,disc2))
  disc2 <- disc2_betap2_weatherhead
  discmag2 <- c(rbind(disc2,disc2))
  disc2 <- disc3_betap2_weatherhead
  discmag3 <- c(rbind(disc2,disc2))
  
  disc <- rep(c(rep("w/o",each=40000),rep("msv",each=40000)),times=23)
  

  Region <- rep(c(1:23),each=80000)
 
  daf <- cbind(Region,Trend,disc,discmag,discmag2,discmag3)
  daf <- as.data.frame(daf)
  daf[,2] <- Trend#weird error converting to data frame ledas to factorisation
  daf[,4] <- discmag
    daf[,5] <- discmag2
 daf[,6] <- discmag3
      daf$Region <- as.factor(Region)
  daf$disc <- as.factor(daf$disc)   


   ResultsTab_comb <- rbind(ResultsTab_no,ResultsTab_weatherhead)
    disc <- c(rep("w/o",each=23),rep("msv",each=23))
     ResultsTab_comb_old <- ResultsTab_comb
    ResultsTab_comb <-cbind(ResultsTab_comb,disc)
     colnames(ResultsTab_comb)[4] <- "upper"
    colnames(ResultsTab_comb)[3] <- "lower"
    colnames(ResultsTab_comb)[2] <- "trend"
ResultsTab_comb<- as.data.frame(ResultsTab_comb)
ResultsTab_comb[,3] <- ResultsTab_comb_old[,3]
ResultsTab_comb[,4] <- ResultsTab_comb_old[,4]
ResultsTab_comb[,2] <- ResultsTab_comb_old[,2]
ResultsTab_comb[,6] <- ResultsTab_comb_old[,6]
ResultsTab_comb[,7] <- ResultsTab_comb_old[,7]
ResultsTab_comb[,8] <- ResultsTab_comb_old[,8]
ResultsTab_comb[,9] <- ResultsTab_comb_old[,9]
ResultsTab_comb[,10] <- ResultsTab_comb_old[,10]
ResultsTab_comb[,11] <- ResultsTab_comb_old[,11]
ResultsTab_comb[,12] <- ResultsTab_comb_old[,12]
ResultsTab_comb[,13] <- ResultsTab_comb_old[,13]
ResultsTab_comb[,14] <- ResultsTab_comb_old[,14]
       ResultsTab_comb$Region <- as.factor(ResultsTab_comb$Region)
  ResultsTab_comb$disc <- as.factor(ResultsTab_comb$disc) 
  
  #create plot
 
      pmsv1 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("dodgerblue4","black"),guide=guide_legend(title="Scenario"),labels=c("SeaWiFS & MERIS/MODIS & VIIRS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-4,4,1),minor_breaks = seq(-4,4,0.5),limits=c(-4.1,4.1)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0))  +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("dodgerblue4","black"))+ggtitle("(a)")
      
         pmsv9 <- ggplot(daf,aes(x=Region,y=Trend,fill=disc))+geom_violin(adjust=2,scale="width")+scale_fill_manual(values=c("dodgerblue4","black"),guide=guide_legend(title="Scenario"),labels=c("SeaWiFS & MERIS/MODIS & VIIRS","No discontinuity term"))+coord_flip()+
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-4,4,1),minor_breaks = seq(-4,4,0.5),limits=c(-4.1,4.1)) +theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))  +
      geom_errorbar(data=ResultsTab_comb,aes(y=trend,ymin=lower,ymax=upper,colour=disc),position = "dodge",size=1.2)+
      scale_colour_manual(values=c("dodgerblue4","black"))+ggtitle("(a)")
      # theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))+
      #scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))

  daf <- daf[rep(c(F,T),each=40000),]
ResultsTab_comb <- ResultsTab_comb[24:46,]
          pmsv2 <- ggplot(daf,aes(x=Region,y=discmag))+geom_violin(adjust=2,scale="width",fill="dodgerblue4")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-0.5,0.5,0.25),minor_breaks = seq(-0.5,0.5,0.125),limits=c(-0.6,0.6)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag1,ymin=ldm1,ymax=udm1),position = "dodge",colour="dodgerblue4",size=1.2)+ggtitle("(b)")
     
                  pmsv3 <- ggplot(daf,aes(x=Region,y=discmag2))+geom_violin(adjust=2,scale="width",fill="dodgerblue4")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-0.5,0.5,0.25),minor_breaks = seq(-0.5,0.5,0.125),limits=c(-0.6,0.6)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag2,ymin=ldm2,ymax=udm2),position = "dodge",colour="dodgerblue4",size=1.2)+ggtitle("(c)")
     
                                    pmsv4 <- ggplot(daf,aes(x=Region,y=discmag3))+geom_violin(adjust=2,scale="width",fill="dodgerblue4")+coord_flip()+
    theme_bw()+labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) "))) + guides(fill=guide_legend(title=NULL))+
      scale_y_continuous(breaks=seq(-0.5,0.5,0.25),minor_breaks = seq(-0.5,0.5,0.125),limits=c(-0.6,0.6)) +theme(text=element_text(family="Times",size=16),legend.position='none',plot.title = element_text(hjust = 0)) +   
          geom_errorbar(data=ResultsTab_comb,aes(y=discmag3,ymin=ldm3,ymax=udm3),position = "dodge",colour="dodgerblue4",size=1.2)+ggtitle("(d)")
     
###plot CI difference bar chart
  
#load sw

load("sw_Results_tables.Rdata")
disc1 <- ResultsTab_no[,1:4]
disc2 <- ResultsTab_weatherhead[,1:4]

#load m
load("m_Results_tables.Rdata")
 disc3 <- ResultsTab_weatherhead[,1:4]
#load sw+m
load("sm_Results_tables.Rdata")
 disc4 <- ResultsTab_weatherhead[,1:4]
 #load sw+m+v
load("msv_Results_tables.Rdata")
 disc5 <- ResultsTab_weatherhead[,1:4]

 #create dataframe
  dafbar <- rbind(disc1,disc2,disc3,disc4,disc5)
  CI <- as.numeric(dafbar[,4])-as.numeric(dafbar[,3]) #prep for second bar chart below  (ci_noprior-ci_prior)/ci_noprior
 # CI_difference <- c(rep(0,times=23),(CI[24:46]-CI[1:23]),(CI[47:69]-CI[1:23]),(CI[70:92]-CI[1:23]))
    CI_difference <- c(rep(0,times=23),((CI[24:46]-CI[1:23])/CI[1:23]),((CI[47:69]-CI[1:23])/CI[1:23]),((CI[70:92]-CI[1:23])/CI[1:23]),((CI[93:115]-CI[1:23])/CI[1:23]))

  dafbar <- cbind(dafbar,CI_difference)
  
  Discontinuity <- c(rep("w/o",each=23),rep("sw",each=23),rep("m",each=23),rep("sw+m",each=23),rep("sw+m+v",each=23))
  dafbar <- cbind(dafbar,Discontinuity)
  dafbar <- as.data.frame(dafbar)
  dafbar$Discontinuity <- as.factor(dafbar$Discontinuity)
   dafbar$CI_difference <- CI_difference#weird error converting to data frame ledas to factorisation
 dafbar$Region <- rep(c(1:23),times=5)
  dafbar$Region <- as.factor(dafbar$Region)  
  
dafbar <- dafbar[-(c(1:23)),]
#plot
      p1 <- ggplot(dafbar,aes(Region,CI_difference,fill=Discontinuity))+scale_fill_manual(values=c("aquamarine3","violetred3","orange","dodgerblue4"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS", "SeaWiFS","SeaWiFS & MERIS/MODIS","SeaWiFS & MERIS/MODIS & VIIRS"))+geom_bar(position="dodge",stat="identity")+coord_flip()+theme_bw()+
        labs(y=expression(paste("   ","Normalised Uncertainty Difference", "  "^"   ")))+ggtitle("(c)")+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0),legend.position='none') #,legend.position='none',plot.margin=unit(c(0,6,0,0),'cm')
    p9 <- ggplot(dafbar,aes(Region,CI_difference,fill=Discontinuity))+scale_fill_manual(values=c("aquamarine3","violetred3","orange","dodgerblue4"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS", "SeaWiFS","SeaWiFS & MERIS/MODIS","SeaWiFS & MERIS/MODIS & VIIRS"))+geom_bar(position="dodge",stat="identity")+coord_flip()+theme_bw()+
        labs(y=expression(paste("   ","Normalised Uncertainty Difference", "  "^"   ")))+ggtitle("(c)")+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0)) #,legend.position='none',plot.margin=unit(c(0,6,0,0),'cm')
save(dafbar,file="Unc_diff_ESA.R")
##plot discontinuity magnitude difference bar chart
#load sw

load("sw_Results_tables.Rdata")

disc2 <- ResultsTab_weatherhead
disc2[,5] <- disc2[,6]
disc2 <- disc2[,-c(6:8)]

#load m
load("m_Results_tables.Rdata")
 disc3 <- ResultsTab_weatherhead
 disc3[,5] <- disc3[,6]
disc3 <- disc3[,-c(6:10)]
#load sw+m
load("sm_Results_tables.Rdata")
 disc4 <- ResultsTab_weatherhead
 disc4[,5] <- rowMeans(disc4[,c(6,9)])
disc4 <- disc4[,-c(6:11)]
 #load sw+m+v
load("msv_Results_tables.Rdata")
 disc5 <- ResultsTab_weatherhead
 disc5[,5] <- rowMeans(disc5[,c(6,9,12)])
disc5 <- disc5[,-c(6:14)]
 
#create dataframe
  dafbar <- rbind(disc2,disc3,disc4,disc5)
colnames(dafbar)[5] <- "discmag"

  
  Discontinuity <- c(rep("sw",each=23),rep("m",each=23),rep("sw+m",each=23),rep("sw+m+v",each=23))
  dafbar <- cbind(dafbar,Discontinuity)
  dafbar <- as.data.frame(dafbar)
  dafbar$Discontinuity <- as.factor(dafbar$Discontinuity)
 dafbar$Region <- rep(c(1:23),times=4)
  dafbar$Region <- as.factor(dafbar$Region)  
  load("significance_for_bar_complete.Rdata")
dafbar$signif <- discsig

   
    dafbar$discmag <- as.numeric(levels(dafbar$discmag)[dafbar$discmag])
   # ((disc2[,2]-disc1[,2])/disc1[,2])*100

    #Create plot
          p5 <- ggplot(dafbar,aes(Region,y=discmag,fill=Discontinuity))+scale_fill_manual(values=c("aquamarine3","violetred3","orange","dodgerblue4"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS", "SeaWiFS","SeaWiFS & MERIS/MODIS","SeaWiFS & MERIS/MODIS & VIIRS"))+geom_bar(position=position_dodge(width=0.9),stat="identity")+geom_text(aes(Region,discmag,color=Discontinuity,family="",fontface="bold",label=ifelse(discsig==F,"","*"),vjust=0.6,hjust=ifelse(sign(discmag)<0, 2, -1)),position = position_dodge(width=0.9),size=5 )+scale_colour_manual(values=c("aquamarine3","violetred3","orange","dodgerblue4"))+coord_flip()+theme_bw()+
        labs(y=expression(paste("Discontinuity Magnitude ", "(log(mg m"^" -3",")) ")))+ggtitle("(a)")+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0),legend.position='none') #
  
      p5
          save(dafbar,file="Disc_mag_ESA.R")
          
##plot trend difference bar chart

#load sw

load("sw_Results_tables.Rdata")
disc1 <- ResultsTab_no[,1:4]
disc2 <- ResultsTab_weatherhead[,1:4]

#load m
load("m_Results_tables.Rdata")
 disc3 <- ResultsTab_weatherhead[,1:4]
#load sw+m
load("sm_Results_tables.Rdata")
 disc4 <- ResultsTab_weatherhead[,1:4]
 #load sw+m+v
load("msv_Results_tables.Rdata")
 disc5 <- ResultsTab_weatherhead[,1:4]

 #create data frame
  dafbar <- rbind(disc1,disc2,disc3,disc4,disc5)
  trend <- as.numeric(dafbar[,2]) #prep for second bar chart below  (trend_trendprior-trend_prior)/trend_noprior
  trend_difference <- c(rep(0,times=23),(trend[24:46]-trend[1:23]),(trend[47:69]-trend[1:23]),(trend[70:92]-trend[1:23]),(trend[93:115]-trend[1:23]))
  dafbar <- cbind(dafbar,trend_difference)
  
  Discontinuity <- c(rep("w/o",each=23),rep("sw",each=23),rep("m",each=23),rep("sw+m",each=23),rep("sw+m+v",each=23))
  dafbar <- cbind(dafbar,Discontinuity)
  dafbar <- as.data.frame(dafbar)
  dafbar$Discontinuity <- as.factor(dafbar$Discontinuity)
   dafbar$trend_difference <- trend_difference#weird error converting to data frame ledas to factorisation
 dafbar$Region <- rep(c(1:23),times=5)
  dafbar$Region <- as.factor(dafbar$Region)  
  dafbar$Discontinuity <- as.character(dafbar$Discontinuity)
dafbar <- dafbar[-(c(1:23)),]
load("significance_for_bar_complete.Rdata")
dafbar$signif <- trendsig
      
#create plot
        p2 <- ggplot(dafbar,aes(Region,trend_difference,fill=Discontinuity))+scale_fill_manual(values=c("aquamarine3","violetred3","orange","dodgerblue4"),guide=guide_legend(title="Scenario"),labels=c("MERIS/MODIS", "SeaWiFS","SeaWiFS & MERIS/MODIS","SeaWiFS & MERIS/MODIS & VIIRS"))+geom_bar(position=position_dodge(width=0.9),stat="identity")+geom_text(aes(Region,trend_difference,color=Discontinuity,family="",fontface="bold",label=ifelse(trendsig==F,"","*"),vjust=0.6,hjust=ifelse(sign(trend_difference)<0, 2, -1)),position = position_dodge(width=0.9),size=5 )+scale_colour_manual(values=c("aquamarine3","violetred3","orange","dodgerblue4"))+coord_flip()+theme_bw()+
        labs(y=expression(paste("Trend Difference ", "(%yr"^" -1",")")))+ggtitle("(b)")+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0),legend.position='none') #
  
      p2 

save(dafbar,file="Trend_diff_ESA.R")

####global plot
      
      
#trend import and sort 0 trends
trend_diff  <- trend[93:115]-trend[1:23]
long_df$trend <- NA
#sorting differing region orders
provsorder <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)
for(i in 1:54){
long_df$trend[which(long_df$id==i)] <- trend_diff[provsorder[i]]
}

#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA
centroids_df <- as.data.frame(coordinates(long))

 centroids_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
 centroids_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
  centroids_df[36,] <- c(160.6,-36.3)
  centroids_df[51,] <- c(75,-36.2)
  for(i in c(4,5,6,7,8,9,10,18,22,23,31,32,33,34,35,36,37,38,39,40,41,51,52)){
long_df$lonc[which(long_df$id==i)] <- centroids_df$V1[i]
long_df$latc[which(long_df$id==i)] <- centroids_df$V2[i]
}
#correct labels
long_df$truid <- NA
provsorder <- c(NA,NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA)#slightly different due to null region

for(i in 1:54){
long_df$truid[which(long_df$id==i)] <- provsorder[i]
}


com <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette
#create plot
p3 <-  ggplot( data=long_df, aes(x=long, y=lat, group = group,fill=trend)) +
 scale_fill_gradientn(colours=com,na.value="black",limits=c(-max(sqrt((trend_diff)^2)),max(sqrt((trend_diff)^2))))+
  geom_polygon(colour='black',size=0.25)+ #longhurst outlines with half width size
  geom_text(data=long_df,aes(label = truid, x = lonc, y = latc,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
   coord_equal() + guides(fill=guide_colorbar(ticks=T,title=expression(atop(textstyle("Trend"), atop(textstyle("Difference") , paste("(%yr"^" -1",")"))))))+
  ylab("Latitude")+xlab("Longitude")+
   coord_cartesian(ylim = c(-75, 75),expand=F)+ggtitle("(a)")+
scale_y_continuous(breaks=seq(-60,60,30))  +
  scale_x_continuous(breaks=seq(-180,180,60))  +
  annotate("text",x=-133,y=-38.1,label="21",size=4,colour='black',family="Times")+ #extra label for region 21 to show that it wraps
  theme_bw()+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))

ggsave("trend_diff_global.png",  width=8.27, height=3.44, dpi=600)
limforlater <- trend_diff
####global plot for uncertainty difference


#unc import and sort 0 trends
unc  <- (CI[93:115]-CI[1:23])/CI[1:23]
long_df$unc <- NA
#sorting differing region orders
provsorder <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)

for(i in 1:54){
long_df$unc[which(long_df$id==i)] <- unc[provsorder[i]]
}



#create plot

com <- c("white","white","dodgerblue4")#colour palette
p4 <-  ggplot( data=long_df, aes(x=long, y=lat, group = group,fill=unc)) +
 scale_fill_gradientn(colours=com,na.value="black",limits=c(0,max(unc)))+
  geom_polygon(colour='black',size=0.25)+ #longhurst outlines with half width size
  geom_text(data=long_df,aes(label = truid, x = lonc, y = latc,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
   coord_equal() + guides(fill=guide_colorbar(title=" Normalised \n Uncertainty \n Difference",ticks=T)) + #atop("Uncertainty", atop(textstyle("Difference") , "%yr"^" -1"))  
  ylab("Latitude")+xlab("Longitude")+
   coord_cartesian(ylim = c(-75, 75),expand=F)+ggtitle("(b)")+
scale_y_continuous(breaks=seq(-60,60,30))  +
  scale_x_continuous(breaks=seq(-180,180,60))  +
  annotate("text",x=-133,y=-38.1,label="21",size=4,colour='black',family="Times")+ #extra label for region 21 to show that it wraps
  theme_bw()+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))

ggsave("unc_diff_global.png",  width=8.27, height=3.44, dpi=600)
###global plot for nodiscontinuity trend map
 
      
#trend import and sort 0 trends
trend_diff  <- trend[1:23]
long_df$trend <- NA
#sorting differing region orders
provsorder <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)
for(i in 1:54){
long_df$trend[which(long_df$id==i)] <- trend_diff[provsorder[i]]
}

#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA
centroids_df <- as.data.frame(coordinates(long))

 centroids_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
 centroids_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
  centroids_df[36,] <- c(160.6,-36.3)
  centroids_df[51,] <- c(75,-36.2)
  for(i in c(4,5,6,7,8,9,10,18,22,23,31,32,33,34,35,36,37,38,39,40,41,51,52)){
long_df$lonc[which(long_df$id==i)] <- centroids_df$V1[i]
long_df$latc[which(long_df$id==i)] <- centroids_df$V2[i]
}
#correct labels
long_df$truid <- NA
provsorder <- c(NA,NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA)#slightly different due to null region

for(i in 1:54){
long_df$truid[which(long_df$id==i)] <- provsorder[i]
}
com <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette
#create plot
pnd1 <-  ggplot( data=long_df, aes(x=long, y=lat, group = group,fill=trend)) +
 scale_fill_gradientn(colours=com,na.value="black",limits=c(-2.1,2.1))+
  geom_polygon(colour='black',size=0.25)+ #longhurst outlines with half width size
  geom_text(data=long_df,aes(label = truid, x = lonc, y = latc,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
   coord_equal() + guides(fill=guide_colorbar(title=expression(atop(textstyle("Trend"), paste("(%yr"^" -1",")")))))+
  ylab("Latitude")+xlab("Longitude")+
   coord_cartesian(ylim = c(-75, 75),expand=F)+ggtitle("(a)")+
scale_y_continuous(breaks=seq(-60,60,30))  +
  scale_x_continuous(breaks=seq(-180,180,60))  +
  annotate("text",x=-133,y=-38.1,label="21",size=4,colour='black',family="Times")+ #extra label for region 21 to show that it wraps
  theme_bw()+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))

ggsave("trend_nodisc_global.png",  width=8.27, height=3.44, dpi=600)

#no discontinuity uncertainty
load("sw_Results_tables.Rdata")
unc<- ResultsTab_no[,4]-ResultsTab_no[,3]

long_df$unc <- NA
#sorting differing region orders
provsorder <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)

for(i in 1:54){
long_df$unc[which(long_df$id==i)] <- unc[provsorder[i]]
}

#create plot

com <- c("white","firebrick2","firebrick4")#colour palette
pnd2 <-  ggplot( data=long_df, aes(x=long, y=lat, group = group,fill=unc)) +
 scale_fill_gradientn(colors=com,na.value="black",limits=c(0,max(unc)))+
  geom_polygon(colour='black',size=0.25)+ #longhurst outlines with half width size
  geom_text(data=long_df,aes(label = truid, x = lonc, y = latc,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
   coord_equal() + guides(fill=guide_colorbar(title=expression(atop(textstyle("Uncertainty"), paste("(%yr"^" -1",")"))))) + #atop("Uncertainty", atop(textstyle("Difference") , "%yr"^" -1"))  
  ylab("Latitude")+xlab("Longitude")+
   coord_cartesian(ylim = c(-75, 75),expand=F)+ggtitle("(b)")+
scale_y_continuous(breaks=seq(-60,60,30))  +
  scale_x_continuous(breaks=seq(-180,180,60))  +
  annotate("text",x=-133,y=-38.1,label="21",size=4,colour='black',family="Times")+ #extra label for region 21 to show that it wraps
  theme_bw()+theme(text=element_text(family="Times",size=16),plot.title = element_text(hjust = 0))

ggsave("unc_nodisc_global.png",  width=8.27, height=3.44, dpi=600)
 
      
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#combine global plots with msv discontinuity
         savename <- paste0("v31_discontinuity_global.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8.27, 
    height=8, 
    pointsize=12, 
    res=600)
    multiplot(p3,p4)
    dev.off()
 #combine global plots with no discontinuity   
             savename <- paste0("v31_nodisc_global.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8.27, 
    height=8, 
    pointsize=12, 
    res=600)
    multiplot(pnd1,pnd2)
    dev.off()
    #combine bar charts
             savename <- paste0("v31_bar_combine_altorder.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=12, 
    height=8.27, 
    pointsize=12, 
    res=600)
    multiplot(p5,p2,p1,cols=3)
    dev.off()
    #save plot with legend for later combining
                 savename <- paste0("v31_bar_combine_legend.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.27, 
    pointsize=12, 
    res=600)
    multiplot(p9,cols=1)
    dev.off()
    
    #save violins
   
       
    savename <- paste0("Violins_v31_weatherhead_sm.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=12, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(psm1,psm2,psm3,cols=3)
    dev.off()

    
       savename <- paste0("Violins_v31_weatherhead_mv.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=12, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(pmv1,pmv2,pmv3,cols=3)
    dev.off()

       savename <- paste0("Violins_v31_weatherhead_msv.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=16, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(pmsv1,pmsv2,pmsv3,pmsv4,cols=4)
    dev.off()

       savename <- paste0("Violins_v31_weatherhead_m.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(pm1,pm2,cols=2)
    dev.off()

    
       savename <- paste0("Violins_v31_weatherhead_sw.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(psw1,psw2,cols=2)
    dev.off()
    #save violin legends for later combining
                      savename <- paste0("Violins_v31_weatherhead_legend_msv.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(pmsv9,cols=1)
    dev.off()
               savename <- paste0("Violins_v31_weatherhead_legend_m.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(pm9,cols=1)
    dev.off()
               savename <- paste0("Violins_v31_weatherhead_legend_sw.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(psw9,cols=1)
    dev.off()
                   savename <- paste0("Violins_v31_weatherhead_legend_mv.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(pmv9,cols=1)
    dev.off()
                   savename <- paste0("Violins_v31_weatherhead_legend_sm.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8, 
    height=8.5, 
    pointsize=12, 
    res=600)
    multiplot(psm9,cols=1)
    dev.off()
#create extra tables for use in supporting ingormation
#sw

load("sw_Results_tables.Rdata")
disc1 <- ResultsTab_no
disc1 <- disc1[,-c(6:8)]
disc2 <- ResultsTab_weatherhead
disc2[,5] <- disc2[,6]
disc2 <- disc2[,-c(6:8)]

#m
load("m_Results_tables.Rdata")
 disc3 <- ResultsTab_weatherhead
 disc3[,5] <- disc3[,6]
disc3 <- disc3[,-c(6:8)]
#sw+m
load("sm_Results_tables.Rdata")
 disc4 <- ResultsTab_weatherhead
 disc4[,5] <- rowMeans(disc4[,c(6,9)])
disc4 <- disc4[,-c(6:11)]
#mv
load("mv_Results_tables.Rdata")
 disc6 <- ResultsTab_weatherhead
 disc6[,5] <- rowMeans(disc6[,c(6,9)])
disc6 <- disc6[,-c(6:11)]
 #sw+m+v
load("msv_Results_tables.Rdata")
 disc5 <- ResultsTab_weatherhead
 disc5[,5] <- rowMeans(disc5[,c(6,9,12)])
disc5 <- disc5[,-c(6:14)]
 #create dataframe
  dafbar <- rbind(disc1,disc3,disc2,disc4,disc6,disc5)
colnames(dafbar)[5] <- "discmag"


  CI <- as.numeric(dafbar[,4])-as.numeric(dafbar[,3])
    trend <- as.numeric(dafbar[,2]) #prep for second bar chart below  (trend_trendprior-trend_prior)/trend_noprior

    unc_diff <- (CI[24:138]-CI[1:23])
    trend_diff  <- trend[24:138]-trend[1:23]
   tab2 <-  c(rbind(unc_diff[1:23],unc_diff[24:46],unc_diff[47:69],unc_diff[70:92],unc_diff[93:115]))
      tab1 <-  c(rbind(trend_diff[1:23],trend_diff[24:46],trend_diff[47:69],trend_diff[70:92],trend_diff[93:115]))
      
      
      
        
    
      tab3<- c(rbind(dafbar[24:46,5],dafbar[47:69,5],dafbar[70:92,5],dafbar[93:115,5],dafbar[116:138,5]))
      
       temp <- cbind(tab1,tab2,tab3)
       temp <- signif(temp,2)
       