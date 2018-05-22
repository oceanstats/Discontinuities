#file to combine separate chains from previous iteration for msv_scenario
user_wd='...'
library(R.matlab)

rm(list=ls())

setwd(user_wd)

temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst
temp <- readMat("ESA_v31_Data.mat")
lats <- c(temp$lats)
lons <- c(temp$lons)


chl <- temp$chl

area <- sort(unique(Longhurst[!is.na(Longhurst)]))

Longstore <- Longhurst  
ResultsTab <- rep(NA,times=54)
newc <- rep(NA,times=54)
fittedChl<- array(NA,dim=c(360,180,232))
total_betap <- array(NA,dim=c(17,40000,length(area)))

temp <- readMat("SST_for_v31.mat")
SST <- temp$SST

overmodel <- 0
overobs <- 0
months <- 0
lengthoftemp=17
#region46 prerun
for(j in 46)
{
 
  for(i in 1:40) #stitching together multiple runs
  {


     savename <- paste0(user_wd,j,"_BGC_model_msv_pt",i,".Rdata")
  load(savename)
  betap2 <- array(NA,dim=c(lengthoftemp,1000))
  betap2[1:(lengthoftemp-11),] <- betap
  if(lengthoftemp==17){
    betap2[15:17,] <- betap2[4:6,]
  }else if(lengthoftemp==16){
    betap2[15:16,] <- betap2[4:5,]
  }else if(lengthoftemp==15){
     betap2[15,] <- betap2[4,]
  }
  betap <- betap2
  save(betap,model_input,fitted,file=savename)
  }
}
for(j in area)
{
  
  print(j)
temp <- array(NA,dim=c(lengthoftemp,1))#for cbind to attach


Longhurst <- Longstore
#Longhurst area
  boundind <- which(Longhurst==j,arr.ind=T)

  
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{
 

}else{

  for(i in 1:40) #stitching together multiple runs
  {


     savename <- paste0(user_wd,j,"_BGC_model_msv_pt",i,".Rdata")
  load(savename)
  if(i==1)
    {
    temp2 <- array(NA,dim=c(nrow(fitted),40))#create on first iteration to guide rest
 
  } 
    temp <- cbind(temp,betap)
    temp2[,i] <- fitted[,1]

  }
  
  betap <- temp[,-1]#remove 1st column which is used for setup
  temp2 <- rowMeans(temp2,na.rm=T)

  fitted[,1] <- temp2

  



time <- 1:232

lonind <- which(model_input$Longitude>179.5)
model_input$Longitude[lonind] <- model_input$Longitude[lonind]-360# removing original correction for having longitudes increasing continuously

  #locations same for all time steps--> work out locations for plugging into fittedChl
  loc <- model_input[seq(1,length(model_input$Longitude),232),2:3] 
  locind <- array(NA,dim=c(nrow(loc),2))
  for(m in 1:nrow(loc))
{
  locind[m,1] <- which(lons==loc[m,1])
  locind[m,2] <- which(lats==loc[m,2])
}
  for(k in 1:232)
{
fittemp <- exp(fitted[seq(1+(k-1),length(model_input$Longitude),232),1])
for(m in 1:length(fittemp))
{
fittedChl[locind[m,1],locind[m,2],k] <- fittemp[m]
}
  }



    savename <- paste0(user_wd,j,"_msv_trace.png")
    png(savename)
  plot(betap[2,],type='l',main=paste0("trace for region=",j),xlab="iteration",ylab="raw trend") 
dev.off()

len <- dim(temp)
total_betap[1:len[1],,j] <- betap
}
}




save(total_betap,model_input,fittedChl,file=paste0(user_wd,"msv_results.Rdata")
