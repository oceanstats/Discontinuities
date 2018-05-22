user_wd='...' #set user working directory containing longhurst data, SST data and combined chains
library(HDInterval)
library(spTimer)
library(fields)
library(R.matlab)

rm(list=ls())
setwd(user_wd)
#load original input data
temp <- read.csv(file="Longhurst_area.csv",header=F)
Area_values <- c(as.matrix(temp))
temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst
temp <- readMat("ESA_v31_Data.mat")
lats <- c(temp$lats)
lons <- c(temp$lons)
chl_obs <- temp$chl

area <- sort(unique(Longhurst[!is.na(Longhurst)]))



temp <- readMat("SST_for_v31.mat")
SST <- temp$SST

#load spTmodel output

load("n_results.Rdata")
no_betap <- total_betap

no_fittedChl <- fittedChl
Longhurst_no <- Longhurst
load("mv_results.Rdata")
weatherhead_betap <-  total_betap #discard coeffs for discontinuity indexes (not currently used) 
weatherhead_fittedChl <- fittedChl
Longhurst_weatherhead <- Longhurst

#create function for posterior mode
 mymode <- function(x){
   d<-density(x)
   return(d$x[which(d$y==max(d$y)[1])])
 }



rm("total_betap")
corc <- c(21,8,54,4,38,10,2,12,33,3,29,20,14,37,32,31,6,13,17,40,42,15,45,49,9,11,7,35,19,22,30,36,34,43,5,18,46,41,44,1,24,25,16,52,51,50,28,47,48,23,53,39,26,27)
area <- area[order(match(corc,area))]#sorting longhurst order to match plot available at http://www.marineplan.es/ES/fichas_kml/mapasfichas/reg_biogeog.jpg
#initialize output
indio <- 0
Area_values_index <- 0
ind_AV <- Area_values_index 
ResultsTab_no <- array(NA,dim=c(23,11))
ResultsTab_weatherhead <- array(NA,dim=c(23,11))
Chlcontent <- array(NA,dim=c(23))
isdiff<- array(NA,dim=c(23))
isdiffdm1<- array(NA,dim=c(23))
isdiffdm2<- array(NA,dim=c(23))

regave <- array(NA,dim=c(232,54))
regave_no <- array(NA,dim=c(232,54))
regave_weatherhead <- array(NA,dim=c(232,54))
temp_obs <- array(NA,dim=c(232,54))
temp_weatherhead <- array(NA,dim=c(232,54))
Test <- array(NA,dim=c(23))
mag_diff <- array(NA,dim=c(23))
unc_diff <- array(NA,dim=c(23))
unc_diff2 <- array(NA,dim=c(23))
unc_diff_per <- array(NA,dim=c(23))

trend_betap2_no <- array(NA,dim=c(40000,23))
trend_betap2_weatherhead <- array(NA,dim=c(40000,23))
disc_betap2_weatherhead <- array(NA,dim=c(40000,23))
disc2_betap2_weatherhead <- array(NA,dim=c(40000,23))

colnames(ResultsTab_no) <- c("Region","Trend Value","Lower Confidence Interval","Upper confidence interval","RMSE","discmag1","ldm1","udm1","discmag2","ldm2","udm2")
colnames(ResultsTab_weatherhead) <- c("Region","Trend Value","Lower Confidence Interval","Upper confidence interval","RMSE","discmag1","ldm1","udm1","discmag2","ldm2","udm2")

#remove from plotting unwanted regions
for(j in area){
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{
  Longhurst_no[which(Longhurst_no==j)] <- NA
  Longhurst_weatherhead[which(Longhurst_weatherhead==j)] <- NA
}
}
#store data for use in next loop
chl_obs_store <- chl_obs
no_fittedChl_store <- no_fittedChl
weatherhead_fittedChl_store <- weatherhead_fittedChl
for(j in area)
{
  ind_AV <- ind_AV+1
  print(j)
#retrieve stored data
chl_obs <- chl_obs_store
  no_fittedChl <- no_fittedChl_store
  weatherhead_fittedChl <- weatherhead_fittedChl_store

  
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{


} else{
  indio <- indio+1

  ##for each region  replace not required with NA
boundind <- which(Longhurst!=j | is.na(Longhurst))


for(i in 1:232) 
{
  temp <- chl_obs[,,i]
  temp[boundind] <- NA
  chl_obs[,,i] <- temp

    temp <- no_fittedChl[,,i]
  temp[boundind] <- NA
  no_fittedChl[,,i] <- temp


}
for(i in 1:232) 
{
    temp <- weatherhead_fittedChl[,,i]
  temp[boundind] <- NA
  weatherhead_fittedChl[,,i] <- temp
  
  temp <- chl_obs[,,i]
  temp[boundind] <- NA
  chl_obs[,,i] <- temp
  }
#convert trend values to percent per year ((e-s)/s)*100
no_betap[2,,j] <- 100*12*no_betap[2,,j]
weatherhead_betap[2,,j] <- 100*12*weatherhead_betap[2,,j]

#calculate trend cofidence intervals
bounds_no <- array(NA,dim=c(17,2))
bounds_weatherhead <- array(NA,dim=c(17,2))
for(n in 1:17)
{
    temp <- hdi(no_betap[n,,j],credMass=0.95)
  bounds_no[n,] <- c(as.numeric(temp[1]),as.numeric(temp[2]))#HDI
   temp <- hdi(weatherhead_betap[n,,j],credMass=0.95)
  bounds_weatherhead[n,] <- c(as.numeric(temp[1]),as.numeric(temp[2]))#HDI
  }

#region trend value if is considered "significant"
Longhurst_no_unc <- Longhurst_no
Longhurst_weatherhead_unc <- Longhurst_weatherhead

###test if significant difference between trends with and without discontinuity
if((bounds_weatherhead[2,1]>bounds_no[2,2] & bounds_weatherhead[2,2]>bounds_no[2,2]) | (bounds_weatherhead[2,1]< bounds_no[2,1] & bounds_weatherhead[2,2]< bounds_no[2,1]))
{
  isdiff[indio] <- T
  }else{
  isdiff[indio] <- F
}

###test if significant difference between discontinuity magnitude and 0
if((bounds_weatherhead[15,1]>0 & bounds_weatherhead[15,2]>0) | (bounds_weatherhead[15,1]< 0 & bounds_weatherhead[15,2]< 0))
{
  isdiffdm1[indio] <- T
  }else{
  isdiffdm1[indio] <- F
  }

###test if significant difference between discontinuity magnitude and 0
if((bounds_weatherhead[16,1]>0 & bounds_weatherhead[16,2]>0) | (bounds_weatherhead[16,1]< 0 & bounds_weatherhead[16,2]< 0))
{
  isdiffdm2[indio] <- T
  }else{
  isdiffdm2[indio] <- F
  }





  #uncertainty difference for global plot
  Longhurst_uncplot <- Longhurst_no
#trend for global plot
  Longhurst_no[which(Longhurst_no==j)] <- mean(no_betap[2,,j])
  Longhurst_weatherhead[which(Longhurst_weatherhead==j)] <- mean(weatherhead_betap[2,,j])
  #discontinuity magnitude
disc <- mean(weatherhead_betap[15,,j],na.rm=T)
disc2 <- mean(weatherhead_betap[16,,j],na.rm=T)

#RMSE
RMSE_no <- mean(sqrt((no_fittedChl-chl_obs)^2),na.rm=T)
RMSE_weatherhead <- mean(sqrt((weatherhead_fittedChl-chl_obs)^2),na.rm=T)

#tabulate output
Chlcontent[indio] <- mean(chl_obs,na.rm=T)#*Area_values[ind_AV]
Test[indio] <- Area_values[ind_AV]
trend_betap2_no[,indio]  <- no_betap[2,,j]
trend_betap2_weatherhead[,indio]  <- weatherhead_betap[2,,j]
#discontinuity for violin and bounds
disc_betap2_weatherhead[,indio]  <- weatherhead_betap[15,,j]
   temp <- hdi(disc_betap2_weatherhead[,indio] ,credMass=0.95)
  bounds_disc <- c(as.numeric(temp[1]),as.numeric(temp[2]))#HDI
disc2_betap2_weatherhead[,indio]  <- weatherhead_betap[16,,j]
     temp <- hdi(disc2_betap2_weatherhead[,indio] ,credMass=0.95)
  bounds_disc2 <- c(as.numeric(temp[1]),as.numeric(temp[2]))#HDI
  
unc_diff[indio] <- ((bounds_weatherhead[2,2]-bounds_weatherhead[2,1])-(bounds_no[2,2]-bounds_no[2,1]))
unc_diff_per[indio] <- (((bounds_weatherhead[2,2]-bounds_weatherhead[2,1])-(bounds_no[2,2]-bounds_no[2,1]))/(bounds_no[2,2]-bounds_no[2,1]))*100
unc_diff2[indio] <- (((bounds_weatherhead[2,2]-bounds_weatherhead[2,1])-(bounds_no[2,2]-bounds_no[2,1]))/(bounds_weatherhead[2,2]-bounds_weatherhead[2,1]))*100
Longhurst_uncplot[which(Longhurst_uncplot==j)] <- unc_diff2[indio]

mag_diff[indio] <- mymode(weatherhead_betap[2,,j])-mymode(no_betap[2,,j])

#table of results
ResultsTab_no[indio,] <- c(indio,mean(no_betap[2,,j]),bounds_no[2,],RMSE_no,disc,bounds_disc,disc2,bounds_disc2)
ResultsTab_weatherhead[indio,] <- c(indio,mean(weatherhead_betap[2,,j]),bounds_weatherhead[2,],RMSE_weatherhead,disc,bounds_disc,disc2,bounds_disc2)


}
}


save(ResultsTab_no,ResultsTab_weatherhead,disc_betap2_weatherhead,disc2_betap2_weatherhead,Chlcontent,trend_betap2_no,trend_betap2_weatherhead,file="mv_Results_tables.Rdata")

     save(isdiff,isdiffdm1,isdiffdm2,file="mv_significance.Rdata")
     