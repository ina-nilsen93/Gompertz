# Clear work space
rm(list=ls(all=TRUE))

# Set .trPaths (temporarily)
.trPaths <- file.path(Sys.getenv('TEMP'), 'Tinn-R', c('', 'search.txt','objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r','reformat-input.r', 'reformat-output.r'), fsep='\\')

# Set work directory
#setwd("D:/Leif_Oslo2/R Barents Sea/Skripter/STAN/Zoocomp")
setwd("~/Documents/Atlantis/Gompertz")

# Add location of stan library to library path
.libPaths(new="~/Documents/Atlantis/Gompertz")

library("rstan") # observe startup messages

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

### Preparing data set:

# Some useful functions:
lag1 <- function(x)c(NA,x[1:(length(x)-1)])
na.replace<-function(x){if(length(x)==0) NA else x}
yr.replace<-function(yr,x,x.yr,lag.yr=0) {out <- rep(NA,length(yr))
   yrs<-as.numeric(as.character(levels(as.factor(as.character(yr)))))
   for(i in 1:length(yrs)){
     out[yr==yrs[i]]<-na.replace(x[x.yr==yrs[i]+lag.yr]) }
   out}

# Model years
yrs <- 1980:2015  

Data <- data.frame(Year=yrs)


##########  Ina stop   ######################

#Data <- read.table(file="D://Leif_Oslo2/R Barents Sea/Skripter/Analyses capelin-zoopl/Jags analyses/capelin-zoopl-dyn2016.txt",header=T)
# 
# ## Mesozooplankton ("Copepods")
# 
# #Cop.Ann <- read.table(file="~/Documents/Dokumenter/PhD/Gompertz/BS.data.txt",dec=".",sep="")
# Cop.Ann <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/Fra Padmini/IMRdataAnn.updatedto2015.txt",dec=".",sep="\t")
# cop.cn.old <- apply(Cop.Ann[,c("Zoo.sum.C","Zoo.sum.N")],1,
#   function(x){log((exp(x[1]) + exp(x[2]) - 1)/2)})
# cop.cn <- apply(Cop.Ann[,c("Zoo.sum.C","Zoo.sum.N")],1,
#   function(x){log((exp(x[1]) + exp(x[2]))/2)})
# 
# # se calculated as a simple weighted average:
# cop.cn.se <- apply(Cop.Ann[,c("Zoo.sum.C.se","Zoo.sum.N.se","Zoo.sum.C","Zoo.sum.N")],1,
#   function(x){((x[1]^2 * (exp(x[3]) - 1) + x[2]^2 * (exp(x[4]) - 1))/
#    (exp(x[3]) + exp(x[4]) - 2))^0.5})
# 
# # se calculated as a simple unweighted average:
# cop.cn.se.simp <- apply(Cop.Ann[,c("Zoo.sum.C.se","Zoo.sum.N.se")],1,
#   function(x){((x[1]^2  + x[2]^2 )/4)^0.5})
# 
# # se calculated from sum (through CVs):
# # CV = sqrt(exp(s^2 - 1))
# # Var = CV^2 * Mean^2
# # log-scale sd = sqrt(log(1 + CV^2)))
# cop.c.cv <- (exp(Cop.Ann$Zoo.sum.C.se^2) - 1)^0.5
# cop.n.cv <- (exp(Cop.Ann$Zoo.sum.N.se^2) - 1)^0.5
# cop.c.bm <- exp(Cop.Ann$Zoo.sum.C) - 1
# cop.n.bm <- exp(Cop.Ann$Zoo.sum.N) - 1
# cop.cn.cv <- (cop.c.cv^2 * cop.c.bm^2 + cop.n.cv^2 * cop.n.bm^2)^0.5 /
#              (cop.c.bm + cop.n.bm) 
# cop.cn.se <- (log(1 + cop.cn.cv^2))^0.5
# 
# cop.cn.se <- cop.cn.se.simp
# Data$Cop <- yr.replace(yr=Data$Year,x=cop.cn,x.yr=Cop.Ann$Year)
# Data$Cop.se <- yr.replace(yr=Data$Year,x=cop.cn.se,x.yr=Cop.Ann$Year)
# median(Data$Cop.se,na.rm=T) #  0.06008891
# range(Data$Cop.se,na.rm=T) #   0.04684301 0.13535702
# secomp <- function(x,se){sum(se[!is.na(x)]^2)/
#   (var(x[!is.na(x)])*(length(x[!is.na(x)])-1))}
# secomp(Data$Cop,Data$Cop.se) #  0.04168351
# 
# ## Krill
# 
# krill.index <- read.table("D:/Leif_Oslo2/Barents Sea/Data/Fra Elena Eriksen/NorthSouth_krill density.txt",head=T)
# #formula sNij = (ln(1 + CVT2))0.5
# krill.n.cv <- (krill.index$KrillNorth.SE/krill.index$KrillNorth)
# krill.n.logse.ts <- (log(1 + krill.n.cv^2))^.5  
# krill.n.logse <- median(log(1 + krill.n.cv^2))^.5  # 0.76
# Data$Krill <- yr.replace(yr=Data$Year,x=log(krill.index$KrillNorth),x.yr=krill.index$Year)
# Data$Krill.se <- yr.replace(yr=Data$Year,x=krill.n.logse.ts,x.yr=krill.index$Year)
# median(Data$Krill.se) # 0.4264508
# range(Data$Krill.se) #  0.2695604 0.7768735
# secomp(Data$Krill,Data$Krill.se) #  0.121181
# 
# 
# ## Amphipods  
# 
# ## Note! Data to 2015!
# 
# amph.index <- read.table("D:/Leif_Oslo2/R Barents Sea/Skripter/STAN/Zoocomp/amph.index.2015.txt")
# Data$Amphipods <- yr.replace(yr=Data$Year,x=amph.index$amph.logn,x.yr=amph.index$year)
# Data$Amphipods.se <- yr.replace(yr=Data$Year,x=amph.index$amph.logse,x.yr=amph.index$year)
# median(Data$Amphipods.se,na.rm=T) # 0.1203213
# range(Data$Amphipods.se,na.rm=T) #  0.0829876 0.2947644
# secomp(Data$Amphipods,Data$Amphipods.se) # 0.04791036
# 
# ## Capelin
# ICES <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/ICES data/capelin-zoopl_ICES-PINRO-IMR_2016.txt",header=T)
# Data$logCapBMtot<-
#   log(yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBMtot,lag.yr=0)) # ln(thousand tonnes)
# Data$CapBMtot<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBMtot,lag.yr=0)) # 
# Data$CapBMtot.lag1<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBMtot,lag.yr=-1)) # ln(thousand tonnes)
# Data$CapBMmat.lag1<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBMmat,lag.yr=-1))
# Data$CapN1<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapN1,lag.yr=0))
# Data$CapN2<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapN2,lag.yr=0))
# Data$CapN3<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapN3,lag.yr=0))
# Data$CapN4<-                                                         
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapN4,lag.yr=0))
# Data$CapBM1<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBM1,lag.yr=0))
# Data$CapBM2<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBM2,lag.yr=0))
# Data$CapBM3<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBM3,lag.yr=0))
# Data$CapBM4<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapBM4,lag.yr=0))
# Data$CapCatchSumAut<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapCatchSumAut,lag.yr=0))
# Data$CapCatchSumAut.lag1<-
#   (yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapCatchSumAut,lag.yr=-1))
# Data$CapCatchSumFrac <- 
#   yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapCatchSumFrac,lag.yr=0)
# Data$CapCatchSumFrac.lag1 <- 
#   yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CapCatchSumFrac,lag.yr=-1)
# 
# Data$cap_fishfraction <- 
#   Data$CapCatchSumAut * Data$CapCatchSumFrac/  # summer fishery yr t /
#   (Data$CapBMtot.lag - Data$CapBMmat.lag1) + # immatures yr t-1 + 
#   (Data$CapCatchSumAut.lag1 * (1 - Data$CapCatchSumFrac.lag1))/ # autumn fishery yr t-1/
#   Data$CapBMtot.lag1                          # immatures + matures yr t-1
# Data$cap_fishfraction[Data$cap_fishfraction < 0] <- 0
# Data$cap_F <- (-log(1-Data$cap_fishfraction))
# 
# # Observation error for capelin abundance
# # Capelin abundance series
# sigma.n2o <- as.numeric(rep(0.2, dim(Data)[1])) # Tjelmeland et al. 2002 reports CV around 0.2, which corresponds to s = 0.2 on log-scale (s = sqrt(log(1 + CV^2)))
# sigma.n3o <- as.numeric(rep(0.2, dim(Data)[1])) # Tjelmeland et al. 2002 reports CV around 0.2, which corresponds to s = 0.2 on log-scale (s = sqrt(log(1 + CV^2)))
# sigma.n2o[is.element(Data$Year,1991)] <- 0.5
# sigma.n3o[is.element(Data$Year,1987:1991)] <- 0.5
# # Observation errors for age-1 is set as for age-2:
# sigma.n1o <- sigma.n2o
# # Observation errors for age-4 is set as for age-3:
# sigma.n4o <- sigma.n3o
# # Higher observation errors assumed for age-groups with < 10 or 1 bill. ind.
# sigma.n1o[Data$CapN1 <= 10] <- 0.5
# sigma.n2o[Data$CapN2 <= 10] <- 0.5
# sigma.n3o[Data$CapN3 <= 10] <- 0.5
# sigma.n4o[Data$CapN4 <= 10] <- 0.5
# sigma.n1o[Data$CapN1 <= 1] <- 1
# sigma.n2o[Data$CapN2 <= 1] <- 1
# sigma.n3o[Data$CapN3 <= 1] <- 1
# sigma.n4o[Data$CapN4 <= 1] <- 1
# # Observation errors for age-1 for year 1979 is set to 1, as estimates are much (>2 unit on ln-scale) lower than for the same year class subsequent years:
# sigma.n1o[is.element(Data$Year,1979)] <- 1
# 
# # Incomplete survey coverage in 2014:
# sigma.n1o[Data$Year==2014] <- 0.5
# sigma.n2o[Data$Year==2014] <- 0.5
# sigma.n3o[Data$Year==2014] <- 0.5
# sigma.n4o[Data$Year==2014] <- 0.5
# 
# # Observation error for capelin length at age is estimated to be 0.02 (Stige et al. 2018, Ecology)
# # This translates to obs. error for biomass at age approx. 0.06
# # This component is added to the abundance component 
# # and converted from log-scale s to CVs using the formula
# # CV = sqrt(exp(s^2 - 1))
# sigma.w <- 0.06
# #cv.bm1o <- (exp((sigma.n1o + sigma.w)^2) - 1)^0.5
# #cv.bm2o <- (exp((sigma.n2o + sigma.w)^2) - 1)^0.5
# #cv.bm3o <- (exp((sigma.n3o + sigma.w)^2) - 1)^0.5
# #cv.bm4o <- (exp((sigma.n4o + sigma.w)^2) - 1)^0.5
# 
# cv.bm1o <- (exp(sigma.n1o^2 + sigma.w^2) - 1)^0.5
# cv.bm2o <- (exp(sigma.n2o^2 + sigma.w^2) - 1)^0.5
# cv.bm3o <- (exp(sigma.n3o^2 + sigma.w^2) - 1)^0.5
# cv.bm4o <- (exp(sigma.n4o^2 + sigma.w^2) - 1)^0.5
# 
# # log-scale s if using average log-scale s (n + w) across ages weighted by
# # biomass of each age - this gives higher values 
# # (range 0.26-0.60 median 0.27, vs range 0.14-0.42 median 0.17)
# #sigma.bm14 <- apply(cbind(sigma.n1o,sigma.n2o,sigma.n3o,sigma.n4o,
# # Data$CapBM1,Data$CapBM2,Data$CapBM3,Data$CapBM4),1,function(x){
# #  (((x[1] + sigma.w)^2 * x[5] + (x[2] + sigma.w)^2 * x[6] 
# #  + (x[3] + sigma.w)^2 * x[7] + (x[4] + sigma.w)^2 * x[8])/
# #  (x[5] + x[6] + x[7] + x[8]))^0.5})
# 
# # CV for total biomass if assuming independent errors across ages:
# cv.bm14o <- apply(cbind(cv.bm1o,cv.bm2o,cv.bm3o,cv.bm4o,
#  Data$CapBM1,Data$CapBM2,Data$CapBM3,Data$CapBM4),1,function(x){
#   (x[1]^2 * x[5]^2 + x[2]^2 * x[6]^2 
#   + x[3]^2 * x[7]^2 + x[4]^2 * x[8]^2)^0.5/
#   (x[5] + x[6] + x[7] + x[8])})
# 
# # log-scale (s = sqrt(log(1 + CV^2))) if assuming independent errors across ages:
# sigma.bm14 <- (log(1 + cv.bm14o^2))^.5
# 
# Data$logCapBMtot.se <- sigma.bm14
# median(Data$logCapBMtot.se) # 0.1433023
# range(Data$logCapBMtot.se)  # 0.1154990 0.3803729
# secomp(Data$logCapBMtot,Data$logCapBMtot.se) # 0.02249513
# secomp(log(Data$CapN2),sigma.n2o) # 0.04328496
# median(sigma.n2o)
# range(sigma.n2o) # 0.2 0.5
# 
# ## Polar cod biomass in stock (thousand tonnes)
# Data$logPolCodBMtot<-
#   log(yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$PolCodSB,lag.yr=0))
# 
# 
# ## Covariates
# 
# SeaIce <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/Sea Ice NSIDC/Ice.mean.N74-80.E20-50.txt",header=T)
# #> tapply(SeaIce$Ice,SeaIce$Month,mean)
# #        1         2         3         4         5         6         7         8         9        10        11        12 
# #46.570817 49.662857 51.836525 54.001705 46.311943 29.294314  9.769224  2.447714  1.815487  8.544170 27.649978 40.667724 
# #> tapply(SeaIce$Ice,SeaIce$Month,sd)
# #        1         2         3         4         5         6         7         8         9        10        11        12 
# #19.315685 20.126740 16.705211 17.232020 18.275671 16.212144  8.120931  3.057903  3.031910  8.887517 17.642835 19.029956 
# yrs.ice <- 1979:2016
# SeaIceAnn <- data.frame(
#   Year=yrs.ice,
#   Ice=NA)
# SeaIceAnn$Ice <- apply(cbind(
#   SeaIce$Ice[SeaIce$Month==12&is.element(SeaIce$Year,yrs.ice-1)],
#   SeaIce$Ice[SeaIce$Month==1&is.element(SeaIce$Year,yrs.ice)],
#   SeaIce$Ice[SeaIce$Month==2&is.element(SeaIce$Year,yrs.ice)],
#   SeaIce$Ice[SeaIce$Month==3&is.element(SeaIce$Year,yrs.ice)],
#   SeaIce$Ice[SeaIce$Month==4&is.element(SeaIce$Year,yrs.ice)],
#   SeaIce$Ice[SeaIce$Month==5&is.element(SeaIce$Year,yrs.ice)]),1,mean)
# Data$Ice <- yr.replace(yr=Data$Year,x=SeaIceAnn$Ice,x.yr=SeaIceAnn$Year)
# 
# # Sea temperature 
# SST <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/ICOADS/sst.74-80N.20-50E.May-Sep_08122016.txt")
# Data$SST <- yr.replace(yr=Data$Year,x=SST$SST,x.yr=SST$year)
# 
# SST.56 <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/ICOADS/sst.74-80N.20-50E.May-Jun_08122016.txt")
# Data$SST.56 <- yr.replace(yr=Data$Year,x=SST.56$SST,x.yr=SST.56$year)
# 
# SST.811 <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/ICOADS/sst.74-80N.20-50E.Aug-Nov_08122016.txt")
# Data$SST.811.lag1 <- yr.replace(yr=Data$Year,x=SST.811$SST,x.yr=SST.811$year,lag.yr=-1)
# 
# 
# # A. Trofimov and R. Ingvaldsen. 2016. Chapter 3.1 Hydrography. 
# # In: Prozorkevich, D. Sunnan? K. 2016. (Ed.) 
# # Survey report from the joint Norwegian/Russian ecosystem survey in the Barents Sea and adjacent
# # waters, August-October 2015. IMR/PINRO Joint Report eries, No. 1/2016. ISSN 1502-8828
# # 147 pages.
# # Table 3.1.1.1. Mean water temperatures in the main parts of standard 
# # oceanographic sections in the Barents Sea and adjacent waters in 
# # August?September 1965?2015. The sections are: ... 
# # Vard? ? North (VN, 72?15?N ? 74?15?N, 31?13?E, 50-200 m depth)...
# VN.yrs <- 1978:2015
# VN <- c(3.2,
#   3.6,3.7,3.4,4.1,4.8,4.2,3.7,3.8,3.5,3.8,
#   5.1,5.0,4.8,4.6,4.2,4.8,4.6,3.7,4.0,3.9,
#   4.8,4.2,4.2,4.6,4.7,4.8,5.0,5.3,4.9,4.8,
#   5.2,NA,5.1,5.7,5.0,5.2,5.6)
# VN[is.na(VN)] <- 5.15 # Replacing NA for 2010 with average of 2009 and 2011  
# Data$VN.lag1 <- yr.replace(yr=Data$Year,x=VN,x.yr=VN.yrs,lag.yr=-1)
# 
# WGIBAR <- read.table(file="D:/Leif_Oslo2/Barents Sea/Data/Fra Elena Eriksen/BS Time series fra WGIBAR.txt",head=T)
# Data$Icemax <- yr.replace(yr=Data$Year,x=WGIBAR$IceareamaxApril,x.yr=WGIBAR$Year)
# Data$ArW <- yr.replace(yr=Data$Year,x=WGIBAR$area_ArW,x.yr=WGIBAR$Year)
# 
# # Young herring biomass (thousand tons) age 1 and 2 (and 3) in the Barents Sea
# Data$laglogHerBM123<-
#   log(yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$HerBM13,lag.yr=-1))
# 
# # Cod BM age 3-6  (log(thousand tonnes))
# Data$logCodBM36<-
#   log(yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CodBM36,lag.yr=0)*10^3)
# Data$logCodBMtot<-
#   log(yr.replace(yr=Data$Year,x.yr=ICES$Year,x=ICES$CodBMtot,lag.yr=0)*10^3)
# 
# 
# BS.data <- Data[,c("Year"
#         ,"Cop"
#         ,"Cop.se"
#         ,"Krill"
#         ,"Krill.se"
#         ,"Amphipods"
#         ,"Amphipods.se"
#         ,"logCapBMtot"
#         ,"logCapBMtot.se"
#         ,"logPolCodBMtot"
#         ,"Ice"
#         ,"Icemax"
#         ,"ArW"
#         ,"SST"
#         ,"SST.56"
#         ,"SST.811.lag1"
#         ,"VN.lag1"
#         ,"logCodBMtot"
#         ,"laglogHerBM123"
#         ,"cap_F"
#         )]
# 
# 
# BS.data <- BS.data[is.element(BS.data$Year,yrs),]
# write.table(BS.data,"BS.data.txt")


#### Ina_start ######
BS.data <- read.table(file="~/Documents/Atlantis/Gompertz/Data/BS.data.txt",dec=".",sep="")



# DEFINE VARIABLES FOR STAN:
oe.factor <- 1.0 # multiplication factors for nominal obs.errors

# Zooplankton series
# (centered, log scale)
cop_offset <- mean(BS.data$Cop,na.rm=T)
cop_scale <- sd(BS.data$Cop,na.rm=T)
cop_obs <- (as.numeric(BS.data$Cop) - cop_offset)/cop_scale
cop_noNA <- c(1:length(yrs))[!is.na(cop_obs)]
cop_se <- c(BS.data$Cop.se / cop_scale)[cop_noNA] * oe.factor

krill_offset <- mean(BS.data$Krill,na.rm=T)
krill_scale <- sd(BS.data$Krill,na.rm=T)
krill_obs <- (as.numeric(BS.data$Krill) - krill_offset)/krill_scale
krill_noNA <- c(1:length(yrs))[!is.na(krill_obs)]
krill_se <- c(BS.data$Krill.se / krill_scale)[krill_noNA] * oe.factor

amph_offset <- mean(BS.data$Amphipods,na.rm=T)
amph_scale <- sd(BS.data$Amphipods,na.rm=T)
amph_obs <- (as.numeric(BS.data$Amphipods) - amph_offset)/amph_scale
amph_noNA <- c(1:length(yrs))[!is.na(amph_obs)]
amph_se <- c(BS.data$Amphipods.se / amph_scale)[amph_noNA] * oe.factor

# Capelin biomass
cap_offset <- mean(BS.data$logCapBMtot, na.rm=T)
cap_scale <- sd(BS.data$logCapBMtot, na.rm=T)
cap_F_offset <- mean(BS.data$cap_F, na.rm=T)
cap_obs <- (BS.data$logCapBMtot - cap_offset) / cap_scale
cap_noNA <- c(1:length(yrs))[!is.na(cap_obs)]
cap_F <- (BS.data$cap_F - cap_F_offset) / cap_scale
cap_se <- c(BS.data$logCapBMtot.se / cap_scale)[cap_noNA] * oe.factor 
                                             
# Polar cod biomass
pc_offset <- mean(BS.data$logPolCodBMtot, na.rm=T)
pc_scale <- sd(BS.data$logPolCodBMtot, na.rm=T)
pc_obs <- (BS.data$logPolCodBMtot - pc_offset)/pc_scale
pc_noNA <- c(1:length(yrs))[!is.na(pc_obs)]
pc_se <- 0.3/pc_scale # Arbitrarily chosen 

# Ice
ice_offset <- mean(BS.data$Ice, na.rm=T)
ice_scale <- sd(BS.data$Ice, na.rm=T)
ice <- (as.numeric(BS.data$Ice) - ice_offset) / ice_scale

# Icemax (in %)
# Note: the alternative model was fitted using Icemax in km2
# Total area 72?82?N, 10?60?E (km^2):
lats <- seq(from=72.25,to=81.75,by=.5)
lons <- seq(from=10.5,to=59.5,by=1)
x <- array(dim=c(length(lats),length(lons)),dimnames=list(lats,lons))
for(i in 1:length(lats)){for(j in 1:length(lons)){
  lat.i <- lats[i]; lon.j <- lons[j]
  ar.ij <- diff(lats)[1]*60*1.852 * diff(lons)[1]*60*1.852*cos(pi*lat.i/180)
  x[i,j] <- ar.ij
  }}
tot.ar <- sum(x)  
icemax_offset <- mean(100*BS.data$Icemax/tot.ar, na.rm=T)
icemax_scale <- sd(100*BS.data$Icemax/tot.ar, na.rm=T)
icemax <- (as.numeric(100*BS.data$Icemax/tot.ar) - icemax_offset) / icemax_scale

# ArW (in %)
# Note: the alternative model was fitted using ArW in 1000 km2
# Total area 72?80?N, 20?50?E (1000 km^2):
lats <- seq(from=72.25,to=79.75,by=.5)
lons <- seq(from=20.5,to=49.5,by=1)
x <- array(dim=c(length(lats),length(lons)),dimnames=list(lats,lons))
for(i in 1:length(lats)){for(j in 1:length(lons)){
  lat.i <- lats[i]; lon.j <- lons[j]
  ar.ij <- diff(lats)[1]*60*1.852 * diff(lons)[1]*60*1.852*cos(pi*lat.i/180)
  x[i,j] <- ar.ij
  }}
tot.ar <- sum(x)/1000  
ArW_offset <- mean(100*BS.data$ArW/tot.ar, na.rm=T)
ArW_scale <- sd(100*BS.data$ArW/tot.ar, na.rm=T)
ArW <- (as.numeric(100*BS.data$ArW/tot.ar) - ArW_offset) / ArW_scale

# Temperature
# Sumemr SST:
sst_offset <- mean(BS.data$SST)
sst_scale <- sd(BS.data$SST, na.rm=T)
sst <- (as.numeric(BS.data$SST) - sst_offset) / sst_scale

# Spring SST:
sst.56_offset <- mean(BS.data$SST.56)
sst.56_scale <- sd(BS.data$SST.56, na.rm=T)
sst.56 <- (as.numeric(BS.data$SST.56) - sst.56_offset) / sst.56_scale

# Lag-1 autumn SST:
sst.811.lag1_offset <- mean(BS.data$SST.811.lag1)
sst.811.lag1_scale <- sd(BS.data$SST.811.lag1, na.rm=T)
sst.811.lag1 <- (as.numeric(BS.data$SST.811.lag1) - sst.811.lag1_offset) / sst.811.lag1_scale

# Lag-1 autumn ST at VN:
vn.89.lag1_offset <- mean(BS.data$VN.lag1)
vn.89.lag1_scale <- sd(BS.data$VN.lag1, na.rm=T)
vn.89.lag1 <- (as.numeric(BS.data$VN.lag1) - vn.89.lag1_offset) / vn.89.lag1_scale

# Predators on capelin
her_offset <- mean(BS.data$laglogHerBM123, na.rm=T)
her_scale <- sd(BS.data$laglogHerBM123, na.rm=T)
her <- (as.numeric(BS.data$laglogHerBM123) - her_offset) / her_scale

#cod_offset <- mean(BS.data$logCodBM36, na.rm=T)
#cod_scale <- sd(BS.data$logCodBM36, na.rm=T)
#cod <- (as.numeric(BS.data$logCodBM36) - cod_offset) / cod_scale
cod_offset <- mean(BS.data$logCodBMtot, na.rm=T)
cod_scale <- sd(BS.data$logCodBMtot, na.rm=T)
cod <- (as.numeric(BS.data$logCodBMtot) - cod_offset) / cod_scale

# A list with all data
zoo_dat <- list(
         N = length(yrs),
         K = 5,
         cop_N = length(cop_noNA),
         cop_obs = cop_obs[cop_noNA], 
         cop_se =  cop_se,
         cop_noNA = cop_noNA,
         krill_N = length(krill_noNA),
         krill_obs = krill_obs[krill_noNA], 
         krill_se = krill_se,
         krill_noNA = krill_noNA,
         amph_N = length(amph_noNA),
         amph_obs = amph_obs[amph_noNA], 
         amph_se = amph_se,
         amph_noNA = amph_noNA,
         cap_N = length(cap_noNA),
         cap_obs = cap_obs[cap_noNA], 
         cap_se = cap_se,
         cap_noNA = cap_noNA,
         cap_F = cap_F,
         pc_N = length(pc_noNA),
         pc_obs = pc_obs[pc_noNA], 
         pc_se = pc_se,
         pc_noNA = pc_noNA,
         her = her,
         cod = cod,
         ice = ice
         )

save(zoo_dat, file = "zoo_dat.rdata")

#plot(zoo_dat$ice*ice_scale+ice_offset,type="l")
#plot(zoo_dat$ice, type="l")

###################################
###################################
###################################
###################################
# Fitting the stan model
#INA
#stanseed <- 1234567
#stanseed <- 7654321
#fit <- stan(file = 'copkrillamph_mod_v3.stan', data = zoo_dat, 
#            warmup = 10000, iter = 20000, chains = 4, thin = 20, seed = stanseed
#            ,control = list(
#            adapt_delta = .995 # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#            ,max_treedepth = 15 # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#            ) # http://mc-stan.org/misc/warnings.html#bfmi-low 
#            )
#save(fit, file = "codkrillamph_stanmod_v3_new_26112018.stanfit")

##### Ina Test
stanseed <- 7654321
fit <- stan(file = 'copkrillamph_mod_vIna.stan', data = zoo_dat, 
            warmup = 10000, iter = 20000, chains = 3, thin = 20, seed = stanseed
            ,control = list(
              adapt_delta = .999 # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
              ,max_treedepth = 15 # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
            ) # http://mc-stan.org/misc/warnings.html#bfmi-low 
)
#save(fit, file = "codkrillamph_stanmod_v3_42-44-55_26112018.stanfit")
save(fit, file = "codkrillamph_stanmod_v3_13022020.stanfit")


#########Shinystan #########
library("shinystan")
#?shinystan

load("~/Documents/Atlantis/Gompertz/Data/codkrillamph_stanmod_v3_42-44-55_26112018.stanfit")
fit_shiny <- as.shinystan(fit)
shinystan::launch_shinystan(fit_shiny)



#save(fit, file = "codkrillamph_stanmod_v3_new.stanfit")
# This model constraints PE to be min. 0.1: No warnings!!! 
# Increasing Obs.error to .2 or .4 (instead of .01 and 0.02), with mean 1.2 for krill
#save(fit, file = "codkrillamph_stanmod_v3_OE.2.R")
# Increasing Obs.error to .1 or .2 (instead of .01 and 0.02)
#save(fit, file = "codkrillamph_stanmod_v3_OE.1.R")


#load("codkrillamph_stanmod_v3_new.stanfit")
load("codkrillamph_stanmod_v3_new_26112018.stanfit")

print(fit)
plot(fit)
pairs(fit)
pairs(fit, pars = c(
 #   "c10","c11","c13","c14","c15","c16", 
 #   "c20","c22","c24","c26", 
 #   "c30","c31","c33","c35","c36",
 #   "c40","c41","c42","c44","c46","c47","c48","c49", 
 #   "c50","c51","c53","c55","c56","c57"#, 
    "c11","c13","c14","c15", 
    "c22","c24", 
    "c31","c33","c35",
    "c47","c48", 
    "c51","c53","c55", 
 #   "Omega[1,2]",
    "L_sigma[1]","L_sigma[2]","L_sigma[3]","L_sigma[4]","L_sigma[5]",
 #   "s1o","s2o","s3o","s4o","s5o"#,
  #  "Latent[1,2]","Latent[4,1]")
  "lp__"
  ))
#pairs(fit, pars = "pc_latent")



library(shinystan)
my_sso <- launch_shinystan(fit)

la <- extract(fit, permuted = TRUE) # return a list of arrays 
la$cop_latent <- la$Latent[,,1]
la$krill_latent <- la$Latent[,,2]
la$amph_latent <- la$Latent[,,3]
la$cap_latent <- la$Latent[,,4]
la$pc_latent <- la$Latent[,,5]

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions as.array (or as.matrix) on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)

## identify parameters that are correlated with energy__
sp <- get_sampler_params(fit, inc_warmup=F)
energy <- NULL
  for(i in 1:4){energy <- c(energy,sp[[i]][,"energy__"])}
rtab <- round(cor(energy, m),2)
rtab[,order(-abs(rtab))][2:17]
# L_sigma[3], L_sigma[4], L_sigma[5], c31, c33, c35, c53, c55

## Some useful functions:
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]


### Plot and print coefficients at the original scale of the data
X11(height=17/2.54,width=8.5/2.54,pointsize=10)
pdf(height=17/2.54,width=8.5/2.54,pointsize=10,file="Fig4.pdf")
tab <- NULL
coefs <- dimnames(m)[[2]]
par(mfrow=c(5,1),mai=c(.1,.4,.1,.1),omi=c(.8,.35,0,0))
for(j in 1:5){
scale.y <- c(cop_scale,krill_scale,amph_scale,cap_scale,pc_scale)[j]
if(j==1) ylims <- c(-1,1)
if(j==2) ylims <- c(-1.5,1.5)
if(j==3) ylims <- c(-1,1.8)
if(j==4) ylims <- c(-2.8,1.5)
if(j==5) ylims <- c(-1.5,2.3)
yat <- c(-2,-1,0,1,2)
plot(0,0,xlim=c(.5,9),ylim=ylims,xlab="",ylab="",axes=F,type="n")
for(i in 1:9){
  scale.x <- c(cop_scale,krill_scale,amph_scale,cap_scale,pc_scale,
     ice_scale/10,cod_scale,her_scale,scale.y)[i]
  coef.ij <- paste("c",j,i,sep="",collapse="")
  coefno.ij <- paste(j,i,sep="")
  if(is.element(coef.ij,coefs)){
    points(x=i, y=apply(m,2,mean)[coef.ij]*scale.y/scale.x, pch=16)
    segments(
      x0=i,
      x1=i,
      y0=apply(m,2,CI1)[coef.ij]*scale.y/scale.x,
      y1=apply(m,2,CI2)[coef.ij]*scale.y/scale.x)
#  mtext(side=3,at=i,text=coef.ij,cex=.8,line=-.5)
  mtext(side=3,at=i,text=bquote(italic(c[.(coefno.ij)])),cex=1,line=-.5)
  tab <- rbind.data.frame(tab,
    data.frame(
      Coef=coef.ij,
      Mean=apply(m,2,mean)[coef.ij]*scale.y/scale.x,
      CI1=apply(m,2,CI1)[coef.ij]*scale.y/scale.x,
      CI2=apply(m,2,CI2)[coef.ij]*scale.y/scale.x
      ))
  }
}
abline(h=0,lty=3)
axis(side=2,at=ylims*2)
axis(side=2,at=yat)
var.j <- c("Cop","Krill","Amph","Cap","Pol")[j] 
mtext(side=2,line=3,var.j,font=3,cex=1)
}
xlabs <- list("Cop","Krill","Amph","Cap","Pol","Ice","Cod","Her","")
for(i in 1:length(xlabs)){
 mtext(side=1,at=i,line=1,las=2,cex=1,bquote(.(xlabs[[i]])),font=3)}
mtext(side=1,at=i,line=1,las=2,cex=1,expression(italic(F[CAP])))
mtext(side=2,line=1.5,outer=T,"Response variable",font=2)
mtext(side=1,line=5,outer=T,"Predictor variable",font=2)
dev.off()

# Climate effects:
pars <- paste("c",1:5,"6",sep="")
cbind.data.frame(tab[pars,1], round(exp(tab[pars,2:4]),2))
cbind.data.frame(tab[pars,1], round(tab[pars,2:4],2))
#    tab[pars, 1]   Mean    CI1    CI2
#c16          c16  0.086  0.002  0.164
#c26          c26 -0.168 -0.406  0.083
#c36          c36  0.165  0.025  0.320
#c46          c46 -0.206 -0.388 -0.011
#c56          c56 -0.069 -0.333  0.155
#> cbind.data.frame(tab[pars,1], round(exp(tab[pars,2:4]),3))
#    tab[pars, 1]  Mean   CI1   CI2
#c16          c16 1.090 1.002 1.178
#c26          c26 0.846 0.666 1.086
#c36          c36 1.180 1.025 1.377
#c46          c46 0.813 0.678 0.989
#c56          c56 0.933 0.717 1.167

# Effect of doubling, bottom-up effects
pars <- c("c31","c41","c42","c51","c53")
cbind.data.frame(tab[pars,1],round(exp(tab[pars,2:4]*log(2)),2))
#    tab[pars, 1] Mean  CI1  CI2
#c31          c31 1.67 0.92 2.98
#c41          c41 0.97 0.36 2.44
#c42          c42 1.53 1.18 2.00
#c51          c51 1.47 0.48 4.32
#c53          c53 0.93 0.44 2.06

# Effect of doubling, top effects
pars <- c("c13","c14","c15","c24","c35","c47","c48","c57")
cbind.data.frame(tab[pars,1],round(exp(tab[pars,2:4]*log(2)),2)-1)
#    tab[pars, 1]  Mean   CI1   CI2
#c13          c13 -0.01 -0.23  0.26
#c14          c14 -0.10 -0.20 -0.01
#c15          c15  0.06 -0.09  0.27
#c24          c24 -0.21 -0.38  0.00
#c35          c35 -0.26 -0.48 -0.02
#c47          c47 -0.58 -0.76 -0.25
#c48          c48 -0.13 -0.25  0.03
#c57          c57 -0.08 -0.51  0.51


### Plot coefficients at the standardised scale the model was fitted in
X11(height=17/2.54,width=8.5/2.54,pointsize=10)
pdf(height=17/2.54,width=8.5/2.54,pointsize=10,file="FigS3.pdf")
coefs <- dimnames(m)[[2]]
par(mfrow=c(5,1),mai=c(.1,.4,.1,.1),omi=c(.9,.35,0,0))
for(j in 1:5){
if(j!=4) ylims <- c(-1.5,2) else ylims <- c(-2.8,2)
plot(0,0,xlim=c(-.5,9),ylim=ylims,xlab="",ylab="",axes=F,type="n")
for(i in 0:9){
  coef.ij <- paste("c",j,i,sep="",collapse="")
  coefno.ij <- paste(j,i,sep="")
  if(is.element(coef.ij,coefs)){
    points(x=i, y=apply(m,2,mean)[coef.ij], pch=16)
    segments(
      x0=i,
      x1=i,
      y0=apply(m,2,CI1)[coef.ij],
      y1=apply(m,2,CI2)[coef.ij])
  #text(x=i,y=2,lab=coef.ij)
  mtext(side=3,at=i,text=bquote(italic(c[.(coefno.ij)])),cex=1,line=-.5)
  }
predictor.i <- c
}
abline(h=0,lty=3)
axis(side=2,at=ylims*2)
axis(side=2,at=c(-2,-1,0,1,2))
var.j <- c("Cop","Krill","Amph","Cap","Pol")[j] 
mtext(side=2,line=3,var.j,font=3,cex=1)
}
xlabs <- list("Intercept","Cop","Krill","Amph","Cap","Pol","Ice","Cod","Her","")
for(i in 1:length(xlabs)){mtext(side=1,at=i-1,line=1,las=2,bquote(.(xlabs[[i]])),font=3)}
mtext(side=1,at=i-1,line=1,las=2,cex=.8,expression(italic(F[CAP])))
mtext(side=2,line=1.5,outer=T,"Response variable",font=2)
mtext(side=1,line=6,outer=T,"Predictor variable",font=2)
dev.off()

## Plot pairwise correlations between variables

par(mfcol=c(5,1),mai=c(.45,.6,.1,.1),omi=c(.3,.1,.1,.1))
Data <- BS.data
rplot <- function(x,y,xlab,ylab){
  plot(x=x,y=y,xlab=xlab,ylab=ylab,pch=16,cex.lab=1.2)
  rtest <- cor.test(x,y)
  rval <- round(rtest$est,2)
  pval <- rtest$p.v
  mtext(side=3,bquote(r==.(rval)))
  if(pval<0.05) mtext(side=3,adj=1,"*")
  }
rplot(Data$Ice,Data$Cop,xlab="", ylab="COPEPODS (ln scale)")
rplot(Data$Ice,Data$Krill,xlab="", ylab="KRILL (ln scale)")
rplot(Data$Ice,Data$Amphipods,xlab="", ylab="AMPHIPODS (ln scale)")
rplot(Data$Ice,Data$logCapBMtot,xlab="", ylab="CAPELIN (ln scale)")
rplot(Data$Ice,Data$logPolCodBMtot,xlab="", ylab="POLAR COD (ln scale)")
mtext(side=1,outer=T,"SEA ICE (%)",adj=.7)

par(mfcol=c(5,5),mai=c(.6,.6,.1,.1),omi=c(.1,.1,.1,.1))
Data <- BS.data
for(i in 1:5){
 for(j in 1:5){
xvar <- c("Cop","Krill","Amphipods","logCapBMtot","logPolCodBMtot")[i] 
yvar <- c("Cop","Krill","Amphipods","logCapBMtot","logPolCodBMtot")[j]
if(i-j==1)xnam=c("COPEPODS","KRILL","AMPHIPODS","CAPELIN","POLAR COD")[i] else xnam="" 
if(i-j==1)ynam=c("COPEPODS","KRILL","AMPHIPODS","CAPELIN","POLAR COD")[j] else ynam=""
if(j<i)rplot(Data[,xvar],Data[,yvar],xlab=xnam, ylab=ynam) else {
plot(0,0,xlab="",ylab="",type="n",axes=F)}
 }}



### Plot time series

X11(height=17/2.54,width=17/2.54,pointsize=10)
pdf(height=17/2.54,width=17/2.54,pointsize=10,file="Fig2.pdf")
plot.posterior <- c(T,F)[1]
# Scaling factors for obs.error:
if(plot.posterior == T){
  oe1 <- 2 * mean(la$s1o)
  oe2 <- 2 * mean(la$s2o)
  oe3 <- 2 * mean(la$s3o)
  oe4 <- 2 * mean(la$s4o)
  oe5 <- 2 * mean(la$s5o)
  } else {
  oe1 <- 2 * 1.2
  oe2 <- 2 * 1.2
  oe3 <- 2 * 1.2
  oe4 <- 2 * 1.2
  oe5 <- 2 * 1
  }
par(mfcol=c(5,2),mai=c(.1,.7,.1,.1),omi=c(.6,0,.2,.5))
ylims <- c(-4,4) 
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=4,font=2)
mtext(side=2,expression(paste("(ln(1 + g DW m"^-2,")",sep="")),line=2,cex=.8)
if(plot.posterior==T){          
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
}
points(yrs[zoo_dat$cop_noNA],zoo_dat$cop_obs,pch=16)
segments(x0 = yrs[zoo_dat$cop_noNA], 
         x1 = yrs[zoo_dat$cop_noNA],
         y0 = zoo_dat$cop_obs + oe1 * zoo_dat$cop_se,
         y1 = zoo_dat$cop_obs - oe1 * zoo_dat$cop_se)

yat <- c(-10:10)*4
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=4,font=2)
mtext(side=2,expression(paste("(ln(g WW m"^-3,")",sep="")),line=2,cex=.8)
if(plot.posterior==T){
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
}
points(yrs[zoo_dat$krill_noNA],zoo_dat$krill_obs,pch=16)
segments(x0 = yrs[zoo_dat$krill_noNA], 
         x1 = yrs[zoo_dat$krill_noNA],
         y0 = zoo_dat$krill_obs + oe2 * zoo_dat$krill_se,
         y1 = zoo_dat$krill_obs - oe2 * zoo_dat$krill_se)

yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=4,font=2)
mtext(side=2,expression(paste("(ln(0.1 + kg WW nm"^-1,")",sep="")),line=2,cex=.8)
if(plot.posterior==T){
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
}
points(yrs[zoo_dat$amph_noNA],zoo_dat$amph_obs,pch=16)
segments(x0 = yrs[zoo_dat$amph_noNA], 
         x1 = yrs[zoo_dat$amph_noNA],
         y0 = zoo_dat$amph_obs + oe3 * zoo_dat$amph_se,
         y1 = zoo_dat$amph_obs - oe3 * zoo_dat$amph_se)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=4,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2,cex=.8)
if(plot.posterior==T){
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
}
points(yrs[zoo_dat$cap_noNA],zoo_dat$cap_obs,pch=16)
segments(x0 = yrs[zoo_dat$cap_noNA], 
         x1 = yrs[zoo_dat$cap_noNA],
         y0 = zoo_dat$cap_obs + oe4 * zoo_dat$cap_se,
         y1 = zoo_dat$cap_obs - oe4 * zoo_dat$cap_se)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=4,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2,cex=.8)
if(plot.posterior==T){
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
}
points(yrs[zoo_dat$pc_noNA],zoo_dat$pc_obs,pch=16)
segments(x0 = yrs[zoo_dat$pc_noNA], 
         x1 = yrs[zoo_dat$pc_noNA],
         y0 = zoo_dat$pc_obs + oe5 * zoo_dat$pc_se,
         y1 = zoo_dat$pc_obs - oe5 * zoo_dat$pc_se)
axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)

ylims <- c(-2.5,2.5) 
yat <- c(-40:40)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cod_offset)/cod_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,expression(bold("Cod")),line=4,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2,cex=.8)
points(yrs,zoo_dat$cod,pch=16,type="b")

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - her_offset)/her_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,expression(bold(paste("Herring"," (lag 1 yr)",sep=""))),line=4,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2,cex=.8)
points(yrs,zoo_dat$her,pch=16,type="b")

yat <- c(0:5)*.2
ylims=c(-.1,.5)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_F_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Fishing",line=4,font=2)
mtext(side=2,expression(paste("(-ln(1 - fraction))",sep="")),line=2,cex=.8)
points(yrs, zoo_dat$cap_F,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:5)*20
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - ice_offset)/ice_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Ice cover",line=4,font=2)
mtext(side=2,expression(paste("(%)",sep="")),line=2,cex=.8)
points(yrs,zoo_dat$ice,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:5)*20
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - ArW_offset)/ArW_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Arctic waters",line=4,font=2)
mtext(side=2,expression(paste("(%)",sep="")),line=2,cex=.8)
points(yrs,ArW,pch=16,type="b")

yat <- c(0:10)
axis(side=4, at=(yat - vn.89.lag1_offset)/vn.89.lag1_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,"Sea temperature",line=2,font=2)
mtext(side=4,expression(paste("("*degree,"C)",sep="")),line=4,cex=.8)
points(yrs,vn.89.lag1,pch=1,type="b")

axis(side=1,las=-.3,pos=-5.5*(2.5/4))
mtext(side=1,"Year",line=5,font=2)
mtext(side=3,line=0,outer=T,adj=0.02,"A")
mtext(side=3,line=0,outer=T,adj=0.52,"B")

dev.off()

### Plot alternative climate variables time series

X11(height=22/2.54,width=8.5/2.54,pointsize=10)
pdf(height=22/2.54,width=8.5/2.54,pointsize=10,file="FigS1.pdf")

par(mfcol=c(7,1),mai=c(.1,.85,.1,.05),omi=c(.6,0,.2,.05))
ylims <- c(-2.5,2.5) 
yat <- c(0:5)*20
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - ice_offset)/ice_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Sea ice",line=6,font=2)
mtext(side=2,"winter",line=4,font=2)
mtext(side=2,expression(paste("(% area)",sep="")),line=2,cex=.8)
points(yrs,zoo_dat$ice,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:5)*20
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - icemax_offset)/icemax_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Sea ice",line=6,font=2)
mtext(side=2,"April",line=4,font=2)
mtext(side=2,expression(paste("(% area)",sep="")),line=2,cex=.8)
points(yrs,icemax,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:5)*20
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - ArW_offset)/ArW_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Arctic waters",line=6,font=2)
mtext(side=2,"autumn",line=4,font=2)
mtext(side=2,expression(paste("(% area)",sep="")),line=2,cex=.8)
points(yrs,ArW,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - sst_offset)/sst_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"SST",line=6,font=2)
mtext(side=2,"summer",line=4,font=2)
mtext(side=2,expression(paste("("*degree,"C)",sep="")),line=2,cex=.8)
points(yrs,sst,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - sst.56_offset)/sst.56_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"SST",line=6,font=2)
mtext(side=2,"spring",line=4,font=2)
mtext(side=2,expression(paste("("*degree,"C)",sep="")),line=2,cex=.8)
points(yrs,sst.56,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - sst.811.lag1_offset)/sst.811.lag1_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"SST",line=6,font=2)
mtext(side=2,"prev. autumn",line=4,font=2)
mtext(side=2,expression(paste("("*degree,"C)",sep="")),line=2,cex=.8)
points(yrs,sst.811.lag1,pch=16,type="b")

ylims <- c(-2.5,2.5) 
yat <- c(0:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - vn.89.lag1_offset)/vn.89.lag1_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Water col. temp.",line=6,font=2)
mtext(side=2,"prev. autumn",line=4,font=2)
mtext(side=2,expression(paste("("*degree,"C)",sep="")),line=2,cex=.8)
points(yrs,vn.89.lag1,pch=16,type="b")

axis(side=1,las=-.3,pos=-5.5*(2.5/4))
mtext(side=1,"Year",line=5,font=2)

dev.off()

# Study area:
contour2 <- read.table("D:/Leif_Oslo2/R Barents Sea/Skripter/Analyses Progr Oceanogr 2013/contour.2.txt")
contour4 <- read.table("D:/Leif_Oslo2/R Barents Sea/Skripter/Analyses Progr Oceanogr 2013/contour.4.txt")
library(maps)
library(mapdata)
library(TeachingDemos)#subplot
library(mapproj) # map projections

X11(width=8.5/2.54, height=8.5/2.54, pointsize=10)
pdf(width=8.5/2.54, height=8.5/2.54, pointsize=10, file="Fig1.pdf")
par(mfrow=c(1,1),mai=rep(0,4),omi=c(.5,.5,0.03,0.03))
xlim=c(20,68);ylim=c(67,80)
plot(0,0,xlim=xlim,ylim=ylim,type="n",axes=F,xlab="",ylab="",main="")
lines(contour2$x,contour2$y)
lines(contour4$x,contour4$y)
text(25,72,"SW")
text(35,76,"C")
text(45,78,"N")
map(fill=T,col="black",add=T,lwd=1)
polygon(x=c(20,50,50,20),y=c(74,74,80,80),border="darkgrey")
box()
x.at <- 10*c(0:20)
y.at <- 2*c(0:45)
axis(1,tcl=.2,at=x.at,mgp=c(2,.25,0))
axis(2,tcl=.2,at=y.at,mgp=c(3,.25,0))
axis(3,lab=F,tcl=.2,at=x.at,col="black")
axis(4,lab=F,tcl=.2,at=y.at,col="black")
mtext(side=1,expression(paste("Longitude ("*degree,"E)",sep="")),line=1.5)
mtext(side=2,expression(paste("Latitude ("*degree,"N)",sep="")),line=1.5)

rect(xleft=50, xright=69.5, ybottom=66.6, ytop=72.4,col="white")
myexpand <- function(x)c(x[1]-0.05*diff(range(x)),x[2]+0.05*diff(range(x)))
myplot <- function(){
         par(new=T)
         plot(0,0,xlim=c(-100,80),ylim=c(20,81),type="n",axes=F,xlab="",ylab="",main="")
         map(add=T,fill=T,col="grey60",interior=F,border=F)
         polygon(x=myexpand(xlim)[c(1,1,2,2)],y=myexpand(ylim)[c(1,2,2,1)],border="black",lwd=3)
         box()
         text(x=-40,y=50,"North",cex=.8,col="black",srt=0)
         text(x=-40,y=40,"Atlantic",cex=.8,col="black",srt=0)
         text(x=-40,y=30,"Ocean",cex=.8,col="black",srt=0)
        }
subplot(myplot(),x=c(50,69.5),y=c(66.6,72.4))

dev.off()


#### Indirect climate effects (lag 1):
# For example, on copepods
# c26 * c12 + c36 * c13 + c46 * c14 + c56 * c15

ice_scale_plot <- ice_scale/(-10)  # plot ln-scale in response per -30% ice

coefs <- dimnames(m)[[2]]

# species interactions
sp.int <- list() ; length(sp.int) <- 5
for(i in 1:5){
  index <- c(1:5)
  index <- index[index!=i] # excluding autoregressive coefficient
  int.i.full <- paste("c",i,index,sep="")
  int.i.used <- int.i.full[is.element(int.i.full,coefs)]
  sp.int[[i]] <- int.i.used
}

# coefficient names for climate effects on interacting species
clim.eff <- list() ; length(clim.eff) <- 5
for(i in 1:5){
  int.i <- sp.int[[i]]
  clim.eff.i <- unlist(lapply(strsplit(int.i,""),
    function(x) paste(x[1],x[3],6,sep="")))
  clim.eff[[i]] <- clim.eff.i
}

# indirect climate effects
ind.clim <- list() ; length(ind.clim) <- 5
for(i in 1:5){
  int.i <- sp.int[[i]]
  clim.eff.i <- clim.eff[[i]]
  func.i <- bquote(sum(x[.(int.i)] * x[.(clim.eff.i)]))
  ind.clim[[i]] <- func.i 
}

# indirect climate effects
c1.ic <- apply(m, 1, function(x) eval(ind.clim[[1]]))*cop_scale/ice_scale_plot
c2.ic <- apply(m, 1, function(x) eval(ind.clim[[2]]))*krill_scale/ice_scale_plot
c3.ic <- apply(m, 1, function(x) eval(ind.clim[[3]]))*amph_scale/ice_scale_plot
c4.ic <- apply(m, 1, function(x) eval(ind.clim[[4]]))*cap_scale/ice_scale_plot
c5.ic <- apply(m, 1, function(x) eval(ind.clim[[5]]))*pc_scale/ice_scale_plot

# plotting direct and indirect climate effects
X11(height=15/2.54,width=8.5/2.54,pointsize=10)
pdf(height=15/2.54,width=8.5/2.54,pointsize=10,file="Fig5.pdf")

par(mfrow=c(5,1),mai=c(.1,.4,.1,.1),omi=c(.6,.35,0,0))
for(j in 1:5){
ylims <- c(-1,1)
if(j==1) ylims <- c(-.2,.2)      
if(j==2) ylims <- c(-.2,.5)  
if(j==3) ylims <- c(-.4,.2)    
if(j==4) ylims <- c(-.2,.5)  
if(j==5) ylims <- c(-.3,.4)    
scale.y <- c(cop_scale,krill_scale,amph_scale,cap_scale,pc_scale)[j]
plot(0,0,xlim=c(0,2),ylim=ylims,xlab="",ylab="",axes=F,type="n")
  coef.j <- paste("c",j,6,sep="",collapse="")
  if(is.element(coef.j,coefs)){
    points(x=.5, y=apply(m,2,mean)[coef.j]*scale.y/ice_scale_plot, pch=16)
    segments(
      x0=.5,
      x1=.5,
      y0=apply(m,2,CI1)[coef.j]*scale.y/ice_scale_plot,
      y1=apply(m,2,CI2)[coef.j]*scale.y/ice_scale_plot)
}
    points(x=1.5, 
      y=c(mean(c1.ic),mean(c2.ic),mean(c3.ic),mean(c4.ic),mean(c5.ic))[j]
      , pch=16)
    segments(
      x0=1.5,
      x1=1.5,
      y0=c(quantile(c1.ic,.025),
        quantile(c2.ic,.025),
        quantile(c3.ic,.025),
        quantile(c4.ic,.025),
        quantile(c5.ic,.025))[j],
      y1=c(quantile(c1.ic,.975),
        quantile(c2.ic,.975),
        quantile(c3.ic,.975),
        quantile(c4.ic,.975),
        quantile(c5.ic,.975))[j])

abline(h=0,lty=3)
axis(side=2,at=ylims*2)
axis(side=2,at=c(-10:10)*.2)
var.j <- c("Cop","Krill","Amph","Cap","Pol",cex=1)[j] 
mtext(side=2,line=3,var.j,font=3)
}
mtext(side=2,line=1.5,outer=T,"Response variable",font=2)
mtext(side=1, line = 3, at = c(.5, 1.5), text = c("Direct", "Indirect"),cex=1)
mtext(side=1, line=5, at=1, outer=F,adj=.5,"Climate effect",font=2)
dev.off()

#### Plotting delayed effects of climate change
X11(height=15/2.54,width=8.5/2.54,pointsize=10)
pdf(height=15/2.54,width=8.5/2.54,pointsize=10,file="Fig6.pdf")

par(mfcol=c(5,2),mai=c(.1,.3,.1,.1),omi=c(.6,.45,.15,0))

#### Delayed effects of transient climate change:
ice_scale_plot <- ice_scale/(-25)  # plot ln-scale in response per +30% ice

coefs <- dimnames(m)[[2]]

# coefficients for species interactions - including autoregressive terms
sp.int <- list() ; length(sp.int) <- 5
for(i in 1:5){
  index <- c(1:5)
  int.i.full <- paste("c",i,index,sep="")
  int.i.used <- int.i.full[is.element(int.i.full,coefs)]
  sp.int[[i]] <- int.i.used
}

# species interactions - numbers 1-5
sp.int.no <- list() ; length(sp.int.no) <- 5
for(i in 1:5){
  sp.int.no[[i]] <- as.numeric(unlist(lapply(strsplit(sp.int[[i]],""),
    function(x)x[[3]])))
}

# array of delayed climate effects at lags 0 to lag.max
lag.max <- 10
climeff <- array(dim=c(dim(m)[1], 5, lag.max + 1))

# lag 0 climate effects
climeff[,1,1] <- m[,"c16"]
climeff[,2,1] <- m[,"c26"]
climeff[,3,1] <- m[,"c36"]
climeff[,4,1] <- m[,"c46"]
climeff[,5,1] <- m[,"c56"]

# lagged climate effects
for(i in 1:lag.max){
  lag.i <- i
  climeff[,1,lag.i + 1] <- apply(m[,sp.int[[1]]] * 
                        climeff[,sp.int.no[[1]],lag.i],1,sum)
  climeff[,2,lag.i + 1] <- apply(m[,sp.int[[2]]] * 
                        climeff[,sp.int.no[[2]],lag.i],1,sum)
  climeff[,3,lag.i + 1] <- apply(m[,sp.int[[3]]] * 
                        climeff[,sp.int.no[[3]],lag.i],1,sum)
  climeff[,4,lag.i + 1] <- apply(m[,sp.int[[4]]] * 
                        climeff[,sp.int.no[[4]],lag.i],1,sum)
  climeff[,5,lag.i + 1] <- apply(m[,sp.int[[5]]] * 
                        climeff[,sp.int.no[[5]],lag.i],1,sum)
}


# plotting lagged climate effects
for(j in 1:5){
ylims <- c(-1,1)
if(j==1) {ylims <- c(-.8,.8); yat <- c(-10:10)*0.5}      
if(j==2) {ylims <- c(-1.5,1.5); yat <- c(-10:10)}  
if(j==3) {ylims <- c(-.8,.8); yat <- c(-10:10)*0.5}    
if(j==4) {ylims <- c(-1.8,2.2); yat <- c(-10:10)}  
if(j==5) {ylims <- c(-1.5,1.5); yat <- c(-10:10)}    
scale.y <- c(cop_scale,krill_scale,amph_scale,cap_scale,pc_scale)[j]

yfit <- apply(climeff[,j,],2,mean)*scale.y/ice_scale_plot
ci1 <- apply(climeff[,j,],2,function(x)quantile(x,0.025))*scale.y/ice_scale_plot
ci2 <- apply(climeff[,j,],2,function(x)quantile(x,0.975))*scale.y/ice_scale_plot
ci1in <- apply(climeff[,j,],2,function(x)quantile(x,0.05))*scale.y/ice_scale_plot
ci2in <- apply(climeff[,j,],2,function(x)quantile(x,0.95))*scale.y/ice_scale_plot
ci1inn <- apply(climeff[,j,],2,function(x)quantile(x,0.1))*scale.y/ice_scale_plot
ci2inn <- apply(climeff[,j,],2,function(x)quantile(x,0.9))*scale.y/ice_scale_plot
xval <- c(0:lag.max)

plot(0,0,xlim=c(0,lag.max),ylim=ylims,xlab="",ylab="",axes=F,type="n")
polygon(x=c(xval,reverse(xval)),y=c(ci1,reverse(ci2)),col="grey90",border=NA)
polygon(x=c(xval,reverse(xval)),y=c(ci1in,reverse(ci2in)),col="grey75",border=NA)
polygon(x=c(xval,reverse(xval)),y=c(ci1inn,reverse(ci2inn)),col="grey60",border=NA)
lines(x=xval, y=yfit, pch=16)
points(x=xval, y=yfit, pch=16)

abline(h=0,lty=3)
axis(side=2,at=ylims*2)
axis(side=2,at=yat)
var.j <- c("Cop","Krill","Amph","Cap","Pol",cex=.8)[j] 
mtext(side=2,line=3,var.j,font=3)
}
mtext(side=2,line=2.5,outer=T,"Response variable",font=2)
axis(side=1,at=c(0,lag.max*2))
axis(side=1)
mtext(side=1, line = 3, at =lag.max/2, text = c("Time lag (years)"),cex=1)
mtext(side=1, line=5, at=lag.max/2, outer=F,adj=.5,"Climate effect",font=2)



#### Delayed effects of persistent climate change:
ice_scale_plot <- ice_scale/(-25)  # plot ln-scale in response per +30% ice

coefs <- dimnames(m)[[2]]

# coefficients for species interactions - including autoregressive terms
sp.int <- list() ; length(sp.int) <- 5
for(i in 1:5){
  index <- c(1:5)
  int.i.full <- paste("c",i,index,sep="")
  int.i.used <- int.i.full[is.element(int.i.full,coefs)]
  sp.int[[i]] <- int.i.used
}

# species interactions - numbers 1-5
sp.int.no <- list() ; length(sp.int.no) <- 5
for(i in 1:5){
  sp.int.no[[i]] <- as.numeric(unlist(lapply(strsplit(sp.int[[i]],""),
    function(x)x[[3]])))
}

# array of delayed climate effects at lags 0 to lag.max
lag.max <- 10
climeff <- array(dim=c(dim(m)[1], 5, lag.max + 1))

# lag 0 climate effects
climeff[,1,1] <- m[,"c16"]
climeff[,2,1] <- m[,"c26"]
climeff[,3,1] <- m[,"c36"]
climeff[,4,1] <- m[,"c46"]
climeff[,5,1] <- m[,"c56"]

# lagged climate effects
for(i in 1:lag.max){
  lag.i <- i
  climeff[,1,lag.i + 1] <- apply(m[,sp.int[[1]]] * 
                        climeff[,sp.int.no[[1]],lag.i],1,sum) +
                        climeff[,1,1]
  climeff[,2,lag.i + 1] <- apply(m[,sp.int[[2]]] * 
                        climeff[,sp.int.no[[2]],lag.i],1,sum) +
                        climeff[,2,1]
  climeff[,3,lag.i + 1] <- apply(m[,sp.int[[3]]] * 
                        climeff[,sp.int.no[[3]],lag.i],1,sum) +
                        climeff[,3,1]
  climeff[,4,lag.i + 1] <- apply(m[,sp.int[[4]]] * 
                        climeff[,sp.int.no[[4]],lag.i],1,sum) +
                        climeff[,4,1]
  climeff[,5,lag.i + 1] <- apply(m[,sp.int[[5]]] * 
                        climeff[,sp.int.no[[5]],lag.i],1,sum) +
                        climeff[,5,1]
}


# plotting lagged climate effects
for(j in 1:5){
ylims <- c(-1,1)
if(j==1) {ylims <- c(-3,1); yat <- c(-10:10)}      
if(j==2) {ylims <- c(-5,2); yat <- c(-10:10)*2}  
if(j==3) {ylims <- c(-2.5,1.5); yat <- c(-10:10)}    
if(j==4) {ylims <- c(-3,18); yat <- c(-10:10)*4}  
if(j==5) {ylims <- c(-7,4.5); yat <- c(-10:10)*2}
scale.y <- c(cop_scale,krill_scale,amph_scale,cap_scale,pc_scale)[j]

yfit <- apply(climeff[,j,],2,mean)*scale.y/ice_scale_plot
ci1 <- apply(climeff[,j,],2,function(x)quantile(x,0.025))*scale.y/ice_scale_plot
ci2 <- apply(climeff[,j,],2,function(x)quantile(x,0.975))*scale.y/ice_scale_plot
ci1in <- apply(climeff[,j,],2,function(x)quantile(x,0.05))*scale.y/ice_scale_plot
ci2in <- apply(climeff[,j,],2,function(x)quantile(x,0.95))*scale.y/ice_scale_plot
ci1inn <- apply(climeff[,j,],2,function(x)quantile(x,0.1))*scale.y/ice_scale_plot
ci2inn <- apply(climeff[,j,],2,function(x)quantile(x,0.9))*scale.y/ice_scale_plot
xval <- c(0:lag.max)

plot(0,0,xlim=c(0,lag.max),ylim=ylims,xlab="",ylab="",axes=F,type="n")
polygon(x=c(xval,reverse(xval)),y=c(ci1,reverse(ci2)),col="grey90",border=NA)
polygon(x=c(xval,reverse(xval)),y=c(ci1in,reverse(ci2in)),col="grey75",border=NA)
polygon(x=c(xval,reverse(xval)),y=c(ci1inn,reverse(ci2inn)),col="grey60",border=NA)
lines(x=xval, y=yfit, pch=16)
points(x=xval, y=yfit, pch=16)

abline(h=0,lty=3)
axis(side=2,at=ylims*2,tcl=0,lab=F)
axis(side=2,at=yat)
var.j <- c("Cop","Krill","Amph","Cap","Pol",cex=.8)[j] 
}
axis(side=1,at=c(0,lag.max*2))
axis(side=1)
mtext(side=1, line = 3, at =lag.max/2, text = c("Time lag (years)"),cex=1)
mtext(side=1, line=5, at=lag.max/2, outer=F,adj=.5,"Climate effect",font=2)
mtext(side=3,line=0,outer=T,adj=0.02,"A")
mtext(side=3,line=0,outer=T,adj=0.52,"B")
dev.off()


############### Plot of lagged versus nonlagged variables:
### Is the linear assumption of the Gompertz model appropriate?
X11(height=12/2.54,width=8.5/2.54,pointsize=10)
pdf(height=12/2.54,width=8.5/2.54,pointsize=10,file="FigS2.pdf")

par(mfcol=c(3,2),mai=c(.5,.5,.1,.1),omi=c(.1,.1,.1,0))

for(i in 1:5){
  y <- list(
    cop_obs * cop_scale + cop_offset,
    krill_obs * krill_scale + krill_offset,
    amph_obs * amph_scale + amph_offset,
    cap_obs * cap_scale + cap_offset,
    pc_obs * pc_scale + pc_offset)[[i]]
  ynam <- c("Cop","Krill","Amph","Cap","Pol")[i]
  
  plot(lag1(y),y,
    xlim=range(y,na.rm=T),ylim=range(y,na.rm=T),
    pch=16,ylab="",xlab="",main="",axes=F)
  box()
  axis(1)
  axis(2)
  mtext(side=1,bquote(italic(.(ynam)[T-1])),line=3)
  mtext(side=2,bquote(italic(.(ynam)[T])),line=3)
  }              
dev.off()


############## Which parameters are correlated?
allvar <- names(fit)
varselect <- allvar[c(1:34,60:64,246:249,252:254,258:259,264)]
varnam <- varselect
varnam[is.element(varnam,paste("L_sigma[",1:5,"]",sep=""))] <- 
  paste("tau",1:5,sep="")
varnam[is.element(varnam,paste("s",1:5,"o",sep=""))] <- 
  paste("sigma",1:5,sep="")
rthd <- 0.4
rtab <- NULL
for(i in 1:(length(varselect)-1)){
  for(j in (i+1):length(varselect)){
    rval <- cor(m[,varselect[i]],m[,varselect[j]])
    if(abs(rval) > rthd){
      rtab.i <- data.frame(v1=varselect[i],v2=varselect[j],
        v1nam=varnam[i],v2nam=varnam[j],
        r = round(rval,2))
      rtab <- rbind.data.frame(rtab,rtab.i)
      }
  }
}
rtab[,3:5]
#    v1nam      v2nam     r
#1     c11        c13 -0.55
#2     c11        c14  0.74
#3     c11        c15 -0.70
#4     c13        c15  0.71
#5     c22        c24  0.62
#6     c22        c26  0.43
#7     c24        c26  0.48
#8     c31        c33 -0.67
#9     c31        c35 -0.65
#10    c33        c35  0.72
#11    c33        c53 -0.49
#12    c33        c55 -0.51
#13    c33       tau5  0.43
#14    c35     sigma5 -0.50
#15    c35       tau3  0.50
#16    c35       tau5  0.49
#17    c41        c42 -0.52
#18    c41        c47  0.51
#19    c42        c44  0.46
#20    c42        c47 -0.76
#21    c42       tau4 -0.53
#22    c47       tau4  0.44
#23    c47 Omega[4,2] -0.46
#24    c51        c53 -0.67
#25    c51        c55 -0.76
#26    c53        c55  0.78
#27    c53        c57  0.46
#28    c55       tau5 -0.54
#29    c56        c57  0.41
#30    c57 Omega[5,3]  0.41
#31 sigma5       tau3 -0.41
#32 sigma5       tau5 -0.52

X11(height=16/2.54,width=16/2.54,pointsize=10)
pdf(height=16/2.54,width=16/2.54,pointsize=10,file="FigS4.pdf")
#jpeg(height=16/2.54,width=16/2.54,pointsize=10,file="FigS3.jpg",units="in",res=1200)
par(mfrow=c(6,6),mai=c(.4,.4,.2,.1),omi=c(.35,.35,0,0))
for(i in 1:(dim(rtab)[1])){
  plot(m[,as.character(rtab$v1[i])],
       m[,as.character(rtab$v2[i])],
       pch=16,xlab="",ylab="",main="",cex=.1)
  abline(h=0,lty=2,col="blue",lwd=1)
  abline(v=0,lty=2,col="blue",lwd=1)
  box()
  nam1 <- as.character(rtab$v1nam[i])
  nam2 <- as.character(rtab$v2nam[i])
  nam.expr <- function(nam){
  nam.str <- unlist(strsplit(nam,""))
  if(nam.str[1]=="c"){nam.expr <- bquote(
    italic("c"[.(paste(nam.str[2:3],sep="",collapse=""))]))}
  if(nam.str[1]=="s"){nam.expr <- bquote(italic(sigma[.(nam.str[6])]))}
  if(nam.str[1]=="t"){nam.expr <- bquote(italic(tau[.(nam.str[4])]))}
  if(nam.str[1]=="O"){nam.expr <- bquote(italic(paste(
    Omega,.(paste(nam.str[6:10],sep="",collapse="")))))}
  nam.expr
  }
  nam1.expr <- nam.expr(nam1)
  nam2.expr <- nam.expr(nam2)
  mtext(side=1,nam1.expr,line=2.2)
  mtext(side=2,nam2.expr,line=2.2)
  mtext(side=3,bquote(r==.(rtab$r[i])),cex=.8,adj=.95,line=.2)
}
mtext(side=1,outer=T,"Coefficient (normalised scale)",line=1)
mtext(side=2,outer=T,"Coefficient (normalised scale)",line=1)
dev.off()

par(mfrow=c(5,2))
myhist <- function(y,nam){
  hist(y,main=nam, xlab="")
  abline(v=quantile(y,c(.025,.975)),col="blue")
  abline(v=quantile(y,c(.5)),col="blue",lwd=2)
  abline(v=0,col="red",lty=2)}
myhist(m[,"c13"],"c13")
myhist(m[,"c13"][m[,"c11"]>0],"c13 [c11>0]")
myhist(m[,"c14"],"c14")
myhist(m[,"c14"][m[,"c11"]>0],"c14 [c11>0]")
myhist(m[,"c15"],"c15")
myhist(m[,"c15"][m[,"c11"]>0],"c15 [c11>0]")
myhist(m[,"c31"],"c31")
myhist(m[,"c31"][m[,"c33"]>0],"c31 [c33>0]")
myhist(m[,"c35"],"c35")
myhist(m[,"c35"][m[,"c33"]>0],"c35 [c33>0]")
