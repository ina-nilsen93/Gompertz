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

######
# Ina: Taking out the middle part here, see original script

########
BS.data <- read.table(file="~/Documents/Dokumenter/PhD/Gompertz/BS.data.txt",dec=".",sep="")



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

save(zoo_dat, file = "Data/zoo_dat.rdata")
#x <-load(file = "zoo_dat.rdata")

#plot(zoo_dat$ice*ice_scale+ice_offset,type="l")
#plot(zoo_dat$ice, type="l")

###################################
###################################
###################################
###################################
