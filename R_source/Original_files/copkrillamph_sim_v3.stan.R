rm(list=ls(all=TRUE))

# Set work directory

setwd("~/Documents/Dokumenter/PhD/Gompertz")
#setwd("D:/Leif_Oslo2/R Barents Sea/Skripter/STAN/Zoocomp")

# Add location of stan library to library path
.libPaths(new="~/Documents/Dokumenter/PhD/Gompertz")


library("rstan") # observe startup messages

#load("codkrillamph_stanmod_v3_new.stanfit")
load("codkrillamph_stanmod_v3_new_26112018.stanfit")


m <- as.matrix(fit)                 # Gjør om "fit" til matrise (dim(m))
#plot(m[,1])                        # Plotter alle verdiene som eks. C11 kan ha
la <- extract(fit, permuted = TRUE) # return a list of arrays 
la$cop_latent <- la$Latent[,,1]
la$krill_latent <- la$Latent[,,2]
la$amph_latent <- la$Latent[,,3]
la$cap_latent <- la$Latent[,,4]
la$pc_latent <- la$Latent[,,5]


#plot(la$pc_latent[1,],type="l")  # Plotter første tidssterie [1,] av capelin
#lines(la$pc_latent[2,],type="l", col="red") 
#lines(la$pc_latent[1000,],type="l", col="blue",lty=3) 
#for(i in 1:2000){lines(la$pc_latent[i,],col="grey")}


# Also run start of call file to make data series  ###########
load(file = "zoo_dat.rdata")

D <- dim(m)[1] # number of posterior samples  ex. 2000 samples
N <- zoo_dat$N # number of estimated years    ex. zoo data in 35 years simulation
K <- zoo_dat$K # number of species            ex. 5 species

## 1. Find process errors (PE)
#Første simulering er kun for å finne process error så den er lagret for de neste simuleringene


Latent.null <- array(dim=c(N, K, D))   # Latent values no perturbations
PE <- array(dim=c(N-1, K, D))     # Process errors

for(d in 1:D){                 # d <- 1:2000

Latent <- array(dim=c(N, K))   # Lager tom array (Latent) med dimensjon N og K

# Parametere fra 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods

  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill

  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods

  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin

  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod

# Initial values (Latent[1,1]= første verdi for første art, initial value)

  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]

  for(n in 2:N){ # n <- 2 siden år 1 allerede er gjort(initial, T-1)                             
      # Expected:
      Mu <- rep(NA,K)     
      # cop:
      Mu[1] = c10 +
             c11 * Latent[n-1, 1] +   #Ligning 1: C11 * X1,T-1
             c13 * Latent[n-1, 3] + 
             c14 * Latent[n-1, 4] +
             c15 * Latent[n-1, 5] +
             c16 * ice[n] 
      # krill:
      Mu[2] = c20 +
             c22 * Latent[n-1, 2] + 
             c24 * Latent[n-1, 4] + 
             c26 * ice[n]  
      # amph:
      Mu[3] = c30 +
             c31 * Latent[n-1, 1] +
             c33 * Latent[n-1, 3] +
             c35 * Latent[n-1, 5] +
             c36 * ice[n] 
      # cap:
      Mu[4] = c40 +
             c41 * Latent[n-1, 1] +
             c42 * Latent[n-1, 2] +
             c44 * Latent[n-1, 4] +
             c46 * ice[n] +    
             c47 * cod[n] +
             c48 * her[n] +
             c49 * cap_F[n] 
      # pc:
      Mu[5] = c50 +
             c51 * Latent[n-1, 1] +
             c53 * Latent[n-1, 3] +
             c55 * Latent[n-1, 5] +
             c56 * ice[n] +  
             c57 * cod[n] 
      # Process error [i] = Latent[i+1] - Expected[i+1]:
      # Latent = expected (likning minus prosessfeil) + prosess error                   
      #Det finnes 2000 prosessfeil for hver art (5) for hvert tidssteg (36-1)
      #Prosessfeilen skal ikke forandre seg, derfor tar vi den førs ut og så setter vi den tilbake igjen her (etter man har gjort endringene man ønsker)
      
      PE[n-1,,d] <- m[d,paste("Latent[",n,",",1:K,"]",sep="")] - Mu  
      Latent[n,] <- Mu + PE[n-1,,d]   #PE = process error
      }
  Latent.null[,,d] <- Latent
}

#EKS: PE[10,2,1005]
  

## 2. Recreate posterior distributions of state variables

Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations

for(d in 1:D){ # d <- 1

Latent <- array(dim=c(N, K))   # Latent values

# parameters 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods

  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill

  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods

  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin

  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod

# Initial values

  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]

  for(n in 2:N){ # n <- 2                               
      # Expected:
      Mu <- rep(NA,K)   
      # cop:
      Mu[1] = c10 +
             c11 * Latent[n-1, 1] +
             c13 * Latent[n-1, 3] + 
             c14 * Latent[n-1, 4] +
             c15 * Latent[n-1, 5] +
             c16 * ice[n] 
      # krill:
      Mu[2] = c20 +
             c22 * Latent[n-1, 2] + 
             c24 * Latent[n-1, 4] + 
             c26 * ice[n]  
      # amph:
      Mu[3] = c30 +
             c31 * Latent[n-1, 1] +
             c33 * Latent[n-1, 3] +
             c35 * Latent[n-1, 5] +
             c36 * ice[n] 
      # cap:
      Mu[4] = c40 +
             c41 * Latent[n-1, 1] +
             c42 * Latent[n-1, 2] +
             c44 * Latent[n-1, 4] +
             c46 * ice[n] +    
             c47 * cod[n] +
             c48 * her[n] +
             c49 * cap_F[n] 
      # pc:
      Mu[5] = c50 +
             c51 * Latent[n-1, 1] +
             c53 * Latent[n-1, 3] +
             c55 * Latent[n-1, 5] +
             c56 * ice[n] +  
             c57 * cod[n] 
      # Latent[i] = Expected[i] + Process error [i-1] :
      Latent[n,] <- Mu + PE[n-1,,d]
      }
  Latent.sim[,,d] <- Latent
  }

identical(Latent.null,Latent.sim)  # Teste at alt er gjort riktig, hvis TRUE -> Riktig


### Plot time series
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

#X11(height=7,width=7,pointsize=10)
par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0))
ylims <- c(-4,4) 

#Plot copepod
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)

#Plot krill
yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=5,font=2)
mtext(side=2,expression(paste("(ln(g m"^-3,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("KRILL")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,2,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,2,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,2,],1,CI2),col="black",lty=3)

#Plot amphipod
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + kg nm"^-1,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("AMPH")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,3,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,3,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,3,],1,CI2),col="black",lty=3)

#Plot capelin
yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("CAP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,4,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,4,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,4,],1,CI2),col="black",lty=3)

#Plot polar cod
yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("POLCOD")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,5,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,5,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,5,],1,CI2),col="black",lty=3)


## 3. Simulate loss of sea ice in year 19

ice.sim <- ice           
ice.sim[19] <- min(ice)  # I år 19 setter vi verdien av ice til den minste mulige verdien i år 19

# Setter til min(ice) fordi det ikke er sannsynlig at isen blir mindre enn noen gang observert

Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations

for(d in 1:D){ # d <- 1

Latent <- array(dim=c(N, K))   # Latent values

# parameters 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods

  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill

  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods

  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin

  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod

# Initial values

  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]

  for(n in 2:N){ # n <- 2                               
      # Expected:
      Mu <- rep(NA,K)   
      # cop:
      Mu[1] = c10 +
             c11 * Latent[n-1, 1] +
             c13 * Latent[n-1, 3] + 
             c14 * Latent[n-1, 4] +
             c15 * Latent[n-1, 5] +
             c16 * ice.sim[n] 
      # krill:
      Mu[2] = c20 +
             c22 * Latent[n-1, 2] + 
             c24 * Latent[n-1, 4] + 
             c26 * ice.sim[n]  
      # amph:
      Mu[3] = c30 +
             c31 * Latent[n-1, 1] +
             c33 * Latent[n-1, 3] +
             c35 * Latent[n-1, 5] +
             c36 * ice.sim[n] 
      # cap:
      Mu[4] = c40 +
             c41 * Latent[n-1, 1] +
             c42 * Latent[n-1, 2] +
             c44 * Latent[n-1, 4] +
             c46 * ice.sim[n] +    
             c47 * cod[n] +
             c48 * her[n] +
             c49 * cap_F[n] 
      # pc:
      Mu[5] = c50 +
             c51 * Latent[n-1, 1] +
             c53 * Latent[n-1, 3] +
             c55 * Latent[n-1, 5] +
             c56 * ice.sim[n] +  
             c57 * cod[n] 
      # Latent[i] = Expected[i] + Process error [i-1] :
      Latent[n,] <- Mu + PE[n-1,,d]
      }
  Latent.sim[,,d] <- Latent
  }


### Plot time series
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

X11(height=7,width=7,pointsize=10)
par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0))
ylims <- c(-4,4) 
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=5,font=2)
mtext(side=2,expression(paste("(ln(g m"^-3,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("KRILL")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,2,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,2,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,2,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + kg nm"^-1,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("AMPH")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,3,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,3,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,3,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("CAP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,4,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,4,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,4,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("POLCOD")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,5,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,5,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,5,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)

## 3. Simulate loss of sea ice in year 19

ice.sim <- ice           
ice.sim[19] <- min(ice)  # I år 19 setter vi verdien av ice til den minste mulige verdien i år 19

# Setter til min(ice) fordi det ikke er sannsynlig at isen blir mindre enn noen gang observert

Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations

for(d in 1:D){ # d <- 1
  
  Latent <- array(dim=c(N, K))   # Latent values
  
  # parameters 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods
  
  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill
  
  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods
  
  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin
  
  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod
  
  # Initial values
  
  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]
  
  for(n in 2:N){ # n <- 2                               
    # Expected:
    Mu <- rep(NA,K)   
    # cop:
    Mu[1] = c10 +
      c11 * Latent[n-1, 1] +
      c13 * Latent[n-1, 3] + 
      c14 * Latent[n-1, 4] +
      c15 * Latent[n-1, 5] +
      c16 * ice.sim[n] 
    # krill:
    Mu[2] = c20 +
      c22 * Latent[n-1, 2] + 
      c24 * Latent[n-1, 4] + 
      c26 * ice.sim[n]  
    # amph:
    Mu[3] = c30 +
      c31 * Latent[n-1, 1] +
      c33 * Latent[n-1, 3] +
      c35 * Latent[n-1, 5] +
      c36 * ice.sim[n] 
    # cap:
    Mu[4] = c40 +
      c41 * Latent[n-1, 1] +
      c42 * Latent[n-1, 2] +
      c44 * Latent[n-1, 4] +
      c46 * ice.sim[n] +    
      c47 * cod[n] +
      c48 * her[n] +
      c49 * cap_F[n] 
    # pc:
    Mu[5] = c50 +
      c51 * Latent[n-1, 1] +
      c53 * Latent[n-1, 3] +
      c55 * Latent[n-1, 5] +
      c56 * ice.sim[n] +  
      c57 * cod[n] 
    # Latent[i] = Expected[i] + Process error [i-1] :
    Latent[n,] <- Mu + PE[n-1,,d]
  }
  Latent.sim[,,d] <- Latent
}


### Plot time series
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

X11(height=7,width=7,pointsize=10)
par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0))
ylims <- c(-4,4) 
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=5,font=2)
mtext(side=2,expression(paste("(ln(g m"^-3,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("KRILL")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,2,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,2,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,2,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + kg nm"^-1,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("AMPH")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,3,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,3,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,3,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("CAP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,4,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,4,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,4,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("POLCOD")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,5,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,5,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,5,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)

## 4. Simulate strong cod population in year 17

cod.sim <- cod           
cod.sim[15:36] <- max(cod)  # I år 19 setter vi verdien av cod til den minste mulige verdien i år 19

# Setter til max(cod) fordi det ikke er sannsynlig at cod blir større enn noen gang observert

Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations

for(d in 1:D){ # d <- 1
  
  Latent <- array(dim=c(N, K))   # Latent values
  
  # parameters 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods
  
  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill
  
  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods
  
  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin
  
  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod
  
  # Initial values
  
  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]
  
  for(n in 2:N){ # n <- 2                               
    # Expected:
    Mu <- rep(NA,K)   
    # cop:
    Mu[1] = c10 +
      c11 * Latent[n-1, 1] +
      c13 * Latent[n-1, 3] + 
      c14 * Latent[n-1, 4] +
      c15 * Latent[n-1, 5] +
      c16 * ice[n] 
    # krill:
    Mu[2] = c20 +
      c22 * Latent[n-1, 2] + 
      c24 * Latent[n-1, 4] + 
      c26 * ice[n]  
    # amph:
    Mu[3] = c30 +
      c31 * Latent[n-1, 1] +
      c33 * Latent[n-1, 3] +
      c35 * Latent[n-1, 5] +
      c36 * ice[n] 
    # cap:
    Mu[4] = c40 +
      c41 * Latent[n-1, 1] +
      c42 * Latent[n-1, 2] +
      c44 * Latent[n-1, 4] +
      c46 * ice[n] +    
      c47 * cod.sim[n] +
      c48 * her[n] +
      c49 * cap_F[n] 
    # pc:
    Mu[5] = c50 +
      c51 * Latent[n-1, 1] +
      c53 * Latent[n-1, 3] +
      c55 * Latent[n-1, 5] +
      c56 * ice[n] +  
      c57 * cod.sim[n] 
    # Latent[i] = Expected[i] + Process error [i-1] :
    Latent[n,] <- Mu + PE[n-1,,d]
  }
  Latent.sim[,,d] <- Latent
}


### Plot time series
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

#X11(height=7,width=7,pointsize=10)
par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0))
ylims <- c(-4,4) 
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=5,font=2)
mtext(side=2,expression(paste("(ln(g m"^-3,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("KRILL")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,2,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,2,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,2,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + kg nm"^-1,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("AMPH")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,3,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,3,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,3,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("CAP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,4,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,4,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,4,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("POLCOD")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,5,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,5,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,5,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)

## 4. Simulate strong cod population in year 17
#cod.at=read.table(at.outout.txt)
#cod.sim <- cod.at           
cod.sim[1:36] <- max(cod)  # I år 19 setter vi verdien av cod til den minste mulige verdien i år 19
cod.sim[1:36] <- quantile(cod, probs=0.9)  #quantile(cod, probs=0.9)
cod.sim[1:36] <- quantile(cod, probs=0.75) #quantile(cod, probs=0.75)
cod.sim[1:36] <- quantile(cod, probs=0.5)   #Gjennomsnitt quantile(cod, probs=0.5)
cod.sim[1:36] <- quantile(cod, probs=0.25)  #quantile(cod, probs=0.25)
cod.sim[1:36] <- quantile(cod, probs=0.1)  #quantile(cod, probs=0.1)
# Setter til max(cod) fordi det ikke er sannsynlig at cod blir større enn noen gang observert

Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations

for(d in 1:D){ # d <- 1
  
  Latent <- array(dim=c(N, K))   # Latent values
  
  # parameters 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods
  
  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill
  
  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods
  
  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin
  
  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod
  
  # Initial values
  
  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]
  
  for(n in 2:N){ # n <- 2                               
    # Expected:
    Mu <- rep(NA,K)   
    # cop:
    Mu[1] = c10 +
      c11 * Latent[n-1, 1] +
      c13 * Latent[n-1, 3] + 
      c14 * Latent[n-1, 4] +
      c15 * Latent[n-1, 5] +
      c16 * ice[n] 
    # krill:
    Mu[2] = c20 +
      c22 * Latent[n-1, 2] + 
      c24 * Latent[n-1, 4] + 
      c26 * ice[n]  
    # amph:
    Mu[3] = c30 +
      c31 * Latent[n-1, 1] +
      c33 * Latent[n-1, 3] +
      c35 * Latent[n-1, 5] +
      c36 * ice[n] 
    # cap:
    Mu[4] = c40 +
      c41 * Latent[n-1, 1] +
      c42 * Latent[n-1, 2] +
      c44 * Latent[n-1, 4] +
      c46 * ice[n] +    
      c47 * cod.sim[n] +
      c48 * her[n] +
      c49 * cap_F[n] 
    # pc:
    Mu[5] = c50 +
      c51 * Latent[n-1, 1] +
      c53 * Latent[n-1, 3] +
      c55 * Latent[n-1, 5] +
      c56 * ice[n] +  
      c57 * cod.sim[n] 
    # Latent[i] = Expected[i] + Process error [i-1] :
    Latent[n,] <- Mu + PE[n-1,,d]
  }
  Latent.sim[,,d] <- Latent
}


### Plot time series
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

#X11(height=7,width=7,pointsize=10)
par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0))
ylims <- c(-4,4) 
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=5,font=2)
mtext(side=2,expression(paste("(ln(g m"^-3,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("KRILL")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,2,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,2,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,2,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + kg nm"^-1,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("AMPH")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,3,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,3,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,3,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("CAP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,4,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,4,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,4,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("POLCOD")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,5,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,5,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,5,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)


## 5. Simulate polar cod fishery

fish.pol <- array(NA,dim=36)  #Make array
fish.pol[1:36]=0.8            # 20 % red every yesr
fish.pol[12]=0.3              # 70 % reduvtion in PCO one year
#fish.pol <-replicate(36, 0.8)

# Setter til max(cod) fordi det ikke er sannsynlig at cod blir større enn noen gang observert

Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations

for(d in 1:D){ # d <- 1
  
  Latent <- array(dim=c(N, K))   # Latent values
  
  # parameters 
  c10 <- m[d,"c10"] # intercept copepods
  c11 <- m[d,"c11"] # autoregressive parameter copepods
  c13 <- m[d,"c13"] # amphipod effect on copepods
  c14 <- m[d,"c14"] # capelin effect on copepods
  c15 <- m[d,"c15"] # polar cod effect on copepods
  c16 <- m[d,"c16"] # ice effect on copepods
  
  c20 <- m[d,"c20"] # intercept krill
  c22 <- m[d,"c22"] # autoregressive parameter krill
  c24 <- m[d,"c24"] # capelin effect on krill
  c26 <- m[d,"c26"] # ice effect on krill
  
  c30 <- m[d,"c30"] # intercept amphipods
  c31 <- m[d,"c31"] # copepod effect on amphipods
  c33 <- m[d,"c33"] # autoregressive parameter amphipods
  c35 <- m[d,"c35"] # polar cod effect on amphipods
  c36 <- m[d,"c36"] # ice effect on amphipods
  
  c40 <- m[d,"c40"] # intercept capelin
  c41 <- m[d,"c41"] # copepod effect on capelin
  c42 <- m[d,"c42"] # krill effect on capelin
  c44 <- m[d,"c44"] # autoregressive parameter capelin
  c46 <- m[d,"c46"] # ice effect on capelin
  c47 <- m[d,"c47"] # cod effect on capelin
  c48 <- m[d,"c48"] # herring effect on capelin
  c49 <- m[d,"c49"] # fishing on capelin
  
  c50 <- m[d,"c50"] # intercept polar cod
  c51 <- m[d,"c51"] # copepod effect on polar cod
  c53 <- m[d,"c53"] # amphipod effect on polar cod
  c55 <- m[d,"c55"] # autoregressive parameter polar cod
  c56 <- m[d,"c56"] # ice effect on polar cod
  c57 <- m[d,"c57"] # cod effect on polar cod
  
  # Initial values
  
  Latent[1,1] <- m[d,"Latent[1,1]"]
  Latent[1,2] <- m[d,"Latent[1,2]"]
  Latent[1,3] <- m[d,"Latent[1,3]"]
  Latent[1,4] <- m[d,"Latent[1,4]"]
  Latent[1,5] <- m[d,"Latent[1,5]"]
  
  
  for(n in 2:N){ # n <- 2                               
    # Expected:
    Mu <- rep(NA,K)   
    # cop:
    Mu[1] = c10 +
      c11 * Latent[n-1, 1] +
      c13 * Latent[n-1, 3] + 
      c14 * Latent[n-1, 4] +
      c15 * Latent[n-1, 5] +
      c16 * ice[n] 
    # krill:
    Mu[2] = c20 +
      c22 * Latent[n-1, 2] + 
      c24 * Latent[n-1, 4] + 
      c26 * ice[n]  
    # amph:
    Mu[3] = c30 +
      c31 * Latent[n-1, 1] +
      c33 * Latent[n-1, 3] +
      c35 * Latent[n-1, 5] +
      c36 * ice[n] 
    # cap:
    Mu[4] = c40 +
      c41 * Latent[n-1, 1] +
      c42 * Latent[n-1, 2] +
      c44 * Latent[n-1, 4] +
      c46 * ice[n] +    
      c47 * cod[n] +
      c48 * her[n] +
      c49 * cap_F[n] 
    # pc:
    Mu[5] = c50 +
      c51 * Latent[n-1, 1] +
      c53 * Latent[n-1, 3] +
      c55 * Latent[n-1, 5] + #Reduserer polar cod med 20 % hvert år
      log(fish.pol[n-1]) +  # Fishery
      c56 * ice[n] +  
      c57 * cod[n] 
    # Latent[i] = Expected[i] + Process error [i-1] :
    Latent[n,] <- Mu + PE[n-1,,d]
  }
  Latent.sim[,,d] <- Latent
}


### Plot time series
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

#X11(height=7,width=7,pointsize=10)
par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0))
ylims <- c(-4,4) 
yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Copepods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - krill_offset)/krill_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Krill",line=5,font=2)
mtext(side=2,expression(paste("(ln(g m"^-3,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("KRILL")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$krill_latent,2,CI1),reverse(apply(la$krill_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$krill_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,2,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,2,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,2,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - amph_offset)/amph_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Amphipods",line=5,font=2)
mtext(side=2,expression(paste("(ln(1 + kg nm"^-1,")",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("AMPH")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$amph_latent,2,CI1),reverse(apply(la$amph_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$amph_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,3,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,3,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,3,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - cap_offset)/cap_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Capelin",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("CAP")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cap_latent,2,CI1),reverse(apply(la$cap_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$cap_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,4,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,4,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,4,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

yat <- c(-10:10)*2
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F)
axis(side=2, at=(yat - pc_offset)/pc_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=2,"Polar cod",line=5,font=2)
mtext(side=2,expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("POLCOD")),line=2.5)
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$pc_latent,2,CI1),reverse(apply(la$pc_latent,2,CI2))),
        border=NA,col="grey")
lines(yrs,apply(la$pc_latent,2,mean),col="blue",pch=3)         
lines(yrs,apply(Latent.sim[,5,],1,mean),col="black")
lines(yrs,apply(Latent.sim[,5,],1,CI1),col="black",lty=3)
lines(yrs,apply(Latent.sim[,5,],1,CI2),col="black",lty=3)
abline(v=yrs[1]+17)

axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)


