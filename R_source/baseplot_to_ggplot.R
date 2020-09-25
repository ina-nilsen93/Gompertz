# Convert plotting from baseplot to ggplot
library("tidyverse")
#Latent.sim[, 1, ] = all 2000 runs over 36 years for species 1 = copepod 

BASE <- (1:5) %>% 
  set_names(c("Copepod","Krill","Amphipods","Capelin","Polar cod")) %>% 
  map_df(~{Latent.base[, .x, ] %>% t() %>% as.data.frame() %>% as_tibble() %>% 
        pivot_longer(everything(),names_to = "Year", values_to = "Biomass", names_prefix = "V", names_ptypes = list(Year = integer()))}
, .id="Species") %>% 
  group_by(Species, Year) %>% 
  summarize(Mean=mean(Biomass), Q1=quantile(Biomass,0.025),Q2=quantile(Biomass,0.975))

SIM <- (1:5) %>% 
  set_names(c("Copepod","Krill","Amphipods","Capelin","Polar cod")) %>% 
  map_df(~{Latent.sim[, .x, ] %>% t() %>% as.data.frame() %>% as_tibble() %>% 
      pivot_longer(everything(),names_to = "Year", values_to = "Biomass", names_prefix = "V", names_ptypes = list(Year = integer()))}
      , .id="Species") %>% 
  group_by(Species, Year) %>% 
  summarize(Mean=median(Biomass), Q1=quantile(Biomass,0.025),Q2=quantile(Biomass,0.975))

df <- bind_rows(`00_Base_run` = BASE,
                `01_Strong_cod_cohort` = SIM,
                .id="Runs")

#example("geom_ribbon")

plotB <-df %>% 
        filter(Species %in% c("Copepod","Krill","Amphipods","Capelin","Polar cod")) %>% 
        ggplot(aes(x=Year, y=Mean, ymin=Q1, ymax=Q2, fill=Runs, colour=Runs)) + 
        geom_ribbon(alpha=.3)+
        geom_line()+
        labs(x="Year", y="Biomass", title="", colour="Runs") +
        theme_minimal()+
        facet_wrap(~Species, scales = "free_y",ncol = 1,)
plotB

# Baseplot
CI1 <- function(x)quantile(x,.025)
CI2 <- function(x)quantile(x,.975)
reverse <- function(x)x[(length(x):1)]

par(mfrow=c(5,1),mai=c(.1,.7,.1,.7),omi=c(.6,0,0,0)) #
ylims <- c(-4,4) 

#copepod
# uses: 
yat <- c(-10:10)               #yat= Optional y axis tick positions.
plot(0,0,xlim=range(yrs),ylim=ylims,xlab="",ylab="",axes=F) #Clears plot?
axis(side=2, at=(yat - cop_offset)/cop_scale,lab=yat,mgp=c(0,.5,0),tcl=-.3,las=1) #
mtext(side=2,"Copepods",line=5,font=2)                                   # Text lab_y left
mtext(side=2,expression(paste("(ln(1 + g m"^-2,")",sep="")),line=2.5)    # Text lab_y left
axis(side=4, at=c(-10:10)*2,mgp=c(0,.5,0),tcl=-.3,las=1)
mtext(side=4,expression(italic("COP")),line=2.5)                         # Text lab_y right
mtext(side=4,expression(paste("(s.d. units)",sep="")),line=5)            # Text lab_y right
polygon(x=c(yrs,reverse(yrs)),
        y=c(apply(la$cop_latent,2,CI1),reverse(apply(la$cop_latent,2,CI2))),
        border=NA,col="grey")                                              #Make 95 % confidence
lines(yrs,apply(la$cop_latent,2,mean),col="blue",pch=3)         # Blue line mean in base run
lines(yrs,apply(Latent.sim[,1,],1,mean),col="black")            # Black line mean of simulated
lines(yrs,apply(Latent.sim[,1,],1,CI1),col="black",lty=3)       #Apply 
lines(yrs,apply(Latent.sim[,1,],1,CI2),col="black",lty=3)       # Apply
abline(v=yrs[1]+23)

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
abline(v=yrs[1]+23)

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
abline(v=yrs[1]+23)

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
abline(v=yrs[1]+23)

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
abline(v=yrs[1]+23)

axis(side=1,las=-.3,pos=-5.5)
mtext(side=1,"Year",line=5,font=2)

