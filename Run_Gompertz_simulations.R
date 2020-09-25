
#library("rstan")    # observe startup messages
library("tidyverse") 
library("conflicted")
conflict_prefer("filter", "dplyr")


source("R_source/Gompertz_model.R")
source("R_source/function(run_Gompertz).R")

BASE_data <- tidy_Gompertz(Latent.base)

BASE <- BASE_data %>% 
  group_by(Species, Year) %>% 
  summarize(Mean=median(Biomass), Q1=quantile(Biomass,0.025),Q2=quantile(Biomass,0.975)) %>% 
  mutate(Scenarios="Gompertz base")

sp.order <- c("Copepod","Krill","Amphipods","Capelin","Polar cod")

plotA <-BASE %>% 
  ungroup() %>% 
  mutate(Species = factor(Species, levels = sp.order)) %>%
  filter(Species %in% sp.order) %>% 
  ggplot(aes(x=Year, y=Mean, ymin=Q1, ymax=Q2, fill=Scenarios)) + 
  geom_ribbon(alpha=.4)+
  geom_line()+
  labs(x="Year", y="Biomass", title="", colour="Runs") +
  theme_minimal()+
  facet_wrap(~Species, scales = "free_y",ncol = 1,)
plotA

#### Convertion ##################################

Sd <- c(0.364,1.43,0.613,1.2,0.845,0.563)
Species <- c("Cop","Krill","Amphipods","Capelin", "Polar cod", "Cod")
sd <- data.frame(Species,Sd) %>% 
      as_tibble()

BS.data <- read.table("Data/BS.data.txt", dec=".", sep=" ", header=TRUE) %>% 
            as_tibble() %>% 
            select(Year,Cop,Krill,Amphipods,logCapBMtot, logPolCodBMtot, logCodBMtot) %>% 
            summarize_at(c("Cop","Krill","Amphipods","logCapBMtot", "logPolCodBMtot", "logCodBMtot"), mean, na.rm = TRUE) %>% 
            rename("Capelin"=logCapBMtot, "Polar cod" = logPolCodBMtot, "Cod" =logCodBMtot) %>% 
            gather(key=Species,value=Mean_b, Cop:Cod) %>% 
            left_join(sd)

#mutate(norm_b = (Biomass - mean)/sd)
#mutate(Biomass = (norm_b * sd) + mean)
            


###########################################################
###########################################################
## Simulation of cod
SIM_data <-list(Base_run = "normb_NCO_00.csv", 
     F_0.5 = "normb_NCO_01.csv", 
     F_2.0 = "normb_NCO_02.csv",
     F_0.9 = "normb_NCO_07.csv", 
     F_1.1 = "normb_NCO_08.csv") %>% 
    map(~paste0("Data/", .x)) %>% 
    map(read.table, dec=".", sep=",", header=TRUE) %>% 
    map(~run_Gompertz(zoo = zoo_dat, cod.sim = .x$norm_b)) %>% 
    map_df(tidy_Gompertz, .id="Scenarios")



######## Plot ##############################################

SIM <- SIM_data %>% 
  group_by(Scenarios, Species, Year) %>% 
  summarize(Mean=median(Biomass), Q1=quantile(Biomass,0.025),Q2=quantile(Biomass,0.975)) %>% 
  ungroup()

df_2 <- bind_rows(BASE,SIM) %>% 
        left_join(BS.data) %>% 
        mutate(Biomass= (Mean * Sd) + Mean_b) %>% 
   #     filter(Species=="Polar cod") %>% 
        mutate(Biomass2=exp(Biomass)*1000)


plotB <-df_2 %>%
  filter(Species %in% c("Copepod","Krill","Amphipods","Capelin","Polar cod")) %>% 
  ggplot(aes(x=Year, y=Biomass2,  fill=Scenarios, colour=Scenarios)) + 
 # geom_ribbon(alpha=.2, linetype=3)+
  geom_line(size=1)+
  labs(x="Year", y="Biomass", title="Cod scenario from Atlantis with Fx0.9", colour="Runs") +
  theme_minimal()+
  facet_wrap(~Species, scales = "free_y",ncol = 1,)
  #facet_grid(Species~Scenarios, scales = "free_y")
plotB

Pal2=c("#FC8D62","#8DA0CB","#E78AC3","#A6D854","#000000")

plotB <-SIM %>%
  filter(Species %in% c("Copepod","Krill","Amphipods","Capelin","Polar cod")) %>%
  filter(Scenarios != "Base_run") %>% 
  filter(Scenarios != "Gompertz base run") %>% 
  ggplot(aes(x=Year, y=Mean, ymin=Q1, ymax=Q2)) + 
  geom_ribbon(data=df_2 %>% filter(Scenarios == "Base_run") %>% select(-Scenarios), alpha=.2, linetype=1, color="black")+
  geom_line(data=df_2 %>% filter(Scenarios == "Base_run") %>% select(-Scenarios),size=1)+
  geom_ribbon(aes(fill=Scenarios, colour=Scenarios), alpha=.2, linetype=3)+
  geom_line(aes(colour=Scenarios), size=1)+
  labs(x="Year", y="Biomass", title="Cod scenario from Atlantis with Fx0.9", colour="Scenarios") +
  theme_minimal()+
  scale_colour_manual(values = Pal2)+  
  scale_fill_manual(values = Pal2)+  
  facet_grid(Species~Scenarios, scales = "free_y")
plotB


#
#
####### Runoff dilemma #################################################
# Variance

find_var <- SIM_data %>% 
        #   filter(Species=="Capelin") %>% 
           group_by(Run) %>% 
           summarize(Var=var(Biomass)) %>%
           mutate(Good = Var < quantile(Var, probs = 0.95)) 


plot_var_l <- left_join(SIM_data,find_var) %>% 
         #     left_join(plot_param)
         #    filter(Species=="Capelin") %>% 
              filter(Good=="FALSE") %>% 
         ggplot(aes(x=Year,y=Biomass, colour=factor(Run)))+
           geom_line(show.legend = FALSE) +
       #    coord_cartesian(ylim=c(-20,20),xlim=c(0,30) ) +
       #    geom_line(data=BASE_data %>% filter(Species=="Capelin"), aes(x=Year,y=Biomass, group=factor(Run)),color="black")+
            geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+              theme_minimal()+
            labs(x="Year", y="Biomass (ln(1000 tonnes)", title="New runs outside 95 % variance") +
        #  facet_wrap(~Good, scales = "free_y")
           facet_wrap(~Species,ncol = 1, scales = "free_y")
           plot_var_l 
      #    ggsave("New_Run_False.png",  plot_var_l)
          
#Go to Esimate_eigenvalue-file
load(file = "eigenvalue.rdata")           
           
eigenvalue %>% 
  left_join(SIM_data) %>% 
  filter(value<1) %>% 
  ggplot(aes(x=Year,y=Biomass, colour=factor(Run)))+
  geom_line(show.legend = FALSE) +
  geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+
  theme_minimal()+
  labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Runs with eigenvalue > 1 (Cod F x 0.9)") +
  facet_wrap(~Species,ncol = 1, scales = "free_y")
 
              
library("GGally")
plot_param <- find_var %>%
              bind_cols(as.data.frame(m) %>% 
              select(c22,c24,c26,c42,c44,c47,c48,c55)) %>% 
              filter(c44<1) %>% 
              filter(c42<1) %>% 
              filter(c55<1) %>% 
              select(-Run, -Var) %>% 
              arrange(!Good) %>% 
              ggpairs(mapping=aes(color=Good), columns = 2:ncol(.), title = "Modified run")
          ggsave("05.03.20_Fig3.png", plot_param, height=8, width=15)

#          
plot_var_box <-  find_var %>%
                 bind_cols(as.data.frame(m) %>% 
                 select(starts_with("c"))) %>% 
                 pivot_longer(starts_with("c")) %>% 
                 rename(Parameter=name, Value=value) %>% 
                 filter(Parameter=="c42" & Value <= 1 |
                        Parameter=="c44" & Value <= 1 |  
                        Parameter=="c55" & Value < 1) %>% 
                 group_by(Run, Good) %>%
                 summarise(Var=mean(Var)) %>% 
                 left_join(SIM_data)

m_biomass <- find_var %>%
              bind_cols(as.data.frame(m)) %>% 
              select("Run","c11","c13","c14","c15","c16","c22","c24","c26","c31","c33","c35","c36","c41","c42","c44","c46","c47","c48","c49","c51","c53","c55","c56","c57") %>% 
              left_join(SIM_data) %>% 
              group_by(Run, Species) %>% 
              summarize_all(mean)
           #   filter(Good=="FALSE") %>% 
         ggplot(aes(x=Year,y=Biomass, colour=factor(Run)))+
             geom_line(show.legend = FALSE) +
             geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+              theme_minimal()+
            labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Original run") +
            facet_wrap(~Species,ncol = 1, scales = "free_y")
        plot_modified
        ggsave("Modified_Run_False.png",  plot_modified)

        #save(m_biomass, file = "m_biomass.rdata")
        #write.csv(m_biomass, file = "m_biomass.csv")

# Identify 
plot_Biom <- Param %>%
            left_join(SIM_data) %>%   
            filter(c44<1) %>%  
            filter(c42<1) %>%
            filter(c55<1) %>%
        #    filter(c22<1) %>%
        #    filter(c33<1) %>%
        #    filter(c11<1) %>%
            filter(Good=="FALSE") %>% 
        #    group_by(Run) %>% 
        #    summarise(Variance=mean(Var))
             ggplot(aes(x=Year,y=Biomass, colour=factor(Run)))+
             geom_line(show.legend = FALSE) +
         #   coord_cartesian(ylim=c(-10,10)) +
           #  geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+              theme_minimal()+
             labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Runs within 95 % confidence interval") +
             labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Runs within 95 % confidence interval") +
  
               facet_wrap(~Species,ncol = 1, scales = "free_y")
        plot_Biom 

        plot_1775_cap <- Param %>%
          left_join(SIM_data) %>%  
          filter(Species=="Capelin") %>% 
          filter(Run=="673") %>% 
          mutate(Biomass_cap=Biomass) %>% 
          select(Year,Biomass_cap,Run)

plot_1775 <- Param %>%
          left_join(SIM_data) %>% 
          filter(Species=="Krill") %>% 
          filter(Run=="673") %>% 
          left_join(plot_1775_cap) %>% 
          select(Run,Species,Year,Biomass,Biomass_cap,c20,c22,c24,c26) %>% 
          mutate(c22x2= c22*Biomass) %>% 
          mutate(c24x4= c24*Biomass_cap) %>% 
        ggplot(aes(x=Year,y=c22x2, color="c22x2"))+
          geom_line(show.legend = FALSE) +
          geom_line(aes(x=Year,y=c24x4,color="c24x4"))+
          geom_line(aes(x=Year,y=c20,color="c20"))+
          geom_line(aes(x=Year,y=c22,color="c22"))+
          geom_line(aes(x=Year,y=c24,color="c24"))+
          geom_line(aes(x=Year,y=c20,color="c20"))+
#          geom_line(aes(x=Year,y=c20,color="c20"))+
#          geom_line(aes(x=Year,y=c20,color="c20"))+
             coord_cartesian(ylim=c(-20,20)) +
          #  geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+              theme_minimal()+
          labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Runs within 95 % confidence interval") +
          facet_wrap(~Species,ncol = 1, scales = "free_y")
        plot_Biom 
  
  
plot_ValBiom %>% 
            filter(Species=="Capelin") %>% 
            ggplot(aes(x=Year,y=ValBiom, colour=factor(Parameter)))+
            geom_line() +
            coord_cartesian(ylim=c(-10,10)) +
            geom_line(data=BASE_data %>% filter(Species=="Capelin"), aes(x=Year,y=Biomass, group=factor(Run)),color="grey")+
            geom_line(data=SIM_data %>% filter(Species=="Capelin") %>% filter(Run==1462), aes(x=Year,y=Biomass),color="black", size=1.5)+
  
            #geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+
            
            theme_minimal()+
            labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Run 1462") +
            #facet_wrap(~Good, scales = "free_y")
            facet_wrap(~Species,ncol = 1, scales = "free_y")
      
          #ggsave("FALSE_runs.png",  plot_var_l, height=10, width=5)
          
  
  
  
