library(tidyverse)
library(RColorBrewer)



workdir="~/Documents/Atlantis/Gompertz/Run_dir/" 


folder0 = "output_00"
folder1 = "output_01"
folder2 = "output_02"
folder3 = "output_07"
folder4 = "output_08"

run_00 <- read.table(paste(workdir, folder0, "/ina_results_001BiomIndx.txt", sep=""), header=TRUE)
run_01 <- read.table(paste(workdir, folder1, "/ina_results_001BiomIndx.txt", sep=""), header=TRUE)
run_02 <- read.table(paste(workdir, folder2, "/ina_results_001BiomIndx.txt", sep=""), header=TRUE)
run_03 <- read.table(paste(workdir, folder3, "/ina_results_001BiomIndx.txt", sep=""), header=TRUE)
run_04 <- read.table(paste(workdir, folder4, "/ina_results_001BiomIndx.txt", sep=""), header=TRUE)


nordic_groups <- read.table("~/Documents/Atlantis/Gompertz/Data/nordic_groups.csv", dec=".", sep=",", header=TRUE) %>% 
  select(Code,Name) 

### Make dataframe ###################################################################
biom.df <-bind_rows(`Base_run` = run_00,
                    `Cod_Fx0.5` = run_01,
                    `Cod_Fx2.0` = run_02,
                    `Cod_Fx0.9` = run_03,
                    `Cod_Fx1.1` = run_04,                
                    .id="Runs")%>%
          gather(key=Species, value=Biomass, -Time, -Runs) %>%
          rename(Day =Time) %>% 
          mutate(Time=Day/365) %>% 
          mutate(Year=Time+1957) %>%
          mutate(Season = Time %% 1) %>%
          filter(Season==0)  %>%
        #  filter(Time>=24) %>% 
          left_join(nordic_groups, by=c("Species"="Code"))


### Plot biomass ###################################################################
Pal1=c("#000000","#33A02C","#B2DF8A","#FB9A99","#E31A1C")


plotB <- biom.df %>%
    filter(Species %in% c("SCR")) %>% 
    ggplot(aes(x=Time, y=Biomass, color=Runs)) +  
   #     scale_x_continuous(limits=c(1980,2015), breaks=seq(1980,2015, 5)) +
        geom_line(lwd=1) +  
        scale_colour_manual(values = Pal1)+  
        labs(x="Year", y="Biomass", title="", colour="Scenarios") +
        theme_minimal()+
        facet_wrap(~Name, scales = "free_y",ncol = 1)
    plotB
    
############# Make dataset #########################################################
biom_NCO <- biom.df %>% as_tibble() %>% 
            filter(Species == "NCO") %>% 
            filter(Runs=="00_Base_run") %>%      
            mutate(Biomass=log(Biomass*1000)) %>% 
            mutate(sd =sd(Biomass), mean=mean(Biomass)) %>% 
            mutate(norm_b = (Biomass - mean)/sd) %>% 
        #    mutate(norm_b = (Biomass - 21.78207)/0.2536322) %>% 
            select(Time, mean, sd) %>% 
            left_join(biom.df) %>% 
            filter(Species == "NCO") %>% 
            filter(Runs=="08_Cod_F x1.1") %>% 
            mutate(Biomass=log(Biomass*1000)) %>% 
            mutate(norm_b = (Biomass - mean)/sd) %>% 
            select(Time, Year, Biomass, sd, mean, norm_b)
write_csv(biom_NCO, "Data/normb_NCO_08.csv")

NCO.sd <- biom_NCO %>% summarise(mean=sd(Biomass)) 
NCO.mean <- biom_NCO %>% summarise(mean=mean(Biomass))




        
### Plot change ###################################################################
change.df <- biom.df %>%
      filter(Runs == "Base_run") %>% 
      rename(Control=Biomass) %>% 
      select(-Runs) %>% 
      left_join(biom.df) %>% 
      mutate(Change=Biomass-Control) %>% 
      mutate(Percentage=Change/Control*100) 
      #filter(Runs !="00_Base_run")

plotC <- change.df %>% 
  filter(Species %in% c("NCO","CAP","PCO")) %>% 
  ggplot(aes(x=Year, y=Percentage, color=Runs)) +  
  scale_x_continuous(limits=c(1980,2015), breaks=seq(1980,2015, 5)) +
  geom_line(lwd=1) +  
  scale_colour_manual(values = Pal1)+  
  labs(x="Year", y="Change in biomass (%)", title="", colour="Scenarios") +
  theme_minimal()+
  facet_wrap(~Name, scales = "free_y",ncol = 1)
plotC

##### Make impact plot ##############

impact.df <- change.df %>% as_tibble() %>% 
              filter(Year>1985) %>% 
              mutate(Time_period=case_when(
                                  Year<1990 ~ "Early",
                                  Year<2005 ~ "Mid",
                                  TRUE ~ "Late")) %>% 
              group_by(Name, Scenarios, Time_period) %>% 
              summarise(Change= mean(Percentage)) %>% 
              mutate(Change = if_else(Change>100, true = 100, false = Change))
              
impact.plot <- impact.df %>% 
            drop_na() %>% 
            ggplot(aes(x=Name, y = Scenarios, fill=Change)) + 
            geom_raster() +
            scale_fill_gradient2() +
            scale_y_discrete(expand = c(0,0))+
            coord_flip() +
            theme_minimal() +
            labs(x="Year", y="Age class", title="Diet of cod") +
            facet_wrap(~Time_period, nrow=1)
  
impact.plot

change.df %>% 
        filter(Scenarios %in% c("Base_run","F_0.5","F_0.9","F_1.1","F_2.0")) %>% 
        filter(Species %in% c("NCO","HAD","LRD", "DF")) %>%
         mutate(Scenarios = factor(Scenarios, levels = run.order)) %>% arrange(Scenarios) %>% 
         ggplot(aes(x=Year, y=Percentage, color=Scenarios)) +  
        #   scale_x_continuous(limits=c(1980,2015), breaks=seq(1980,2015, 5)) +
          coord_cartesian(ylim = c(-100,500))+
           geom_line(lwd=1, show.legend = TRUE) +  
           scale_colour_manual(values = Pal1)+  
           labs(x="Year", y="Biomass change (%)", title="Atlantis results") +
           theme_minimal()+
           facet_wrap(~Name, scales = "free_y",ncol = 1)


##### Make diet matrix ##############

Diet <- read.table(paste(workdir, folder0, "/ina_results_001DietCheck.txt", sep=""), header=TRUE)

diet_df <-bind_rows(`Run_00` = Diet,
                    .id="Runs")%>%
          gather(key=Prey, value=Diet, -Time, -Runs, -Cohort, -Predator, -Stock, -Updated) %>%
         # filter(Predator %in% c("NCO")) %>% 
          filter(Diet>0.01) %>% 
          mutate(Day = Time) %>%   
          filter(Day>=8760) %>% 
          mutate(Time=Time/365) %>% 
          mutate(Year=Time+1957) %>%
          mutate(Season = Time %% 1) %>%
          filter(Season==0) %>%  
          mutate(Cohort=Cohort+1) %>% 
          group_by(Predator, Prey) %>% 
          summarise(Diet = mean(Diet))
          left_join(nordic_groups, by=c("Prey"="Code"))


# Adult cod
diet_adult <-diet_df %>%  
  filter(Cohort >3) %>% 
  group_by(Predator, Year, Prey, Runs, Name) %>% 
  summarise(Mean_diet=mean(Diet, na.rm = TRUE))

diet_plot <-diet_adult %>% 
            ggplot(aes(x=Year, y=Mean_diet, fill=Name)) +  
            geom_bar(stat="identity") +
            labs(x="Year", y="Diet portion", title="Diet of cod") +
            theme_minimal()
          #  facet_wrap(~Cohort, scales = "free_y")
          diet_plot
          
#
  
  
  
  
