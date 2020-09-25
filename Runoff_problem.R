# Parameter problem
# Run the "Run_Gompertz_simulations.R" script

library("rstan") # observe startup messages
library("tidyverse") # observe startup messages
library("conflicted")
conflict_prefer("filter", "dplyr")

####### Runoff dilemma #################################################

# plot(Latent.sim[,4,i], type = "l",ylim=c(-10,10), main="Capelin", xlab="Year",ylab="Biomass")
#  mtext(side=2,padj=0.8, expression(paste("(ln(10"^3," tonnes)",sep="")),line=2.5)
#  for(i in 1:2000){lines(Latent.sim[,4,i],col="grey")}
#  for(i in 1:2000){lines(Latent.base[,4,i],col="black")}
#legend(0, 3, legend=c("Atlantis simulation", "Base run"),col=c("grey", "black"), lty=1:1, cex=0.8)

# Range
find_range <- SIM_data %>% 
  filter(Species=="Capelin") %>% 
  group_by(Run) %>% 
  summarize(good=all(between(Biomass,-5,3))) %>% 
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c"))

find_range_h_l <- SIM_data %>% 
  filter(Species=="Capelin") %>% 
  group_by(Run) %>% 
  summarize(good=case_when(
    max(Biomass) > 5 ~ "high", 
    min(Biomass) < (-5) ~ "low",
    TRUE ~ "good")) %>% 
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c")) 

plot_range_box <- find_range_h_l %>%
  ggplot(aes(x=parameter,y=value, fill=good))+
  labs(x="Parameters", y="Value", title="", fill="Within range") +
  geom_boxplot()

plot_range_l <- semi_join(SIM_data,find_range_h_l %>% filter(good != "good")) %>% 
  filter(Species=="Capelin") %>% 
  ggplot(aes(x=Year,y=Biomass, colour=factor(Run)))+
  geom_line(show.legend = FALSE) +
  coord_cartesian(ylim=c(-50,50))

######################################################################
# Variance
Param <- read.table("Data/Parameters_Gompertz.csv", dec=".", sep=";", header=TRUE)

find_var <- SIM_data %>% 
  #   filter(Species=="Capelin") %>% 
  group_by(Run) %>% 
  summarize(Var=var(Biomass)) %>%
  mutate(good = Var < quantile(Var, probs = 0.90))


plot_var_l <- left_join(SIM_data,find_var) %>% 
  #    filter(Species=="Capelin") %>% 
  filter(good=="TRUE") %>% 
  ggplot(aes(x=Year,y=Biomass, colour=factor(Run)))+
  geom_line(show.legend = FALSE) +
  #    coord_cartesian(ylim=c(-20,20),xlim=c(0,30) ) +
  #    geom_line(data=BASE_data %>% filter(Species=="Capelin"), aes(x=Year,y=Biomass, group=factor(Run)),color="black")+
  geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+              theme_minimal()+
  labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Original runs within 90 % confidence interval = OK") +
  #  facet_wrap(~good, scales = "free_y")
  facet_wrap(~Species,ncol = 1, scales = "free_y")
plot_var_l 
#ggsave("FALSE_runs.png",  plot_var_l, height=10, width=5)


plot_var_box <-  find_var %>%
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c")) %>%
  #   group_by(parameter, good) %>% 
  #   summarise(Mean=mean(value), Q1=quantile(value,0.25),Q2=quantile(value,0.75))
  #   write_csv(plot_var_box,"plot_var_box")
  filter(parameter %in% c("c22","c24","c26","c41","c42","c44","c47","c48")) %>% 
  ggplot(aes(x=parameter,y=value, fill=good))+
  #     scale_fill_manual(values=c("#FB9A99","#33A02C","#E31A1C")) +
  labs(x="Parameters", y="Value", title="", fill="Within range") +
  geom_boxplot()
plot_var_box
ggsave(" plot_var_box_selected.png",  plot_var_box, height=5, width=8)

library("GGally")
plot_param <- find_var %>%
  bind_cols(as.data.frame(m) %>% 
              select(c22,c24,c26,c41,c42,c44,c47,c48)) %>% 
  select(-Run, -Var) %>% 
  arrange(!good) %>% 
  ggpairs(mapping=aes(color=good), columns = 2:ncol(.))
ggsave("biomass_ALL.png", plot_param, height=8, width=15)

plot_var_box <-  find_var %>%
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c")) %>% 
  filter(parameter==)

# Identify 
plot_Biom <- Param %>%
  left_join(SIM_data) %>%   
  filter(c44<1) %>%  
  filter(c42<1) %>%
  filter(c55<1) %>%
  #    filter(c22<1) %>%
  #    filter(c33<1) %>%
  #    filter(c11<1) %>%
  filter(good=="FALSE") %>% 
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
  ggplot(aes(x=Year,y=ValBiom, colour=factor(parameter)))+
  geom_line() +
  coord_cartesian(ylim=c(-10,10)) +
  geom_line(data=BASE_data %>% filter(Species=="Capelin"), aes(x=Year,y=Biomass, group=factor(Run)),color="grey")+
  geom_line(data=SIM_data %>% filter(Species=="Capelin") %>% filter(Run==1462), aes(x=Year,y=Biomass),color="black", size=1.5)+
  
  #geom_line(data=BASE_data, aes(x=Year,y=Biomass, group=factor(Run)),color="black")+
  
  theme_minimal()+
  labs(x="Year", y="Biomass (ln(1000 tonnes)", title="Run 1462") +
  #facet_wrap(~good, scales = "free_y")
  facet_wrap(~Species,ncol = 1, scales = "free_y")

#ggsave("FALSE_runs.png",  plot_var_l, height=10, width=5)


ice=zoo_dat$ice
cod=zoo_dat$cod
her=zoo_dat$her
fish=zoo_dat$cap_F


find_var <- BASE_data %>% 
  #   filter(Species=="Capelin") %>% 
  group_by(Run) %>% 
  summarize(Var=var(Biomass)) %>%
  mutate(good = Var < quantile(Var, probs = 0.9995))

c42 <- find_var %>%
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c")) %>%
  select(Run, parameter, value) %>% 
  filter(parameter %in% c("c42")) %>% 
  rename("c42_val"=value) %>% 
  select(-parameter) %>% 
  summarize(Q1=quantile(c42_val,0.025),Q2=quantile(c42_val,0.975))

c44 <- find_var %>%
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c")) %>%
  select(Run, parameter, value) %>% 
  filter(parameter %in% c("c44")) %>% 
  rename("c44_val"=value) %>% 
  select(-parameter)


c47 <- find_var %>%
  bind_cols(as.data.frame(m) %>% 
              select(starts_with("c"))) %>% 
  pivot_longer(starts_with("c")) %>%
  select(Run, parameter, value) %>% 
  filter(parameter %in% c("c47")) %>% 
  rename("c47_val"=value) %>% 
  select(-parameter) %>% 
  group_by(Run) %>% 
  summarize(Median=median(c47_val)) %>%
  mutate(Over_median = Median > quantile(Median, probs = 0.50))

trouble_param <- left_join(c42,c44) %>% 
  left_join(c47) %>% 
  left_join(find_var) %>% 
  arrange(!good) %>% 
  ggplot(aes(x=c42_val,y=c44_val, colour=factor(good)))+
  geom_point(show.legend = TRUE) +
  labs(x="c42", y="c44", title="c47 value over median (-0.595)") +
  
  facet_wrap(~Over_median,ncol = 1, scales = "free_y")     

plot_Biom <- find_var %>%
  left_join(SIM_data)   


