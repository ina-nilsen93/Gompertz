library("qgraph")
library("tidyverse")

# Create edgelist:
dat.3 <- matrix(c(1:15 * 2 - 1, 1:15 * 2), ,2)
dat.3 <- cbind(dat.3, round(seq(-0.7, 0.7, length = 15), 1))

# Create grid layout:
L.3 <- matrix(1:30, nrow = 2)

# Different esize:
invertebrate <- c("PWN","CEP","KCR","ZL","ZM","ZS","ZG","PS","PL","DF","BC","BD","BFF","SPO","COR","PB","BB","DR","DC","DL")

workdir="~/Documents/Atlantis/Gompertz/Run_dir/"
folder0 = "output_00"
Diet <- read.table(paste(workdir, folder0, "/ina_results_001DietCheck.txt", sep=""), header=TRUE)

diet_df<-bind_rows(`Run_00` = Diet,
                    .id="Runs")%>%
      gather(key=Prey, value=Diet, -Time, -Runs, -Cohort, -Predator, -Stock, -Updated) %>%
      filter(Diet>0.001) %>% 
      mutate(Day = Time) %>%   
      mutate(Time=Time/365) %>% 
      mutate(Year=Time+1957) %>%
      mutate(Season = Time %% 1) %>%
      filter(Season==0, Time >=24) 

######## Foodweb #######################
foodweb <- diet_df %>%
        #    mutate(G=case_when(
       #     Predator %in% c("PCO","CAP","ZL","ZM","NCO","SSH") ~ "Gompertz",
        #    TRUE ~ "Non_Gompertz")) %>% 
        #   mutate(Diet=ifelse(G == "Gompertz",true = Diet* (-1), false = Diet)) 
        #   mutate(Cohort=Cohort+1) %>%
        #   filter(Type=="Vertebrate" & Cohort >3) %>%
            group_by(Predator, Prey) %>% 
            summarise(Diet = mean(Diet)) 

left_join(nordic_groups, by=c("Predator"="Code", "Prey"="Code"))



####### Plot foodweb
group <- list()
group$Gompertz <- c(6,23,24,37,41,42)

qgraph(foodweb, directed = TRUE, edge.labels = FALSE, vsize = 4, layout="spring", 
       posCol = "grey",negCol= "lightblue", groups=group, color = c("lightblue"))
title("Foodweb", line = 2.5)

group$Gompertz <- c(1:6)

## Plot fluxes
flux <- diet_df %>% 
        filter(Predator %in% c("PCO","CAP","ZL","ZM","NCO","SSH")) %>% 
        filter(Year>2010) %>% 
        group_by(Predator, Prey) %>% 
        summarise(Diet = mean(Diet)) 

group$Gompertz <- c(1:6)

qgraph(flux, directed = TRUE, edge.labels = FALSE, vsize = 4, layout="circle", 
       posCol = "black",negCol= "lightblue", groups=group, color = c("lightblue"))
title("Cohort >3", line = 2.5)

# qgraph
# mode="direct" - Same arrows but varying colour
# edge.color = rainbow(9) - All arrows vary in colour
# layout="group"/"spring" - Order network into circle or network
# directed = TRUE - Arrows
# bidirectional=TRUE - Arrows both ways
# groups=group, 
# color = c("lightblue", "lightsalmon")

input<- matrix(1,3,3)
print(input)
qgraph(input,directed=TRUE)

input<-matrix(c(0,1,2,
                0,0,3,
                0,0,0),3,3,byrow=TRUE)

# List:
groups <- list(A = c(1,2,3,4,5),
               B = c(6,7,8,9,10))
# Factor:
groups <- c("A","A","A","A","A",
            "B","B","B","B","B")
# Result:
qgraph(matrix(1,10,10),groups=groups)

