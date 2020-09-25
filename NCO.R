
library("tidyverse") # observe startup messages


workdir="~/Documents/Atlantis/Gompertz/" 


folder = "Data"



NCO_00 <- read.table(paste(workdir, folder, "/normb_NCO_00.csv", sep=""),sep = ",", header=TRUE)
NCO_01 <- read.table(paste(workdir, folder, "/normb_NCO_01.csv", sep=""),sep = ",", header=TRUE)
NCO_02 <- read.table(paste(workdir, folder, "/normb_NCO_02.csv", sep=""),sep = ",", header=TRUE)
NCO_03 <- read.table(paste(workdir, folder, "/normb_NCO_07.csv", sep=""),sep = ",", header=TRUE)
NCO_04 <- read.table(paste(workdir, folder, "/normb_NCO_08.csv", sep=""),sep = ",", header=TRUE)

NCO.df <-bind_rows(`00_Base_run` = NCO_00,
                   `01_Fmsy` = NCO_01,
                   `02_Fmsy2` = NCO_02,
                   `07_Fmsy10` = NCO_03,
                   `08_Cod_F x1.1` = NCO_04,                
                    .id="Runs") %>% as_tibble()
NCO.df     %>%
  ggplot(aes(y=norm_b, x=Year, color=Runs))+
  geom_line()

change.df <- NCO.df %>%
  filter(Runs == "00_Base_run") %>% 
  rename(Control=norm_b) %>% 
  select(-Runs) %>% 
  left_join(NCO.df) %>% 
  mutate(Change=norm_b-Control) %>% 
  mutate(Percentage=Change/Control*100) 

change.df     %>%
  ggplot(aes(y=Percentage, x=time, color=Runs))+
  geom_line()
  