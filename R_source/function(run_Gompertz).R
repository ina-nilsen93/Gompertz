# Run Gompertz model
# run_Gompertz(zoo = zoo_dat, cod.sim = .x$norm_b)

#Old:
#NCO_csv <- read.table("Data/Old/norm_NCO_2.csv", dec=".", sep=";", header=TRUE) # F x 2
#cod.sim <- NCO_csv$norm_b  # Cod biomass from Atlantis         
#
#Latent.sim <- with(zoo_dat, {........osv

run_Gompertz <- function(zoo = zoo_dat, cod.sim){
            Latent.sim <- with(zoo, {
              Latent.sim <- array(dim=c(N, K, D))   # Latent values no perturbations
              for(d in 1:D){ # d <- 1
                
                Latent <- array(dim=c(N, K))   # Latent values
                
                # Parameters 
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
              Latent.sim
            })  
    return(Latent.sim)
}

# Make latent.sim (array) into SIM_data (tibble)
# tidy_Gompertz(Latent.base)
tidy_Gompertz <- function(Latent.sim){
    SIM_data <- (1:5) %>% 
      set_names(c("Copepod","Krill","Amphipods","Capelin","Polar cod")) %>% 
      map_df(~{Latent.sim[, .x, ] %>% 
          t() %>% 
          as.data.frame() %>% 
          as_tibble() %>% 
          mutate(Run = 1:n()) %>%  
          pivot_longer(-Run, names_to = "Time", values_to = "Biomass", 
                       names_prefix = "V", names_transform = list(Time = as.integer))}
          , .id="Species")   %>% 
          
          mutate(Year=Time+1979)
    return(SIM_data)
    }
      




