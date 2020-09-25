#load("Data/codkrillamph_stanmod_v3_new.stanfit")
load("Data/codkrillamph_stanmod_v3_new_26112018.stanfit")
#load("Data/codkrillamph_stanmod_v3_42-44-55_26112018.stanfit")


m <- as.matrix(fit)                 # Gjør om "fit" til matrise (dim(m))
#plot(m[,1])                        # Plotter alle verdiene som eks. C11 kan ha
la <- rstan::extract(fit, permuted = TRUE) # return a list of arrays 
la$cop_latent <- la$Latent[,,1]
la$krill_latent <- la$Latent[,,2]
la$amph_latent <- la$Latent[,,3]
la$cap_latent <- la$Latent[,,4]
la$pc_latent <- la$Latent[,,5]


# Also run start of call file to make data series  ###########
load(file = "Data/zoo_dat.rdata")

D <- dim(m)[1] # number of posterior samples  ex. 2000 samples
N <- zoo_dat$N # number of estimated years    ex. zoo data in 35 years simulation
K <- zoo_dat$K # number of species            ex. 5 species

## 1. Find process errors (PE)
#Første simulering er kun for å finne process error så den er lagret for de neste simuleringene


Latent.null_list <- with(zoo_dat, {
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
  list(Latent.null = Latent.null, PE = PE)
  
})

PE <- Latent.null_list$PE
Latent.base <- Latent.null_list$Latent.null

