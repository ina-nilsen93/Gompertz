
library(ncdf4)
library(tidyverse)

workdir="~/Documents/Atlantis/Gompertz/Run_dir/"

folder0 = "output_00/"

#load ncfiles
nc_0 = nc_open(paste(workdir,folder0,"ina_results_001.nc",sep=""))


# Create timeframe
time=ncvar_get(nc_0,"t")

# Species
sel.Fish=c("North_atl_cod")
#sel.Fish=c("Mackerel", "Blue_whiting","North_atl_cod", "Haddock", "Saithe", "Demersal_large", "Demersals_other", "Norwegian_ssh","Green_halibut", "Polar_cod", "Capelin", "Pelagic_small", "Mesop_fish", "Long_rough_dab", "Redfish_other", "Redfish", "Minke_whale", "Skates_rays", "Snow_crab")
#sel.Fish=c("Mackerel", "Blue_whiting","North_atl_cod", "Haddock", "Saithe", "Demersal_large", "Demersals_other", "Sharks_other", "Norwegian_ssh","Green_halibut", "Polar_cod", "Capelin", "Pelagic_small", "Mesop_fish", "Long_rough_dab", "Pelagic_large", "Flatfish_other", "Redfish_other", "Redfish", "Polar_bear", "Killer_whale", "Sperm_whale", "Humpb_whale", "Minke_whale", "Fin_whale", "Beard_seal", "Harp_seal", "Hood_seal", "Ring_seal", "Sea_b_arct", "Sea_b_bor", "Skates_rays", "Snow_crab")

############    Make dataframe #############################

Biomass_00 <- map_df(set_names(sel.Fish), function(f) {          # All age-structured groups have
    ycl=case_when(
      f == "Capelin"~5, 
      f == "Snow_crab"~6,
      f == "Sperm_whale"~8,
      TRUE ~10)
  map_df(1:ycl, function(y){ 
    nums = ncvar_get(nc_0, paste(f, y, "_Nums", sep = ""))      # Extract numbers from nc-file
    struct = ncvar_get(nc_0,paste(f, y, "_StructN", sep = ""))  # Extract structural weight
    res = ncvar_get(nc_0,paste(f, y, "_ResN", sep = ""))        # Extract reserved weight
    biom=(struct+res)*nums*5.7*20/1e9                           # Convert from Nmg to tonnes
    biom_ts=colSums(colSums(biom))
    nums_ts=colSums(colSums(nums))
    struct_ts=colSums(colSums(struct))*5.7*20/1e9
    res_ts=colSums(colSums(res))*5.7*20/1e9                            #Sums layers and polygons
    tibble(Biomass=biom_ts, nums_ts, struct_ts, res_ts, Age_class = y) %>% 
      mutate(time=0:(n()-1)) %>% 
      mutate(day = time*73, Year = day/365, Season=Year%%1) %>% 
      select(-time)
  })  %>%
  filter(Year>=24, near(Season,0.2))
}, .id = "Species") 

####






