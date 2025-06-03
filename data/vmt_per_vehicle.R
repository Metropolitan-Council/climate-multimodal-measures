AnnualVMTCommunityType <- AnnualVMT %>%
  left_join(CommunityTypeShape) %>% # Join with community type shape data
  group_by(year, MappedCommunity) %>% # Group by year and community type
  summarise(vmt = sum(vmt)) # Summarise total VMT by community type

VehiclePopulationCTU <- VehiclePopulation %>%
  filter(var == "TotStock", mode == "PLDV", year == 2025) %>% # Filter for passenger light-duty vehicles in 2025
  group_by(ctu, year) %>% # Group by CTU and year
  rename(CTU_NAME = ctu)

VehiclePopulationCommunityType <- VehiclePopulationCTU %>%
  left_join(CommunityTypeShape) %>% # Join with community type shape data
  filter(!is.na(year), !is.na(MappedCommunity), !is.na(value)) %>% # Filter out rows with missing values
  group_by(year, MappedCommunity) %>% # Group by year and mapped community
  summarise(population = sum(value)) # Summarise total vehicle population by community type

PerVehicleVMT <- AnnualVMTCommunityType %>%
  inner_join(VehiclePopulationCommunityType, by = c("year", "MappedCommunity")) %>% # Join annual VMT with vehicle population data
  mutate(PerVehicleVMT = vmt / population) %>% # Calculate VMT per vehicle
  select(year, MappedCommunity, PerVehicleVMT) # Select mapped community type and vmt per vehicle
