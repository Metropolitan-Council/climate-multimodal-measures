AnnualVehicleMilesTraveled <- AnnualVehicleMilesTraveled %>%
  filter(mode == "PLDV") %>%
  group_by(ctu, year) %>%
  summarise(annual_vmt = sum(vmt, na.rm = TRUE)) %>% rename(CTU_NAME = ctu) %>%  filter(year == 2025)

AnnualVMTCommunityType <- AnnualVehicleMilesTraveled %>%
  left_join(CommunityTypeShape) %>%
  group_by(year, MappedCommunity) %>%
  summarise(vmt = mean(annual_vmt))

VehiclePopulationCTU <- VehiclePopulation %>%
  filter(var == "TotStock", mode == "PLDV", year == 2025) %>%
  group_by(ctu, year) %>% rename(CTU_NAME = ctu)

VehiclePopulationCommunityType <- VehiclePopulationCTU %>%
  left_join(CommunityTypeShape)  %>%
  filter(!is.na(year), !is.na(MappedCommunity), !is.na(value)) %>%
  group_by(year, MappedCommunity) %>%
  summarise(population = mean(value))

PerVehicleVMT <- AnnualVMTCommunityType %>%
  inner_join(VehiclePopulationCommunityType, by = c("year", "MappedCommunity")) %>%
  mutate(PerVehicleVMT = vmt / population) %>%
  select(year, MappedCommunity, PerVehicleVMT)
