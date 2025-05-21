AnnualVMTCommunityType <- AnnualVMT %>%
  left_join(CommunityTypeShape) %>%
  group_by(year, MappedCommunity) %>%
  summarise(vmt = sum(vmt))

VehiclePopulationCTU <- VehiclePopulation %>%
  filter(var == "TotStock", mode == "PLDV", year == 2025) %>%
  group_by(ctu, year) %>%
  rename(CTU_NAME = ctu)

VehiclePopulationCommunityType <- VehiclePopulationCTU %>%
  left_join(CommunityTypeShape) %>%
  filter(!is.na(year), !is.na(MappedCommunity), !is.na(value)) %>%
  group_by(year, MappedCommunity) %>%
  summarise(population = sum(value))

PerVehicleVMT <- AnnualVMTCommunityType %>%
  inner_join(VehiclePopulationCommunityType, by = c("year", "MappedCommunity")) %>%
  mutate(PerVehicleVMT = vmt / population) %>%
  select(year, MappedCommunity, PerVehicleVMT)
