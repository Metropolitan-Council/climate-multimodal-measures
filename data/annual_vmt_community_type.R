AnnualVehicleMilesTraveled <- AnnualVehicleMilesTraveled %>%
  filter(mode == "PLDV") %>%
  group_by(ctu, year) %>%
  summarise(annual_vmt = sum(vmt, na.rm = TRUE)) %>% rename(CTU_NAME = ctu)

AnnualVMTCommunityType <- AnnualVehicleMilesTraveled %>%
  left_join(CommunityTypeShape) %>%
  filter(!is.na(year), !is.na(MappedCommunity), !is.na(annual_vmt)) %>%
  group_by(year, MappedCommunity) %>%
  summarise(vmt = mean(annual_vmt))
