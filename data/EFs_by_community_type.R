FleetDataCommunityType <- FleetData %>%
  rename(CTU_NAME = ctu) %>%
  left_join(CommunityTypeShape) # Join with community type shape data


DieselEFsCommunityType <- FleetDataCommunityType %>%
  filter(stock == "CIStock") %>% # Filter for CIStock (diesel) vehicles
  group_by(MappedCommunity, year) %>% # Group by community and year
  summarise(
    total_vmt_diesel = sum(vmt, na.rm = TRUE), # Sum VMT for diesel vehicles
    total_dir_ghg_diesel = sum(dir_ghg, na.rm = TRUE), # Sum direct GHG emissions for diesel vehicles
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>% 
  mutate(EF = (total_dir_ghg_diesel / total_vmt_diesel) * 1000 * 1.21) %>% # Calculate EF for diesel vehicles
  group_by(MappedCommunity) %>%
  complete(year = full_seq(year, 1)) %>% # Ensure all years are present
  mutate(EF = zoo::na.approx(EF, rule = 2)) %>% # Interpolate missing values
  ungroup() %>%
  select(MappedCommunity, year, EF) # Select relevant columns

DieselCommercialCommunityType <- FleetDataCommunityType %>%
  filter(mode == "BU") %>% # Filter for bus mode
  filter(stock == "BCIStock") %>% # Filter for BCIStock (diesel bus) vehicles
  group_by(MappedCommunity, year) %>% # Group by community and year
  summarise(
    total_vmt_diesel = sum(vmt, na.rm = TRUE), # Sum VMT for diesel buses
    total_dir_ghg_diesel = sum(dir_ghg, na.rm = TRUE),# Sum direct GHG emissions for diesel buses
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(EF = (total_dir_ghg_diesel / total_vmt_diesel) * 1000 * 1.21) %>% # Calculate EF for diesel buses (MOVES4)
  group_by(MappedCommunity) %>%
  complete(year = full_seq(year, 1)) %>% # Ensure all years are present
  mutate(EF = zoo::na.approx(EF, rule = 2)) %>% # Interpolate missing values
  ungroup() %>%
  select(MappedCommunity, year, EF) # Select relevant columns


GasolineEFsCommunityType <- FleetDataCommunityType %>%
  filter(stock %in% c("SIStock", "HEVStock")) %>% # Filter for SIStock (gasoline) and HEVStock (hybrid) vehicles
  group_by(MappedCommunity, year) %>% # Group by community and year
  summarise(
    total_vmt_gasoline = sum(vmt, na.rm = TRUE), # Sum VMT for gasoline vehicles
    total_dir_ghg_gasoline = sum(dir_ghg, na.rm = TRUE), # Sum direct GHG emissions for gasoline vehicles
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(EF = (total_dir_ghg_gasoline / total_vmt_gasoline) * 1000 * 1.24) %>% # Calculate EF for gasoline vehicles
  group_by(MappedCommunity) %>%
  complete(year = full_seq(year, 1)) %>% # Ensure all years are present
  mutate(EF = zoo::na.approx(EF, rule = 2)) %>% # Interpolate missing values
  ungroup() %>%
  select(MappedCommunity, year, EF) # Select relevant columns
