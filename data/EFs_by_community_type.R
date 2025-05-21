FleetDataCommunityType <- FleetData %>%
  rename(CTU_NAME = ctu) %>%
  left_join(CommunityTypeShape)


DieselEFsCommunityType <- FleetDataCommunityType %>%
  filter(stock == "CIStock") %>%
  group_by(MappedCommunity, year) %>%
  summarise(
    total_vmt_diesel = sum(vmt, na.rm = TRUE),
    total_dir_ghg_diesel = sum(dir_ghg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(EF = (total_dir_ghg_diesel / total_vmt_diesel) * 1000 * 1.21) %>%
  group_by(MappedCommunity) %>%
  complete(year = full_seq(year, 1)) %>%
  mutate(EF = zoo::na.approx(EF, rule = 2)) %>%
  ungroup() %>%
  select(MappedCommunity, year, EF)

DieselCommercialCommunityType <- FleetDataCommunityType %>%
  filter(mode == "BU") %>%
  filter(stock == "BCIStock") %>%
  group_by(MappedCommunity, year) %>%
  summarise(
    total_vmt_diesel = sum(vmt, na.rm = TRUE),
    total_dir_ghg_diesel = sum(dir_ghg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(EF = (total_dir_ghg_diesel / total_vmt_diesel) * 1000 * 1.21) %>%
  group_by(MappedCommunity) %>%
  complete(year = full_seq(year, 1)) %>%
  mutate(EF = zoo::na.approx(EF, rule = 2)) %>%
  ungroup() %>%
  select(MappedCommunity, year, EF)


GasolineEFsCommunityType <- FleetDataCommunityType %>%
  filter(stock %in% c("SIStock", "HEVStock")) %>%
  group_by(MappedCommunity, year) %>%
  summarise(
    total_vmt_gasoline = sum(vmt, na.rm = TRUE),
    total_dir_ghg_gasoline = sum(dir_ghg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(EF = (total_dir_ghg_gasoline / total_vmt_gasoline) * 1000 * 1.24) %>%
  group_by(MappedCommunity) %>%
  complete(year = full_seq(year, 1)) %>%
  mutate(EF = zoo::na.approx(EF, rule = 2)) %>%
  ungroup() %>%
  select(MappedCommunity, year, EF)
