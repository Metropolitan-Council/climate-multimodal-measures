# Create a mapping of stock to fuel type
stock_to_fuel_type <- c(
  "BCIStock" = "diesel",
  "BEVStock" = "electricity",
  "CIStock" = "diesel",
  "EVStock" = "electricity",
  "HEVStock" = "gasoline",
  "PHEVStock" = "electricity",
  "SIStock" = "gasoline"
)

# Filter out NA values and add fuel_type column
FleetData <- FleetData %>%
  filter(!is.na(stock)) %>%
  mutate(fuel_type = stock_to_fuel_type[stock]) %>%
  rename(CTU_NAME = ctu) %>%
  left_join(CommunityTypeShape)

# Calculate the total VMT for each community type, year, and fuel type
total_vmt_by_fuel_type <- FleetData %>%
  group_by(MappedCommunity, year, fuel_type) %>%
  summarise(total_vmt = sum(vmt, na.rm = TRUE), .groups = "drop")

# Calculate the total VMT for each community type and year
total_vmt_by_community_type_year <- FleetData %>%
  group_by(MappedCommunity, year) %>%
  summarise(total_vmt = sum(vmt, na.rm = TRUE), .groups = "drop")

# Join the total VMT data frames and calculate percentage VMT by fuel type
FleetData <- total_vmt_by_fuel_type %>%
  left_join(total_vmt_by_community_type_year, by = c("MappedCommunity", "year")) %>%
  mutate(percentage_vmt = (total_vmt.x / total_vmt.y) * 100) %>%
  select(MappedCommunity, year, fuel_type, percentage_vmt)

FleetData <- FleetData %>% pivot_wider(names_from = fuel_type, values_from = c(percentage_vmt))

FleetData <- FleetData %>%
  mutate(year = as.numeric(year))

FleetData <- FleetData %>%
  mutate(
    year = as.numeric(year),
    diesel = diesel / 100,
    electricity = electricity / 100,
    gasoline = gasoline / 100
  )
