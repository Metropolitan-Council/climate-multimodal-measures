FleetData <- read_xlsx("C:/Users/61886/OneDrive - ICF/Desktop/transport-emission-shiny/data/FleetData.xlsx")

# Create a mapping of stock to fuel type
stock_to_fuel_type <- c(
  "BCIStock" = "electricity",
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
  mutate(fuel_type = stock_to_fuel_type[stock])

# Calculate the total VMT for each ctu, year, and fuel type
total_vmt_by_fuel_type <- FleetData %>%
  group_by(ctu, year, fuel_type) %>%
  summarise(total_vmt = sum(vmt, na.rm = TRUE), .groups = 'drop')

# Calculate the total VMT for each ctu and year
total_vmt_by_ctu_year <- FleetData %>%
  group_by(ctu, year) %>%
  summarise(total_vmt = sum(vmt, na.rm = TRUE), .groups = 'drop')

# Join the total VMT data frames and calculate percentage VMT by fuel type
FleetData <- total_vmt_by_fuel_type %>%
  left_join(total_vmt_by_ctu_year, by = c("ctu", "year")) %>%
  mutate(percentage_vmt = (total_vmt.x / total_vmt.y) * 100) %>%
  select(ctu, year, fuel_type, percentage_vmt)

FleetData <- FleetData %>% pivot_wider(names_from = fuel_type, values_from = c(percentage_vmt))

FleetData <- FleetData %>%
  mutate(year = as.numeric(year))
