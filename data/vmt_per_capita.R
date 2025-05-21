AnnualVMTCommunityType <- AnnualVMT %>%
  left_join(CommunityTypeShape) %>%
  group_by(year, MappedCommunity) %>%
  summarise(vmt = sum(vmt))

population <- get_acs(
  geography = "tract",
  table = "B01003",
  state = "MN",
  geometry = TRUE,
  cache_table = TRUE
) %>%
  sf::st_transform("+proj=longlat +datum=WGS84")

population <- ms_simplify(population, keep = 0.05, keep_shapes = TRUE)

CommunityType <- CommunityType %>%
  mutate(
    MappedCommunity = case_when(
      COMDESNAME %in% c("Urban", "Urban Center") ~ "Urban",
      COMDESNAME == "Suburban" ~ "Suburban",
      COMDESNAME %in% c("Suburban Edge", "Emerging Suburban Edge") ~ "Suburban Edge",
      COMDESNAME %in% c(
        "Diversified Rural",
        "Rural Residential",
        "Rural Center",
        "Agricultural",
        "Non-Council Area"
      ) ~ "Rural / Non-Council",
      TRUE ~ NA_character_
    )
  )

population <- st_transform(population, st_crs(CommunityType))
joined_data <- st_join(population, CommunityType, join = st_intersects)
intersected <- st_intersection(population %>% select(GEOID), CommunityType)
intersected$intersection_area <- st_area(intersected)

assigned_community <- intersected %>%
  group_by(GEOID) %>%
  slice_max(order_by = intersection_area, n = 1) %>%
  ungroup() %>%
  select(GEOID, MappedCommunity)

assigned_community <- as.data.frame(assigned_community)
census_tracts_with_community <- left_join(population, assigned_community, by = "GEOID")

VMTPerCapitaByCommunityType <- census_tracts_with_community %>%
  group_by(MappedCommunity) %>%
  summarise(estimate = sum(estimate)) %>%
  left_join(AnnualVMTCommunityType) %>%
  mutate(VMTperCapita = vmt / estimate)
