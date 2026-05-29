AnnualVMTCommunityType <- AnnualVMT %>%
  left_join(CommunityTypeShape) %>% # Join with community type shape data
  group_by(year, MappedCommunity) %>% # Group by year and community type
  summarise(vmt = sum(vmt), .groups = "keep") # Summarise total VMT by community type



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
    ) # Map community types to broader categories
  )

population <- st_transform(population, st_crs(CommunityType)) # Transform population data to match the CRS of CommunityType
joined_data <- st_join(population, CommunityType, join = st_intersects) # Join population data with community type data based on spatial intersection
intersected <- st_intersection(population %>% select(GEOID), CommunityType) # Perform spatial intersection to get overlapping areas
intersected$intersection_area <- st_area(intersected) # Calculate the area of the intersection

assigned_community <- intersected %>%
  group_by(GEOID) %>% # Group by GEOID to assign community type
  slice_max(order_by = intersection_area, n = 1) %>% # Select the community type with the largest intersection area
  ungroup() %>%
  select(GEOID, MappedCommunity) # Select GEOID and Mapped Community - output is an assigned community type per census tract

assigned_community <- as.data.frame(assigned_community)
census_tracts_with_community <- left_join(population, assigned_community, by = "GEOID") # Join census tract population data with assigned community types

VMTPerCapitaByCommunityType <- census_tracts_with_community %>%
  group_by(MappedCommunity) %>%
  summarise(estimate = sum(estimate)) %>%
  left_join(AnnualVMTCommunityType) %>%
  mutate(VMTperCapita = vmt / estimate) # Calculate VMT per capita by community type
