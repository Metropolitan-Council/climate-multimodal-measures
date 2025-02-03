# Assuming community_type_shape is your data frame
CommunityTypeShape <- CommunityType %>%
  mutate(MappedCommunity = case_when(
    COMDESNAME %in% c("Urban", "Urban Center", "Urban Edge") ~ "Urban",
    COMDESNAME %in% c("Suburban", "Diversified Residential") ~ "Suburban",
    COMDESNAME %in% c("Suburban Edge", "Emerging Suburban Edge") ~ "Suburban Edge",
    COMDESNAME %in% c("Diversified Rural", "Rural Residential", "Rural Center", "Agricultural", "Non-Council Community") ~ "Rural / Non-Council",
    TRUE ~ NA_character_  # For any unexpected values
  ))

CommunityTypeShape <- CommunityTypeShape %>%
  group_by(CTU_NAME) %>%
  filter(ACRES == max(ACRES)) %>%
  ungroup()