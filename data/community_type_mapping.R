# Assuming community_type_shape is your data frame
CommunityTypeShape <- CommunityType %>%
  mutate(MappedCommunity = case_when(
    COMDESNAME %in% c("Urban", "Urban Center", "Urban Edge") ~ "Urban",
    COMDESNAME %in% c("Suburban", "Diversified Residential") ~ "Suburban",
    COMDESNAME %in% c("Suburban Edge", "Emerging Suburban Edge") ~ "Suburban Edge",
    COMDESNAME %in% c("Diversified Rural", "Rural Residential", "Rural Center", "Agricultural", "Non-Council Community") ~ "Rural / Non-Council",
    TRUE ~ NA_character_ # For any unexpected values
  ))

CommunityTypeShape <- CommunityTypeShape %>%
  group_by(CTU_NAME) %>%
  filter(ACRES == max(ACRES)) %>%
  ungroup()

# Create a new row for "Other"
new_row <- data.frame(
  OBJECTID = max(CommunityTypeShape$OBJECTID, na.rm = TRUE) + 1, # Ensure unique ID
  CTU_NAME = "Other",
  ProposalB = NA, # Set to NA or a default value
  ACRES = NA, # Set to NA or 0 if needed
  UNIQ = "Other",
  COMDESNAME = "Rural / Non-Council",
  MappedCommunity = "Rural / Non-Council"
)

# Append the new row to CommunityTypeShape
CommunityTypeShape <- bind_rows(CommunityTypeShape, new_row)
