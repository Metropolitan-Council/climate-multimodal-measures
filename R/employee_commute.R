employeeCommute <- function(dailyCommuteNo, projectStart, projectLifetime, averageCommute) {
  
  workingDays <- 260  # Assuming 260 working days per year
  
  # Generate list of project years
  projectYears <- seq(projectStart, projectStart + projectLifetime - 1)
  
  # Initialize vectors to store results
  vmtDisplaced <- numeric(length(projectYears))
  ghgImpact <- numeric(length(projectYears))
  
  for (i in seq_along(projectYears)) {
    year <- projectYears[i]
    
    # Calculate VMT displaced for the current year
    vmtDisplacedYear <- dailyCommuteNo * averageCommute * workingDays
    
    # Filter GHG emission factor (EF) for the current year
    greetEfYear <- GREETCarbonIntensity %>% filter(year == year)
    
    discountRate <- SocialCostCarbon %>% filter(year == year )
    
    # Calculate GHG impact for the current year
    ghgImpactYear <- vmtDisplacedYear * greetEfYear$EF * discountRate
    
    # Store results for the current year
    vmtDisplaced[i] <- vmtDisplacedYear
    ghgImpact[i] <- ghgImpactYear
  }
  
  # Returning the results as a data frame
  return(data.frame(year = projectYears, vmtDisplaced = vmtDisplaced, ghgImpact = ghgImpact))
}

