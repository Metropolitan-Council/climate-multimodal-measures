
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Regional Transportation and Climate Change Multimodal Measures

## Transportation Project Emission Reduction Calculator Tool

The Transportation Project Emission Reduction Calculator is a map-based
tool that can be used to quantify regional solicitation projects
greenhouse gas emissions impact.

Project types include Bicycle and Pedestrian Facilities (Pedestrian
Facilities and Multi-Use Trails & Bicycle Facilities), Electric Vehicles
(EV Education and Outreach and Public Infrastructure Installation),
Roadways (Intersection Delay Reductions and Corridor Speed Increases),
Transit Expansion (Transit Expansion and Mobility Hubs) and Travel
Demand Management (Employee Commute and Shared Mobility).

<!-- Further details and study conclusions available on [metrocouncil.org](https://metrocouncil.org/Transportation/Performance/Travel-Behavior-Inventory/Data/Maximum-Mode-Shift.aspx). -->

> \[!CAUTION\]  
> This repository is contains materials for the interactive R Shiny app
> and does not include all project deliverables. Please [contact
> us](#contacts) for more information.

Presentations and reports

| Item | Link |
|----|----|
| Final PDF report | [link](task_memos/Met%20Council_Climate%20Measures_Final%20Report_05082025.pdf) |
| Task 4 Memo | [link](task_memos/Task4_Recommendation_Methodology_TPP_RS_Final.pdf) |
| Presentation to Transportation Advisory Board (TAB), March 2025 | [link](https://metrocouncil.org/Council-Meetings/Committees/Transportation-Advisory-Board-TAB/2025/03-19-2025/info-2.aspx) |

### Repository details

- `data` and `data/raw/` contain R scripts, Excel workbooks, and other
  raw data sources. These are read in in `global.R`
- R Shiny
  - `ui.R` User interface
  - `server.R` Server
  - `global.R` Read in data, libraries, and run data processing.
- `task_memos` contain PDF and Word versions of memoranda delivered to
  the Met Council
- `R` contain R scripts functions for each section of the Shiny App

All data needed to run the app are included in the repository or
imported directly using R.

#### Running the shiny app

1.  Clone the repository and open the `.Rproj` file in RStudio
2.  Restore package environment using `renv::restore()`.
3.  Launch the app from `global.R`, `ui.R`, or `server.R` using the
    green “Run App” button in the upper right corner or from the console
    with `shiny::runApp()`.

The tool uses the Census Bureau Data API to extract necessary
information for project quantifications. To start quantifying GHG
emissions, users need to first use the interactive map on the front page
to identify the project area. This is critical for estimating the
residential population for Mobility Hubs and aligning the CTU with
designated land use categories that may have varying fleet mixes and
community VMT assumptions. Once the location has been identified, users
should proceed to navigate through the project categories located in the
left sidebar to select the most suitable category for their projects.
Each project requires users to fill in the specific project information
as requested on each page. Some input fields also have pre-populated
default values provided by the consultant team based on existing data.
However, it is recommended that users gather input suitable for their
specific projects to obtain more accurate results.

The tool also includes a ‘Methodology and Sources’ section for a
detailed explanation of the calculations and data sources. The results
are presented both annually and cumulatively, including impact on
vehicle activities (e.g., VMT), GHG emissions, and social costs of
carbon (SCC). These results can also be downloaded as a .CSV file
directly using the integrated button function.

### Data documentation

Data sources, including elasticites and default values, are available in
two Excel workbooks.

- [MetCouncilTables.xlsx](data/raw%20data/MetCouncilTables.xlsx)
- [input_default_values_sources.xlsx](data/raw%20data/input_default_values_sources.xlsx)

<br>
<details>

<summary>

Key data sources
</summary>

Essential data sources

| Table | Source | Description |
|:---|:---|:---|
| Adjustment Factors And Trip Lengths | Barbour, E., Handy, S., Kendall, A., & Volker, J. (2019, August 13). Updated default values for transit dependency and average length of unlinked transit passenger trips, for calculations using TAC methods for California Climate Investments programs (Technical Report No. 16TTD004). California Air Resources Board; Institute of Transportation Studies, University of California, Davis. Retrieved from <https://ww2.arb.ca.gov/sites/default/files/auction-proceeds/transit_factors_technical_081319.pdf> | Route types include Bus Rapid Transit, Commuter Express, Core Local, Suburban Local, Support |
| Trip Distances | Travel Behavior Inventory, Communications with Metropolitan Council staff | Mode types include Bicycle, For-Hire Vehicle, Household Vehicle, Long distance passenger mode, Micromobility, Missing, Other, Other Bus, Other Vehicle, Public Bus, Rail, School Bus, Smartphone ridehailing service, Walk |
| Default Lifetime | U.S. Bureau of Labor Statistics (BLS), 2024, Employee Tenure in 2024 (USDL-24-1971), retrieved June 30, 2025, <https://www.bls.gov/news.release/pdf/tenure.pdf> | TDM – Employee Commute VMT Reduction |
| Default Lifetime | Minnesota Department of Transportation (MnDOT), 2023, Minnesota Carbon Reduction Strategy (Document No. 240817), retrieved June 27, 2025, <https://www.lrl.mn.gov/docs/2024/other/240817.pdf> | TDM – Micromobility |
| Default Lifetime | U.S. Department of Transportation (USDOT), 2023, Benefit–Cost Analysis Guidance for Discretionary Grant Programs (Updated May 13, 2025), Office of the Secretary, retrieved June 27, 2025, <https://www.transportation.gov/sites/dot.gov/files/2023-12/Benefit%20Cost%20Analysis%20Guidance%202024%20Update.pdf> | Traffic Management Technologies |
| Default Lifetime | Federal Transit Administration (FTA), 2021, Default Useful Life Benchmark (ULB) Cheat Sheet, retrieved June 27, 2025, <https://www.transit.dot.gov/sites/fta.dot.gov/files/2021-11/TAM-ULB-CheatSheet.pdf> | Electrification Education and Outreach –Light Duty Personal, TDM – Carshare, TDM - Carpooling/Vanpooling |
| Default Lifetime | Minnesota Department of Transportation (MnDOT), 2022, Minnesota Electric Vehicle Infrastructure Plan (NEVI Formula Program state EV infrastructure deployment plan), retrieved June 27, 2025, <https://www.lrl.mn.gov/docs/2022/other/221021.pdf> | Public Infrastructure Deployment |
| Default Lifetime | Federal Transit Administration (FTA), 2021, Default Useful Life Benchmark (ULB) Cheat Sheet, retrieved June 27, 2025, <https://www.transit.dot.gov/sites/fta.dot.gov/files/2021-11/TAM-ULB-CheatSheet.pdf> | Electrification Education and Outreach –Medium and Heavy-Duty Fleets and Commercial, Transit Expansion – New or Expanded Transit Service |
| Default Lifetime | U.S. Department of Transportation (USDOT), 2023, Benefit–Cost Analysis Guidance for Discretionary Grant Programs (Updated May 13, 2025), Office of the Secretary, retrieved June 27, 2025, <https://www.transportation.gov/sites/dot.gov/files/2023-12/Benefit%20Cost%20Analysis%20Guidance%202024%20Update.pdf> | Spot Mobility and Safety, Strategic Capacity, Transit Hubs, Transit Expansion – Bus Rapid Transit Conversion, Transit Modernization, Bike & Ped Facilities |
| Annual VMT | Met Council GHG Strategy Planning Tool | Passenger light-duty vehicle |
| Vehicle Population | Met Council GHG Strategy Planning Tool | NA |
| Transit Dependency Adjustments | Barbour, E., Handy, S., Kendall, A., & Volker, J. (2019, August 13). Updated default values for transit dependency and average length of unlinked transit passenger trips, for calculations using TAC methods for California Climate Investments programs (Technical Report No. 16TTD004). California Air Resources Board; Institute of Transportation Studies, University of California, Davis. Retrieved from <https://ww2.arb.ca.gov/sites/default/files/auction-proceeds/transit_factors_technical_081319.pdf> | Route types include Bus Rapid Transit, Commuter Express, Diesel Commuter Rail, Electric Commuter Rail, Core Local, Light Rail Transit, Suburban Local, Support |
| GREET Carbon Intensity | Argonne National Laboratory. (2023). GREET: Greenhouse Gases, Regulated Emissions, and Energy use in Technologies Model® (2023 Excel) \[Software\]. U.S. Department of Energy. <https://greet.es.anl.gov/> (<doi:10.11578/GREET-Excel-2023/dc.20230907.1>) energy.gov | Wheel-to-well in grams CO₂e per mile |
| Fuel Efficiency | Average of currently available model from ICF’s Proprietary EV library | Vehicle types include Light-Duty, Medium-Duty, Heavy-Duty |
| VMT By Community Type | Metropolitan Council. (2022). Travel Behavior Inventory: 2021 Household Survey Synthesis Report. Retrieved June 27, 2025, from <https://metropolitan-council.github.io/TBI_Household_Synthesis_Report/> | NA |
| Total VMT Reduction Potential | Orange County Transportation Authority (OCTA). (2022, September). Mobility Hubs Study Final Report. Retrieved June 11, 2025, from <https://octa.net/pdf/MobilityHubsStudyFinalReport.pdf> | Mobility types include Pedestrian Facility, Bike Share, Scooter and Moped Share, Bicycle Parking, Car Share, Microtransit |
| Charger Utilization Rates And Power | Energetics, “EV WATTS Charging Station Dashboard Q4-23,” 2024. \[Online\]. Available: <https://www.energetics.com/evwatts>. \[Accessed: 11-06-2025\]. | Rates for DC fast and Level 2 |
| Mode Shift Factor | California Air Resources Board (CARB). (2023, November 1). Clean Mobility Benefits Quantification Methodology \[Final\]. California Climate Investments. Retrieved June 11, 2025, from <https://ww2.arb.ca.gov/sites/default/files/auction-proceeds/Clean_Mobility_QM_FINAL_November2023.pdf> | Mode shift factor by average daily vehicle trips per day |
| Credit For Key Destinations | California Air Resources Board (CARB). (2023, November 1). Clean Mobility Benefits Quantification Methodology \[Final\]. California Climate Investments. Retrieved June 11, 2025, from <https://ww2.arb.ca.gov/sites/default/files/auction-proceeds/Clean_Mobility_QM_FINAL_November2023.pdf> | Credits for number of key destinations and distance from facility |
| Social Cost Carbon | U.S. Environmental Protection Agency (EPA), 2023, Report on the Social Cost of Greenhouse Gases: Estimates Incorporating Recent Scientific Advances, retrieved June 5, 2025, <https://www.epa.gov/system/files/documents/2023-12/epa_scghg_2023_report_final.pdf> | NA |

</details>

### Funding

This project was completed over 2022-2025. ICF and HFTE (subcontractor)
were selected through a competitive request for proposal process and
compensated approximately \$300,000 under contract 22P040.

### Contacts

[Metropolitan Council](https://metrocouncil.org/)

- Primary contact: Tony Fischer
  [email](mailto:tony.fischer@metc.state.mn.us)  
- Liz Roten [email](mailto:liz.roten@metc.state.mn.us) @eroten

ICF Inc.

- Stephanie Kong [email](mailto:stephanie.kong@icf.com)
- Mallory Giesie [email](mailto:Mallory.Giesie@icf.com)
- Ramon Garcia Molina [email](mailto:ramon.molinagarcia@icf.com)

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree
to abide by its terms.

<img role="img" aria-label="Metropolitan Council, ICF, and HFTE logos" src="org-logos.png" alt="Metropolitan Council, ICF, and HFTE logos" style="max-width: 40%; display: block; margin: 0 auto; box-sizing: content-box;background-color: transparent;">
