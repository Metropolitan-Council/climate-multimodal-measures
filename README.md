
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Regional Transportation and Climate Change Multimodal Measures

*Transportation Project Emission Reduction Calculator tool*

The Transportation Project Emission Reduction Calculator is an easy
map-based tool that can be used to quantify RS project GHG emissions
impact, the tool uses the Census Bureau Data API to extract necessary
information for project quantifications. To start quantifying GHG
emissions, users need to first use the interactive map on the front page
to identify the project area. This is critical for estimating the
residential population for Mobility Hubs and aligning the CTU with
designated land use categories that may have varying fleet mixes and
community VMT assumptions. Once the location has been identified, users
should proceed to navigate through the project categories located in the
left sidebar to select the most suitable category for their projects. As
shown in Figure 6, each project requires users to fill in the specific
project information as requested on each page. Some input fields also
have pre-populated default values provided by the consultant team based
on existing data. However, it is recommended that users gather input
suitable for their specific projects to obtain more accurate results.
The tool also includes a ‘Methodology and Sources’ section for a
detailed explanation of the calculations and data sources. The results
are presented both annually and cumulatively, including impact on
vehicle activities (e.g., VMT), GHG emissions, and social costs of
carbon (SCC). These results can also be downloaded as a .CSV file
directly using the integrated button function.

This repository is contains materials for the interactive R Shiny app
and does not include all project deliverables. Please contact us for
more information.

Presentations and reports

| Item | Link |
|----|----|
| Final PDF report | [link](task_memos/Met%20Council_Climate%20Measures_Final%20Report_05082025.pdf) |
| Task 4 Memo | [link](task_memos/Task4_Recommendation_Methodology_TPP_RS_Final.pdf) |
| Presentation to Transportation Advisory Board (TAB), March 2025 | [link](https://metrocouncil.org/Council-Meetings/Committees/Transportation-Advisory-Board-TAB/2025/03-19-2025/info-2.aspx) |

## Repository details

- `data`, and `data/raw data` contain R scripts, Excel workbooks, and
  other raw data sources. These are read in in `global.R`
- R Shiny
  - `ui.R`
  - `server.R`
  - `global.R`
- `task_memos` contain PDF and Word versions of memoranda delivered to
  the Met Council
- `R Scripts` contain R functions for each section of the Shiny App

### Running the shiny app

1.  Clone the repository and open the `.Rproj` file in RStudio
2.  Restore package environment using `renv::restore()`.
3.  Launch the app from `global.R`, `ui.R`, or `server.R` using the
    green “Run App” button in the upper right corner or from the console
    with `shiny::runApp()`.

### Data documentation

Data sources, including elasticites and default values, are available in
two Excel workbooks.

- [MetCouncilTables.xlsx](data/raw%20data/MetCouncilTables.xlsx)
- [input_default_values_sources.xlsx](data/raw%20data/input_default_values_sources.xlsx)

## Funding

This project was completed over 2022-2025. ICF and HFTE (subcontractor)
were selected through a competitive request for proposal process and
compensated approximately \$300,000 under contract 22P040.

## Contacts

[Metropolitan Council](https://metrocouncil.org/)

- Primary contact: Tony Fischer [email](tony.fischer@metc.state.mn.us)  
- Liz Roten [email](liz.roten@metc.state.mn.us) @eroten

ICF

- Stephanie Kong [email](stephanie.kong@icf.com)
- Mallory Giesie [email](Mallory.Giesie@icf.com)
- Ramon Garcia Molina [email](ramon.molinagarcia@icf.com)

<img role="img" aria-label="Metropolitan Council, ICF, and HFTE logos" src="org-logos.png" alt="Metropolitan Council, ICF, and HFTE logos" style=" max-width: 40%; display: block; margin: 0 auto; box-sizing: content-box;background-color: transparent;">
