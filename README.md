# ICU.dashboard
icu.dashboard is a web application based on R shiny for export and analysis of patient data which were documented and collected by the intensive care information system (patient data management system) ICM of Dräger Medical Germany (Lübeck, Germany). This web application accesses the ICM reporting database, which is provided by ICM for this purpose (e.g. data analysis, export, quality management).

ICU.dashboard contains the following features:
- Raw export of all patient-related data, which is available in the order tables of ICM reporting database.
- Aggregation and transformation of raw data: e.g. aggregation of multiple values, drug applications etc. over a specified time interval.
- Calculation of statistical parameters of aggregated data over the specified time interval.
- Export of patient demographics.
- Provision of a dashboard for custom graphical presentations of e.g. case numbers, outcomes, quality key figures, etc. THIS FEATURE IS STILL UNDER CONSTRUCTION.

All documentation and the source code of this R shiny application is in English language. However, the user interface (UI) is in German language. Currently there is no english translation available for the UI.

## License
ICU.dashboard is an open source application and distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3). Therefore, sharing and improvements of this application are highly appreciated.
This application comes with a Oracle Database JDBC driver (ojdbc7.jar) which is governed by the No-clickthrough FUTC license (see LICENSE_OJDBC).

## Installation instructions
Please obtain a current release of [R](https://cran.r-project.org/) and [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/) and install them (first R, then RStudio).

This R shiny applications is dependent on the following R packages, please install them as well:
- RJDBC
- stringr
- tidyverse
- lubridate
- rintrojs
- shiny
- shinyBS
- shinydashboard
- shinydashboardPlus
- shinyjs
- waiter

## Basic Configuration
The main configuration of the web application is done in the .Renviron file.

- Please set `JAVA_HOME` to the location of your java installation (if not set via PATH variables).
- Please set server credentials for the variables `DB_NAME, DB_SERVER, DB_PORT, DB_DATABASE, DB_USERNAME, DB_PASSWORD`. If you want to configure more than one database connection please separate each setting by a comma (,) without spacing characters. The number of settings separated by commas need to be identical in all of these variables.
  - `DB_NAME`: Identifying name of database connection (will be displayed in a dropdown for selection)
  - `DB_SERVER`: Derver address (domain name or IP)
  - `DB_PORT`: Server port (normally 1521 for Oracle databases)
  - `DB_DATABASE`: Database name
  - `DB_USERNAME`: Username
  - `DB_PASSWORD`: Password
- Please set the ICM target ward, where patients are discharged to when they died on ICU in the variable `CONFIG_DASHBOARD_DEATH_IDENTIFYING_TARGETWARD` (e.g. "Exitus")
- If settings are changed within the .Renviron file the R project needs to be loaded again for update of the settings.

## Instruction manual
TODO
