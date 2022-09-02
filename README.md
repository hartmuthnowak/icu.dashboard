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

## Configuration
The main configuration of the web application is done in the .Renviron file.

## Instruction manual
TODO
