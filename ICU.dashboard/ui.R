# =============
# ICU.dashboard
# - Frontend --
# =============

ui <- dashboardPage(
    dashboardHeader(title = Sys.getenv("INFO_APP_NAME")),
    dashboardSidebar(minified = TRUE,
        useShinyjs(),
        sidebarMenu(
            menuItem("Datenbank-Verbindung", icon = icon("database"), startExpanded = TRUE,
                selectInput("server", "Reporting-DB auswählen",
                            choices = fn_global_getNamedList("DB_NAME")
                ),
                fluidRow(
                    column(width = 5, actionButton("connect", "Verbinden")),
                    column(width = 5, disabled(actionButton("disconnect", "Trennen")))
                )
            ),
            menuItem("Daten-Export", tabName = "rawdata", icon = icon("download")),
            menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"),
                     menuSubItem("Fälle", tabName = "db_cases", icon = icon("user")),
                     menuSubItem("Mortalität", tabName = "db_death", icon = icon("skull-crossbones"))
            ),
            menuItem("Info über diese App", tabName = "about", icon = icon("info"))
        )
    ),
    dashboardBody(
        useShinyjs(),
        introjsUI(),
        use_waiter(),
        tabItems(
            # Tab - Rawdata
            tabItem(tabName = "rawdata",
                fluidRow(
                    box(title = "Fall-Filter", width = 12, status = "primary",
                        collapsible = TRUE, collapsed = FALSE,

                        fluidRow(
                            column(width = 1,
                                   disabled(checkboxGroupInput("gender", label = h4("Geschlecht"),
                                               width = 4,
                                               choices = fn_global_getNamedList("CONFIG_RAW_GENDER_NAMES", "CONFIG_RAW_GENDER_IDS", values_numeric = TRUE),
                                               selected = as.numeric(fn_global_vectorFromEnv("CONFIG_RAW_GENDER_PRESELECTED_IDS"))))
                            ),
                            column(width = 1,
                                   disabled(numericInput("age_start", label = h4("Alter (von)"), value = Sys.getenv("CONFIG_RAW_AGE_START")))
                            ),
                            column(width = 1,
                                   disabled(numericInput("age_end", label = h4("(bis)"), value = Sys.getenv("CONFIG_RAW_AGE_END")))
                            ),
                            column(width = 3,
                                   disabled(dateRangeInput("dates", label = h4("Zeitraum"),
                                                           language = "de", separator = "bis",
                                                           startview = "decade",
                                                           start = dmy(Sys.getenv("CONFIG_RAW_DATE_START"))))
                            ),
                            column(width = 4,
                                   disabled(textAreaInput("case_ids", label = h4("Fallnummern"),
                                                          height = "7em", placeholder = "Bitte im Format FALLNUMMER;FALLNUMMER;... (getrennt durch Semikolon, keine Leerzeichen) eingeben/einfügen (STRG+V)"))
                            ),
                            column(width = 2,
                                   disabled(radioButtons("type_case_ids", label = h4("Art der Fallnummern"),
                                                         choices = fn_global_getNamedList("CONFIG_RAW_CASE_IDS_NAMES", values_numeric = TRUE),
                                                         selected = fn_global_findPosInEnvVector("CONFIG_RAW_CASE_IDS_NAMES", "CONFIG_RAW_CASE_IDS_NAMES_PRESELECTED")))
                            )
                        )
                    ),
                    box(title = "Maßnahmen-Filter", width = 12, status = "primary",
                        collapsible = TRUE, collapsed = FALSE,

                        fluidRow(
                            column(width = 12,
                                   disabled(selectInput("treatments", label = "Zu exportierende Maßnahmen", multiple = TRUE, selectize = TRUE,
                                               choices = NULL))
                            )
                        )
                    ),
                    box(title = "Ausgabe-Optionen", width = 12, status = "primary",
                        collapsible = TRUE, collapsed = FALSE,
                        
                        fluidRow(
                            column(width = 4,
                                   disabled(checkboxInput("output_boxes", label = "Export von Verordnungsboxen (Kombinationslistenfelder)", value = FALSE))
                            ),
                            column(width = 8,
                                   disabled(selectInput("output_selected_boxes", label = "zu exportierender Boxen-Inhalt", multiple = TRUE, selectize = TRUE, choices = NULL))
                            )
                        ),
                        fluidRow(
                            column(width = 4,
                                   disabled(checkboxInput("output_timesplit", label = "Zeit-Split bei kontinuierlichen Maßnahmen einfügen", value = FALSE))
                            ),
                            column(width = 4,
                                   disabled(selectInput("output_timesplit_bin", label = NULL, multiple = FALSE, selectize = FALSE, 
                                                        choices = fn_global_getNamedList("CONFIG_AGGREGATED_TIMESPAN_BINS", values_numeric = TRUE),
                                                        selected = fn_global_findPosInEnvVector("CONFIG_AGGREGATED_TIMESPAN_BINS", "CONFIG_AGGREGATED_TIMESPAN_PRESELECTED")))
                            )
                        )
                    ),
                    box(title = "Daten-Aggregation", width = 12, status = "warning",
                        collapsible = TRUE, collapsed = FALSE,
                        
                        fluidRow(
                            column(width = 4,
                                   disabled(checkboxInput("aggregated_timespan", label = "Zeitliche Aggregation", value = FALSE)),
                                   
                            ),
                            column(width = 4,
                                   disabled(selectInput("aggregated_timespan_bin", label = NULL, multiple = FALSE, selectize = FALSE, 
                                                        choices = fn_global_getNamedList("CONFIG_AGGREGATED_TIMESPAN_BINS", values_numeric = TRUE),
                                                        selected = fn_global_findPosInEnvVector("CONFIG_AGGREGATED_TIMESPAN_BINS", "CONFIG_AGGREGATED_TIMESPAN_PRESELECTED")))
                            )
                        )
                    )
                ),
                box(title = "Daten-Export", width = NULL, status = "success", collapsible = FALSE,
                    fluidRow(
                        column(width = 12,
                               p("Für einen Download der exportierten Daten bitte immer erst auf 'Daten exportieren' klicken, insbesondere wenn zwischenzeitlich Parameter verändert wurden. Erst danach steht der neue Datensatz zum Download zur Verfügung.")
                        )
                    ),
                    fluidRow(
                        column(width = 2,
                               disabled(actionButton(
                                   inputId = "calculate_count",
                                   label = "Fallzahl berechnen", 
                                   icon = icon("users")
                               ))    
                        ),
                        column(width = 2,
                               disabled(actionButton(
                                   inputId = "export_raw",
                                   label = "Daten exportieren", 
                                   icon = icon("table")
                               ))
                        ),
                        column(width = 2,
                               disabled(downloadButton(
                                   outputId = "download_raw",
                                   label = "Daten-Download", 
                                   icon = icon("download")
                               ))  
                        ),
                        column(width = 2,
                               disabled(downloadButton(
                                   outputId = "download_demographics",
                                   label = "Demografie-Download", 
                                   icon = icon("download")
                               ))  
                        )
                    )
                ),
                fluidRow(
                    column(width = 2,
                           valueBoxOutput("countCases", width = 100)
                    ),
                    column(width = 2,
                           valueBoxOutput("countValues", width = 100)
                    )
                )
            ),
            
            # Tabs - Dashboards
            tabItem(tabName = "db_cases",
                    box(title = "Auswertungs-Einstellungen", width = 12, status = "warning",
                        fluidRow(
                            column(width = 3,
                                   disabled(dateRangeInput("db_cases_dates", label = h4("Zeitraum"),
                                                           language = "de", separator = "bis",
                                                           startview = "decade",
                                                           start = dmy(Sys.getenv("CONFIG_DASHBOARD_CASES_DATE_START"))))
                            ),
                            column(width = 4,
                                   disabled(radioButtons("db_cases_interval", label = h4("Zeitintervall"),
                                                               width = 4,
                                                               choices = fn_global_getNamedList("CONFIG_DASHBOARD_CASES_INTERVALS", values_numeric = TRUE),
                                                               selected = as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PRESELECTED_INTERVAL"))))       
                            )
                        )
                    ),
                    box(title = "Fälle (nach Aufnahmezeitpunkt)", width = 12, status = "primary",
                        plotOutput("db_cases_plot")
                    )
            ),
            tabItem(tabName = "db_death",
                    box(title = "Auswertungs-Einstellungen", width = 12, status = "warning",
                        fluidRow(
                            column(width = 3,
                                   disabled(dateRangeInput("db_death_dates", label = h4("Zeitraum"),
                                                           language = "de", separator = "bis",
                                                           startview = "decade",
                                                           start = dmy(Sys.getenv("CONFIG_DASHBOARD_DEATH_DATE_START"))))
                            ),
                            column(width = 4,
                                   disabled(radioButtons("db_death_interval", label = h4("Zeitintervall"),
                                                         width = 4,
                                                         choices = fn_global_getNamedList("CONFIG_DASHBOARD_DEATH_INTERVALS", values_numeric = TRUE),
                                                         selected = as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PRESELECTED_INTERVAL"))))       
                            )
                        )
                    ),
                    box(title = "ICU-Mortalität (nach Aufnahmezeitpunkt)", width = 12, status = "primary",
                        plotOutput("db_death_plot")
                    )
            ),

            # Tab - Info über diese App
            tabItem(tabName = "about",
                fluidRow(
                    box(title = "Informationen zu dieser App", width = 12, status = "success",
                        p(
                            strong(Sys.getenv("INFO_APP_NAME")),
                            br(),
                            em("Web-basierte App für Exporte aus der Reporting-Datenbank des Patienten-Daten-Management-Systems (PDMS) Dräger ICM")
                        ),
                        p(
                            tags$u("erstellt durch:"),
                            "Dr. med. Hartmuth Nowak, MSc, DESAIC",
                            br(),
                            tags$u("Version:"),
                            paste(Sys.getenv("INFO_VERSION"), Sys.getenv("INFO_DATE"), sep = " vom "),
                            br(),
                            br(),
                            tags$u("Kontaktdaten:"),
                            br(),
                            "Dr. med. Hartmuth Nowak",
                            br(),
                            "Zentrum für Künstliche Intelligenz, Medizininformatik und Datenwissenschaften",
                            br(),
                            "Universitätsklinikum Knappschaftskrankenhaus Bochum",
                            br(),
                            "hartmuth.nowak@kk-bochum.de"
                        )
                    )
                )
            )
        )
    )
)