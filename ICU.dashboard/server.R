# =============
# ICU.dashboard
# -- Backend --
# =============

server <- function(input, output, session) {
  
  # Connect with DB
  observeEvent(input$connect, {
    
    showNotification("Verbindung mit Datenbank wird hergestellt", type = "warning")

    selectedServer <- as.numeric(input$server)
    
    database_connection <<- fn_global_dbConnect(fn_global_vectorFromEnv("DB_SERVER")[selectedServer],
                                                fn_global_vectorFromEnv("DB_PORT")[selectedServer], 
                                                fn_global_vectorFromEnv("DB_DATABASE")[selectedServer],
                                                fn_global_vectorFromEnv("DB_USERNAME")[selectedServer],
                                                fn_global_vectorFromEnv("DB_PASSWORD")[selectedServer])

    showNotification("Erfolgreich mit Datenbank verbunden", type = "message")

    # Pull of available treatments
    showNotification("Verfügbare Maßnahmen werden abgerufen", type = "warning")
    treatments <<- fn_raw_getAllTreatments(database_connection)
    inputTreatments <<- setNames(treatments$TREATMENTID,
                                 paste0(treatments$TREATMENTNAME, " (", if_else(!is.na(treatments$SUBSTANCEUNIT), treatments$SUBSTANCEUNIT, "ohne Einheit") ,") - ID: ", treatments$TREATMENTID)
                                )
    updateSelectizeInput(session, "treatments", choices = inputTreatments, server = TRUE)
    
    # Pull of available boxes
    showNotification("Verfügbare Boxen (Kombinationslistenfelder) werden abgerufen", type = "warning")
    boxes <<- fn_raw_getAllBoxes(database_connection)
    inputBoxes <<- setNames(boxes$ID,
                            paste0(boxes$NAME, " - ID: ", boxes$ID)
                           )
    updateSelectizeInput(session, "output_selected_boxes", choices = inputBoxes, server = TRUE)
    
    connected <<- TRUE
    
    # UI changes
    # - database UI
    disable("connect")
    disable("server")
    enable("disconnect")
    # - rawdata UI
    enable("gender")
    enable("age_start")
    enable("age_end")
    enable("dates")
    enable("case_ids")
    enable("type_case_ids")
    enable("treatments")
    enable("output_boxes")
    enable("output_timesplit")
    enable("aggregated_timespan")
    enable("calculate_count")
    enable("export_raw")
    # - dashboard UI
    #   - cases
    enable("db_cases_dates")
    enable("db_cases_interval")
    #   - ICU mortality
    enable("db_death_dates")
    enable("db_death_interval")
  })

  # Disconnect from DB
  observeEvent(input$disconnect, {
    dbDisconnect(database_connection)
    
    connected <<- FALSE

    showNotification("Verbindung zur Datenbank getrennt", type = "message")

    # UI changes
    # - database UI
    disable("disconnect")
    enable("connect")
    enable("server")
    # - rawdata UI
    updateSelectInput(session, "treatments", choices = NULL)
    disable("gender")
    disable("age_start")
    disable("age_end")
    disable("dates")
    disable("case_ids")
    disable("type_case_ids")
    disable("treatments")
    disable("output_boxes")
    disable("output_selected_boxes")
    disable("output_timesplit")
    disable("output_timesplit_bin")
    disable("aggregated_timespan")
    disable("aggregated_timespan_bin")
    disable("calculate_count")
    disable("export_raw")
    disable("download_raw")
    disable("download_demographics")
    # - dashboard UI
    #   - cases
    disable("db_cases_dates")
    disable("db_cases_interval")
    #   - ICU mortality
    disable("db_death_dates")
    disable("db_death_interval")
  })
  
  # Enable/disable Box-Selection for output
  observeEvent(input$output_boxes, {
    if(input$output_boxes) {
      enable("output_selected_boxes")
    } else {
      disable("output_selected_boxes")
      if(exists("connected")) {
        if(connected) {
          updateSelectizeInput(session, "output_selected_boxes", choices = inputBoxes, selected = NULL, server = TRUE)
        } else {
          updateSelectizeInput(session, "output_selected_boxes", selected = NULL, server = TRUE)
        }
      }
    }
  })
  
  # Enable/disable timesplit selection for output
  observeEvent(input$output_timesplit, {
    if(input$output_timesplit) {
      enable("output_timesplit_bin")
    } else {
      disable("output_timesplit_bin")
    }
  })
  
  # Enable/disable Aggregation-Selection for output
  observeEvent(input$aggregated_timespan, {
    if(input$aggregated_timespan) {
      enable("aggregated_timespan_bin")
    } else {
      disable("aggregated_timespan_bin")
    }
  })
  
  # Synchronize aggregation with timesplit in output
  observeEvent(input$aggregated_timespan, {
    if(input$aggregated_timespan) {
      updateCheckboxInput(session, "output_timesplit", value = TRUE)
    }
  })
  
  # Synchronize aggregation with timesplit in output
  observeEvent(input$output_timesplit, {
    if(!input$output_timesplit) {
      updateCheckboxInput(session, "aggregated_timespan", value = FALSE) 
    }
  })
  
  # Synchronize time bins for output and aggregation
  observeEvent(input$output_timesplit_bin, {
    updateSelectInput(session, "aggregated_timespan_bin", selected = input$output_timesplit_bin)
  })
  
  # Synchronize time bins for output and aggregation
  observeEvent(input$aggregated_timespan_bin, {
    updateSelectInput(session, "output_timesplit_bin", selected = input$aggregated_timespan_bin)
  })

  # Do case and value count calculation
  observeEvent(input$calculate_count, {
    # show waiter animation
    waiter_show(html = tagList(
      spin_fading_circles(),
      "Fallzahlberechnung wird durchgeführt."
    ))

    caseAndValueCount <<- fn_raw_getCaseAndValueCount(database_connection, input$gender, input$age_start, 
                                                      input$age_end, input$dates, 
                                                      fn_global_vectorFromString(input$case_ids, sep =";"), 
                                                      input$type_case_ids, input$treatments) %>%
      group_by(CASEID) %>% 
      summarise(n_values = sum(NUMADMINS), .groups = "keep")
    
    output$countCases <- renderValueBox(
      valueBox(formatC(nrow(caseAndValueCount), big.mark = ".", decimal.mark = ",", format = "d"),
               ifelse(nrow(caseAndValueCount) == 1, "Fall", "Fälle"),
               icon = icon("users"), color = "yellow")
    )

    output$countValues <- renderValueBox(
      valueBox(formatC(sum(caseAndValueCount$n_values), big.mark = ".", decimal.mark = ",", format = "d"),
               ifelse(sum(caseAndValueCount$n_values) == 1, "Wert (roh)", "Werte (roh)"),
               icon = icon("list"), color = "green")
    )

    waiter_hide()
  })

  # Export data
  observeEvent(input$export_raw, {
    # show waiter animation
    waiter_show(html = tagList(
      spin_fading_circles(),
      "Die gewünschten Daten werden aus der Datenbank abgerufen. Je nach Größe kann dies mehrere Minuten (oder noch deutlich länger) dauern. Nach dem Datenabruf werden die Daten zum Speichern bereitgestellt."
    ))

    # pull raw data
    rawdata <<- fn_raw_getData(database_connection, input$gender, input$age_start, input$age_end, 
                               input$dates, fn_global_vectorFromString(input$case_ids, sep =";"), 
                               input$type_case_ids, input$treatments, input$output_selected_boxes)
    
    # pull demographics
    demographicdata <<- fn_raw_getDemographicData(database_connection,input$gender, input$age_start, 
                                                  input$age_end, input$dates, 
                                                  fn_global_vectorFromString(input$case_ids, sep =";"), 
                                                  input$type_case_ids, input$treatments)
    
    # insert timesplit in rawdata?
    if (input$output_timesplit) {
      bin_timesplit <- names(fn_global_getNamedList("CONFIG_AGGREGATED_TIMESPAN_BINS", values_numeric = TRUE))[as.numeric(input$output_timesplit_bin)]
      rawdata <<- fn_raw_insertTimesplit(rawdata, bin = bin_timesplit)
    }
    
    # aggregate rawdata?
    if (input$aggregated_timespan) {
      bin_timesplit <- names(fn_global_getNamedList("CONFIG_AGGREGATED_TIMESPAN_BINS", values_numeric = TRUE))[as.numeric(input$aggregated_timespan_bin)]
      rawdata <<- fn_aggregated_getData(rawdata, demographicdata, interval = bin_timesplit)
    }
    
    enable("download_raw")
    enable("download_demographics")
    
    waiter_hide()
  })
  
  # Download of raw/aggregated data
  output$download_raw <- downloadHandler(
      filename = function() {
        paste0("export_raw_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
      },
      content = function(file) {
        shiny::withProgress(
          message = "Downloading data...",
          value = 0,
          {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            shiny::incProgress(5/10)
            write.csv2(rawdata, file, row.names = FALSE, na = "")
          }
        )
      }
  )
  
  # Download of demographic data
  output$download_demographics <- downloadHandler(
    filename = function() {
      paste0("export_demographic_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading demographic data...",
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          write.csv2(demographicdata, file, row.names = FALSE, na = "")
        }
      )
    }
  )
  
  # Dashboard - cases
  output$db_cases_plot <- renderPlot({
    if(exists("connected")) {
      if(connected) {
        getCasesPlot(database_connection, input$db_cases_dates, input$db_cases_interval)
      }
    }
  })
  
  # Dashboard - ICU mortality
  output$db_death_plot <- renderPlot({
    if(exists("connected")) {
      if(connected) {
        getDeathPlot(database_connection, input$db_death_dates, input$db_death_interval)
      }
    }
  })
}