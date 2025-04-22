# manage_participants.R
# Simplified module for adding and deleting participants from the database
# with transaction support and rollback capability.

# UI Component --------------------------------------------------------
dataImportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Left box for enrollment ----
      box(
        title = "Enroll NEW Participants",
        width = 6,
        height = '100%',
        status = "primary",
        solidHeader = TRUE,
        
        # Number of participants slider (now at the top)
        sliderInput(
          ns("num_participants"),
          "Number of New Participants:",
          value = 1,
          min = 1,
          max = 100,
          step = 1
        ),
        
        hr(),
        
        # Wave selection
        radioButtons(
          ns("wave_option"),
          "Study Wave:",
          choices = c("Create New Wave" = "new", "Use Existing Wave" = "existing"),
          selected = "new"
        ),
        
        # Conditional panels based on wave selection
        conditionalPanel(
          condition = sprintf("input['%s'] == 'new'", ns("wave_option")),
          dateInput(
            ns("new_wave_date"),
            "Wave Start Date:",
            value = NULL,  # Default value will be overridden in server
            daysofweekdisabled = c(0, 2, 3, 4, 5, 6)  # Only allow Mondays (day 1)
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'existing'", ns("wave_option")),
          uiOutput(ns("wave_selection_ui"))
        ),
        
        div(style = "text-align: center;",
            actionButton(
              ns("enroll_participants"),
              icon = icon("user-plus"),
              "Enroll Participants",
              class = "btn-success",
              style = "margin-top: 20px;"
            )
        ),
        div(
          style = "text-align: center;",
          helpText(
            "Note: Most recently added participants will be highlighted after enrollment."
          )
        )
      ),
      
      # Right box for removal -----
      box(
        title = "Remove Existing Participants",
        width = 6,
        height = '100%',
        status = "primary",
        solidHeader = TRUE,
        tags$b("Select the participants you want to remove by clicking on rows in the table below."),
        helpText("Note: Only participants who are not associated with a survey response may be removed."),
        
        div(style = "text-align: center;",
            actionButton(
              ns("select_rows_with_no_responses"),
              "Select all participants without a survey response",
              style = "margin-bottom: 15px;"
            )
        ),
        
        div(style = "text-align: center;",
            actionButton(
              ns("remove_participants"),
              icon = icon("circle-xmark"),
              "Remove Selected Participants",
              class = "btn-danger",
              disabled = TRUE
            )
        ),
        div(
          style = "text-align: center;",
          helpText(
            "Note: This will also remove orphaned waves."
          )
        )
      )
    ),
    
    # Data box below
    fluidRow(
      box(
        title = "Data",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        
        tabsetPanel(
          id = ns("participant_tabs"),
          # Table Tab 1: Preview -----
          tabPanel(
            title = "Preview Pending Participants",
            value = "preview",
            br(),
            withSpinner(
              DT::dataTableOutput(ns("participants_preview")),
              type = 8
            )
          ),
          # Table Tab 2: Existing ----
          tabPanel(
            title = "View Currently Enrolled Participants",
            value = "enrolled",
            br(),
            withSpinner(
              DT::dataTableOutput(ns("participants_current")),
              type = 8
            ),
            br(),
            div(style = "text-align: center;",
                actionButton(
                  ns("refresh_participants"), 
                  "Refresh Participant Data", 
                  icon = icon("sync"), 
                  class = "btn-info"
                )
            )
          )
        )
      )
    )
  )
}

# Server Component -----------------------------------------------------
dataImportServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store data throughout the module ----
    values <- reactiveValues(
      participants_preview_data = NULL,
      refresh_trigger = 0,         # Trigger to force refresh of data
      last_added_ids = NULL        # To track newly added participants for highlighting
    )
    
    # Fetch existing waves ----
    waves_data <- reactive({
      req(pool)
      # Depend on the refresh trigger
      refresh_trigger <- values$refresh_trigger
      
      tryCatch({
        query <- "SELECT wave_id, DATE_FORMAT(start_date, '%Y-%m-%d') as start_date FROM wave ORDER BY start_date DESC"
        waves <- dbGetQuery(pool, query)
        message("Fetched ", nrow(waves), " waves")
        return(waves)
      }, error = function(e) {
        showNotification(
          paste("Error fetching waves:", e$message),
          type = "error",
          duration = 10
        )
        return(data.frame(wave_id = integer(), start_date = character()))
      })
    })
    
    # Initialize module
    observe({
      message("Initializing manage_participants module")
      
      # Trigger initial data fetch
      values$refresh_trigger <- 1
      
      # Signal that all observers should be ready
      outputOptions(output, "wave_selection_ui", suspendWhenHidden = FALSE)
    })
    
    # IMPORTANT: Create UI for wave selection AFTER initialization ----
    output$wave_selection_ui <- renderUI({
      waves <- waves_data()
      
      # Handle empty data case
      if (nrow(waves) == 0) {
        return(div(
          p("No existing waves found. Please create a new wave."),
          style = "color: #888;"
        ))
      }
      
      selectInput(
        ns("existing_wave_id"),
        "Select Wave:",
        choices = setNames(waves$wave_id, paste("Wave", waves$wave_id, "-", waves$start_date)),
        selected = waves$wave_id[1]
      )
    })
    
    # Server-side date initialization for new wave
    observe({
      
      # Get the most recent wave date from waves_data() reactive
      waves <- waves_data()
      
      if (nrow(waves) > 0) {
        # Convert to Date objects
        dates <- as.Date(waves$start_date)
        # Find the maximum date
        max_date <- max(dates)
        # Calculate the next Monday (7 days later)
        next_monday <- max_date + 7
      } else {
        # If no waves exist, use next Monday from today
        today <- Sys.Date()
        days_until_monday <- (1 - as.numeric(format(today, "%u")) + 7) %% 7
        if (days_until_monday == 0) days_until_monday <- 7  # If today is Monday, go to next Monday
        next_monday <- today + days_until_monday
      }
      
      # Update the date input
      updateDateInput(session, "new_wave_date", value = next_monday)
    })
    
    # Fetch existing participants ----
    participants_data <- reactive({
      req(pool)
      # Depend on the refresh trigger
      refresh_trigger <- values$refresh_trigger
      
      # Limit query to improve performance with a reasonable number of results
      tryCatch({
        # Optimized query with LIMIT to improve performance
        query <- "
        SELECT 
          p.participant_id, 
          p.experimental_condition, 
          w.wave_id, 
          DATE_FORMAT(w.start_date, '%Y-%m-%d') as wave_start_date,
          (SELECT COUNT(*) FROM survey_response sr WHERE sr.participant_id = p.participant_id) AS survey_response_count 
        FROM 
          participant p
        JOIN 
          wave w ON p.wave_id = w.wave_id
        ORDER BY 
          participant_id DESC
        "
        participants <- dbGetQuery(pool, query)
        message("Fetched ", nrow(participants), " participants")
        return(participants)
      }, error = function(e) {
        showNotification(
          paste("Error fetching participants:", e$message),
          type = "error",
          duration = 10
        )
        return(data.frame(
          participant_id = integer(),
          experimental_condition = character(),
          wave_id = integer(),
          wave_start_date = character(),
          survey_response_count = integer()
        )
        )
      })
    })
    
    # Manual refresh trigger for participants data
    observeEvent(input$refresh_participants, {
      message("Manual refresh triggered")
      values$refresh_trigger <- values$refresh_trigger + 1
    })
    
    # Get next available participant ID - optimized for accurate prediction
    get_next_participant_id <- reactive({
      req(pool)
      # Force dependence on refresh trigger to get the latest info
      refresh_trigger <- values$refresh_trigger
      
      tryCatch({
        # Direct query that fetches IDs even if some have been deleted
        query <- "SELECT IFNULL(MAX(participant_id) + 1, 101) AS next_id FROM participant"
        result <- dbGetQuery(pool, query)
        next_id <- as.integer(result$next_id[1])
        message("Next available ID: ", next_id)
        return(next_id)
      }, error = function(e) {
        message("Error getting next participant ID: ", e$message)
        return(101)  # Default if query fails
      })
    })
    
    # Fixed condition options - hardcoded as requested
    fixed_conditions <- c("Control group", "Experimental group")
    
    # Helper function to check if a date is a Monday
    # is_monday <- function(date) {
    #   weekday <- weekdays(date)
    #   return(weekday == "Monday")
    # }
    
    # Function to generate participant preview data
    generate_preview_data <- function() {
      req(input$num_participants > 0)
      
      # Get wave information
      wave_id <- NULL
      wave_start_date <- NULL
      
      if (input$wave_option == "new") {
        req(input$new_wave_date)
        wave_start_date <- input$new_wave_date
        # Max wave_id + 1
        wave_id <- max(waves_data()$wave_id) + 1
      } else {
        req(input$existing_wave_id)
        wave_id <- as.integer(input$existing_wave_id)
        
        # Get wave date from existing waves
        wave_info <- waves_data()[waves_data()$wave_id == wave_id, ]
        if (nrow(wave_info) > 0) {
          wave_start_date <- as.Date(wave_info$start_date)
        } else {
          # Fallback
          wave_start_date <- Sys.Date()
        }
      }
      
      # Generate random experimental conditions
      conditions <- sample(fixed_conditions, input$num_participants, replace = TRUE)
      
      # Generate sequential participant IDs for display only
      # Actual IDs will be determined during database insertion
      start_id <- get_next_participant_id()
      participant_ids <- start_id:(start_id + input$num_participants - 1)
      
      # Create preview data
      participant_preview <- data.frame(
        participant_id = participant_ids,
        experimental_condition = conditions,
        wave_id = wave_id,
        wave_start_date = wave_start_date,
        stringsAsFactors = FALSE
      )
      
      return(participant_preview)
    }
    
    # Generate participant preview data when inputs change ----
    observe({
      # This will only run when the relevant inputs change
      req(input$num_participants)
      req(input$wave_option)
      if (input$wave_option == "new") {
        req(input$new_wave_date)
      } else {
        req(input$existing_wave_id)
      }
      
      values$participants_preview_data <- generate_preview_data()
    })
    
    # Render participants preview table
    output$participants_preview <- DT::renderDataTable({
      req(values$participants_preview_data)
      DT::datatable(
        values$participants_preview_data,
        options = list(scrollY = '400px', paging = FALSE),
        rownames = FALSE
      )
    })
    
    # Display currently enrolled participants ----
    output$participants_current <- DT::renderDataTable({
      participants <- participants_data()
      req(participants)
      
      message("Table rendering with ", nrow(participants), " rows")
      message("Last added IDs: ", paste(values$last_added_ids, collapse = ", "))
      
      # Generate array of IDs to highlight for JavaScript
      highlight_ids_js <- "[]"
      if (!is.null(values$last_added_ids) && length(values$last_added_ids) > 0) {
        highlight_ids_js <- jsonlite::toJSON(values$last_added_ids)
        message("IDs to highlight: ", highlight_ids_js)
      }
      
      # Create datatable with custom initialization
      DT::datatable(
        participants,
        options = list(
          scrollY = '400px',
          paging = FALSE,
          processing = TRUE,
          initComplete = DT::JS(sprintf("
        function(settings, json) {
          var highlightIds = %s;
          var api = this.api();
          
          // Check each row
          api.rows().every(function(rowIdx) {
            var data = this.data();
            var id = data[0]; // Assuming participant_id is the first column
            
            // Check if ID should be highlighted
            if (highlightIds.includes(id)) {
              $(this.node()).addClass('highlighted-row');
            }
          });
        }
      ", highlight_ids_js))
        ),
        selection = "multiple",
        rownames = FALSE
      ) %>%
        # Add CSS for highlighted rows
        htmlwidgets::onRender("
      function(el, x) {
        // Add CSS for highlighted rows
        var style = document.createElement('style');
        style.textContent = `
          .highlighted-row {
            background-color: #d9edf7 !important;
          }
          .highlighted-row.selected {
            background-color: #c4e3f3 !important;
          }
        `;
        document.head.appendChild(style);
      }
    ")
    })
    
    # Enable/disable remove button based on selection
    observe({
      selected_rows <- input$participants_current_rows_selected
      has_selection <- !is.null(selected_rows) && length(selected_rows) > 0
      shinyjs::toggleState("remove_participants", condition = has_selection)
    })
    
    # Select all participants with no responses
    observeEvent(input$select_rows_with_no_responses, {
      participants <- participants_data()
      rows_without_responses <- which(participants$survey_response_count == 0)
      
      message("Found ", length(rows_without_responses), " participants with no survey responses")
      
      # Use proxy to update selection without redrawing table
      proxy <- DT::dataTableProxy("participants_current")
      DT::selectRows(proxy, rows_without_responses)
      
      # Manually enable the remove button if we found rows to select
      if (length(rows_without_responses) > 0) {
        shinyjs::enable("remove_participants")
      }
    })
    
    # Enroll participants ----
    observeEvent(input$enroll_participants, {
      # Clear any previous highlights when starting a new enrollment
      values$last_added_ids <- NULL
      
      req(values$participants_preview_data)
      req(input$num_participants > 0)
      
      # Validate that we have a wave_id or can create one
      if (input$wave_option == "existing") {
        req(input$existing_wave_id)
      } else {
        req(input$new_wave_date)
        
        # Validate the wave date is a Monday and doesn't already exist
        if (!is_monday(input$new_wave_date)) {
          showNotification(
            "Wave start date must be a Monday",
            type = "error",
            duration = 10
          )
          return()
        }
        
        if (as.Date(input$new_wave_date) %in% as.Date(waves_data()$start_date)) {
          showNotification(
            "A wave with this start date already exists",
            type = "error",
            duration = 10
          )
          return()
        }
      }
      
      # Get wave ID or create a new one
      wave_id <- NULL
      new_wave_created <- FALSE
      wave_date <- NULL
      
      # Get the next available ID directly before starting transaction
      next_available_id <- get_next_participant_id()
      
      # Start a direct transaction 
      conn <- pool::poolCheckout(pool)
      
      # Use tryCatch with finally to ensure connection is always returned to pool
      tryCatch({
        # Begin transaction manually
        DBI::dbBegin(conn)
        
        # 1. Create new wave if needed
        if (input$wave_option == "new") {
          wave_date <- format(input$new_wave_date, "%Y-%m-%d")
          wave_id <- unique(values$participants_preview_data$wave_id)
          message("Creating new wave with date: ", wave_date, ", ID: ", wave_id)
          
          # Use direct SQL concatenation instead of parameterized query
          wave_query <- paste0("INSERT INTO wave (wave_id, start_date) VALUES (", wave_id, ", '", wave_date, "')")
          DBI::dbExecute(conn, wave_query)
          
          wave_id_result <- DBI::dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")
          wave_id_result <- as.integer(wave_id_result$id[1])
          new_wave_created <- TRUE
          message("Created wave with ID: ", wave_id_result)
        } else {
          wave_id <- as.integer(input$existing_wave_id)
          # Get the date for display purposes
          wave_query <- paste0("SELECT DATE_FORMAT(start_date, '%Y-%m-%d') as date FROM wave WHERE wave_id = ", wave_id)
          wave_result <- DBI::dbGetQuery(conn, wave_query)
          if (nrow(wave_result) > 0) {
            wave_date <- wave_result$date[1]
          }
          message("Using existing wave with ID: ", wave_id)
        }
        
        # 2. Create participants one by one with explicit IDs
        participant_ids <- numeric(input$num_participants)
        conditions <- values$participants_preview_data$experimental_condition
        
        for (i in 1:input$num_participants) {
          condition <- conditions[i]
          participant_id <- next_available_id + i - 1
          message("Creating participant with ID: ", participant_id, ", condition: ", condition, ", wave_id: ", wave_id)
          
          # Use explicit ID in the INSERT query
          insert_query <- paste0(
            "INSERT INTO participant (participant_id, experimental_condition, wave_id) VALUES (", 
            participant_id, ", '", condition, "', ", wave_id, ")"
          )
          DBI::dbExecute(conn, insert_query)
          
          participant_ids[i] <- participant_id
        }
        
        # Commit transaction
        DBI::dbCommit(conn)
        
        # Store ids for highlighting
        values$last_added_ids <- participant_ids
        
        # Debugging message:
        # message("Last added IDs: ", paste(values$last_added_ids, collapse = ", "))
        
        # Force refresh of data 
        values$refresh_trigger <- values$refresh_trigger + 1
        message("Updated refresh trigger after enrollment")
        
        # Switch to the Current tab
        updateTabsetPanel(session, "participant_tabs", selected = "enrolled")
        
        # Enrollment Success Handler ----
        # Show a success modal
        showModal(modalDialog(
          title = "Enrollment Successful",
          HTML(paste0(
            "<p><strong>", length(participant_ids), " participants successfully enrolled!</strong></p>",
            "<p>",
            if (new_wave_created) paste0("New wave (ID: ", wave_id, ") created with start date: ", wave_date, "<br>") else "",
            "Participant IDs: ", paste(participant_ids, collapse = ", "), 
            "</p>"
          )),
          footer = actionButton(ns("close_success_modal"), "Close", class = "btn-primary")
        ))
        
      }, error = function(e) {
        # Roll back transaction on error
        if (DBI::dbIsValid(conn)) {
          DBI::dbRollback(conn)
        }
        showNotification(
          paste("Error enrolling participants:", e$message),
          type = "error",
          duration = 10
        )
        message("Error in transaction:", e$message)
      }, finally = {
        # Always return the connection to the pool
        if (DBI::dbIsValid(conn)) {
          pool::poolReturn(conn)
        }
      })
    })
    
    # Handle success modal close button
    observeEvent(input$close_success_modal, {
      # Close the modal
      removeModal()
      
      # Reset/update form values
      if (input$wave_option == "new") {
        # Update to the next Monday
        next_monday <- input$new_wave_date + 7
        updateDateInput(session, "new_wave_date", value = next_monday)
      }
      
      # # Clear highlighting after a delay
      # later::later(function() {
      #   values$last_added_ids <- NULL
      # }, 5)  # Clear after 5 seconds
    })
    
    # Remove participants handler
    observeEvent(input$remove_participants, {
      selected_rows <- input$participants_current_rows_selected
      if (length(selected_rows) > 0) {
        participants <- participants_data()
        selected_ids <- participants$participant_id[selected_rows]
        
        # Check if any have survey responses
        has_responses <- participants$survey_response_count[selected_rows] > 0
        if (any(has_responses)) {
          showNotification("Cannot remove participants with survey responses", type = "error")
          return()
        }
        
        # Ask for confirmation
        showModal(modalDialog(
          title = "Confirm Deletion",
          paste("Are you sure you want to remove", length(selected_ids), "participants?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_remove"), "Yes, Remove", class = "btn-danger")
          )
        ))
      }
    })
    
    # Confirmation handler for removal
    observeEvent(input$confirm_remove, {
      removeModal()
      selected_rows <- input$participants_current_rows_selected
      participants <- participants_data()
      selected_ids <- participants$participant_id[selected_rows]
      
      # Execute removal
      conn <- pool::poolCheckout(pool)
      tryCatch({
        DBI::dbBegin(conn)
        
        # 1. Delete each participant using direct SQL
        for (id in selected_ids) {
          delete_query <- paste0("DELETE FROM participant WHERE participant_id = ", id)
          DBI::dbExecute(conn, delete_query)
          message("Deleted participant with ID:", id)
        }
        
        # 2. Identify waves with no participants
        orphaned_waves_query <- "
        SELECT w.wave_id 
        FROM wave w 
        LEFT JOIN participant p ON w.wave_id = p.wave_id 
        WHERE p.participant_id IS NULL
        "
        orphaned_waves <- DBI::dbGetQuery(conn, orphaned_waves_query)
        
        # 3. Delete orphaned waves if any exist
        if (nrow(orphaned_waves) > 0) {
          orphaned_wave_ids <- orphaned_waves$wave_id
          for (wave_id in orphaned_wave_ids) {
            delete_wave_query <- paste0("DELETE FROM wave WHERE wave_id = ", wave_id)
            DBI::dbExecute(conn, delete_wave_query)
            message("Deleted orphaned wave with ID:", wave_id)
          }
          
          # Update message to include deleted waves
          orphaned_message <- paste(", and", nrow(orphaned_waves), 
                                    "wave(s) with no remaining participants")
        } else {
          orphaned_message <- ""
        }
        
        DBI::dbCommit(conn)
        
        # Clear selection
        proxy <- DT::dataTableProxy("participants_current")
        DT::selectRows(proxy, NULL)
        
        # Disable the remove button
        shinyjs::disable("remove_participants")
        
        # Update data by triggering refresh
        values$refresh_trigger <- values$refresh_trigger + 1
        
        # Show success notification
        showNotification(
          paste(length(selected_ids), "participants successfully removed", orphaned_message), 
          type = "message"
        )
      }, error = function(e) {
        DBI::dbRollback(conn)
        showNotification(paste("Error removing participants:", e$message), type = "error", duration = 10)
      }, finally = {
        pool::poolReturn(conn)
      })
    })
  })
}