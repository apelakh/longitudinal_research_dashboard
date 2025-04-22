# server.R
server <- function(input, output, session) {
  # In server.R, update your connection management:
  
  # Global connection status
  connection_active <- reactiveVal(FALSE)
  
  # Check connection on app startup with NULL handling
  observe({
    # Make sure the pool exists and is not NULL
    if (exists("pool", envir = .GlobalEnv) && !is.null(get("pool", envir = .GlobalEnv))) {
      pool_obj <- get("pool", envir = .GlobalEnv)
      current_status <- check_db_connection(pool_obj)
      connection_active(current_status)
      
      # If connection is not active, try to reconnect automatically
      if (!current_status && exists("reconnect_db", envir = .GlobalEnv)) {
        message("Connection invalid at session start, attempting automatic reconnection")
        tryCatch({
          new_pool <- reconnect_db()
          assign("pool", new_pool, envir = .GlobalEnv)
          new_status <- check_db_connection(new_pool)
          connection_active(new_status)
          
          if (new_status) {
            showNotification("Database connection restored automatically", type = "message")
          } else {
            showNotification(
              "Automatic database reconnection failed. Click the refresh button to try again.",
              type = "warning",
              duration = NULL
            )
          }
        }, error = function(e) {
          message("Error during automatic reconnection: ", e$message)
          connection_active(FALSE)
          showNotification(
            paste("Error reconnecting to database:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }
    } else {
      connection_active(FALSE)
      message("Pool not available during initialization")
    }
  })
  
  # Display connection status text
  output$connection_status_text <- renderText({
    if (connection_active()) {
      "Connected"
    } else {
      "Disconnected"
    }
  })
  
  # Style the connection indicator based on status
  observe({
    if (connection_active()) {
      shinyjs::addClass(selector = "#connection_status_indicator", class = "text-success")
      shinyjs::removeClass(selector = "#connection_status_indicator", class = "text-danger")
      shinyjs::removeClass(selector = "#connection_status_indicator", class = "text-muted")
    } else {
      shinyjs::addClass(selector = "#connection_status_indicator", class = "text-danger")
      shinyjs::removeClass(selector = "#connection_status_indicator", class = "text-success")
      shinyjs::removeClass(selector = "#connection_status_indicator", class = "text-muted")
    }
  })
  
  # Global refresh connection button handler
  observeEvent(input$refresh_connection, {
    showNotification("Attempting to refresh connection...", type = "message")
    
    # Check if reconnect_db exists in global environment
    if (!exists("reconnect_db", envir = .GlobalEnv)) {
      showNotification("Reconnection function not available", type = "error")
      return()
    }
    
    # Get the current pool status
    current_pool <- NULL
    if (exists("pool", envir = .GlobalEnv)) {
      current_pool <- get("pool", envir = .GlobalEnv)
    }
    
    if (is.null(current_pool) || !check_db_connection(current_pool)) {
      tryCatch({
        # Get the reconnect function
        reconnect_func <- get("reconnect_db", envir = .GlobalEnv)
        
        # Create a new pool
        new_pool <- reconnect_func()
        
        # Update the global environment
        assign("pool", new_pool, envir = .GlobalEnv)
        
        # Check if the new connection works
        if (!is.null(new_pool) && check_db_connection(new_pool)) {
          connection_active(TRUE)
          showNotification("Connection successfully refreshed!", type = "message")
        } else {
          connection_active(FALSE)
          showNotification(
            "Failed to reconnect to database. Check your credentials.",
            type = "error",
            duration = NULL
          )
        }
      }, error = function(e) {
        connection_active(FALSE)
        showNotification(
          paste("Error reconnecting to database:", e$message),
          type = "error",
          duration = NULL
        )
      })
    } else {
      connection_active(TRUE)
      showNotification("Connection is already valid!", type = "message")
    }
  })
  
  # Navigation handlers for the overview page buttons ----
  observeEvent(input$goto_database_design, {
    updateTabItems(session, "sidebar", "database_design")
  })
  
  observeEvent(input$goto_missing, {
    updateTabItems(session, "sidebar", "missing_data")
  })
  
  observeEvent(input$goto_psychometrics, {
    updateTabItems(session, "sidebar", "psychometrics")
  })
  
  observeEvent(input$goto_descriptives, {
    updateTabItems(session, "sidebar", "descriptives")
  })
  
  observeEvent(input$goto_models, {
    updateTabItems(session, "sidebar", "models")
  })
  
  observeEvent(input$goto_manage_participants, {
    updateTabItems(session, "sidebar", "manage_participants")
  })
  
  # Module server logic ----
  databaseDesignServer("database_design")
  missingDataServer("missing_data", pool)
  psychometricsServer("psychometrics", pool)
  dataImportServer("manage_participants", pool)
  connectionTestServer("connection_test", pool)
  
  
  # Close the connection pool on shutdown
  session$onSessionEnded(function() {
    message("Session ending, closing pool")
    if (exists("pool")) {
      safe_pool_close(pool)
    }
  })
}