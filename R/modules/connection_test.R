# R/modules/connection_test.R

# UI Component
connectionTestUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Database Connection Test",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        p("This tool helps diagnose database connection issues."),
        
        actionButton(ns("test_connection"), "Test Connection", 
                     icon = icon("database"), class = "btn-primary"),
        actionButton(ns("check_env"), "Check Environment Variables", 
                     icon = icon("cog"), class = "btn-info"),
        
        hr(),
        
        # Results area
        uiOutput(ns("diagnostic_result"))
      )
    )
  )
}

# Server logic
connectionTestServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store test results
    diagnostic <- reactiveVal(NULL)
    
    # Check environment variables
    observeEvent(input$check_env, {
      # Get environment variables (mask password)
      env_vars <- list(
        DB_NAME = Sys.getenv("DB_NAME"),
        DB_HOST = Sys.getenv("DB_HOST"),
        DB_USER = Sys.getenv("DB_USER"),
        DB_PASS = if(nchar(Sys.getenv("DB_PASS")) > 0) "******" else "[not set]",
        DB_PORT = Sys.getenv("DB_PORT", "3306")
      )
      
      # Check if essential variables are missing
      missing_vars <- names(env_vars)[sapply(env_vars, function(x) x == "" || x == "[not set]")]
      
      diagnostic(list(
        type = "env_vars",
        env_vars = env_vars,
        missing_vars = missing_vars,
        has_missing = length(missing_vars) > 0
      ))
    })
    
    # Run connection test
    observeEvent(input$test_connection, {
      # Show a modal while testing
      showModal(modalDialog(
        title = "Testing Connection",
        "Checking database connection...",
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Try direct connection first (without pool)
      direct_connection <- tryCatch({
        # Create direct connection
        con <- DBI::dbConnect(
          RMySQL::MySQL(),
          dbname = Sys.getenv("DB_NAME", "avp36"),
          host = Sys.getenv("DB_HOST", "localhost"),
          username = Sys.getenv("DB_USER", ""),
          password = Sys.getenv("DB_PASS", ""),
          port = as.numeric(Sys.getenv("DB_PORT", "3306"))
        )
        
        # Test if connection works
        res <- DBI::dbGetQuery(con, "SELECT 1 AS test")
        db_tables <- DBI::dbListTables(con)
        
        # Clean up direct connection
        DBI::dbDisconnect(con)
        
        list(
          success = TRUE,
          tables = db_tables
        )
      }, error = function(e) {
        return(list(
          success = FALSE,
          error = e$message
        ))
      })
      
      # Try pool connection
      # Update the pool connection test in connectionTestServer
      pool_connection <- tryCatch({
        if (is.null(pool)) {
          list(
            success = FALSE,
            error = "Pool object is NULL - connection failed during initialization"
          )
        } else if (!inherits(pool, "Pool")) {
          list(
            success = FALSE,
            error = paste("Invalid pool object type:", class(pool)[1])
          )
        } else {
          # Test if pool works
          res <- pool::poolWithTransaction(pool, function(conn) {
            DBI::dbGetQuery(conn, "SELECT 1 AS test")
          })
          
          # Get tables through the pool
          tables <- pool::poolWithTransaction(pool, function(conn) {
            DBI::dbListTables(conn)
          })
          
          list(
            success = TRUE,
            tables = tables
          )
        }
      }, error = function(e) {
        return(list(
          success = FALSE,
          error = e$message
        ))
      })
      
      # Store comprehensive diagnostic information
      diagnostic(list(
        type = "connection_test",
        timestamp = Sys.time(),
        direct_connection = direct_connection,
        pool_connection = pool_connection,
        env_vars = list(
          DB_NAME = Sys.getenv("DB_NAME"),
          DB_HOST = Sys.getenv("DB_HOST"),
          DB_USER = Sys.getenv("DB_USER"),
          DB_PASS = if(nchar(Sys.getenv("DB_PASS")) > 0) "******" else "[not set]",
          DB_PORT = Sys.getenv("DB_PORT", "3306")
        )
      ))
      
      # Close the modal
      removeModal()
    })
    
    # Render diagnostic results
    output$diagnostic_result <- renderUI({
      req(diagnostic())
      
      d <- diagnostic()
      
      if (d$type == "env_vars") {
        tagList(
          h4("Environment Variables"),
          
          if (d$has_missing) {
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              strong("Missing variables: "), 
              paste(d$missing_vars, collapse = ", "),
              p("Please set these variables in your .Renviron file.")
            )
          } else {
            div(
              class = "alert alert-success",
              icon("check-circle"),
              "All required environment variables are set."
            )
          },
          
          h5("Current Settings:"),
          tags$pre(
            paste(
              paste0("DB_NAME: ", d$env_vars$DB_NAME),
              paste0("DB_HOST: ", d$env_vars$DB_HOST),
              paste0("DB_USER: ", d$env_vars$DB_USER),
              paste0("DB_PASS: ", d$env_vars$DB_PASS),
              paste0("DB_PORT: ", d$env_vars$DB_PORT),
              sep = "\n"
            )
          )
        )
      } else if (d$type == "connection_test") {
        tagList(
          h4("Connection Test Results"),
          
          # Direct connection results
          h5("Direct Database Connection:"),
          if (d$direct_connection$success) {
            div(
              class = "alert alert-success",
              icon("check-circle"),
              "Direct connection successful!",
              
              if (length(d$direct_connection$tables) > 0) {
                tagList(
                  p(strong("Tables found:"), paste(length(d$direct_connection$tables), "tables")),
                  tags$ul(
                    lapply(head(d$direct_connection$tables, 10), function(tbl) {
                      tags$li(tbl)
                    })
                  ),
                  if (length(d$direct_connection$tables) > 10) {
                    p("(", length(d$direct_connection$tables) - 10, " more tables not shown)")
                  }
                )
              } else {
                p("No tables found in the database.")
              }
            )
          } else {
            div(
              class = "alert alert-danger",
              icon("exclamation-circle"),
              strong("Direct connection failed: "), 
              p(d$direct_connection$error)
            )
          },
          
          # Pool connection results
          h5("Connection Pool Test:"),
          if (d$pool_connection$success) {
            div(
              class = "alert alert-success",
              icon("check-circle"),
              "Connection pool is working correctly."
            )
          } else {
            div(
              class = "alert alert-danger",
              icon("exclamation-circle"),
              strong("Connection pool error: "), 
              p(d$pool_connection$error)
            )
          },
          
          # Troubleshooting advice
          if (!d$direct_connection$success || !d$pool_connection$success) {
            div(
              class = "alert alert-info",
              icon("info-circle"),
              strong("Troubleshooting suggestions:"),
              tags$ul(
                tags$li("Make sure your MySQL/MariaDB server is running"),
                tags$li("Verify username and password in your .Renviron file"),
                tags$li("Check that the database name exists"),
                tags$li("Ensure your user has proper permissions"),
                tags$li("Verify that the host and port are correct"),
                tags$li("If using remote database, check firewall settings")
              )
            )
          }
        )
      }
    })
  })
}