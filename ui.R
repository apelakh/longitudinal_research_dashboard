# Main UI structure using shinydashboard layout

ui <- dashboardPage(

  
  # Dashboard header with title
  
  dashboardHeader(
    title = "Study Dashboard",
    
    # Connection status in header
    tags$li(
      class = "dropdown",
      tags$a(
        id = "connection_status_indicator",
        class = "text-muted",
        tags$span(
          icon("database"), 
          textOutput("connection_status_text", inline = TRUE)
        ),
        style = "padding-right: 15px;"
      )
    ),
    
    # Refresh connection button in header
    tags$li(
      class = "dropdown",
      actionButton(
        "refresh_connection", 
        label = NULL,
        icon = icon("sync"), 
        class = "btn-xs",
        style = "margin-top: 8px; margin-right: 10px;"
      )
    )
  ),
  
  # Dashboard sidebar with menu items
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Database Design", tabName = "database_design", icon = icon("diagram-project"), badgeLabel = icon("circle-check")),
      menuItem("Missing Data", tabName = "missing_data", icon = icon("chart-line"), badgeLabel = icon("circle-check")),
      menuItem("Psychometrics", tabName = "psychometrics", icon = icon("chart-bar"), badgeLabel = icon("circle-check")),
      menuItem("Descriptives", tabName = "descriptives", icon = icon("chart-pie"), 
               badgeLabel = icon("person-digging"), badgeColor = "red"),
      menuItem("Models", tabName = "models", icon = icon("chart-area"), 
               badgeLabel = icon("person-digging"), badgeColor = "red"),
      menuItem("Manage Participants", tabName = "manage_participants", icon = icon("user-check"), badgeLabel = icon("circle-check")),
      menuItem("Connection Test", tabName = "connection_test", icon = icon("database"), badgeLabel = icon("circle-check"))
    )
  ),
  
  # Dashboard body with tab content
  dashboardBody(
    
    includeCSS("www/css/styles.css"),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = "collapse"),
    
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Overview: Longitudinal Study Dashboard",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("This database driven dashboard provides tools for exploring and analyzing longitudinal survey data."),
                  p("Select a module from the sidebar or below to begin."),
                  helpText("Note: Only the green modules are currently functional.")
                )
              ),
              fluidRow(
                # style = "display: flex;",
                
                # Database design box
                box(
                  title = "Database Design",
                  width = 4,
                  height = "90%",
                  status = "success",
                  p("Understand the structure and organization of the database."),
                  actionButton("goto_database_design", "Explore Database Design", 
                               class = "btn-block btn-success")
                ),
                
                # Missing data box
                box(
                  title = "Missing Data",
                  width = 4,
                  height = "90%",
                  status = "success",
                  p("Visualize and analyze patterns of missing data across participants and time."),
                  actionButton("goto_missing", "Explore Missing Data", 
                               class = "btn-block btn-success")
                ),
                
                # Psychometrics box
                box(
                  title = "Psychometrics",
                  width = 4,
                  height = "90%",
                  status = "success",
                  p("Evaluate the psychometric properties of your measures."),
                  actionButton("goto_psychometrics", "Explore Psychometrics", 
                               class = "btn-block btn-success")
                )
              ),
              fluidRow(
                # Descriptives box
                box(
                  title = "Descriptive Statistics",
                  width = 4,
                  height = "90%",
                  # status = "primary",
                  p("View descriptive statistics and distributions of your measures over time."),
                  helpText("Note: Module under construction"),
                  actionButton("goto_descriptives", "Explore Descriptives",
                               class = "btn-block")
                ),
                # Models box
                box(
                  title = "Effects Over Time",
                  width = 4,
                  height = "90%",
                  # status = "primary",
                  p("Analyze the effects of independent variables over time using mixed models."),
                  helpText("Note: Module under construction"),
                  actionButton("goto_models", "Explore Models",
                               class = "btn-block")
                ),
                # Manage Participants box
                box(
                  title = "Manage Participants",
                  width = 4,
                  height = "90%",
                  status = "success",
                  p("Enroll new participants or delete existing ones."),
                  actionButton("goto_manage_participants", "Manage Participants", 
                               class = "btn-block btn-success")
                )
              )
              
      ),
      
      tabItem(tabName = "database_design",
              databaseDesignUI("database_design")
      ),
      
      # Missing Data tab
      tabItem(tabName = "missing_data",
              missingDataUI("missing_data")
      ),
      
      # Psychometrics tab
      tabItem(tabName = "psychometrics",
              psychometricsUI("psychometrics")
      ),
      
      # Descriptives tab
      tabItem(tabName = "descriptives",
              # This is where the descriptives UI module would be called
              # descriptivesUI("descriptives")
              h2("Descriptive Statistics")
              # The actual module will be added once it's created
      ),
      
      # Models tab
      tabItem(tabName = "models",
              # This is where the models UI module would be called
              # modelsUI("models")
              h2("Statistical Models")
              # The actual module will be added once it's created
      ),
      
      # Data Import tab
      tabItem(tabName = "manage_participants",
              dataImportUI("manage_participants")
      ),
    
      # Test db connection
      tabItem(tabName = "connection_test",
              connectionTestUI("connection_test")
      )
    )
  )
)