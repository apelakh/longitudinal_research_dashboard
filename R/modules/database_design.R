# R/modules/database_design.R

# UI Component
databaseDesignUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Database Schema",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        tabsetPanel(
          id = ns("db_tabs"),
          
          # EER Diagram Tab
          tabPanel("Database Diagram", 
                   br(),
                   p("This diagram shows the relationships between tables in the longitudinal survey database."),
                   div(style = "text-align: center;",
                       div(style = "max-width: 1040px; margin: 0 auto;",
                           img(src = "images/eer_diagram.png", 
                               style = "width: 100%;",
                               alt = "Database EER Diagram")
                       )
                   )
          ),
          
          # Table Description Tabs
          tabPanel("Materials", uiOutput(ns("materials_desc"))),
          tabPanel("Study Design", uiOutput(ns("study_design_desc"))),
          tabPanel("Enrollment", uiOutput(ns("enrollment_desc"))),
          tabPanel("Study Data", uiOutput(ns("study_data_desc")))
        )
      )
    )
  )
}

# Server logic
databaseDesignServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Materials tables description
    output$materials_desc <- renderUI({
      tagList(
        h4("Assessment Materials"),
        p("This section describes the tables that store information about the assessment instruments used in the study."),
        
        h5("Table: `measure`"),
        p("Stores information about psychological instruments and constructs:"),
        tags$ul(
          tags$li(strong("measure_id:"), "Unique identifier for each measure"),
          tags$li(strong("construct_name:"), "Name of the psychological construct being measured (e.g., depression, anxiety)"),
          tags$li(strong("instrument:"), "Name of the specific assessment instrument (e.g., Beck's Depression Inventory)"),
          tags$li(strong("measure_type:"), "Categorization of the measure (e.g., self-report, skill assessment)"),
          tags$li(strong("scoring:"), "Method used for scoring responses (e.g., Likert-type ratings)"),
          tags$li(strong("variable_stem:"), "Prefix used for variable naming in the raw data (e.g., 'bdi' for Beck's Depression Inventory items)")
        ),
        
        h5("Table: `item`"),
        p("Contains individual questions or prompts that make up each measure:"),
        tags$ul(
          tags$li(strong("item_id:"), "Unique identifier for each item"),
          tags$li(strong("measure_id:"), "Foreign key linking to the `measure` table"),
          tags$li(strong("text_prompt:"), "The actual text of the question presented to participants"),
          tags$li(strong("item_number:"), "Position or number of the item within its measure"),
          tags$li(strong("is_reverse_scored:"), "Boolean indicating whether higher values represent lower levels of the construct")
        ),
        
        h5("Table: `response_option`"),
        p("Defines the possible answer choices for items:"),
        tags$ul(
          tags$li(strong("response_option_id:"), "Unique identifier for each response option"),
          tags$li(strong("value:"), "Numeric value associated with the response (used in scoring)"),
          tags$li(strong("label:"), "Text shown to participants (e.g., 'Strongly agree')")
        ),
        
        h5("Table: `item_response_option`"),
        p("Junction table defining which response options are available for each item:"),
        tags$ul(
          tags$li(strong("item_id:"), "Foreign key linking to the `item` table"),
          tags$li(strong("measure_id:"), "Foreign key linking to the `measure` table"),
          tags$li(strong("response_option_id:"), "Foreign key linking to the `response_option` table")
        ),
        p("This structure allows for flexible response options across different items and measures.")
      )
    })
    
    # Study Design tables description
    output$study_design_desc <- renderUI({
      tagList(
        h4("Study Design"),
        p("This section describes the tables that define the structure and schedule of data collection."),
        
        h5("Table: `survey`"),
        p("Defines the survey instruments used to collect data:"),
        tags$ul(
          tags$li(strong("survey_id:"), "Unique identifier for each survey"),
          tags$li(strong("survey_name:"), "Name of the survey as presented to researchers or participants"),
          tags$li(strong("administration_method:"), "How the survey is administered (e.g., lab, self-administered, experience sampling)"),
          tags$li(strong("recording_device:"), "Device used to record responses (e.g., computer, mobile)"),
          tags$li(strong("is_repeated:"), "Boolean indicating whether participants complete this survey multiple times")
        ),
        
        h5("Table: `expected_response`"),
        p("Defines the schedule of when participants should complete surveys:"),
        tags$ul(
          tags$li(strong("expected_response_id:"), "Unique identifier for each scheduled survey"),
          tags$li(strong("days_from_start:"), "Number of days after enrollment when this survey should be completed"),
          tags$li(strong("survey_id:"), "Foreign key linking to the `survey` table")
        ),
        p("This table enables tracking of survey completion rates and timing."),
        
        h5("Table: `survey_measure`"),
        p("Junction table that defines which measures are included in each survey:"),
        tags$ul(
          tags$li(strong("survey_id:"), "Foreign key linking to the `survey` table"),
          tags$li(strong("measure_id:"), "Foreign key linking to the `measure` table")
        ),
        p("This structure allows for measures to be reused across different surveys and enables flexible survey composition.")
      )
    })
    
    # Enrollment tables description
    output$enrollment_desc <- renderUI({
      tagList(
        h4("Participant Enrollment"),
        p("This section describes the tables related to participant recruitment and experimental design."),
        
        h5("Table: `wave`"),
        p("Tracks cohorts of participants enrolled at different times:"),
        tags$ul(
          tags$li(strong("wave_id:"), "Unique identifier for each enrollment cohort"),
          tags$li(strong("start_date:"), "Date when this enrollment wave began")
        ),
        p("The wave structure allows for analysis of cohort effects and helps organize the relative timing of data collection across participants."),
        
        h5("Table: `participant`"),
        p("Stores information about study participants:"),
        tags$ul(
          tags$li(strong("participant_id:"), "Unique identifier for each person enrolled in the study"),
          tags$li(strong("experimental_condition:"), "The experimental condition or group assignment"),
          tags$li(strong("wave_id:"), "Foreign key linking to the `wave` table, indicating when they were enrolled")
        ),
        p("This table serves as the central reference point for all participant-related data and enables between-subject analyses based on experimental conditions.")
      )
    })
    
    # Study Data tables description
    output$study_data_desc <- renderUI({
      tagList(
        h4("Study Data"),
        p("This section describes the tables that store the actual response data collected from participants."),
        
        h5("Table: `survey_response`"),
        p("Records each instance of a participant completing a survey:"),
        tags$ul(
          tags$li(strong("survey_response_id:"), "Unique identifier for each completed survey instance"),
          tags$li(strong("participant_id:"), "Foreign key linking to the `participant` table"),
          tags$li(strong("expected_response_id:"), "Foreign key linking to the `expected_response` table"),
          tags$li(strong("start_date_time:"), "Timestamp when the participant began the survey"),
          tags$li(strong("end_date_time:"), "Timestamp when the participant completed the survey")
        ),
        p("This table enables tracking of survey completion, timing information, and serves as the parent record for all item responses within a survey."),
        
        h5("Table: `item_response`"),
        p("Stores individual answers to specific questions:"),
        tags$ul(
          tags$li(strong("item_response_id:"), "Unique identifier for each response to an item"),
          tags$li(strong("survey_response_id:"), "Foreign key linking to the `survey_response` table"),
          tags$li(strong("item_id:"), "Foreign key linking to the `item` table"),
          tags$li(strong("measure_id:"), "Foreign key linking to the `measure` table"),
          tags$li(strong("response_option_id:"), "Foreign key linking to the `response_option` table")
        ),
        p("This table contains the actual raw data points from participants' responses, which can be aggregated to compute measure scores and perform various analyses."),
        p("The database design allows for efficiently tracking missing data (when expected responses don't have corresponding survey responses) and enables complex longitudinal analyses of how responses change over time.")
      )
    })
  })
}