# R/modules/psychometrics.R

# Global Variables ----

names_item_stats <- list(
  raw.r = "The correlation of each item with the total score, not corrected for item overlap.",
  std.r = "The correlation of each item with the total score (not corrected for item overlap) if the items were all standardized",
  r.cor = "Item whole correlation corrected for item overlap and scale reliability",
  r.drop = "Item whole correlation for this item against the scale without this item"
  # mean = "The mean score of each item"
)

irt_model_types <- c(
  "Rasch" = "Rasch Model (1PL)",
  "2PL" = "2-Parameter Logistic",
  "3PL" = "3-Parameter Logistic"
)

irt_fit_indices <- list(
  AIC = "Akaike Information Criterion - Lower values indicate better fit",
  BIC = "Bayesian Information Criterion - Lower values indicate better fit",
  SABIC = "Sample-size Adjusted BIC - Lower values indicate better fit",
  HQ = "Hannan-Quinn - Lower values indicate better fit",
  logLik = "Log-Likelihood - Higher values indicate better fit"
)


# UI Component
psychometricsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Select Measure ----
    fluidRow(
      column(
        width = 12,
        box(
          title = "Browse Available Measures", 
          id = ns("measures_box"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          markdown("**Instructions:** To begin, click on a row in the table. Use the dropdown menus to filter, or click on the column names to sort the data. Scroll down or minimize the box (upper right hand corner) to explore the measure"),
          
          # Main table for selection
          withSpinner(
            reactableOutput(ns("measures_table"))
          )
        )
      )
    ),
    
    # Selected Measure Data ----
    fluidRow(
      column(
        width = 12,
        box(
          title = "Explore Measure",
          width = NULL,
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "warning",
          
          # Conditional UI based on whether a measure is selected
          uiOutput(ns("measure_exploration_ui"))
        )
      )
    )
  )
}

# Server Component
psychometricsServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # For debugging - print when the module initializes
    # message("Psychometrics module initialized")
    
    # Selected Measure Data ----
    selected_measure_data <- reactive({
      selected_row <- getReactableState("measures_table", "selected")
      req(selected_row)
      return(measures_data()[selected_row, ])
    })
    
    # Automatically collapse the measures box when a measure is selected
    observeEvent(getReactableState("measures_table", "selected"), {
      js$collapse(session$ns("measures_box"))
    })
    
    # 3. Add this reactive value at the beginning of your server function to track button clicks:
    lastModelRun <- reactiveVal(0)
    
    # 4. Add this observer to update the lastModelRun value when the button is clicked:
    observeEvent(input$run_irt_analysis, {
      lastModelRun(input$run_irt_analysis)
    })
    
    # UI ----
    
    # Conditional UI for measure exploration
    output$measure_exploration_ui <- renderUI({
      ns <- session$ns
      
      if(is.null(getReactableState("measures_table", "selected"))){
        # Show instructions when no measure is selected
        tagList(
          div(
            class = "text-center",
            style = "padding: 40px 20px;",
            icon("arrow-up", class = "fa-3x text-muted"),
            h3("No Measure Selected"),
            p("Please select a row in the measures table above to begin exploring."),
            p("You'll be able to view measure items, participant responses, and psychometric properties.")
          )
        )
      } else {
        # Show full content when a measure is selected
        tagList(
          # h3(textOutput(ns("selected_measure_title"))),
          # h4(textOutput(ns("selected_measure_info"))),
          # br(),
          tabsetPanel(
            width = NULL,
            selected = "Psychometric Analyses",
            
            header = tagList(
              h3(textOutput(ns("selected_measure_title"))),
              h4(textOutput(ns("selected_measure_info"))),
              br()
            ),
            
            tabPanel(
              "Measure Items",
              markdown("**Instructions:** Click on the arrows to the left of the variable names to view response options and scoring values."),
              withSpinner(reactableOutput(ns("measure_items_table")))
            ),
            tabPanel(
              "Participant Responses",
              withSpinner(reactableOutput(ns("participant_data_table")))
            ),
            ## Psychometric UI -----
            ### Likert ----
            tabPanel(
              "Psychometric Analyses",
              # This is where the psychometric analysis output will go.
              # Placeholder for now until we build it out
              fluidRow(
                column(
                  width = 12,
                  if(selected_measure_data()$scoring == "Likert"){
                    
                    tagList(
                      fluidRow(
                        valueBox(
                          subtitle = "Chronbach's Alpha",
                          value = alpha_obj()$total$raw_alpha %>% scales::number(accuracy = .01), 
                          icon = icon("chart-simple"),
                          color = "light-blue"
                        ),
                        valueBox(
                          subtitle = "Mean Score",
                          value = alpha_obj()$total$mean %>% scales::number(accuracy = .01), 
                          icon = icon("chart-simple"),
                          color = "light-blue"
                        ),
                        valueBox(
                          subtitle = "Standard Deviation",
                          value = alpha_obj()$total$sd %>% scales::number(accuracy = .01), 
                          icon = icon("chart-simple"),
                          color = "light-blue"
                        )
                      ), 
                      
                      
                      fluidRow(
                        box(
                          title = "Drop Statistics",
                          solidHeader = TRUE,
                          status = "primary",
                          width = 6,
                          plotOutput(ns("alpha_drop_plot"))
                        ),
                        box(
                          title = "Item Statistics",
                          solidHeader = TRUE,
                          status = "primary",
                          width = 6,
                          selectInput(
                            ns("alpha_item_plot_x_var"),
                            label = "Select X-Axis Variable",
                            choices = names_item_stats %>% names(),
                            selected = "r.drop"
                          ),
                          plotOutput(ns("alpha_item_plot"))
                        )
                      )
                    )
                    
                    
                  } else {
                    ### IRT ----
                    tagList(
                      fluidRow(
                        column(
                          width = 12,
                          box(
                            title = "Binary IRT Model Specification",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            fluidRow(
                              column(
                                width = 4,
                                selectInput(
                                  ns("irt_model_type"),
                                  "Select IRT Model Type",
                                  choices = irt_model_types,
                                  selected = "Rasch"
                                ),
                                helpText("For binary (0/1) response data only")
                              ),
                              column(
                                width = 4,
                                actionButton(
                                  ns("run_irt_analysis"),
                                  "Run IRT Analysis",
                                  icon = icon("calculator"),
                                  class = "btn-primary",
                                  style = "margin-top: 25px;"
                                )
                              )
                            )
                          )
                        )
                      ),
                      
                      # Results will only appear after the button is clicked
                      conditionalPanel(
                        condition = "input.run_irt_analysis > 0",
                          ns = ns,
                        
                        fluidRow(
                          column(
                            width = 12,
                            box(
                              title = "Model Fit",
                              status = "primary", 
                              solidHeader = TRUE,
                              width = NULL,
                              
                              fluidRow(
                                column(
                                  width = 6,
                                  h4("Model Information"),
                                  box(
                                    width = NULL,
                                    status = "warning",
                                    style = "height: 462px; overflow-y: scroll;",
                                    verbatimTextOutput(ns("irt_model_summary"))
                                  )
                                ),
                                column(
                                  width = 6,
                                  h4("Model Fit Indices"),
                                  box(
                                    width = NULL,
                                    status = "warning",
                                    verbatimTextOutput(ns("irt_fit_indices"))
                                  )
                                )
                              )
                            )
                          )
                        ),
                        
                        fluidRow(
                          column(
                            width = 6,
                            box(
                              title = "Item Parameter Estimates",
                              status = "primary",
                              solidHeader = TRUE,
                              width = NULL,
                              withSpinner(tableOutput(ns("irt_parameter_table"))),
                              style = "height: 420px; overflow-y: scroll;"
                            )
                          ),
                          column(
                            width = 6,
                            box(
                              title = "Item Characteristic Curves",
                              status = "primary",
                              solidHeader = TRUE,
                              width = NULL,
                              withSpinner(plotOutput(ns("item_characteristic_curves")))
                            )
                          )
                        ),
                        
                        fluidRow(
                          column(
                            width = 6,
                            box(
                              title = "Item Information Functions",
                              status = "primary",
                              solidHeader = TRUE,
                              width = NULL,
                              withSpinner(plotOutput(ns("item_information_plot")))
                            )
                          ),
                          column(
                            width = 6,
                            box(
                              title = "Test Information Function",
                              status = "primary",
                              solidHeader = TRUE,
                              width = NULL,
                              withSpinner(plotOutput(ns("test_information_plot")))
                            )
                          )
                        ),
                        
                        fluidRow(
                          column(
                            width = 12,
                            box(
                              title = "Person Ability Estimates",
                              status = "primary",
                              solidHeader = TRUE,
                              width = NULL,
                              fluidRow(
                                column(
                                  width = 6,
                                  withSpinner(plotOutput(ns("person_ability_distribution")))
                                ),
                                column(
                                  width = 6,
                                  withSpinner(reactableOutput(ns("person_ability_table")))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  }
                  
                )
              )
            )
          )
        )
      }
    })
    
    # SQL Queries ----
    ## Measures Data ----
    measures_data <- reactive({
      req(pool)
      
      # To track when this function is called:
      message("measures_data reactive function called")
      
      query <- "
      SELECT 
        m.measure_id,
        m.construct_name AS construct,
        m.instrument,
        m.measure_type,
        m.scoring,
        er.expected_response_id,
        er.days_from_start,
        s.survey_name AS survey,
        s.administration_method,
        s.recording_device,
        COUNT(DISTINCT sr.survey_response_id) AS response_count
      FROM 
        measure m
      JOIN 
        survey_measure sm ON m.measure_id = sm.measure_id
      JOIN 
        survey s ON sm.survey_id = s.survey_id
      JOIN 
        expected_response er ON s.survey_id = er.survey_id
      LEFT JOIN 
        survey_response sr ON er.expected_response_id = sr.expected_response_id
      GROUP BY 
        m.measure_id, 
        er.expected_response_id
      ORDER BY 
        m.construct_name, 
        m.instrument, 
        er.days_from_start"
      
      tryCatch({
        result <- dbGetQuery(pool, query)
        return(result)
      }, error = function(e) {
        showNotification(
          paste("Error fetching measures data:", e$message),
          type = "error",
          duration = NULL
        )
        return(data.frame())
      })
    })
    
    ## Selected measure items ----
    selected_measure_items <- reactive({
      req(pool)
      req(selected_measure_data())
      
      selected_data <- selected_measure_data()
      measure_id <- selected_data$measure_id
      
      tryCatch({
        # Instead of calling the stored procedure directly, use a regular SQL query
        # that accomplishes the same task
        query <- paste0("
      SELECT 
        i.item_id,
        CONCAT(m.variable_stem, '_', i.item_number) AS variable_name,
        i.text_prompt,
        ro.label AS response_option_label,
        ro.value AS response_option_value,
        i.is_reverse_scored
    FROM 
        `item` i
    JOIN 
        `measure` m ON i.measure_id = m.measure_id
    JOIN 
        `item_response_option` iro ON i.item_id = iro.item_id AND i.measure_id = iro.measure_id
    JOIN 
        `response_option` ro ON iro.response_option_id = ro.response_option_id
    WHERE 
        i.measure_id = ", measure_id, " 
    ORDER BY 
        i.item_id, ro.value;
    ")
        
        result <- dbGetQuery(pool, query)
        message("Successfully retrieved ", nrow(result), " items for measure_id: ", measure_id)
        return(result)
      }, error = function(e) {
        message("Error retrieving measure items: ", e$message)
        showNotification(
          paste("Error retrieving measure items:", e$message),
          type = "error"
        )
        return(data.frame())
      })
    })
    
    ## Participant data ----
    participant_data <- reactive({
      req(pool)
      req(selected_measure_data())
      
      selected_data <- selected_measure_data()
      measure_id <- selected_data$measure_id
      expected_response_id <- selected_data$expected_response_id
      
      tryCatch({
        # Instead of calling the stored procedure directly, use a regular SQL query
        # that accomplishes the same task
        query <- paste0("
      SELECT 
          sr.participant_id,
          sr.survey_response_id,
          CONCAT(m.variable_stem, '_', i.item_number) AS variable_name,
          ro.value
      FROM 
          item_response ir
      JOIN 
          survey_response sr ON ir.survey_response_id = sr.survey_response_id
      JOIN 
          measure m ON ir.measure_id = m.measure_id
      JOIN 
          item i ON ir.item_id = i.item_id AND ir.measure_id = i.measure_id
      JOIN 
          response_option ro ON ir.response_option_id = ro.response_option_id
      WHERE 
          ir.measure_id = ", measure_id, "
          AND sr.expected_response_id = ", expected_response_id, "
      ORDER BY
          sr.participant_id,
          sr.survey_response_id,
          i.item_number;
    ")
        
        result <- dbGetQuery(pool, query) %>% 
          pivot_wider(
            names_from = variable_name,
            values_from = value
          )
        message("Successfully retrieved ", nrow(result), " rows of data for measure_id: ", measure_id, ", expected_response_id: ", expected_response_id)
        return(result)
      }, error = function(e) {
        message("Error retrieving participant data: ", e$message)
        showNotification(
          paste("Error retrieving participant data:", e$message),
          type = "error"
        )
        return(data.frame())
      })
    })
    
    # Render Text ----
    output$selected_measure_title <- renderText({
      
      if(is.null(selected_measure_data())){
        return("Select a measure to view data")
      }
      
      paste0(selected_measure_data()$construct, ": ", selected_measure_data()$instrument)
    })
    
    output$selected_measure_info <- renderText({
      selected_data <- selected_measure_data()
      
      paste0(selected_data$administration_method, " administration during the ",
             str_to_lower(selected_data$survey), " survey, ",
             selected_data$days_from_start, " days from enrollment")
    })
    
    
    # Render Reactables ----
    
    ## Measures table ----
    output$measures_table <- renderReactable({
      data <- measures_data()

      if (nrow(data) == 0) {
        return(reactable(
          data.frame(message = "No data available"),
          sortable = FALSE,
          filterable = FALSE,
          searchable = FALSE
        ))
      }

      reactable(
        data,
        filterable = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        selection = "single",
        onClick = "select",
        showSortable = TRUE,
        showSortIcon = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        elementId = "measures-select",
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        ),
        defaultColDef = colDef(
          filterInput = function(values, name) {
            tags$select(
              # Set to undefined to clear the filter
              onchange = sprintf("Reactable.setFilter('measures-select', '%s', event.target.value || undefined)", name),
              # "All" has an empty value to clear the filter, and is the default option
              tags$option(value = "", "All"),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          }
        ),
        columns = list(
          measure_id = colDef(show = FALSE),
          expected_response_id = colDef(show = FALSE),
          construct = colDef(
            name = "Construct"
            ),
          instrument = colDef(name = "Instrument", minWidth = 150),
          measure_type = colDef(name = "Measure Type"),
          scoring = colDef(name = "Scoring"),
          days_from_start = colDef(name = "Days from Start"),
          survey = colDef(name = "Survey", minWidth = 150),
          administration_method = colDef(name = "Administration Method"),
          recording_device = colDef(name = "Recording Device"),
          response_count = colDef(name = "Response Count")
        )
      )
    })
    
    
    ## Measure items table ----
    output$measure_items_table <- renderReactable({
      items <- selected_measure_items()
      req(items)
      
      
      if(nrow(items) == 0) {
        return(reactable(
          data.frame(message = "No items available for this measure"),
          sortable = FALSE,
          filterable = FALSE,
          searchable = FALSE
        ))
      }
      
      prompts <- items %>% 
        select(item_id, variable_name, text_prompt, is_reverse_scored) %>% 
        mutate(is_reverse_scored = if_else(is_reverse_scored == 1, "Yes", "No")) %>% 
        unique()
      
      reactable(
        prompts,
        filterable = FALSE,
        searchable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        details = function(index){
          responses <- items %>% 
            filter(variable_name == prompts[index, "variable_name"]) %>% 
            select(response_option_label, response_option_value)
          reactable(
            responses, 
            outlined = FALSE,
            resizable = TRUE,
            fullWidth = FALSE,
            style = "margin-left: 250px;",
            columns = list(
              response_option_value = colDef(name = "Response Value", 
                                             align = "left",
                                             minWidth = 150),
              response_option_label = colDef(name = "Response Label",
                                             minWidth = 300)
            )
          )
        },
        defaultPageSize = 10,
        columns = list(
          item_id = colDef(show = FALSE),
          variable_name = colDef(name = "Variable Name", maxWidth = 200),
          text_prompt = colDef(name = "Prompt", minWidth = 200),
          is_reverse_scored = colDef(name = "Reverse Scored?")
        )
      )
    })
    
    ## Participant data table ----
    output$participant_data_table <- renderReactable({
      p_data <- participant_data()
      req(p_data)
      
      message("I'm in the measure_items_table render function")
      
      if(nrow(p_data) == 0) {
        return(reactable(
          data.frame(message = "No data available for this measure"),
          sortable = FALSE,
          filterable = FALSE,
          searchable = FALSE
        ))
      }
      
      reactable(
        p_data,
        filterable = F,
        searchable = F,
        sortable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        defaultPageSize = 10,
        columns = list(
          survey_response_id = colDef(show = FALSE),
          participant_id = colDef(filterable = TRUE)
        )
      )
    })
    
    # Render Psychometric Elements ----
    ## Likert Measures ----
    # output$alpha_results <- renderPrint({
    #   p_data <- participant_data() %>% 
    #     select(-survey_response_id, -participant_id)
    #   req(p_data)
    #   
    #   psych::alpha(p_data)
    # })
    
    alpha_obj <- reactive({
      if(selected_measure_data()$scoring != "Likert"){
        return()
      }
      p_data <- participant_data() %>% 
        select(-survey_response_id, -participant_id)
      req(p_data)
      
      psych::alpha(p_data)
    })
    
    output$alpha_drop_plot <- renderPlot({
      req(alpha_obj())
      
      data <- alpha_obj()$alpha.drop
      measure_alpha <- alpha_obj()$total$raw_alpha
      
      p <- data %>% 
        rownames_to_column("item") %>% 
        mutate(across(item, ~ fct_reorder(., raw_alpha))) %>% 
        ggplot(mapping = aes(y = item, x = raw_alpha)) +
        geom_col(mapping = aes(fill = raw_alpha == max(raw_alpha))) +
        geom_vline(
          xintercept = measure_alpha,
          linewidth = 1.2,
          color = app_colors$danger, linetype = "dashed") +
        annotate(
          "text",
          x = measure_alpha + .01,
          y = 1,
          label = "Chronbach's Alpha: All Items",
          color = app_colors$danger,
          fontface = "bold",
          size = 5,
          angle = -90,
          vjust = 0,
          hjust = 1
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, .2))) +
        theme_longitudinal() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          legend.position = "none"
        ) +
        scale_fill_manual(
          values = c("TRUE" = app_colors$warning, "FALSE" = app_colors$primary)) +
        labs(
          title = "Alpha if item is dropped",
          x = NULL,
          y = NULL
        )
      
      return(p)
      
    })
    
    output$alpha_item_plot <- renderPlot({
      
      req(alpha_obj())
      req(input$alpha_item_plot_x_var)
      
      data <- alpha_obj()$item.stats
      x_var <- input$alpha_item_plot_x_var
      
      
      
      p <- data %>% 
        rownames_to_column("item") %>% 
        mutate(across(item, ~ fct_reorder(., .data[[x_var]]))) %>% 
        ggplot(mapping = aes(x = .data[[x_var]], y = item)) +
        geom_col(mapping = aes(fill = .data[[x_var]] == min(.data[[x_var]]))) +
        scale_x_continuous(expand = expansion(mult = c(0, .2))) +
        labs(
          title = names_item_stats[[x_var]] %>% str_wrap(width = 50),
          x = NULL,
          y = NULL
        ) +
        theme_longitudinal() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          legend.position = "none"
        ) +
        scale_fill_manual(
          values = c("TRUE" = app_colors$warning, "FALSE" = app_colors$primary)) 
      
      return(p)
      
    })
    
    ## Reactive expressions for IRT analysis ----
    irt_data <- reactive({
      if(selected_measure_data()$scoring != "Accuracy"){
        return(NULL)
      }
      
      req(participant_data())
      
      # Get the data without identifiers
      data <- participant_data() %>%
        select(-survey_response_id, -participant_id)
      
      # Ensure all variables are numeric and binary (0/1)
      data <- data %>%
        mutate(across(everything(), as.numeric)) %>%
        # Verify the data is binary
        mutate(across(everything(), ~ifelse(. %in% c(0,1), ., NA)))
      
      # Check if we have any non-binary data and warn
      if(any(is.na(data))) {
        showNotification(
          "Some values in your data are not binary (0/1). Non-binary values have been converted to NA.",
          type = "warning",
          duration = NULL
        )
      }
      
      return(data)
    })
    
    #### IRT model ----
    irt_model <- reactive({
      req(input$run_irt_analysis)
      req(irt_data())
      
      # Get the NAMES of the selected value, not the value itself
      # This is the key fix - we want "Rasch", "2PL", or "3PL" as they appear in the names
      selected_display_name <- isolate(input$irt_model_type)
      model_type <- names(irt_model_types)[irt_model_types == selected_display_name]
      
      if (length(model_type) == 0) {
        # Default to Rasch if we can't match
        model_type <- "Rasch"
        message("Could not match model type '", selected_display_name, "', defaulting to Rasch")
      } else {
        message("Selected model type: ", selected_display_name, ", using model: ", model_type)
      }
      
      # Show a notification while model is running
      id <- showNotification(
        paste("Fitting", model_type, "model... This may take a moment."),
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      
      # Clean up notification when reactive completes
      on.exit(removeNotification(id), add = TRUE)
      
      # Fit the model with tryCatch to handle errors
      tryCatch({
        # Check data requirements first - make sure we have binary data
        data_check <- apply(irt_data(), 2, function(x) {
          vals <- unique(x[!is.na(x)])
          all(vals %in% c(0,1)) && length(vals) == 2
        })
        
        if(!all(data_check)) {
          stop("All items must have exactly two response categories (0 and 1) for binary IRT models.")
        }
        
        # For binary IRT, we're sticking with unidimensional models
        # IMPORTANT: mirt is very specific about itemtype parameter
        # Use explicit string values that match mirt's expectations
        if(model_type == "Rasch") {
          message("Fitting Rasch model")
          model <- mirt::mirt(irt_data(), 1, itemtype = "Rasch", verbose = FALSE)
        } else if(model_type == "2PL") {
          message("Fitting 2PL model")
          model <- mirt::mirt(irt_data(), 1, itemtype = "2PL", verbose = FALSE)
        } else if(model_type == "3PL") {
          message("Fitting 3PL model")
          model <- mirt::mirt(irt_data(), 1, itemtype = "3PL", verbose = FALSE)
        } else {
          # Default to Rasch if something unexpected happens
          message("Unrecognized model type:", model_type, ", defaulting to Rasch model")
          model <- mirt::mirt(irt_data(), 1, itemtype = "Rasch", verbose = FALSE)
        }
        
        # Return the fitted model
        return(model)
        
      }, error = function(e) {
        message("Error in IRT model fitting: ", e$message)
        showNotification(
          paste("Error fitting IRT model:", e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })
    })
    
    # Parameter estimates table
    output$irt_parameter_table <- renderTable({
      req(irt_model())
      
      # Extract the parameters
      params <- mirt::coef(irt_model(), simplify = TRUE)$items
      
      # For better readability, round and rename
      params <- as.data.frame(params) %>%
        rownames_to_column("Item") %>%
        mutate(across(where(is.numeric), ~ round(., 3)))
      
      # Get the actual model type same way as in irt_model reactive
      selected_display_name <- isolate(input$irt_model_type)
      model_type <- names(irt_model_types)[irt_model_types == selected_display_name]
      
      if (length(model_type) == 0) {
        model_type <- "Rasch"  # Default
      }
      
      # Handle different parameter names by model type
      if (model_type == "Rasch") {
        params <- params %>%
          select(Item, b = d, a = a1) %>%
          mutate(a = 1) # In 1PL, a is fixed at 1
      } else if (model_type == "2PL") {
        params <- params %>%
          select(Item, a = a1, b = d) %>%
          mutate(b = -b/a) # Convert to usual parameterization
      } else if (model_type == "3PL") {
        params <- params %>%
          select(Item, a = a1, b = d, c = g) %>%
          mutate(b = -b/a)
      }
      
      return(params)
    })
    
    # Model summary output
    output$irt_model_summary <- renderPrint({
      req(input$run_irt_analysis)  # Require button press
      req(irt_model())            # Require model to be available
      summary(irt_model())
    })
    
    # Fit indices
    output$irt_fit_indices <- renderPrint({
      req(irt_model())
      fit_stats <- mirt::M2(irt_model())
      fit_indices <- mirt::anova(irt_model())
      
      # Create formatted output
      cat("M2 Statistic:\n")
      print(fit_stats)
      cat("\n\nFit Indices:\n")
      print(fit_indices)
    })
    
    # Item Characteristic Curves
    output$item_characteristic_curves <- renderPlot({
      req(irt_model())
      
      # Plot ICC with error handling
      tryCatch({
        # Plot with custom styling
        plot(irt_model(), type = "trace", 
             main = "Item Characteristic Curves",
             col = pal_cat_default,
             lwd = 2,
             cex.main = 1.2,
             cex.lab = 1.1)
        
      }, error = function(e) {
        # Create an empty plot with error message
        plot(0, 0, type = "n", 
             main = "Error Generating Item Characteristic Curves",
             xlab = "", ylab = "",
             xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, paste("Error:", e$message))
      })
    })
    
    # Item Information Functions
    output$item_information_plot <- renderPlot({
      req(irt_model())
      
      # Plot item information with error handling
      tryCatch({
        plot(irt_model(), type = "infotrace", 
             main = "Item Information Functions",
             col = pal_cat_default,
             lwd = 2,
             cex.main = 1.2,
             cex.lab = 1.1)
      }, error = function(e) {
        # Create an empty plot with error message
        plot(0, 0, type = "n", 
             main = "Error Generating Item Information Functions",
             xlab = "", ylab = "",
             xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, paste("Error:", e$message))
      })
    })
    
    # Test Information Function
    output$test_information_plot <- renderPlot({
      req(irt_model())
      
      # Plot test information with error handling
      tryCatch({
        plot(irt_model(), type = "info", 
             main = "Test Information Function",
             col = app_colors$primary,
             lwd = 3,
             cex.main = 1.2,
             cex.lab = 1.1)
        
        # Add standard error curve
        plot(irt_model(), type = "SE", col = "red", lwd = 3, add = TRUE)
        
        # Add legend
        legend("topright", 
               legend = c("Information", "Standard Error"),
               col = c(app_colors$primary, "red"),
               lwd = 3,
               bty = "n")
        
      }, error = function(e) {
        # Create an empty plot with error message
        plot(0, 0, type = "n", 
             main = "Error Generating Test Information Function",
             xlab = "", ylab = "",
             xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, paste("Error:", e$message))
      })
    })
    
    # Person ability estimates
    person_abilities <- reactive({
      req(irt_model())
      req(participant_data())
      
      # Get theta estimates
      thetas <- mirt::fscores(irt_model())
      
      # Join with participant IDs
      ability_data <- data.frame(
        participant_id = participant_data()$participant_id,
        survey_response_id = participant_data()$survey_response_id,
        thetas
      )
      
      return(ability_data)
    })
    
    # Ability distribution plot
    output$person_ability_distribution <- renderPlot({
      req(person_abilities())
      
      abilities <- person_abilities()
      
      # Get ability values (only F1 since we're using unidimensional models)
      theta_col <- "F1"
      
      # Create histogram of ability estimates
      ggplot(abilities, aes_string(x = theta_col)) +
        geom_histogram(fill = app_colors$primary, color = "white", bins = 20) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        scale_x_continuous(limits = c(-4, 4)) +
        labs(
          title = "Distribution of Person Ability Estimates",
          x = "Ability (θ)",
          y = "Count"
        ) +
        theme_longitudinal()
    })
    
    # Ability estimates table
    output$person_ability_table <- renderReactable({
      req(person_abilities())
      
      abilities <- person_abilities()
      
      # Format for display
      ability_table <- abilities %>%
        mutate(across(starts_with("F"), ~ round(., 3))) %>%
        rename_with(~ gsub("F", "Factor_", .), starts_with("F"))
      
      reactable(
        ability_table,
        filterable = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultPageSize = 10,
        columns = list(
          participant_id = colDef(name = "Participant ID"),
          survey_response_id = colDef(name = "Response ID", show = FALSE)
        )
      )
    })

  })
}