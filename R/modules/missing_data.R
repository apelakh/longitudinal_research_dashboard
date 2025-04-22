# R/modules/missing_data.R

# UI Component
missingDataUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Missing Survey Responses",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        
        tabsetPanel(
          selected = "Visualize",
          header = tagList(
            br(),
            div(
              style = "text-align: right; margin-right: 15px;",
              actionButton(
                ns("refresh_missing_data"), 
                "Refresh Data", 
                icon = icon("sync"), 
                class = "btn-sm"
              )
            )
          ),
          
          tabPanel(
            title = "Visualize",
            fluidRow(
              column(4,
                     selectInput(ns("fill_variable"), "Color by:",
                                 choices = c(
                                   "None" = "none",
                                   "Administration Method" = "administration_method",
                                   "Survey Name" = "survey_name",
                                   "Recording Device" = "recording_device",
                                   "Experimental Condition" = "experimental_condition",
                                   "Wave" = "wave_id"),
                                 selected = "none")
              ),
              column(4,
                     # Disabled this option because I didn't want surveys to overlap
                     # checkboxInput(ns("group_by_device"), "Separate by Recording Device", TRUE)
              ),
              column(4,
                     div(
                       style = "text-align: right; margin-right: 15px; margin-top: 15px;", 
                       downloadButton(ns("download_plot"), 
                                      "Download Plot", class = "btn-sm")
                       )
              )
            ),
            
            br(),
            
            plotOutput(ns("missing_data_plot"), height = "600px") %>% 
              withSpinner(type = 4)
          ),
          
          tabPanel(
            title = "Raw Data",
            markdown("Raw data from saved view: **vw_missing_data** used to generate the plots and summaries."),
            DT::DTOutput(ns("missing_data_table")) %>% 
              withSpinner(type = 4)
          ),
          
          tabPanel(
            title = "Participant Overview",
            markdown("Response patterns by participant from saved view: **vw_participant_overview**."),
            DT::DTOutput(ns("participant_overview_table")) %>% 
              withSpinner(type = 4)
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Response Rate Summary",
        width = 6,
        status = "warning",
        solidHeader = FALSE,
        
        plotOutput(ns("response_rate_plot"), height = "300px") %>%
          withSpinner(type = 4)
      ),
      
      box(
        title = "Completion Statistics",
        width = 6,
        status = "warning",
        solidHeader = FALSE,
        selectInput(
          inputId = ns("summary_grouping_vars"),
          label = "Choose a variable (or multiple variables) to group by:",
          choices = list(
            "Participant ID" = "participant_id",
            "Administration Method" = "administration_method",
            "Survey Name" = "survey_name",
            "Recording Device" = "recording_device",
            "Experimental Condition" = "experimental_condition",
            "Wave" = "wave_id",
            "Days from Baseline" = "days_from_start"
          ),
          multiple = TRUE,
          selected = "survey_name"
        ),
        reactable::reactableOutput(ns("missing_data_summary")) %>%
          withSpinner(type = 4)
        
      )
    )
  )
}

# Server logic
missingDataServer <- function(id, pool, connection_active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    
    # Add reactive value for refresh trigger
    refresh_trigger <- reactiveVal(0)
    
    # Add observer for refresh button
    observeEvent(input$refresh_missing_data, {
      message("Missing data refresh triggered")
      showNotification("Refreshing missing data...", type = "message")
      refresh_trigger(refresh_trigger() + 1)
    })
    
    # Fetch missing data from the view with connection handling
    missing_data <- reactive({
      req(pool)
      
      # Use the global connection status if available
      if (!connection_active()) {
        return(NULL)
      }
      
      # Add dependency on refresh trigger
      force(refresh_trigger())
      
      tryCatch({
        pool::poolWithTransaction(pool, function(conn) {
          DBI::dbGetQuery(conn, "SELECT * FROM vw_missing_data") %>% 
            mutate(survey_name = factor(survey_name, levels = c("Baseline", "Posttest", "Follow-up", "Daily Diary")))
        })
      }, error = function(e) {
        showNotification(
          paste("Error fetching missing data:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
    })
    
    # Generate missing data DT ----
    output$missing_data_table <- DT::renderDT({
      req(missing_data())
      
      
      missing_data() %>% 
        mutate(across(c(experimental_condition, administration_method, recording_device, wave_id), factor)) %>% 
      datatable(
        rownames = FALSE,
        filter = 'top',
        selection = 'none'
      ) %>% return()
    })
    
    output$missing_data_summary <- reactable::renderReactable({
      message("Group columns: ", input$summary_grouping_vars)
      
      req(missing_data())
      req(input$summary_grouping_vars)
      
      group_by_cols <- input$summary_grouping_vars
      
      missing_data() %>%
        group_by(across(all_of(group_by_cols))) %>%
        summarise(Completion = mean(is_complete), .groups = "drop") %>%
        reactable(
          columns = list(
            Completion = colDef(
              format = colFormat(percent = TRUE, digits = 2)
              )
            ),
          defaultSorted = list(Completion = "desc"),
          filterable = FALSE,
          searchable = TRUE,
          bordered = TRUE,
          striped = TRUE,
          highlight = TRUE,
          wrap = FALSE,
          paginationType = "jump",
          defaultPageSize = 10,
          showPageSizeOptions = TRUE
        ) %>% return()
    })
    
     # Fetch participant overview data ----
    participant_overview <- reactive({
      req(pool)
      
      # Use the global connection status if available
      if (!connection_active()) {
        return(NULL)
      }
      
      # Add dependency on refresh trigger
      force(refresh_trigger())
      
      tryCatch({
        pool::poolWithTransaction(pool, function(conn) {
          DBI::dbGetQuery(conn, "SELECT * FROM vw_participant_overview") %>% 
            mutate(across(enrollment_date, as.Date))
        })
      }, error = function(e) {
        showNotification(
          paste("Error fetching participant overview:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
    })
    
    # Generate participant overciew DT ----
    output$participant_overview_table <- DT::renderDT({
      req(participant_overview())
      participant_overview() %>% 
        mutate(across(c(experimental_condition, enrollment_date), factor)) %>% 
        DT::datatable(
          rownames = FALSE,
          filter = 'top',
          selection = 'none'
        ) %>% return()
    })
    
    # Generate the missing data tile plot -----
    output$missing_data_plot <- renderPlot({
      req(missing_data())
      
      df <- missing_data() %>% 
        mutate(across(c(ends_with("id"), days_from_start), factor))
      
      # Basic plot structure
      p <- ggplot(df, aes(x = days_from_start, y = fct_rev(participant_id)))
      
      # Add fill if selected
      if (input$fill_variable == "none") {
        # When no fill is selected, use a default color
        p <- p + geom_raster(fill = col_default, aes(alpha = factor(is_complete)))
      }
      
      # Complete the plot with appropriate geoms and scales
      if (input$fill_variable != "none") {
        p <- p + geom_raster(mapping = aes(fill = .data[[input$fill_variable]], 
                                           alpha = factor(is_complete))) 
        
        # Add appropriate color scales
        n_values <- length(unique(df[[input$fill_variable]]))
        p <- p + 
          scale_fill_manual(values = pal_cat_default[1:n_values]) +
          scale_color_manual(values = pal_cat_default[1:n_values])
      }
      
      # Facet by recording device (checkbox disabled)
      p <- p + 
        facet_wrap(~ recording_device, ncol = 1) 

      
      # Add formatting and labels
      p <- p + 
        labs(
          title = "Participant Survey Completion Patterns",
          x = "Days from Study Start",
          y = "Participant ID",
          fill = str_to_title(gsub("_", " ", input$fill_variable))
        ) +
        theme_longitudinal() +
        theme(
          axis.text.y = element_blank(),  # Remove participant IDs
          axis.ticks.y = element_blank(),
          panel.grid = element_blank()    # Remove grid for tile plots
        ) +
        coord_cartesian(expand = FALSE) +
        scale_alpha_manual(values = c("0" = 0, "1" = 1), guide = 'none')
      
      p
    })
    
    # Generate response rate summary plot (horizontal bars) ----
    output$response_rate_plot <- renderPlot({
      req(missing_data())
      
      df <- missing_data() %>% 
        mutate(across(c(ends_with("id"), days_from_start), factor))
      
      lbl_size <- 5.5
      
      # Calculate response rates
      if (input$fill_variable != "none") {
        # Group by the fill variable
        summary_data <- df %>%
          group_by(.data[[input$fill_variable]]) %>%
          summarize(
            total = n(),
            completed = sum(is_complete),
            rate = completed / total * 100
          ) %>%
          # Reorder for horizontal bar chart
          mutate(label = sprintf("%.1f%%", rate)) %>%
          arrange(desc(rate))
        
        
        # For horizontal bars, x and y are flipped
        p <- ggplot(summary_data, aes(
          x = reorder(.data[[input$fill_variable]], rate), 
          y = rate, 
          fill = .data[[input$fill_variable]]
        )) +
          geom_col() +
          geom_text(aes(label = label), hjust = -0.2, size = lbl_size, fontface = "bold") +
          coord_flip() +  # Make bars horizontal
          xlim(rev(levels(factor(summary_data[[input$fill_variable]]))))  # Preserve order
        
        # Add appropriate color scales
        n_values <- length(unique(df[[input$fill_variable]]))
        p <- p + scale_fill_manual(values = pal_cat_default[1:n_values])
        
      } else {
        # Overall response rate
        total <- nrow(df)
        completed <- sum(df$is_complete)
        rate <- completed / total * 100
        
        summary_data <- data.frame(
          category = "Overall",
          rate = rate,
          label = sprintf("%.1f%%", rate)
        )
        
        p <- ggplot(summary_data, aes(x = category, y = rate)) +
          geom_col(fill = col_default) +
          geom_text(aes(label = label), hjust = -0.2, size = lbl_size, fontface = "bold") +
          coord_flip()   # Make bar horizontal
      }
      
      # Format the plot
      p <- p + 
        scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25), expand = expansion(mult = c(0, .2))) +
        labs(
          title = "Response Rate by Category",
          y = "Response Rate (%)",
          x = NULL
        ) +
        theme_longitudinal(base_size = 15) +
        theme(
          legend.position = "none",
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold")
        ) 
      
      p
    })
    
    # Generate completion statistics table
    output$completion_stats <- renderTable({
      req(missing_data())
      
      df <- missing_data()
      
      # Overall statistics
      total_expected <- nrow(df)
      total_completed <- sum(df$is_complete)
      overall_rate <- total_completed / total_expected * 100
      
      # Participant-level statistics
      participant_stats <- df %>%
        group_by(participant_id) %>%
        summarize(
          expected = n(),
          completed = sum(is_complete),
          rate = completed / expected * 100
        )
      
      perfect_completion <- sum(participant_stats$rate == 100)
      participants_with_zero <- sum(participant_stats$rate == 0)
      
      # Timepoint-level statistics
      timepoint_stats <- df %>%
        group_by(days_from_start) %>%
        summarize(
          expected = n(),
          completed = sum(is_complete),
          rate = completed / expected * 100
        )
      
      min_timepoint_rate <- min(timepoint_stats$rate)
      max_timepoint_rate <- max(timepoint_stats$rate)
      
      # Create summary table
      stats_table <- data.frame(
        Statistic = c(
          "Total Expected Responses",
          "Total Completed Responses",
          "Overall Completion Rate",
          "Participants with 100% Completion",
          "Participants with 0% Completion",
          "Lowest Timepoint Completion Rate",
          "Highest Timepoint Completion Rate"
        ),
        Value = c(
          as.character(total_expected),
          as.character(total_completed),
          sprintf("%.1f%%", overall_rate),
          sprintf("%d (%.1f%%)", perfect_completion, perfect_completion / length(unique(df$participant_id)) * 100),
          sprintf("%d (%.1f%%)", participants_with_zero, participants_with_zero / length(unique(df$participant_id)) * 100),
          sprintf("%.1f%%", min_timepoint_rate),
          sprintf("%.1f%%", max_timepoint_rate)
        )
      )
      
      stats_table
    }, striped = TRUE, hover = TRUE, spacing = 'l')
    
    # Download handler for the plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("missing_data_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
      },
      content = function(file) {
        req(missing_data())
        
        df <- missing_data() %>%
          mutate(participant_id = factor(participant_id),
                 days_from_start = factor(days_from_start))
        
        # Recreating the plot logic with high resolution settings
        p <- ggplot(df, aes(x = days_from_start, y = participant_id))
        
        if (input$fill_variable != "none") {
          p <- p + aes_string(fill = input$fill_variable) + geom_tile(color = "white")
          
          # Add appropriate color scales
          n_values <- length(unique(df[[input$fill_variable]]))
          p <- p + scale_fill_manual(values = pal_cat_default[1:n_values])
        } else {
          p <- p + geom_tile(fill = col_default, color = "white")
        }
        
        if (input$group_by_device) {
          p <- p + facet_wrap(~ recording_device, ncol = 1)
        }
        
        p <- p + 
          labs(
            title = "Participant Survey Completion Patterns",
            x = "Days from Study Start",
            y = "Participant ID",
            fill = str_to_title(gsub("_", " ", input$fill_variable))
          ) +
          theme_longitudinal(base_size = 16) +
          theme(
            axis.text.y = element_text(size = 9),
            panel.grid = element_blank()
          ) +
          coord_cartesian(expand = FALSE)
        
        # Save the plot
        ggsave(file, plot = p, width = 12, height = 10, units = "in", dpi = 300)
      }
    )
  })
}