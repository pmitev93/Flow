# ==============================================================================
# FLOW CYTOMETRY ANALYSIS - SHINY APP
# ==============================================================================

library(shiny)
library(flowCore)
library(tidyverse)
library(sp)
library(DT)
library(openxlsx)
library(sortable)

# Source the master script to load all functions
# Make sure master script path is correct!
source("R_Flow_Claude.r")  # Uncomment and adjust path
source("plotting_functions.r")
#source("Claude_testing_script.R") 
#source("Claude_testing_script2.R") 
#source("Final_analysis.R") 
#source("Analyze_everything.R") 

#To run it:
#shiny::runApp("/Users/petar.mitev/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Experiments/Flow/R_New/Flow_GUI.r")
#or
#shiny::runApp("Flow_GUI.r")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("The Mitev EdU Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Folder selection
      h4("1. Select Data Folder"),
      textInput("master_folder", "Master Folder Path:", 
                value = "Experiments/"),
      actionButton("browse_folder", "Browse...", class = "btn-secondary"),
      actionButton("load_experiments", "Load Experiments", 
                   class = "btn-primary"),
      hr(),
      
      # Experiment selection
      h4("2. Select Experiments"),
      uiOutput("experiment_selector"),
      fluidRow(
        column(6, actionButton("select_all", "Select All", class = "btn-sm")),
        column(6, actionButton("deselect_all", "Deselect All", class = "btn-sm"))
      ),
      br(),  # Add line break
      actionButton("analyze_selected", "Analyze Selected Experiments",
                   class = "btn-success"),
      hr(),
      
      # Sample browser
      h4("3. Browse Samples"),
      selectInput("selected_experiment", "Experiment:", choices = NULL),
      selectInput("selected_sample", "Sample:", choices = NULL),
      hr(),
      
      # Download
      h4("4. Download Results"),
      downloadButton("download_results", "Download Excel")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        # Welcome tab
        tabPanel("Welcome",
                 h3("Welcome to the Mitev EdU Analysis Tool"),
                 p("This tool processes flow cytometry data with automated gating and correlation analysis."),
                 h4("How to use:"),
                 tags$ol(
                   tags$li("Enter the path to your master folder containing experiment subfolders"),
                   tags$li("Click 'Load Experiments' to scan for experiments"),
                   tags$li("Select which experiments to analyze"),
                   tags$li("Click 'Analyze Selected Experiments' to process data"),
                   tags$li("View results in the 'Results Table' tab"),
                   tags$li("Browse individual gates in the 'Gate Inspection' tabs"),
                   tags$li("Download results as Excel file")
                 ),
                 hr(),
                 h4("Status:"),
                 verbatimTextOutput("status_text")
        ),
        
        # Results table
        tabPanel("Results Table",
                 h3("Correlation Results"),
                 DTOutput("results_table")
        ),
        
        # ADD THIS NEW TAB:
        tabPanel("Overview Plots",
                 h3("Experiment Overview"),
                 selectInput("overview_experiment", "Select Experiment:", 
                             choices = NULL),
                 selectInput("overview_gate", "Select Gate:", 
                             choices = c("Gate 1: Debris" = "gate1",
                                         "Gate 2: Singlets" = "gate2",
                                         "Gate 3: Live Cells" = "gate3",
                                         "Gate 4: S-phase Outliers" = "gate4",
                                         "Gate 5: FxCycle Quantile" = "gate5",
                                         "Gate 6: EdU + FxCycle" = "gate6",
                                         "Gate 7: HA-Positive" = "gate7",
                                         "Final: Correlation" = "correlation")),
                 plotOutput("overview_plot", height = "800px")
        ),
        
        tabPanel("Multi-Sample Comparison",
                 h3("Compare Multiple Samples"),
                 
                 fluidRow(
                   column(12,
                          h4("Select Samples to Compare"),
                          DTOutput("comparison_sample_selector"),
                          actionButton("clear_selection", "Clear Selection", class = "btn-sm")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h4("Selected Samples"),
                          verbatimTextOutput("selected_samples_list"),
                          fluidRow(
                            column(6, actionButton("plot_comparison", "Generate Comparison Plot", 
                                                   class = "btn-primary")),
                            column(6, actionButton("reorder_samples", "Reorder Samples", 
                                                   class = "btn-secondary"))
                          )
                   )
                 ),
                 hr(),
                 
                 fluidRow(
                   column(12,
                          h4("Selected Samples"),
                          verbatimTextOutput("selected_samples_list"),
                          fluidRow(
                            column(4, actionButton("plot_comparison", "Generate Comparison Plot", 
                                                   class = "btn-primary")),
                            column(4, actionButton("reorder_samples", "Reorder Samples", 
                                                   class = "btn-secondary")),
                            column(4, selectInput("reference_group", "Reference for comparisons:",
                                                  choices = NULL))
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h4("Correlation Comparison"),
                          plotOutput("comparison_plot", height = "600px")
                   )
                 ),
                 
                 hr(),
                 
                 fluidRow(
                   column(12,
                          h4("Statistical Analysis"),
                          verbatimTextOutput("stats_output")
                   )
                 )
        ),
        
        # Gate inspection tabs
        tabPanel("Gate 1: Debris",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate1_plot", height = "600px")
        ),
        
        tabPanel("Gate 2: Singlets",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate2_plot", height = "600px")
        ),
        
        tabPanel("Gate 3: Live Cells",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate3_plot", height = "600px")
        ),
        
        tabPanel("Gate 4: S-phase Outliers",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate4_plot", height = "600px")
        ),
        
        tabPanel("Gate 5: FxCycle Quantile",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate5_plot", height = "600px")
        ),
        
        tabPanel("Gate 6: EdU + FxCycle",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate6_plot", height = "600px")
        ),
        
        tabPanel("Gate 7: HA-Positive",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("gate7_plot", height = "600px")
        ),
        
        tabPanel("Final: Correlation",
                 fluidRow(
                   column(2, actionButton("gate1_prev", "◄ Previous", class = "btn-sm")),
                   column(8, h4(textOutput("current_sample_name"), align = "center")),
                   column(2, actionButton("gate1_next", "Next ►", class = "btn-sm", style = "float: right;"))
                 ),
                 plotOutput("correlation_plot", height = "600px")
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(
    experiments = NULL,
    all_results = NULL,
    ha_thresholds = list()
  )
  
  # Browse for folder
  observeEvent(input$browse_folder, {
    folder <- choose.dir(default = getwd(), caption = "Select Master Folder")
    if(!is.null(folder) && !is.na(folder)) {
      updateTextInput(session, "master_folder", value = folder)
    }
  })
  
  # Auto-scan for experiments on startup (quick metadata scan)
  observe({
    isolate({
      if(!is.null(rv$all_results)) return()  # Already scanned
    })
    
    master_folder <- isolate(input$master_folder)
    if(is.null(master_folder) || master_folder == "") {
      master_folder <- "Experiments/"
    }
    
    tryCatch({
      exp_folders <- list.dirs(master_folder, recursive = FALSE, full.names = TRUE)
      
      if(length(exp_folders) == 0) {
        showNotification("No experiment folders found!", type = "warning")
        return()
      }
      
      withProgress(message = 'Scanning experiments...', value = 0, {
        # Quick scan all experiments (just metadata, no FCS loading)
        all_metadata <- list()
        for(i in seq_along(exp_folders)) {
          incProgress(1/length(exp_folders))
          all_metadata[[i]] <- quick_scan_experiment(exp_folders[i])
        }
        
        # Combine all metadata
        rv$all_results <- bind_rows(all_metadata)
        
        # Store folder paths for later loading
        exp_names <- basename(exp_folders)
        rv$experiment_folders <- setNames(exp_folders, exp_names)
        
        # Update experiment selector
        output$experiment_selector <- renderUI({
          checkboxGroupInput("experiments_to_analyze", 
                             "Select experiments:",
                             choices = exp_names,
                             selected = NULL)
        })
        
        showNotification(sprintf("Found %d experiments with %d samples!", 
                                 length(exp_names), nrow(rv$all_results)), 
                         type = "message")
      })
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Rescan when button clicked
  observeEvent(input$load_experiments, {
    req(input$master_folder)
    rv$experiment_folders <- NULL
    invalidateLater(100)
  })
  
  # Select all experiments
  observeEvent(input$select_all, {
    req(rv$experiment_folders)
    updateCheckboxGroupInput(session, "experiments_to_analyze",
                             selected = names(rv$experiment_folders))
  })
  
  # Deselect all experiments
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "experiments_to_analyze",
                             selected = character(0))
  })
  
  # Update sample selector when experiment changes
  observeEvent(input$selected_experiment, {
    req(rv$experiments, input$selected_experiment)
    
    exp <- rv$experiments[[input$selected_experiment]]
    samples <- exp$metadata$sample_name
    
    updateSelectInput(session, "selected_sample", 
                      choices = setNames(seq_along(samples), samples))
  })
  
  # Analyze selected experiments (load data now)
  observeEvent(input$analyze_selected, {
    req(rv$experiment_folders, input$experiments_to_analyze)
    
    withProgress(message = 'Loading and analyzing experiments...', value = 0, {
      
      # Load only selected experiments
      if(is.null(rv$experiments)) {
        rv$experiments <- list()
      }
      
      all_results <- list()
      n_exp <- length(input$experiments_to_analyze)
      
      for(i in seq_along(input$experiments_to_analyze)) {
        exp_name <- input$experiments_to_analyze[i]
        exp_folder <- rv$experiment_folders[[exp_name]]
        
        incProgress(1/(n_exp*2), detail = sprintf("Loading %s", exp_name))
        
        # Load experiment if not already loaded
        if(is.null(rv$experiments[[exp_name]])) {
          rv$experiments[[exp_name]] <- load_experiment(exp_folder)
        }
        
        exp <- rv$experiments[[exp_name]]
        
        incProgress(1/(n_exp*2), detail = sprintf("Analyzing %s", exp_name))
        
        # Find control and analyze
        control_idx <- find_control_sample(exp$metadata, "Empty_Vector_Dox-")
        
        if(is.null(control_idx)) {
          showNotification(sprintf("No control found for %s", exp_name), 
                           type = "warning")
          next
        }
        
        control_fcs <- exp$flowset[[control_idx]]
        control_name <- exp$metadata$sample_name[control_idx]
        control_result <- calculate_ha_threshold_from_control(control_fcs, control_name)
        ha_threshold <- control_result$threshold

        rv$ha_thresholds[[exp_name]] <- ha_threshold

        exp_results <- extract_correlations(exp, ha_threshold)
        all_results[[i]] <- exp_results
         }
      
      # Update sample browser with loaded experiments
      updateSelectInput(session, "selected_experiment", 
                        choices = names(rv$experiments))
      
      # Combine new results
      new_results <- bind_rows(all_results)
      
      # Update existing results table with analyzed data
      for(i in seq_len(nrow(new_results))) {
        # Find matching row in all_results
        match_idx <- which(rv$all_results$Experiment == new_results$Experiment[i] & 
                             rv$all_results$Well == new_results$Well[i])
        
        if(length(match_idx) > 0) {
          # Update correlation, n_cells, and notes for analyzed samples
          rv$all_results$Correlation[match_idx] <- new_results$Correlation[i]
          rv$all_results$N_cells[match_idx] <- new_results$N_cells[i]
          rv$all_results$Notes[match_idx] <- new_results$Notes[i]
        }
      }
      
      showNotification(sprintf("Analysis complete! %d samples processed.", 
                               nrow(new_results)), 
                       type = "message")
      })
    })
  
  # Display results table with row selection
  output$results_table <- renderDT({
    req(rv$all_results)
    
    datatable(rv$all_results, 
              selection = 'single',  # Enable single row selection
              options = list(pageLength = 25, scrollX = TRUE),
              filter = 'top')
  })
  
  # When a row is clicked, load that sample
  observeEvent(input$results_table_rows_selected, {
    req(rv$all_results, input$results_table_rows_selected)
    
    selected_row <- input$results_table_rows_selected
    selected_data <- rv$all_results[selected_row, ]
    
    exp_name <- selected_data$Experiment
    well <- selected_data$Well
    
    # Check if experiment is loaded
    if(!exp_name %in% names(rv$experiments)) {
      showNotification("Please analyze this experiment first to view individual gates", 
                       type = "warning")
      return()
    }
    
    # Find the sample index in the experiment
    exp <- rv$experiments[[exp_name]]
    sample_idx <- which(exp$metadata$well == well)
    
    if(length(sample_idx) == 0) {
      showNotification("Sample not found in loaded experiment", type = "error")
      return()
    }
    
    # Update the selectors to show this sample
    updateSelectInput(session, "selected_experiment", selected = exp_name)
    updateSelectInput(session, "selected_sample", selected = as.character(sample_idx))
    
    showNotification(sprintf("Loaded: %s from %s", selected_data$Sample, exp_name), 
                     type = "message")
  })
  
  # Update overview experiment selector when experiments are loaded
  observe({
    req(rv$experiments)
    updateSelectInput(session, "overview_experiment", 
                      choices = names(rv$experiments))
  })
  
  # Render overview plot based on selected gate
  output$overview_plot <- renderPlot({
    req(rv$experiments, input$overview_experiment, input$overview_gate)
    
    exp <- rv$experiments[[input$overview_experiment]]
    exp_name <- input$overview_experiment
    
    if(input$overview_gate == "gate1") {
      plot_debris_gate_overview(exp)
      
    } else if(input$overview_gate == "gate2") {
      plot_singlet_gate_overview(exp)
      
    } else if(input$overview_gate == "gate3") {
      plot_live_gate_overview(exp)
      
    } else if(input$overview_gate == "gate4") {
      plot_sphase_outlier_gate_overview(exp)
      
    } else if(input$overview_gate == "gate5") {
      plot_fxcycle_quantile_gate_overview(exp)
      
    } else if(input$overview_gate == "gate6") {
      plot_edu_fxcycle_gate_overview(exp)
      
    } else if(input$overview_gate == "gate7") {
      # Calculate HA threshold
      control_idx <- find_control_sample(exp$metadata, "Empty_Vector_Dox-")
      if(is.null(control_idx)) {
        plot.new()
        text(0.5, 0.5, "No control sample found", cex = 2)
        return()
      }
      control_fcs <- exp$flowset[[control_idx]]
      control_name <- exp$metadata$sample_name[control_idx]
      control_result <- calculate_ha_threshold_from_control(control_fcs, control_name)
      ha_threshold <- control_result$threshold
      
      plot_ha_gate_overview(exp, ha_threshold)
      
    } else if(input$overview_gate == "correlation") {
      # Calculate HA threshold
      control_idx <- find_control_sample(exp$metadata, "Empty_Vector_Dox-")
      if(is.null(control_idx)) {
        plot.new()
        text(0.5, 0.5, "No control sample found", cex = 2)
        return()
      }
      control_fcs <- exp$flowset[[control_idx]]
      control_name <- exp$metadata$sample_name[control_idx]
      control_result <- calculate_ha_threshold_from_control(control_fcs, control_name)
      ha_threshold <- control_result$threshold
      
      plot_edu_ha_correlation_overview(exp, ha_threshold)
    }
  })
  
  # Gate plots
  
  # Display current sample name
  output$current_sample_name <- renderText({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    exp$metadata$sample_name[idx]
  })
  
  # Navigation functions for all gates
  navigate_sample <- function(direction) {
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    current_idx <- as.numeric(input$selected_sample)
    n_samples <- length(exp$flowset)
    
    if(direction == "next") {
      new_idx <- if(current_idx < n_samples) current_idx + 1 else 1
    } else {  # previous
      new_idx <- if(current_idx > 1) current_idx - 1 else n_samples
    }
    
    updateSelectInput(session, "selected_sample", selected = as.character(new_idx))
  }
  
  # Gate 1 navigation
  observeEvent(input$gate1_prev, { navigate_sample("prev") })
  observeEvent(input$gate1_next, { navigate_sample("next") })
  
  # Gate 2 navigation
  observeEvent(input$gate2_prev, { navigate_sample("prev") })
  observeEvent(input$gate2_next, { navigate_sample("next") })
  
  # Gate 3 navigation
  observeEvent(input$gate3_prev, { navigate_sample("prev") })
  observeEvent(input$gate3_next, { navigate_sample("next") })
  
  # Gate 4 navigation
  observeEvent(input$gate4_prev, { navigate_sample("prev") })
  observeEvent(input$gate4_next, { navigate_sample("next") })
  
  # Gate 5 navigation
  observeEvent(input$gate5_prev, { navigate_sample("prev") })
  observeEvent(input$gate5_next, { navigate_sample("next") })
  
  # Gate 6 navigation
  observeEvent(input$gate6_prev, { navigate_sample("prev") })
  observeEvent(input$gate6_next, { navigate_sample("next") })
  
  # Gate 7 navigation
  observeEvent(input$gate7_prev, { navigate_sample("prev") })
  observeEvent(input$gate7_next, { navigate_sample("next") })
  
  # Correlation navigation
  observeEvent(input$correlation_prev, { navigate_sample("prev") })
  observeEvent(input$correlation_next, { navigate_sample("next") })
  
  output$gate1_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    plot_debris_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx])
  })
  
  output$gate2_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    plot_singlet_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx])
  })
  
  output$gate3_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    plot_live_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx])
  })
  
  output$gate4_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    plot_sphase_outlier_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx])
  })
  
  output$gate5_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    plot_fxcycle_quantile_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx])
  })
  
  output$gate6_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    plot_edu_fxcycle_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx])
  })
  
  output$gate7_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    
    # Calculate HA threshold for this experiment
    control_idx <- find_control_sample(exp$metadata, "Empty_Vector_Dox-")
    if(is.null(control_idx)) {
      plot.new()
      text(0.5, 0.5, "No control sample found", cex = 1.5)
      return()
    }
    
    control_fcs <- exp$flowset[[control_idx]]
    control_name <- exp$metadata$sample_name[control_idx]
    control_result <- calculate_ha_threshold_from_control(control_fcs, control_name)
    ha_threshold <- control_result$threshold
    
    plot_ha_gate_single(exp$flowset[[idx]], exp$metadata$sample_name[idx], ha_threshold)
  })
  
  output$correlation_plot <- renderPlot({
    req(rv$experiments, input$selected_experiment, input$selected_sample)
    exp <- rv$experiments[[input$selected_experiment]]
    idx <- as.numeric(input$selected_sample)
    
    # Calculate HA threshold for this experiment
    control_idx <- find_control_sample(exp$metadata, "Empty_Vector_Dox-")
    if(is.null(control_idx)) {
      plot.new()
      text(0.5, 0.5, "No control sample found", cex = 1.5)
      return()
    }
    
    control_fcs <- exp$flowset[[control_idx]]
    control_name <- exp$metadata$sample_name[control_idx]
    control_result <- calculate_ha_threshold_from_control(control_fcs, control_name)
    ha_threshold <- control_result$threshold
    
    plot_edu_ha_correlation_single(exp$flowset[[idx]], exp$metadata$sample_name[idx], ha_threshold)
  })
  
  # Status text
  output$status_text <- renderText({
    status <- "Ready"
    if(!is.null(rv$experiments)) {
      status <- sprintf("%s\nLoaded %d experiments", status, length(rv$experiments))
    }
    if(!is.null(rv$all_results)) {
      status <- sprintf("%s\nAnalyzed %d samples", status, nrow(rv$all_results))
    }
    status
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("flow_analysis_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$all_results)
      write.xlsx(rv$all_results, file)
    }
  )
  
  # Multi-sample comparison
  
  # Create reactive value to store selected samples for comparison
  rv$comparison_samples <- reactiveVal(data.frame())
  
  # Display sample selector table (only analyzed samples)
  output$comparison_sample_selector <- renderDT({
    req(rv$all_results)
    
    # Filter to only analyzed samples (those with numeric correlation)
    analyzed <- rv$all_results[rv$all_results$Correlation != "Not analyzed", ]
    
    if(nrow(analyzed) == 0) {
      return(data.frame(Message = "No analyzed samples available. Please analyze experiments first."))
    }
    
    # Show relevant columns
    display_data <- analyzed[, c("Experiment", "Sample", "Cell_line", "Gene", 
                                 "Mutation", "Correlation", "N_cells")]
    
    datatable(display_data,
              selection = 'multiple',
              options = list(pageLength = 10, scrollX = TRUE),
              filter = 'top')
  })
  
  # Auto-select samples from same cell line
  observeEvent(input$comparison_sample_selector_rows_selected, {
    req(rv$all_results)
    
    analyzed <- rv$all_results[rv$all_results$Correlation != "Not analyzed", ]
    selected_rows <- input$comparison_sample_selector_rows_selected
    
    if(length(selected_rows) == 0) return()
    
    # Get cell lines of selected samples
    selected_cell_lines <- unique(analyzed$Cell_line[selected_rows])
    
    # Find all rows with matching cell lines
    matching_rows <- which(analyzed$Cell_line %in% selected_cell_lines)
    
    # Update selection to include all matching rows
    if(!identical(sort(matching_rows), sort(selected_rows))) {
      dataTableProxy('comparison_sample_selector') %>% 
        selectRows(matching_rows)
    }
  })
  
  # Show selected samples
  output$selected_samples_list <- renderText({
    req(input$comparison_sample_selector_rows_selected)
    
    analyzed <- rv$all_results[rv$all_results$Correlation != "Not analyzed", ]
    selected_rows <- input$comparison_sample_selector_rows_selected
    selected_data <- analyzed[selected_rows, ]
    
    # Group by cell line
    grouped <- selected_data %>%
      group_by(Cell_line, Mutation) %>%
      summarize(n = n(), .groups = 'drop') %>%
      arrange(Cell_line)
    
    paste(sprintf("%s (%s): %d replicates", 
                  grouped$Cell_line, 
                  grouped$Mutation,
                  grouped$n),
          collapse = "\n")
  })
  
  # Update reference group selector
  observe({
    req(rv$comparison_samples())
    
    plot_data <- rv$comparison_samples()
    
    grouped <- plot_data %>%
      group_by(Cell_line, Mutation) %>%
      summarize(n = n(), .groups = 'drop')
    
    choices <- setNames(grouped$Cell_line, 
                        paste0(grouped$Mutation, " (", grouped$Cell_line, ")"))
    
    updateSelectInput(session, "reference_group", 
                      choices = choices,
                      selected = grouped$Cell_line[1])
  })
  
  # Clear selection
  observeEvent(input$clear_selection, {
    dataTableProxy('comparison_sample_selector') %>% 
      selectRows(NULL)
  })
  
  # Generate comparison plot
  observeEvent(input$plot_comparison, {
    req(input$comparison_sample_selector_rows_selected)
    
    analyzed <- rv$all_results[rv$all_results$Correlation != "Not analyzed", ]
    selected_rows <- input$comparison_sample_selector_rows_selected
    selected_data <- analyzed[selected_rows, ]
    
    # Store for plotting
    rv$comparison_samples(selected_data)
  })
  
  # Render comparison plot
  output$comparison_plot <- renderPlot({
    req(rv$comparison_samples())
    
    plot_data <- rv$comparison_samples()
    
    if(nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "Select samples and click 'Generate Comparison Plot'", cex = 1.5)
      return()
    }
    
    # Convert correlation to numeric
    plot_data$Correlation <- as.numeric(plot_data$Correlation)
    
    # Convert correlation to numeric
    plot_data$Correlation <- as.numeric(plot_data$Correlation)
    
    # Group by cell line to get means
    plot_summary <- plot_data %>%
      group_by(Cell_line, Mutation, Gene) %>%
      summarize(
        mean_corr = mean(Correlation),
        sd_corr = sd(Correlation),
        n = n(),
        .groups = 'drop'
      )
    
    # Sort: use custom order if available, otherwise WT first then by correlation
    if(!is.null(rv$sample_order())) {
      # Apply custom order
      plot_summary$Cell_line <- factor(plot_summary$Cell_line, levels = rv$sample_order())
      plot_summary <- plot_summary %>% arrange(Cell_line)
    } else {
      # Default: WT first, others by correlation
      wt_rows <- grep("^WT", plot_summary$Mutation, ignore.case = TRUE)
      if(length(wt_rows) > 0) {
        wt_data <- plot_summary[wt_rows, ]
        other_data <- plot_summary[-wt_rows, ] %>% arrange(mean_corr)
        plot_summary <- bind_rows(wt_data, other_data)
      } else {
        plot_summary <- plot_summary %>% arrange(mean_corr)
      }
    }
    
    # Create labels (mutation only)
    plot_summary$label <- plot_summary$Mutation
    
    # Assign lighter colors to experiments for points
    exp_colors <- rainbow(length(unique(plot_data$Experiment)), alpha = 0.5)
    names(exp_colors) <- unique(plot_data$Experiment)
    
    # Create the plot with adaptive top margin based on number of experiments
    n_experiments <- length(unique(plot_data$Experiment))
    top_margin <- 8 + ceiling(n_experiments * 0.7)  # Scales with number of experiments
    par(mar = c(8, 4, top_margin, 4))
    
    # Fixed narrow bars, add empty space on right when few samples
    n_samples <- nrow(plot_summary)
    bar_width <- 0.8  # Fixed narrow width
    bar_space <- 1  # Fixed moderate spacing
    
    # Calculate total x-range: expand to fixed width regardless of sample count
    bars_width <- n_samples * (bar_width + bar_space)
    x_max <- max(bars_width, 15)  # Always extend to at least 15 units
    
    bp <- barplot(plot_summary$mean_corr,
                  las = 2,
                  xlim = c(0, x_max),  # Fixed x-range
                  ylim = c(-0.8, 
                           max(c(0, plot_data$Correlation, plot_summary$mean_corr)) + 0.3),
                  ylab = "EdU vs HA Correlation (r)",
                  main = "",
                  col = "lightgrey",
                  border = "black",
                  cex.names = 0.8,
                  width = bar_width,
                  space = bar_space)
    
    # Add angled labels
    text(x = bp, y = par("usr")[3] - 0.05, labels = plot_summary$label,
         srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
    
    # Add title with space for legend
    title(main = "Multi-Sample Correlation Comparison", line = 3.5)
    
    # Add error bars (SD)
    arrows(bp, plot_summary$mean_corr - plot_summary$sd_corr,
           bp, plot_summary$mean_corr + plot_summary$sd_corr,
           angle = 90, code = 3, length = 0.1)
    
    # Add horizontal line at 0
    abline(h = 0, lty = 2, col = "gray40")
    
    # Add individual data points
    for(i in seq_len(nrow(plot_summary))) {
      cell_line <- plot_summary$Cell_line[i]
      cell_data <- plot_data[plot_data$Cell_line == cell_line, ]
      
      # Add jitter to x position for visibility
      x_pos <- bp[i] + runif(nrow(cell_data), -0.15, 0.15)
      
      for(j in seq_len(nrow(cell_data))) {
        exp_name <- cell_data$Experiment[j]
        points(x_pos[j], cell_data$Correlation[j], 
               pch = 21, cex = 2, 
               bg = exp_colors[exp_name], 
               col = "black", lwd = 1.5)
      }
    }
    
    # Add statistical significance using Holm-Šídák correction (all at same height)
    if(nrow(plot_summary) > 1) {
      # Prepare data for matched analysis
      wide_data <- plot_data %>%
        select(Experiment, Cell_line, Correlation) %>%
        pivot_wider(names_from = Cell_line, values_from = Correlation)
      
      long_data <- wide_data %>%
        pivot_longer(cols = -Experiment, names_to = "Cell_line", values_to = "Correlation") %>%
        filter(!is.na(Correlation))
      
      # Reference group (user-selected or first)
      ref_group <- if(!is.null(input$reference_group) && input$reference_group %in% plot_summary$Cell_line) {
        input$reference_group
      } else {
        plot_summary$Cell_line[1]
      }
      ref_data <- long_data %>% filter(Cell_line == ref_group)
      
      # Perform pairwise t-tests
      p_values <- c()
      test_groups <- c()
      
      for(i in 2:nrow(plot_summary)) {
        test_group <- plot_summary$Cell_line[i]
        test_data <- long_data %>% filter(Cell_line == test_group)
        
        # Match by experiment
        merged <- merge(ref_data, test_data, by = "Experiment", suffixes = c("_ref", "_test"))
        
        if(nrow(merged) >= 2) {
          # Paired t-test
          t_result <- t.test(merged$Correlation_ref, merged$Correlation_test, paired = TRUE)
          p_values <- c(p_values, t_result$p.value)
          test_groups <- c(test_groups, test_group)
        } else {
          p_values <- c(p_values, NA)
          test_groups <- c(test_groups, test_group)
        }
      }
      
      # Apply Holm-Šídák correction
      p_adjusted <- p.adjust(p_values, method = "holm")
      
      # Find the maximum y position for all bars
      max_y <- max(c(plot_summary$mean_corr + plot_summary$sd_corr, 
                     plot_data$Correlation))
      sig_y_pos <- max_y + 0.12
      
      # Display results
      for(i in seq_along(test_groups)) {
        plot_idx <- which(plot_summary$Cell_line == test_groups[i])
        
        if(is.na(p_adjusted[i])) {
          stars <- "n/a"
          p_text <- ""
        } else {
          stars <- if(p_adjusted[i] < 0.001) "***"
          else if(p_adjusted[i] < 0.01) "**"
          else if(p_adjusted[i] < 0.05) "*"
          else "ns"
          
          # Format p-value
          if(p_adjusted[i] < 0.001) {
            p_text <- sprintf("(p<0.001)")
          } else {
            p_text <- sprintf("(p=%.3f)", p_adjusted[i])
          }
        }
        
        # Show stars/ns at consistent height
        text(bp[plot_idx], sig_y_pos, stars, cex = 1.2, font = 2, col = "black")
        
        # Show p-value below stars
        if(p_text != "") {
          text(bp[plot_idx], sig_y_pos - 0.06, p_text, cex = 0.7, col = "black")
        }
      }
    }
    
    # Add grid
    #grid(nx = NA, ny = NULL)
    
    # Add legend above the plot (anchored to right edge)
    par(xpd = TRUE)
    legend(x = par("usr")[2], y = par("usr")[4] + 0.25,  # Right edge of plot area
           legend = names(exp_colors),
           pch = 21, pt.bg = exp_colors, pt.cex = 1.3, col = "black",
           title = "Experiments",
           ncol = 1,
           cex = 0.7,
           bg = "white",
           box.lty = 1,
           xjust = 1,
           yjust = 0)
    par(xpd = FALSE)
    
  })
  
  # Store custom order
  rv$sample_order <- reactiveVal(NULL)
  
  # Reorder samples with drag-and-drop
  observeEvent(input$reorder_samples, {
    req(rv$comparison_samples())
    
    plot_data <- rv$comparison_samples()
    plot_data$Correlation <- as.numeric(plot_data$Correlation)
    
    # Get current order
    current_order <- plot_data %>%
      group_by(Cell_line, Mutation) %>%
      summarize(mean_corr = mean(Correlation), .groups = 'drop') %>%
      arrange(mean_corr)
    
    # WT first
    wt_rows <- grep("^WT", current_order$Mutation, ignore.case = TRUE)
    if(length(wt_rows) > 0) {
      wt_data <- current_order[wt_rows, ]
      other_data <- current_order[-wt_rows, ]
      current_order <- bind_rows(wt_data, other_data)
    }
    
    # Create labels for sortable list
    sample_labels <- setNames(
      paste0(current_order$Mutation, " (", current_order$Cell_line, ")"),
      current_order$Cell_line
    )
    
    showModal(modalDialog(
      title = "Reorder Samples (Drag to Reorder)",
      size = "m",
      p(strong("Drag samples to reorder. WT will remain first.")),
      rank_list(
        text = "Sample Order:",
        labels = sample_labels,
        input_id = "sample_rank_list"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_reorder", "Apply Order", class = "btn-primary")
      )
    ))
  })
  
  # Apply custom order from drag-and-drop
  observeEvent(input$apply_reorder, {
    req(input$sample_rank_list)
    
    # The rank_list returns cell_line values in the new order
    new_order <- input$sample_rank_list
    
    rv$sample_order(new_order)
    
    removeModal()
    showNotification("Custom order applied! Click 'Generate Comparison Plot' to update.", 
                     type = "message")
  })
  
  # Apply custom order
  observeEvent(input$apply_reorder, {
    req(input$reorder_sequence, rv$comparison_samples())
    
    plot_data <- rv$comparison_samples()
    plot_data$Correlation <- as.numeric(plot_data$Correlation)
    
    current_order <- plot_data %>%
      group_by(Cell_line, Mutation) %>%
      summarize(mean_corr = mean(Correlation), .groups = 'drop') %>%
      arrange(mean_corr)
    
    # WT first
    wt_rows <- grep("^WT", current_order$Mutation, ignore.case = TRUE)
    if(length(wt_rows) > 0) {
      wt_data <- current_order[wt_rows, ]
      other_data <- current_order[-wt_rows, ]
      current_order <- bind_rows(wt_data, other_data)
    }
    
    # Parse the sequence
    tryCatch({
      sequence <- as.integer(strsplit(input$reorder_sequence, ",")[[1]])
      
      if(length(sequence) != nrow(current_order) || 
         !all(sort(sequence) == seq_len(nrow(current_order)))) {
        showNotification("Invalid sequence! Must include all numbers from 1 to the number of samples.", 
                         type = "error")
        return()
      }
      
      # Apply the new order
      new_order <- current_order$Cell_line[sequence]
      rv$sample_order(new_order)
      
      removeModal()
      showNotification("Custom order applied! Click 'Generate Comparison Plot' to update.", 
                       type = "message")
      
    }, error = function(e) {
      showNotification("Error parsing order sequence. Use format: 1,2,3,4", type = "error")
    })
  })
  
  # Statistical analysis (matching GraphPad approach)
  output$stats_output <- renderText({
    req(rv$comparison_samples())
    
    plot_data <- rv$comparison_samples()
    
    if(nrow(plot_data) < 2) {
      return("Select at least 2 samples for statistical comparison")
    }
    
    plot_data$Correlation <- as.numeric(plot_data$Correlation)
    
    # Prepare grouped data (for reference, not displayed)
    grouped <- plot_data %>%
      group_by(Cell_line, Mutation) %>%
      summarize(
        mean_corr = mean(Correlation),
        sd_corr = sd(Correlation),
        n = n(),
        .groups = 'drop'
      )
    
    stats_text <- ""
    
    # Repeated measures one-way ANOVA
    if(length(unique(plot_data$Cell_line)) > 1) {
      
      # Check if we have matched data (same experiments across groups)
      experiments_per_group <- plot_data %>%
        group_by(Cell_line) %>%
        summarize(exps = list(unique(Experiment)), .groups = 'drop')
      
      # Prepare data for RM-ANOVA
      # Need balanced design with same experiments
      wide_data <- plot_data %>%
        select(Experiment, Cell_line, Correlation) %>%
        pivot_wider(names_from = Cell_line, values_from = Correlation)
      
      # Check if data is suitable for RM-ANOVA
      n_complete <- sum(complete.cases(wide_data))
      
      if(n_complete >= 2) {
        # Perform repeated measures ANOVA
        stats_text <- paste0(stats_text, "\n=== Repeated Measures One-Way ANOVA ===\n")
        stats_text <- paste0(stats_text, "(Samples matched by experiment, assuming Gaussian distribution)\n\n")
        
        # Convert back to long format for aov
        long_data <- wide_data %>%
          pivot_longer(cols = -Experiment, names_to = "Cell_line", values_to = "Correlation") %>%
          filter(!is.na(Correlation))
        
        # Perform RM-ANOVA
        aov_model <- aov(Correlation ~ Cell_line + Error(Experiment/Cell_line), data = long_data)
        aov_summary <- summary(aov_model)
        
        # Extract F-statistic and p-value
        # The within-subjects effect is in the second element
        within_effect <- aov_summary$`Error: Experiment:Cell_line`[[1]]
        
        if(!is.null(within_effect) && nrow(within_effect) > 0) {
          f_stat <- within_effect["Cell_line", "F value"]
          p_val <- within_effect["Cell_line", "Pr(>F)"]
          df1 <- within_effect["Cell_line", "Df"]
          df2 <- within_effect["Residuals", "Df"]
          
          stats_text <- paste0(stats_text, sprintf("F(%d, %d) = %.3f, p = %.4f\n", 
                                                   df1, df2, f_stat, p_val))
          
          if(p_val < 0.05) {
            stats_text <- paste0(stats_text, "Result: Significant difference detected (p < 0.05)\n\n")
            
            # Pairwise comparisons with Holm-Šídák correction
            stats_text <- paste0(stats_text, "\n=== Pairwise Comparisons (Holm-Šídák) ===\n")
            stats_text <- paste0(stats_text, "(Compared to WT/reference group)\n\n")
            
            # Reference group (user-selected or first)
            ref_group <- if(!is.null(input$reference_group) && input$reference_group %in% plot_summary$Cell_line) {
              input$reference_group
            } else {
              plot_summary$Cell_line[1]
            }
            ref_data <- long_data %>% filter(Cell_line == ref_group)
            
            # Perform pairwise t-tests
            p_values <- c()
            comparisons <- c()
            
            for(i in 2:length(unique(long_data$Cell_line))) {
              test_group <- grouped$Cell_line[i]
              test_data <- long_data %>% filter(Cell_line == test_group)
              
              # Match by experiment
              merged <- merge(ref_data, test_data, by = "Experiment", suffixes = c("_ref", "_test"))
              
              if(nrow(merged) >= 2) {
                # Paired t-test
                t_result <- t.test(merged$Correlation_ref, merged$Correlation_test, paired = TRUE)
                p_values <- c(p_values, t_result$p.value)
                comparisons <- c(comparisons, sprintf("%s vs %s", test_group, ref_group))
              }
            }
            
            # Apply Holm-Šídák correction
            if(length(p_values) > 0) {
              p_adjusted <- p.adjust(p_values, method = "holm")
              
              for(i in seq_along(test_groups)) {
                # Get mutation names
                test_mut <- grouped$Mutation[grouped$Cell_line == test_groups[i]]
                ref_mut <- grouped$Mutation[grouped$Cell_line == ref_group]
                
                sig <- if(p_adjusted[i] < 0.001) "***"
                else if(p_adjusted[i] < 0.01) "**"
                else if(p_adjusted[i] < 0.05) "*"
                else "ns"
                
                stats_text <- paste0(stats_text, 
                                     sprintf("%s (#%s) vs %s (#%s): p = %.4f, %s\n",
                                             test_mut,
                                             test_groups[i],
                                             ref_mut,
                                             ref_group,
                                             p_adjusted[i],
                                             sig))
              }
            }
          } else {
            stats_text <- paste0(stats_text, "Result: No significant difference (p ≥ 0.05)\n")
          }
        }
        
      } else {
        stats_text <- paste0(stats_text, "\n=== Statistical Analysis ===\n")
        stats_text <- paste0(stats_text, "Note: Insufficient matched data for repeated measures ANOVA.\n")
        stats_text <- paste0(stats_text, sprintf("Only %d experiments have data for all groups.\n", n_complete))
        stats_text <- paste0(stats_text, "Need at least 2 complete sets for RM-ANOVA.\n")
      }
    }
    
    return(stats_text)
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)
