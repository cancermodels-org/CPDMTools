# RShiny App Template for CPDM QC Application

library(shiny)
library(dplyr)
library(readxl)
library(writexl)
library(DT)
library(plotly)
library(flextable)
library(CPDMTools) # Your custom package

ui <- fluidPage(
  titlePanel("CPDM Data Quality Control App"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("data_type", "Data Type",
                   choices = c("Growth Data", "End-Point Assay Data"),
                   selected = "Growth Data"),

      conditionalPanel(
        condition = "input.data_type == 'Growth Data'",
        fileInput("growth_file", "Import Growth Data (.xlsx, .csv, .txt)", accept = c(".csv", ".xlsx", ".txt"))
      ),

      conditionalPanel(
        condition = "input.data_type == 'End-Point Assay Data'",
        fileInput("ctg_file", "Import End-Point Assay Data (.xlsx, .csv, .txt)", accept = c(".csv", ".xlsx", ".txt"))
      ),

      hr(),
      h4("Exporting Data"),

      conditionalPanel(
        condition = "input.data_type == 'Growth Data'",
        selectInput("outlier_manual_only", "Outliers to Exclude",
                    choices = c("Outliers Manually Annotated" = TRUE,
                                "Outliers Auto and Manually Annotated" = FALSE), selected = TRUE),
        checkboxInput("growthcurveme", "GrowthCurveME", value = TRUE),
        checkboxInput("lgrscore", "LGRscore", value = TRUE),
        checkboxInput("prism", "GraphPad PRISM", value = TRUE),
        downloadButton("export_growth_data", "Export Data")
      ),

      conditionalPanel(
        condition = "input.data_type == 'End-Point Assay Data'",
        selectInput("outlier_manual_only_ctg", "Outliers to Exclude",
                    choices = c("Outliers Manually Annotated" = TRUE,
                                "Outliers Auto and Manually Annotated" = FALSE), selected = TRUE),
        checkboxInput("prism_ctg", "GraphPad PRISM", value = TRUE),
        downloadButton("export_ctg_data", "Export Data")
      )
    ),

    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Replicates and Concentrations", value = "tab_rep_conc",
                           numericInput("round_by", "Round Concentrations By", value = 4),
                           selectInput("use_nearest_10", "Round to Nearest 10", choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                           actionButton("lock_round", "Lock In Rounded Concentrations"),
                           DTOutput("rep_conc_table")
                  ),

                  tabPanel("Growth Data QC", value = "tab_growth_qc"),
                  tabPanel("Controls QC and Normalization", value = "tab_ctg_controls",
                           checkboxInput("show_outlier", "Show Outliers", value = TRUE),
                           checkboxInput("make_interactive", "Make Interactive", value = TRUE),
                           uiOutput("positive_control_ui"),
                           plotlyOutput("ctg_control_plot"),
                           actionButton("normalize_ctg", "Normalize Data")
                  ),
                  tabPanel("End-Point Assay QC", value = "tab_ctg_qc"),
                  tabPanel("Updated Dataset", value = "tab_updated")
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    input_data = NULL,
    updated_data = NULL,
    rounded_input_data = NULL,
    ctg_list = NULL,
    file_name = NULL
  )

  # Import handler for Growth Data
  observeEvent(input$growth_file, {
    req(input$growth_file)
    ext <- tools::file_ext(input$growth_file$name)
    rv$file_name <- tools::file_path_sans_ext(input$growth_file$name)

    df <- switch(ext,
                 csv = read.csv(input$growth_file$datapath),
                 txt = read.delim(input$growth_file$datapath),
                 xlsx = readxl::read_excel(input$growth_file$datapath))

    rv$input_data <- df
    rv$updated_data <- df %>%
      mutate(outlier_auto_yn = "No", outlier_auto_flag_reason = NA,
             outlier_manual_yn = NA, outlier_manual_flag_reason = NA) %>%
      CPDMTools::color_palette_mono()

    updateTabsetPanel(session, "main_tabs", selected = "tab_growth_qc")
    showTab("main_tabs", "tab_updated")

    if ("concentration" %in% names(df)) {
      showTab("main_tabs", "tab_rep_conc")
    }
  })

  # Import handler for CTG Data
  observeEvent(input$ctg_file, {
    req(input$ctg_file)
    ext <- tools::file_ext(input$ctg_file$name)
    rv$file_name <- tools::file_path_sans_ext(input$ctg_file$name)

    df <- switch(ext,
                 csv = read.csv(input$ctg_file$datapath),
                 txt = read.delim(input$ctg_file$datapath),
                 xlsx = readxl::read_excel(input$ctg_file$datapath))

    rv$input_data <- df
    showTab("main_tabs", "tab_ctg_controls")
    showTab("main_tabs", "tab_rep_conc")
  })

  # Replicates and Concentration Rounding Table
  observeEvent(input$lock_round, {
    req(rv$input_data)
    rv$rounded_input_data <- CPDMTools::round_concentration(
      data_frame = rv$input_data,
      round_by = input$round_by,
      use_nearest_10 = as.logical(input$use_nearest_10))

    rv$updated_data <- rv$rounded_input_data %>%
      mutate(outlier_auto_yn = "No", outlier_auto_flag_reason = NA,
             outlier_manual_yn = NA, outlier_manual_flag_reason = NA) %>%
      CPDMTools::color_palette_mono()

    if (input$data_type == "Growth Data") {
      showTab("main_tabs", "tab_growth_qc")
      showTab("main_tabs", "tab_updated")
    } else {
      showTab("main_tabs", "tab_ctg_controls")
      showTab("main_tabs", "tab_ctg_qc")
      showTab("main_tabs", "tab_updated")
    }
  })

  output$rep_conc_table <- renderDT({
    req(rv$rounded_input_data)
    summary_table <- rv$rounded_input_data %>%
      count(treatment_name, concentration) %>%
      arrange(treatment_name, concentration)
    datatable(summary_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$positive_control_ui <- renderUI({
    req(rv$updated_data)
    has_pos <- "Positive Control" %in% rv$updated_data$treatment_type
    selectInput("use_positive_control", "Normalization Technique",
                choices = c("Negative and Positive Control" = TRUE, "Negative Control Only" = FALSE),
                selected = if (has_pos) TRUE else FALSE)
  })

  output$ctg_control_plot <- renderPlotly({
    req(rv$updated_data)
    CPDMTools::ctg_qc_control_plot(
      data_frame = rv$updated_data,
      show_outlier = input$show_outlier,
      make_interactive = input$make_interactive
    )
  })

  observeEvent(input$normalize_ctg, {
    req(rv$updated_data)
    rv$updated_data <- CPDMTools::ctg_normalize(
      data_frame = rv$updated_data,
      use_positive_control = as.logical(input$use_positive_control)
    )
    showTab("main_tabs", "tab_ctg_qc")
    showTab("main_tabs", "tab_updated")
  })

  # Export Growth Data
  output$export_growth_data <- downloadHandler(
    filename = function() {
      paste0(rv$file_name, "_qc.xlsx")
    },
    content = function(file) {
      req(rv$updated_data)
      output_list <- CPDMTools::growth_qc_output(
        ctg_list = list(rv$updated_data),
        outlier_manual_only = as.logical(input$outlier_manual_only),
        growthcurveme = input$growthcurveme,
        lgrscore = input$lgrscore,
        prism = input$prism
      )
      writexl::write_xlsx(output_list, path = file)
    }
  )

  # Export CTG Data
  output$export_ctg_data <- downloadHandler(
    filename = function() {
      paste0(rv$file_name, "_qc.xlsx")
    },
    content = function(file) {
      req(rv$updated_data)
      output_list <- CPDMTools::ctg_qc_output(
        ctg_list = list(rv$updated_data),
        outlier_manual_only = as.logical(input$outlier_manual_only_ctg),
        prism = input$prism_ctg
      )
      writexl::write_xlsx(output_list, path = file)
    }
  )
}

shinyApp(ui, server)
