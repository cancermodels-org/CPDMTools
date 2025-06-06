library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(flextable)
library(DT)
library(ggplot2)
library(plotly)
library(writexl)
library(CPDMTools) # your custom package

ui <- fluidPage(
  titlePanel("Growth and End-Point Assay QC App"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("data_type", "Data Type",
                   choices = c("Growth Data", "End-Point Assay Data"),
                   selected = "Growth Data"),
      uiOutput("file_input_ui"),
      hr(),
      h4("Exporting Data"),
      conditionalPanel(
        condition = "input.data_type == 'Growth Data'",
        selectInput("outlier_manual_only", "Outliers to Exclude",
                    choices = c("Outliers Manually Annotated" = TRUE,
                                "Outliers Auto and Manually Annotated" = FALSE),
                    selected = TRUE),
        uiOutput("growthcurveme_ui"),
        uiOutput("lgrscore_ui")
      ),
      checkboxInput("prism", "GraphPad PRISM", value = FALSE),
      downloadButton("export_data", "Export Data")
    ),

    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Replicates and Concentrations",
                           numericInput("round_by", "Round Concentrations By", 4),
                           selectInput("use_nearest_10", "Round to Nearest 10?",
                                       choices = c("Yes" = TRUE, "No" = FALSE),
                                       selected = TRUE),
                           actionButton("lock_conc", "Lock In Rounded Concentrations"),
                           uiOutput("replicate_table")),

                  tabPanel("Growth Data QC",
                           uiOutput("qc_controls_ui"),
                           helpText("Use box or lasso selection on the interactive plot to highlight wells."),
                           plotlyOutput("qc_plot", height = "500px"),
                           fluidRow(
                             column(4, actionButton("mark_outlier", "Mark as Outlier")),
                             column(4, selectInput("outlier_reason", "Outlier Reason",
                                                   choices = c("Use Outlier Auto Flag Reason",
                                                               "Imaging Error",
                                                               "Masking Error",
                                                               "Technical Error",
                                                               "Other"))),
                             column(4, conditionalPanel(
                               condition = "input.outlier_reason == 'Other'",
                               textInput("other_reason", "Specify Other Reason")
                             )),
                             column(4, actionButton("remove_outlier", "Mark as Not an Outlier"))
                           ),
                           DTOutput("brush_table")),

                  tabPanel("Updated Dataset",
                           DTOutput("updated_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  input_data <- reactiveVal(NULL)
  updated_data <- reactiveVal(NULL)
  rounded_locked <- reactiveVal(FALSE)

  output$file_input_ui <- renderUI({
    fileInput("input_file", paste("Import", input$data_type, "(.xlsx, .csv, .txt)"))
  })

  observeEvent(input$input_file, {
    req(input$input_file)
    ext <- tools::file_ext(input$input_file$name)
    file_path <- input$input_file$datapath

    df <- switch(ext,
                 "csv" = read.csv(file_path),
                 "txt" = read.delim(file_path),
                 "xlsx" = readxl::read_excel(file_path),
                 stop("Unsupported file type"))

    input_data(df)

    df <- df %>%
      mutate(outlier_auto_yn = "No",
             outlier_auto_flag_reason = NA,
             outlier_manual_yn = NA,
             outlier_manual_flag_reason = NA)

    df <- CPDMTools::color_palette_mono(df)
    updated_data(df)

    if ("concentration" %in% colnames(df)) {
      updateTabsetPanel(session, "main_tabs", selected = "Replicates and Concentrations")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Growth Data QC")
    }
  })

  output$growthcurveme_ui <- renderUI({
    req(updated_data())
    if ("Media Control" %in% updated_data()$treatment_type |
        "Negative Control" %in% updated_data()$treatment_type) {
      checkboxInput("growthcurveme", "GrowthCurveME", value = FALSE)
    }
  })

  output$lgrscore_ui <- renderUI({
    req(updated_data())
    if (all(c("Negative Control", "Monotherapy") %in% updated_data()$treatment_type)) {
      checkboxInput("lgrscore", "LGRscore", value = FALSE)
    }
  })

  output$export_data <- downloadHandler(
    filename = function() {
      name <- tools::file_path_sans_ext(input$input_file$name)
      paste0(name, "_qc.xlsx")
    },
    content = function(file) {
      out <- CPDMTools::growth_qc_output(
        updated_data(),
        outlier_manual_only = as.logical(input$outlier_manual_only),
        growthcurveme = input$growthcurveme %||% FALSE,
        lgrscore = input$lgrscore %||% FALSE,
        prism = input$prism
      )
      writexl::write_xlsx(out, path = file)
    }
  )

  observeEvent(input$lock_conc, {
    req(input_data(), input$round_by, input$use_nearest_10)

    df_rounded <- CPDMTools::round_concentration(
      data_frame = input_data(),
      round_by = input$round_by,
      use_nearest_10 = as.logical(input$use_nearest_10)
    )

    df_rounded <- df_rounded %>%
      mutate(outlier_auto_yn = "No",
             outlier_auto_flag_reason = NA,
             outlier_manual_yn = NA,
             outlier_manual_flag_reason = NA)

    df_rounded <- CPDMTools::color_palette_mono(df_rounded)
    updated_data(df_rounded)
    rounded_locked(TRUE)
  })

  output$replicate_table <- renderUI({
    req(input_data())
    if ("concentration" %in% colnames(input_data())) {
      df_rounded <- CPDMTools::round_concentration(
        data_frame = input_data(),
        round_by = input$round_by,
        use_nearest_10 = as.logical(input$use_nearest_10)
      )
      summary <- df_rounded %>%
        dplyr::count(treatment_name, concentration)
      flextable::flextable(summary) %>%
        flextable::htmltools_value()
    }
  })

  output$qc_controls_ui <- renderUI({
    req(updated_data())
    tagList(
      selectInput("treatment_name", "Treatment Name",
                  choices = unique(updated_data()$treatment_name),
                  selected = unique(updated_data()$treatment_name)[1]),
      if ("concentration" %in% colnames(updated_data())) {
        selectInput("concentration", "Concentration",
                    choices = c("All Concentrations", unique(updated_data()$concentration)))
      },
      numericInput("span_value", "Span Value", 0.3),
      numericInput("residual_threshold", "Residual Threshold", 3),
      checkboxInput("show_outlier", "Show Outliers", TRUE),
      checkboxInput("show_only_outlier_wells", "Show Only Outlier Wells", FALSE),
      checkboxInput("make_interactive", "Make Interactive", TRUE),
      textInput("growth_metric_name", "Growth Metric Name", "growth_metric"),
      textInput("time_units", "Time Units", "hours"),
      actionButton("run_outlier_detection", "Run/Update Outlier Detection")
    )
  })

  observeEvent(input$run_outlier_detection, {
    req(updated_data())
    if ("concentration" %in% colnames(updated_data()) && !rounded_locked()) {
      showModal(modalDialog("Please lock in rounded concentrations before running outlier detection."))
      return()
    }

    df_out <- CPDMTools::loess_outlier_fit(
      data_frame = updated_data(),
      span_value = input$span_value,
      residual_threshold = input$residual_threshold
    )

    updated_data(df_out)
  })

  output$qc_plot <- renderPlotly({
    req(updated_data(), input$treatment_name)
    df <- updated_data()
    if (!is.null(input$concentration) && input$concentration != "All Concentrations") {
      df <- df %>% filter(concentration == input$concentration)
    }
    df <- df %>% filter(treatment_name == input$treatment_name)

    plot_obj <- CPDMTools::growth_plot_qc_mono(
      data_frame = df,
      treatment_name = input$treatment_name,
      show_outlier = input$show_outlier,
      show_only_outlier_wells = input$show_only_outlier_wells,
      make_interactive = input$make_interactive,
      growth_metric_name = input$growth_metric_name,
      time_units = input$time_units,
      n_x_axis_breaks = 12,
      n_y_axis_breaks = 10
    )

    if (isTRUE(input$make_interactive)) {
      plot_obj <- ggplotly(plot_obj, source = "qc_plot") %>%
        layout(dragmode = "select") %>%
        config(displayModeBar = TRUE)
      plotly::event_register(plot_obj, "plotly_selected")
    }

    plot_obj
  })

  selected_points <- reactiveVal()

  observe({
    brush <- event_data("plotly_selected", source = "qc_plot")
    df <- updated_data()
    if (!is.null(brush) && "key" %in% names(brush)) {
      brushed_wells <- brush$key
      selected <- df %>% filter(well %in% brushed_wells)
      selected_points(selected)
    } else {
      selected_points(NULL)
    }
  })

  output$brush_table <- renderDT({
    req(selected_points())
    brushed <- selected_points()
    display_cols <- c("well", "treatment_name", "concentration",
                      "outlier_auto_yn", "outlier_auto_flag_reason",
                      "outlier_manual_yn", "outlier_manual_flag_reason")
    brushed %>%
      select(any_of(display_cols)) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 5))
  })

  observeEvent(input$mark_outlier, {
    req(selected_points())
    df <- updated_data()
    brushed <- selected_points()
    reason <- input$outlier_reason

    if (reason == "Use Outlier Auto Flag Reason") {
      brushed$outlier_manual_flag_reason <- brushed$outlier_auto_flag_reason
    } else if (reason == "Other") {
      brushed$outlier_manual_flag_reason <- input$other_reason
    } else {
      brushed$outlier_manual_flag_reason <- reason
    }

    brushed$outlier_manual_yn <- "Yes"
    df[df$well %in% brushed$well, ] <- brushed
    updated_data(df)
  })

  observeEvent(input$remove_outlier, {
    req(selected_points())
    df <- updated_data()
    brushed <- selected_points()
    df <- df %>%
      mutate(outlier_manual_yn = ifelse(well %in% brushed$well, "No", outlier_manual_yn))
    updated_data(df)
  })

  output$updated_table <- renderDT({
    req(updated_data())
    datatable(updated_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
