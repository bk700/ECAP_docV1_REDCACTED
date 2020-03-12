library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)
library(readr)
library(purrr)
names <- c("Wavelength", "Intensity")
f_4_13 <- list.files(path = "Apr_13_2018", full.names = T, recursive = T)
df_4_13 <- f_4_13 %>% set_names(.) %>% map_df(read_delim, .id = "FileName", delim = "\t", col_names = names)
df_4_13 <- df_4_13 %>% group_by(FileName) %>% nest()
#df_4_13 <- df_4_13 %>% unnest()

header <- dashboardHeader(title = "ECAP OES", 
                          tasks <- dropdownMenu(type = "tasks", badgeStatus = "success", 
                                                taskItem(value = 20, color = "green", 
                                                         "Documentation"), 
                                                taskItem(value = 70, color = "aqua", 
                                                         "Plotting Interface"), 
                                                taskItem(value = 45, color = "yellow", 
                                                         "Data I/O Code"), 
                                                taskItem(value = 45, color = "red", 
                                                         "Overall project")
                          )
)

sidebar <- dashboardSidebar(sidebarMenu(id = "tabs", 
                                        menuItem("Plot", tabName = "plot", icon = icon("line-chart"), 
                                                 selected = TRUE), 
                                        menuItem("Data", tabName = "table", icon = icon("hdd", lib = "glyphicon")
                                        ),
                                        menuItem("Method", tabName = "method", icon = icon("list", lib = "glyphicon")
                                        ),
                                        menuItem("Experiment Info", tabName = "info", icon = icon("users")
                                        )
)
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plot",
            fluidRow(
              column(width = 2, 
                     box(width = NULL, status = "primary", solidHeader = TRUE,
                         tabPanel(h5("Parameters"),
                                  selectInput("dates", "Date:", 
                                              choices = c("Apr_13_2018", "Jun_11_2018")), 
                                  selectInput("speclist", "Spectra:",
                                              choices = c("He 200W", "He 400W", "He 500W", "He 600W"))
                         )
                     ),
                     box(width = NULL, status = "primary", solidHeader = TRUE, title = "SpecAir Info", 
                         "Optical Emission Spectroscopy data for the ECAP experiment was obtained using a SpectraPro 275 monochrometer equipped with a Mightex Systems SSE-1304-U CCD using the 2400 g/mm grating.  Fitting was done by calculating a hypothetical spectra using Specair that, if well fitted to the experimental data, implies the hypothetical spectra is a good approximation for the actual characteristics of the experimental plasma"
                     )
              ),
              column(width = 10, 
                     plotlyOutput("plot", height = 500), 
                     verbatimTextOutput("event")
              )
            ),
            fluidRow(
              column(width = 6, 
                     box(width = NULL, 
                         downloadButton("dls", "Download Specair Summary / NOT IMPLEMENTED"), 
                         "ECAP experiment on hold, feature not implemented"
                     )
              ),
              column(width = 6, 
                     box(width = NULL, 
                         downloadButton("dlp", "Download Plot / NOT IMPLEMENTED"), 
                         "Plotly has built in download function, redundant"
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "warning", solidHeader = TRUE, title = "ECAP EXPERIMENT ON HOLD",
                         "As of October 2018 the ECAP experiment has been put on hold and so continued developement of this tool has been suspended"))
            )
    ),
    tabItem(
      tabName = "table",
      box(width = 6, status = "primary", solidHeader = TRUE, title = "Measured Data / NOT IMPLEMENTED", 
          selectInput("dates", "Date:", choices = c("1", "2")), 
          selectInput("speclist", "Spectra:", choices = c("a", "b")), 
          downloadButton('dldm', 'Download Data'), 
          br(), br(), 
          tableOutput("table1")
      ),
      box(width = 6, status = "primary", solidHeader = TRUE, title = "Calculated Data / NOT IMPLEMENTED", 
          selectInput("dates", "Date:", choices = c("1", "2")), 
          selectInput("speclistb", "Spectra:", choices = c("a", "b")), 
          downloadButton("dldc", "Download Data"),
          br(), br(), 
          tableOutput("table2")
      )
    ),
    tabItem(
      tabName = "method", 
      fluidRow(
        column(width = 4, 
               tabBox(width = NULL, 
                      tabPanel(h5("Data Formatting"), 
                               "Getting data from Raw to Processed"),
                      tabPanel(h5("Calibration Curve"), 
                               "Methodology for calculating Wavelength "),
                      tabPanel(h5("Extra Space"))
               )
        )
      )
    ),
    tabItem(
      tabName = "info",
      fluidRow(
        column(width = 4, 
               tabBox(width = NULL,
                      tabPanel(h5("Experiment"),
                               "Evaporative Coating at Atmospheric Pressure (ECAP) uses a microwave plasma for cleaning and activating the surfaces of different metallic substrates, Optical Emission Spectroscopy (OES) allows us to determine specific characteristics of the plasma generated.  Will add more info as needed"), 
                      tabPanel(h5("Equipment"), 
                               "OES system, Plasma generating system, other diagnostic stuff: Schlieren system/data"), 
                      tabPanel(h5("Team"), 
                               "Lucia (Postdoc Supervisor), Drew (Undergrad supervisor for ECAP), Nirbhav (Tandem with Schlieren experiment), Andrew (URA), Emily (URA), Brock (URA), anyone else?  Fill in details/roles")
               )
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  # date <- switch (
  #   input$dates,
  #   "Apr_13_2018" = action,
  #   "Jun_11_2018" =
  # ) For adding more experiment sets later
  output$plot <- renderPlotly({
    d_m = switch(
      input$speclist,
      "He 200W" = df_4_13$data[[5]],
      "He 400W" = df_4_13$data[[6]],
      "He 500W" = df_4_13$data[[7]],
      "He 600W" = df_4_13$data[[8]]
    )
    
    d_c = switch(
      input$speclist,
      "He 200W" = df_4_13$data[[1]],
      "He 400W" = df_4_13$data[[2]],
      "He 500W" = df_4_13$data[[3]],
      "He 600W" = df_4_13$data[[4]]
    )
    ggplot(d_m, aes(Wavelength, Intensity, color = "measured")) +
      geom_line() +
      geom_line(data = d_c, aes(Wavelength, Intensity, color = "calculated"))
  })
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d))
      "Hover on a point for data, click and drag to zoom, double click to reset, use the top bar to scale/zoom/download plot"
    else
      d
  })
}

shinyApp(ui, server)

