library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Demerit classification"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("datafile", "Upload dataset (CSV file)",
                    accept = c("text/csv", "text/comma-separated-values,text/plain",
                               ".csv")),
          selectInput("defects", "Select product defects:", 
                      choices = c("Leakage", "Bulging", "Rust and Corrosion", "Bacterial Contamination","Texture Changes",
                                  "Nutrient Loss", "Off Odors or Unusual Smells", "Expired"),
                      multiple = TRUE),
          verbatimTextOutput("selectedOptions")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabPanel("Class A Defects", tableOutput("classAdefects")),
          tabPanel("Class B Defects", tableOutput("classBdefects")),
          tabPanel("Class C Defects", tableOutput("classCdefects")),
          tabPanel("Class D Defects", tableOutput("classDdefects")),
          tabPanel("Class E Defects", tableOutput("classEdefects")),
          tabPanel("Plot", plotOutput("plot"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  selectedDefects <- reactive({
    defects <- input$defects
    classification <- character(length(defects))
    
    for (i in seq_along(defects)) {
      if (defects[i] %in% c("Leakage", "Bacterial Contamination")) {
        classification[i] <- "Class A"
      } else if (defects[i] %in% c("Bulging", "Rust and Corrosion")) {
        classification[i] <- "Class B"
      } else if (defects[i] %in% c("Texture Changes", "Nutrient Loss")) {
        classification[i] <- "Class C"
      } else if (defects[i] == "Off Odors or Unusual Smells") {
        classification[i] <- "Class D"
      } else if (defects[i] == "Expired") {
        classification[i] <- "Class E"
      }

    }
    
    data.frame(Defect = defects, Classification = classification)
  })
  
  output$classAdefects <- renderTable({
    subset(selectedDefects(), Classification == "Class A")
  })
  
  output$classBdefects <- renderTable({
    subset(selectedDefects(), Classification == "Class B")
  })
  
  output$classCdefects <- renderTable({
    subset(selectedDefects(), Classification == "Class C")
  })
  
  output$classDdefects <- renderTable({
    subset(selectedDefects(), Classification == "Class D")
  })
  
  output$classEdefects <- renderTable({
    subset(selectedDefects(), Classification == "Class E")
  })
  
  df <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      return(NULL)
    }
    read.csv(file$datapath)
  })
  
  output$plot <- renderPlot({
    if (is.null(df())) {
      return()
    }
    
    d=c();
    
    for (i in 1:length(df()[[1]])){
      sum = 100*df()$A[i] + 50*df()$B[i] + 10*df()$C[i] + df()$D[i] + df()$E[i];
      d = append(d, sum);
    }
    
    meanA = sum(df()$A)/length(df()$A);
    meanB = sum(df()$B)/length(df()$B);
    meanC = sum(df()$C)/length(df()$C);
    meanD = sum(df()$D)/length(df()$D);
    meanE = sum(df()$E)/length(df()$E);
    
    D = sum(d);
    mean = D/length(df()[[1]]);
    std = sqrt(((100^2)*meanA + (50^2)*meanB + (10^2)*meanC + meanD + meanE)/length(df()$A))
    UCL = mean + 3*std
    LCL = mean - 3*std
    
    if(LCL<0){
      LCL = 0
    }
    
    #plot total number of demerits
    plot(1:length(df()$A),d, type = "b",
         main="The demerit chart", xlab = "Unit",
         ylab = "Total of demerits",
         ylim = c(min(d),max(d)));
    points(1:length(df()$A),d, pch = 16, col = ifelse(d >= UCL | d <= LCL, "red", "green"));
    
    abline(h = UCL, lty = 2)
    abline(h = mean)
    abline(h = LCL, lty = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
