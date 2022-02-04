

library(shiny)
library(readxl)
library(dplyr)
library(DT)



car <- function(data,c){
    ##### Data will be uploaded raw
    # A few cleaning and data handling
    aux <- c(3,9)
    for (i in aux) {
        data[1,i] <- paste0("mm",i)
    }
    
    data <- data[,-c(7,11,13,15,17,19,21,23,25)]
    
    colnames(data) <- data[1,]
    
    data <- data[-c(1),]
    data$mm3 <- as.numeric(data$mm3)
    data$mm9 <- as.numeric(data$mm9)
    
    #####
    c1 <- which(data[,1]==c) # Which carcas axel diameter matchs with the collar
    compare1 <- which.min(abs(data$mm9-data$mm3[c1])) #First comparison
    aux_data <- data[-compare1,]
    compare2 <- which.min(abs(aux_data$mm9-aux_data$mm3[c1])) #Second
    
    # Data handling
    aux <- data[c(compare1,compare2),8:ncol(data)]
    p <- c()
    p[1] <- round((data$mm9[compare1]/data$mm3[c1]-1)*100, digits = 2) # Difference between the carcass and the first collar type
    p[2] <- round((data$mm9[compare2]/data$mm3[c1]-1)*100, digits = 2) # Difference between the carcass and the second collar type
    
    #Table per se
    aux <- cbind(p,data[c1,c(3,5,6)],aux)
    # Rename columns names
    colnames(aux) <- c("Difference %",
                       "FAE diameter (mm)",
                       "FAE, E - length (mm)",
                       "FAE, ES - Keyway",
                       "Gola diameter (mm)",
                       paste0("EU max-", intToUtf8(0x03C6), "(mm)"),
                       paste0("EU min-", intToUtf8(0x03C6), "(mm)"),
                       "AH length (mm)",
                       "Relation",
                       "E max-width Gola (mm)",
                       "E min-width Gola (mm)",
                       "F max-axle end (mm)",
                       "F min-axle end (mm)")
    # Set binary parameter to be used to color the rows.
    aux$StyleCol <- c(ifelse(abs(aux[1,1])<=abs(aux[2,1]),1,0),
                      ifelse(abs(aux[2,1])==abs(aux[1,1]),1,0))
    return(aux)
}

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Gola parameters by carcass model"),

    # Sidebar with a input file, select input and 
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            p("After selecting the desired carcass model the gola specifications wil be displayed. "),
            selectInput("variable", "Engine Carcass Model:",
                        c("63" = "63",
                          "71" = "71",
                          "80" = "80",
                          "L80" = "80",
                          "90S" = "90S",
                          "L90S" = "L90S",
                          "90L" = "90L",
                          "L90L" = "L90L",
                          "100L" = "100L",
                          "L100L" = "L100L",
                          "112M" = "112M",
                          "L112M" = "L112M",
                          "132S" = "132S",
                          "L132S" = "L132S",
                          "132M" = "132M",
                          "L132M" = "L132M",
                          "132ML" = "132ML",
                          "L132ML" = "L132ML",
                          "160M" = "160M",
                          "160L" = "160L",
                          "L160L" = "L160L",
                          "180M" = "180M",
                          "L180M" = "L180M",
                          "180L" = "180L",
                          "200M" = "200M",
                          "200L" = "200L",
                          "225SM1)" = "225SM1)",
                          "225SM" = "225SM",
                          "250SM1)" = "250SM1)",
                          "250SM" = "250SM",
                          "280SM1)" = "280SM1)",
                          "280SM" = "280SM",
                          "315SM1)" = "315SM1)",
                          "315SM" = "315SM",
                          "355ML1)" = "355ML1)",
                          "355ML"= "355ML",
                          "355AB" = "355AB")),
            br(),
            br()
        ),
        
        mainPanel(
            dataTableOutput("line"),
            #Download button (excell)
            downloadButton('download',"Download the data"),
            h1("Caption:"),
            p("Difference: percentage difference between the first and second most close values."),
            p("ES, EU, AH, E and F are values attributed to the respective technical draw."),
            p("FAE - front axel end")
            )
    )

)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Read Uploaded file
    aux2 <- eventReactive(input$file, {
        read_excel(input$file$datapath, sheet = 3)
    })
    
    # Apply the data handling function above
    aux1 <- reactive({car(aux2(),input$variable)})
    
    # Ouputa data per se with colored row and  horizontal sline bar
    output$line <- renderDataTable({
        datatable(aux1(),
                  options = list(paging = FALSE, scrollX = TRUE))%>%
                    formatStyle("StyleCol",
                                target = "row",
                                backgroundColor = styleEqual(c(1,0), c("green","red")))})
    output$download <- downloadHandler(
        filename = function(){"thename.csv"}, 
        content = function(fname){
            write.csv(thedata(), fname)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
