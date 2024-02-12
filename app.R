# imports libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# imports the csv file for simplex method
# imports all necessary function and R files
database <- read.csv(file = "C:/Users/USER/Documents/nadz_codes/Anareta_150PROJECT/database.csv")
source("QSplineInterpolation.R")
source("PolynomialRegression.R")
source("SimplexMethod.R")

# ui for all the tabs/functions
ui <- dashboardPage(
  skin = "purple", # theme of the program
  dashboardHeader(title = "CMSC 150 PROJECT"), # title 
  dashboardSidebar(
    sidebarMenu( # all the sidebar items
      menuItem("Quadratic Spline Interpolation", tabName = "spline", icon = icon("q")),
      menuItem("Polynomial Regression", tabName = "poly", icon = icon("chart-line")),
      menuItem("Simplex Method", tabName = "simplex", icon = icon("utensils")),
      menuItem("User's Manual", tabName = "user_manual", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems( 
      tabItem("spline", # ui for quadratic spline interpolation
        fluidPage(
          h1("QUADRATIC SPLINE INTERPOLATION", # title and its style
             style = "font-family: Verdana; font-weight: bold; font-size: 32px; color: #685cac;"),
          sidebarLayout(
            sidebarPanel( # input
              fileInput("spline_file", "CHOOSE CSV FILE:"), # csv file input
              numericInput("spline_estimate", "ENTER ESTIMATE:", value = 5), # estimate value
              textOutput("spline_txt"), # prints the x values
              tags$head(tags$style("#spline_txt{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
              )),
              actionBttn("spline_btn", "PLOT", style = "stretch", color = "royal", size = "sm") # plot button
            ), 
            mainPanel( # output
              h4("Estimated Value", # outputs the estimated value
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px;"),
              verbatimTextOutput("spline_val"),
              h4("Correct Function for the Estimate", # outputs the function used
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("spline_correct"),
              h4("Function Per Interval", # outputs all functions per interval
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("spline_func"),
              h4("Quadratic Spline Interpolation Graph", # outputs the quadratic spline interpolation graph
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              plotOutput("spline_plot", width = "100%")
            )
          )
        )
      ),
      tabItem("poly", # ui for polynomial regression
        fluidPage(
          h1("POLYNOMIAL REGRESSION", # title and its style
             style = "font-family: Verdana; font-weight: bold; font-size: 32px; color: #685cac;"),
          sidebarLayout(
            sidebarPanel( # input
              fileInput("poly_file", "CHOOSE CSV FILE:"), # csv file input
              sliderInput("poly_degree", "ENTER DEGREE:", min = 1, max = 20, value = 3, step = 1), # slider for degree
              numericInput("poly_estimate", "ENTER ESTIMATE:", value = 5), # estimate value
              textOutput("poly_txt"), # prints the x values
              tags$head(tags$style("#poly_txt{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
              )),
              actionBttn("poly_btn", "PLOT", style = "stretch", color = "royal", size = "sm") # plot button
            ),
            mainPanel( # output
              h4("Estimated Value", # outputs the estimated value
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("poly_val"),
              h4("Polynomial Function", # outputs the polynomial function
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("poly_func"),
              h4("Polynomial Regression Graph", # polynomial regression graph
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              plotOutput("poly_plot", width = "100%")
            )
          )
        )
      ), 
      tabItem("simplex", # ui for simplex method
        fluidPage(
          h1("SIMPLEX METHOD", # title and its style
             style = "font-family: Verdana; font-weight: bold; font-size: 32px; color: #685cac;"),
          h4("Diet Solver", #subtitle and its style
             style = "font-family: Verdana; font-weight: bold; font-size: 20px;"),
          sidebarLayout(
            sidebarPanel( # input
              pickerInput( # dropdown checkbox for selection of food
                inputId = "simplex_picker",
                label = "SELECT/DESELECT FOOD:", 
                choices = database[,1],
                options = list(
                  `actions-box` = TRUE), 
                multiple = TRUE
              ),
              actionBttn("simplex_btn", "SOLVE", style = "stretch", color = "royal", size = "sm"), # solve button
              h4("CHOSEN FOOD:", # printing statement
                 style = "font-family: Verdana; font-weight: bold; font-size: 14px"),
              textOutput("simplex_desc"), # prints the number of food
              tags$head(tags$style("#simplex_desc{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
              )),
              textOutput("simplex_txt"), # prints the list of selected food
              tags$head(tags$style("#simplex_txt{color: grey;
                                 font-size: 12px;
                                 }"
              ))
            ),
            mainPanel( # output
              h3("Result Table", # outputs the table/matrix
                 style = "font-family: Verdana; font-weight: bold; font-size: 20px"),
              tableOutput("simplex_table"),
              h4("Final Solution", # outputs the final solution
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("simplex_final"),
              h4("Basic Solutions", # outputs the basic solutions every iteration
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("simplex_solutions"),
              h4("Initial Tableau", # outputs the initial tableau
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("simplex_init"),
              tags$head(tags$style("#simplex_init{color: black;
                                 font-size: 10px;
                                 }"
              )),
              h4("Matrix Per Iteration", # outputs the tableau every iteration
                 style = "font-family: Verdana; font-weight: bold; font-size: 16px"),
              verbatimTextOutput("simplex_mat"),
              tags$head(tags$style("#simplex_mat{color: black;
                                 font-size: 9.5px;
                                 }"
              ))
            )
          )
        )
      ),
      tabItem("user_manual", # ui for user's manual
        fluidPage(
          h1("USER'S MANUAL", # title and its style
             style = "font-family: Verdana; font-weight: bold; font-size: 32px; color: #685cac;"), 
          fluidRow( # renders images of manual for each function
            column(4, imageOutput("img_qsi")),
            column(4, imageOutput("img_poly")),
            column(4, imageOutput("img_simplex"))
          )
        )
      ),
      tabItem("about", # ui for about
        fluidPage(
          h1("ABOUT", # title and its style
             style = "font-family: Verdana; font-weight: bold; font-size: 32px; color: #685cac;"),
          sidebarLayout(
            sidebarPanel( # about the creator
              img(src = "me1.png", width = "30%", align = "right"),
              h4("NADINE ARABEL A. ANARETA",
                 style = "font-family: Verdana; font-weight: bold; font-size: 14px; color: #685cac;"),
              h4("A second-year BSCS student",
                 style = "font-family: Verdana; font-weight: bold; font-size: 12px;"),
              h4("She created this program as part of the completion requirements for CMSC 150, 
                 Section B2L, under the supervision of Sir Ariel Doria.",
                 style = "font-family: Verdana; font-weight: normal; font-size: 12px;"),
              h4("Date: December 2023",
                 style = "font-family: Verdana; font-weight: bold; font-size: 9px;"),
            ),
            mainPanel( # about the program
              h5("This program was created as a part of the CMSC 150 course project, 
             specifically to meet the project's requirements. The program incorporates 
             generic solvers-- quadratic spline interpolation & polynomial regression, 
             and a diet solver using the simplex method for minimization. 
             Its purpose is to show applications of mathematical optimization techniques to 
             solve diverse problems, fulfilling the requirements of their coursework.",
              style = "font-family: Verdana; font-weight: bold; font-size: 12px; text-align: justify;")
            )
          )
        )
      )
    )
  )
)

# server for all functions
server <- function(input, output, session){
  
  ################ QUADRATIC SPLINE INTERPOLATION ################
  spline_data <- reactiveValues(spline_file = NULL, spline_result = NULL, spline_plot = NULL,
                                spline_x = NULL, spline_y = NULL, spline_estimate = NULL) # reactive values
  
  # reactive to csv file of QSI
  observeEvent(input$spline_file, {
    spline_data$spline_file <- read.csv(input$spline_file$datapath, header = FALSE, sep = ",", dec = ".") # gets info
    
    # gets the x and y values
    x1 <- spline_data$spline_file[,1]
    y1 <- spline_data$spline_file[,2]
    
    x1 <- x1[order(x1)]
    
    # updates estimate according to x input
    updateNumericInput(session, "spline_estimate", "ENTER ESTIMATE:", min = min(x1), max = max(x1))
    
    output$spline_txt <- renderText({ # prints the x values to avoid errors
      paste("Values of x: ", paste0(x1, collapse = ", "))
    })
  })
  
  # reactive to spline plot button
  observeEvent(input$spline_btn,{
    if (is.null(input$spline_file)){ # if there is no csv file input
      output$spline_correct <- renderText(paste("No File Attached"))
      output$spline_func <- renderText(paste("No File Attached"))
      output$spline_val <- renderText(paste("No File Attached"))
    }else{
      req(input$spline_estimate) # required estimate value
      
      # gets the x and y values
      x <- spline_data$spline_file[,1]
      y <- spline_data$spline_file[,2]
      
      # solves and gets the answer using qsi
      spline_answer <- QuadraticSplineInterpolation(list(x, y), length(x)-1, input$spline_estimate)
      spline_data$spline_result <- spline_answer
      
      # render plot
      output$spline_plot <- renderPlot({
        result <- spline_data$spline_result
        
        # performs plotting if not NULL
        if (!is.null(result)){
          xlim <- c(min(x), max(x))
          plot(x, y, main = "Quadratic Spline Interpolation", xlab = "X", ylab = "Y", pch = 16)
          
          for (i in 1:length(result$Function_Per_Interval)) {
            PlotQuadraticSpline(result$Function_Per_Interval[[i]], xlim, col = rainbow(length(result$Function_Per_Interval))[i])
          }
        }
        
        # outputs the correct function
        output$spline_correct <- renderPrint({
          if (is.null(spline_data$spline_result$Function)){
            paste("Invalid Estimate Value")
          }else{
            spline_data$spline_result$Function
          }
        })
        
        # outputs the functions per interval
        output$spline_func <- renderPrint({
          if (is.null(spline_data$spline_result$Function_Per_Interval)){ 
            paste("Invalid Estimate Value")
          }else{
            spline_data$spline_result$Function_Per_Interval
          }
        })
        
        # outputs the estimated value
        output$spline_val <- renderText({
          if (input$spline_estimate > max(x)){
            paste("Invalid Estimate Value")
          }else{
            spline_data$spline_result$Estimated_Value
          }
        })
      })
    }
  })
  
  ################ POLYNOMIAL REGRESSION ################
  
  poly_data <- reactiveValues(poly_file = NULL, poly_result = NULL, poly_plot = NULL,
                              poly_x = NULL, poly_y = NULL, poly_degree = NULL, poly_estimate = NULL) # reactive values
  
  # reactive to csv file of polynomial regression
  observeEvent(input$poly_file, {
    poly_data$poly_file <- read.csv(input$poly_file$datapath, header = FALSE, sep = ",", dec = ".")
    
    # gets the a and b values
    a1 <- poly_data$poly_file[,1]
    b1 <- poly_data$poly_file[,2]
    
    a1 <- a1[order(a1)]
    
    # updates slider input according to a input values
    updateSliderInput(session, "poly_degree", "ENTER DEGREE:", min = 1, max = length(a1)-1, step = 1)
    
    output$poly_txt <- renderText({ # prints the x values to avoid errors
      paste("Values of x: ", paste0(a1, collapse = ", "))
    })
  })
  
  # reactive to the plot button
  observeEvent(input$poly_btn,{
    if (is.null(input$poly_file)){ # if there is no csv file input
      output$poly_func <- renderText(paste("No File Attached"))
      output$poly_val <- renderText(paste("No File Attached"))
    }else{
      req(input$poly_estimate) # required estimate value
      
      # gets a and b values
      a <- poly_data$poly_file[,1]
      b <- poly_data$poly_file[,2]
      
      # solves and gets answer using polynomial regression
      poly_answer <- PolynomialRegression(input$poly_degree, list(a, b), input$poly_estimate)
      poly_data$poly_result <- poly_answer
      
      # outputs the plot
      output$poly_plot <- renderPlot({
        req(poly_data$poly_result) # required result
        result <- poly_data$poly_result
        funct1 <- result$polynomial_function
        plot(a, b, pch = 16, col = "#685cac", main = "Polynomial Regression", xlab = "X", ylab = "Y") #plots the curve
        curve(funct1, add = TRUE, col = "#685cac", lwd = 2)
      })
      
      # outputs the polynomial function
      output$poly_func <- renderPrint({
        req(poly_data$poly_result)
        poly_data$poly_result$polynomial_function
      })
      
      # outputs the estimates value
      output$poly_val <- renderText({
        req(poly_data$poly_result)
        poly_data$poly_result$value
      })
    }
  })
  
  ################ SIMPLEX METHOD ################
  
  # concatenates the selected food names
  solve_simplex <- function(selected_food) {
    result <- paste(selected_food, collapse = " + ")
    
    return(result)
  }

  # reactive to simplex method button
  observeEvent(input$simplex_btn, {
    selected_food <- input$simplex_picker
    result <- solve_simplex(selected_food)
    indices <- which(database[,1] %in% selected_food) # gets indices and compares with the database
    
    # if there is no selected food
    if (is.null(selected_food)){
      output$simplex_table <- renderTable(NoInput())
      output$simplex_final <- renderText(paste("No Selected Food"))
      output$simplex_solutions <- renderText(paste("No Selected Food"))
      output$simplex_init <- renderPrint(invisible())
      output$simplex_mat <- renderPrint(invisible())
      output$simplex_desc <- renderText(paste("No Selected Food"))
    }else{
      
      # outputs the table/matrix
      output$simplex_table <- renderTable({
        if (is.null(TableVal(database, indices))){
          NoInput()
        }else{
          TableVal(database, indices)$matrix
        }
      })
      
      # outputs the final solution
      output$simplex_final <- renderPrint({
        if (is.null(TableVal(database, indices))){
          paste("The Problem is Infeasible")
        }else{
          TableVal(database, indices)$fsol
        }
      })
      
      # outputs the basic solutions every iteration
      output$simplex_solutions <- renderPrint({
        if (is.null(SimplexMethod(database, indices))){
          paste("The Problem is Infeasible")
        }else{
          SimplexMethod(database, indices)$BASIC_SOLUTIONS
        }
      })
      
      # outputs the matrices/tableau per iteration
      output$simplex_mat <- renderPrint({
        if (is.null(SimplexMethod(database, indices))){
          return(invisible())
        }else{
          SimplexMethod(database, indices)$MATRICES_PER_ITERATION
        }
      })
      
      # outputs the initial tableau
      output$simplex_init <- renderPrint({
          InitialTableau(database, indices)
      })
      
      # outputs the total number of selected fod
      output$simplex_desc <- renderText({
        paste0("Total of ", length(indices), " food!", sep = "")
      })
    }
    
    # outputs the list of selected food
    output$simplex_txt <- renderText(result)
    
  })
  
  ################ USER'S MANUAL ################

  # qsi manual
  output$img_qsi <- renderImage({
    
    list(src = "www/qsi.png",
         width = "100%")
  }, deleteFile = F)
  
  # polynomial regression manual
  output$img_poly <- renderImage({
    
    list(src = "www/poly.png",
         width = "100%")
  }, deleteFile = F)
  
  # simplex method manual
  output$img_simplex <- renderImage({
    
    list(src = "www/simplex.png",
         width = "100%")
  }, deleteFile = F)

}

shinyApp(ui = ui, server = server)