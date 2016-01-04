library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("ROULETTE SIMULATOR"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
   selectInput("system", "Choose system:", 
                choices = c("Martingale", "Dalembert","Fibonacci",
		    "1-3-2-6","Paroli")),
# The user interface file that makes up the GUI in the Shiny App

   sliderInput("obs", 
                "Number of spins:", 
                min = 1,
                max = 10000, 
                value = 5000),

   selectInput("color", "Choose color:", 
                choices = c("Black", "Red")),

   helpText("Note: choose which color to bet on the first spin on the wheel"),
 
    numericInput("bet", 
		"Minimal bet:", 10),

    submitButton("SIMULATE"),
   
    helpText("Note: you must change at least one of the parameters above to simulate")

	
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))
