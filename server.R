# The server file required to run the Shiny App

library(shiny)

source("Martingale_inarow_exact.R")
source("Dalembert.R")
source("Paroli.R")
source("1-3-2-6.R")
source("Fibonacci.R")

df<-data.frame(x=c("Martingale","Dalembert","Fibonacci","1-3-2-6","Paroli"),
               y=c("Lets.play.Martingale","Lets.play.Dalembert","Lets.play.Fibonacci",
                   "F1326","Lets.play.Paroli"))


# Define server logic required to generate and plot a random distribution
      shinyServer(function(input, output) {

  
      output$distPlot <- renderPlot({

    # run Lets.play function
      color<-ifelse(input$color=="Black",1,0)
       
      dist<-do.call(as.character(df$y[which(df$x==input$system)]),list(input$bet,color,input$obs))
       
      par(mar=c(5, 6, 4, 2))
          plot(1:length(dist[[3]]),dist[[3]],type="n",xlab="",main=paste("Simulation using the",input$system,"system"),
		            ylab="",bty="l",cex.lab=1.3,las=1)

    		  lines(1:length(dist[[3]]),dist[[3]],col="red")
		      loc <- par("usr")
		      text(loc[1], loc[4]+(loc[4]*0.1), labels=expression(paste("Net profit\n(your currency)")), adj = c(1,0), xpd = T)
		      mtext(side = 1, "Number of spins", line = 3)

  })
})
