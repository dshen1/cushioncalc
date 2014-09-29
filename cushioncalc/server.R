################################################################################
## Server code for Shiny app to calculate Cushion diamond expected price
## Author: matt.b.hudson@gmail.com
################################################################################

## Call libraries
library(memisc)
library(shiny)
library(scales)
library(ggplot2)

## Import data and new column for the log price
load("BigDiamonds.Rda")
diamondsbig$logprice  <-  log(diamondsbig$price) 

## Create a model to estimate value of diamond based on attributes
m1 = lm(logprice~  I(carat^(1/3)), 
        data=diamondsbig[diamondsbig$price < 50000 &
                           diamondsbig$cert == "GIA" &
                           diamondsbig$price > 10000,])
m2 = update(m1, ~ . + carat)
m3 = update(m2, ~ . + cut )
m4 = update(m3, ~ . + color + clarity)
mtable(m1, m2, m3, m4)

## Function to calc cube root in plots
cubroot_trans = function() trans_new("cubroot", 
                                     transform= function(x) x^(1/3), 
                                     inverse = function(x) x^3 )

## Call server
shinyServer(function(input, output) {
  
  ## Create text summary
  output$text1 <- renderText({
    thisDiamond <- data.frame(carat = input$carat, 
                              cut = input$cut, 
                              color = input$color, 
                              clarity=input$clarity)
    modEst <- predict(m4, newdata = thisDiamond, 
                      interval="prediction", level = .95)
    
    paste("Expected Value:", dollar(exp(modEst[1])),"    ",
          "Lower Bound:", dollar(exp(modEst[2])),"     ",
          "Upper Bound:", dollar(exp(modEst[3])))
  })
  
   ## Plot summary
   output$plot1 <- renderPlot({
     thisDiamond <- data.frame(carat = input$carat, 
                               cut = input$cut, 
                               color = input$color, 
                               clarity=input$clarity)
     modEst <- predict(m4, newdata = thisDiamond, 
                       interval="prediction", level = .95)
     expected <- exp(modEst[1])
     lower <- exp(modEst[2])
     upper <- exp(modEst[3])
     
     qplot(carat, price, data=
            subset(diamondsbig,
                   cut == input$cut & cut == input$cut &
                     color == input$color & clarity == input$clarity & cert == 'GIA'),
           geom="point", alpha=I(.3)) +
       scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
                          breaks = c(0.2, 0.5, 1, 2, 3, 4)) +
       scale_y_continuous(trans=log10_trans(), limits = c(350,70000),
                          breaks = c(350, 1000, 5000, 10000, 15000, 20000, 30000, 40000)) +
       theme_bw() + ggtitle("Cushion Diamonds by price") +
       geom_point(data = data.frame(carat = input$carat,
                                    price = expected), aes(x = carat, y = price, 
                                                           color = "red")) +
       geom_errorbar(data=data.frame(carat = input$carat,
                                     price = expected,
                                     lower = lower,
                                     upper = upper),
                     aes(x=carat, ymin=lower, ymax=upper, y = price), colour = "red")

   })
})

