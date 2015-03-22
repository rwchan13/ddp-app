# server.R

library(poweRlaw)
library(ggplot2)
library(dplyr)
source("plaw.R")

data(moby)
tab <- table(moby)
value <- as.numeric(names(tab))
frequency <- as.vector(tab)
raw_dat <- data.frame(value = value, frequency = frequency)

raw_dat <- mutate(raw_dat, 
                  pdf = frequency/sum(frequency),
                  ccdf = rev(cumsum(rev(pdf))))

ks <- sapply(raw_dat$value, ksCal, raw_dat=raw_dat)
i_xmin <- raw_dat$value[which(ks == min(ks))]
mod_pdf <- modpdfCal(i_xmin, raw_dat)
mod_ccdf <- modccdfCal(i_xmin, raw_dat)

shinyServer(function(input, output) {
  
  mod_ccdf <- reactive({
    if (input$xmin == 0)
      modccdfCal(i_xmin, raw_dat)
    else
      modccdfCal(as.numeric(input$xmin), raw_dat)
  })
  
  xmin <- reactive({
    input$goButton
    isolate({
      if (input$xmin == 0)
        i_xmin
      else
        as.numeric(input$xmin)
    })
  })
 
  mod <- reactive({    
    switch(input$func,
           "Probability Density Function" = modpdfCal(xmin(), raw_dat), 
           "Inverse Cummulative Density Function" = modccdfCal(xmin(), raw_dat))
  })
  
  output$cmin <- renderText({
    paste("Current starting point is ", xmin(), " or ", 
          format(log(xmin()), digits=5), " in the log scale.", sep="")
  })
  
  output$plot <- renderPlot({
      dat <- switch(input$func,
                    "Probability Density Function" = raw_dat$pdf, 
                    "Inverse Cummulative Density Function" = raw_dat$ccdf)
      
      if ("2" %in% input$logscale){
        y <- log(dat)
        my <- log(mod())
      } else {
        my <- mod()
        y <- dat
      }
      
      if ("1" %in% input$logscale)
        x <- log(raw_dat$value)
      else
        x <- raw_dat$value
      
      dis <- data.frame(x = x, y = y, my = my)
      p <- ggplot(dis, aes(x = x, y = y)) + geom_point() +
        geom_line(aes(x = x, y = my), color = "red") + 
        xlab("Values") + ylab(input$func)
      plot(p)
  })
})
