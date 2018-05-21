```yaml

Name of Quantlet: Randnmbrs

Published in:      
  
Description: Different functions for creating random numbers, includes Shiny App.
 
Keywords: random numbers, shiny app, plots, performance 

See also:

Author: Nikolaj Bewer
  
Submitted: Fri, March 30 2018 by Nikolaj Bewer

```

<img src=https://user-images.githubusercontent.com/36474992/40311574-7b67c460-5d10-11e8-8a79-a892aeae7ff5.png>


```r
n = 1000
x = 1
Seed = 654321

mcg.lehmer = function(n, a = 48271, m = 2147483647) {
    rnum      = as.numeric(Sys.time())
    Rnumb.out = vector(length = n)
    for (i in 1:n) {
        rnum         = (a * rnum)%%m
        Rnumb.out[i] = rnum/m
    }
    return(Rnumb.out)
}


LFG = function(n, j = 7, k = 10, m = 2147483647, Seed = c(4, 8, 2, 8, 3, 9, 1, 8, 7, 1)) {
    gn.out = NULL
    for (i in 0:n) {
        Output    = (Seed[j + i] + Seed[k + i])%%m
        i         = i + 1
        Seed      = append(Seed, Output)
        gn.out[i] = Output
    }
    return(gn.out)
}

msm = function(Seed, n) {
    a         = nchar(Seed)
    b         = nchar(Seed)/2
    empty.vec = NULL
    for (i in 1:n) {
        Seedseed  = Seed^2
        Seed      = (Seedseed%/%10^b)%%10^a
        empty.vec = c(empty.vec, Seed)
    }
    return(empty.vec)
}

ui = fluidPage(titlePanel("PRNG"), sidebarLayout(sidebarPanel(helpText("If checked, 
               the numbers are stored as"), 
    checkboxInput("check", "Lehmer RNG", value = FALSE), helpText("Rnd.MCG"), checkboxInput("check1", "Lagged Fibonacci Generator", 
        value = FALSE), helpText("Rnd.LFG"), checkboxInput("check2", "Mersenne-Twister", value = FALSE), helpText("Rnd.MT"), 
    checkboxInput("check3", "Middle-square method", value = FALSE), helpText("Rnd.MSM"), helpText("Note: Additionally to checking the box, 
               the respective panel has to be clicked, 
               in order to store the numbers"), 
    numericInput("n", "Generated numbers", n, step = 100), numericInput("x", "Visibility", x, step = 0.05), numericInput("Seed", 
        "Seed (only for MSM)", Seed)), mainPanel(tabsetPanel(tabPanel("Lehmer RNG", plotOutput("plot")), tabPanel("LFG", plotOutput("plot1")), 
    tabPanel("Mersenne-Twister", plotOutput("plot2")), tabPanel("MSM", plotOutput("plot3"))))))


server = function(input, output) {
    output$plot = renderPlot({
        plot(mcg.lehmer(input$n), type = "p", main = "Lehmer RNG", xlab = "Generated random numbers", ylab = "Interval", cex = input$x)
        if (input$check) {
            Rnd.MCG <<- mcg.lehmer(input$n)
        }
    })
    output$plot1 = renderPlot({
        plot(LFG(input$n), type = "p", main = "Lagged Fibonacci Generator", xlab = "Generated random numbers", ylab = "Interval", 
            cex = input$x)
        if (input$check1) {
            Rnd.LFG <<- LFG(input$n)
        }
    })
    output$plot2 = renderPlot({
        plot(runif(input$n), type = "p", main = "Mersenne-Twister", xlab = "Generated random numbers", ylab = "Interval", 
            cex = input$x)
        if (input$check2) {
            Rnd.MT <<- runif(input$n)
        }
    })
    output$plot3 = renderPlot({
        plot(msm(input$Seed, input$n), type = "p", main = "Middle-square Method", xlab = "Generated random numbers", ylab = "Interval", 
            cex = input$x)
        if (input$check3) {
            Rnd.MSM <<- msm(input$Seed, input$n)
        }
    })
}


shinyApp(ui = ui, server = server)

```
