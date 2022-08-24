#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Synapsis"),
  dashboardSidebar(
    width = 375,
    sidebarMenu(
      menuItem("Proportional Sampling Error for Infinite Universes", tabName = "EMPUI", icon = icon("th")),
      menuItem("Proportional Sampling Error for Finite Universes", tabName = "EMPUF", icon = icon("th")),
      menuItem("Non-proportional Sampling Error", tabName = "EMNP", icon = icon("th")),
      menuItem("Sample Size for Infinite Universes", tabName = "TMUI", icon = icon("th")),
      menuItem("Sample Size for Finite Universes", tabName = "TMUF", icon = icon("th")),
      menuItem("Calculation of Number of Sample Points", tabName = "CNPM", icon = icon("th")),
      menuItem(HTML("Significant Differences between<br/> Proportions of the Same Sample"), tabName = "PMM", icon = icon("th")),
      menuItem(HTML("Significant Differences between<br/> Proportions of Independent Samples."), tabName = "PMI", icon = icon("th")),
      menuItem(HTML("Significant Differences Between<br/>Two Distributions."), tabName = "DD", icon = icon("th")),
      menuItem(HTML("Significant Differences Between<br/>Two Dependent Sample Means."), tabName = "DMMD", icon = icon("th")),
      menuItem(HTML("Significant Differences Between<br/>Two Independent Sample Means."), tabName = "DMMI", icon = icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "EMPUI",
             fluidPage( fluidRow(
           h2("Proportional Sampling Error for Infinite Universes"),
           numericInput("pr1","Probability (%)", value = 0.5, min = 0, max= 1),
           numericInput("ms1","Sample Size", value =400),
           div(style="border: 2px solid black; padding: 1rem",
           h3("Statistical Error"),
           span(textOutput("empui"), style = "color:red;font-weight: bold;font-size: 20px "))
      )
      )),
      
      # Second tab content
      tabItem(tabName = "EMPUF",
              fluidPage( fluidRow(
              h2("Proportional Sampling Error for Finite Universes"),
              numericInput("pr2","Probability (%)", value = 0.5, min = 0, max= 1),
              numericInput("uv2","Universe Size", value =800),
              numericInput("ms2","Sample Size", value =400),
              div(style="border: 2px solid black; padding: 1rem",
              h3("Statistical Error"),
              span(textOutput("empuf"), style = "color:red;font-weight: bold;font-size: 20px "))
              ))
      ),
      tabItem(tabName = "EMNP",
              fluidPage(fluidRow(h2("Non-proportional Sampling Error"),
                                 column(4,
                                       
                                        br(),
                                        ##put input boxes here
                                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                                              strong("Probability:"), 
                                            numericInput("pr3", NULL,value = 0.5, width = 70),
                                            strong("Proportional Sample:"), 
                                            numericInput("ps31",NULL, value = 100, width = 70),
                                            numericInput("ps32",NULL, value = 200, width = 70),
                                            numericInput("ps33",NULL, value = 400, width = 70),
                                            numericInput("ps34",NULL, value = 800, width = 70),
                                        ),
                                        ),
                                 column(4,
                                      
                                        br(),
                                        ##put input boxes here
                                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                                            strong("Total Sample:"), 
                                            numericInput("ts3", NULL,value = 1000, width = 70),
                                            strong("Approportional Sample:"), 
                                            numericInput("as31", NULL,value = 100 ,width = 70),
                                            numericInput("as32", NULL,value = 200 ,width = 70),
                                            numericInput("as33", NULL,value = 300 ,width = 70),
                                            numericInput("as34", NULL,value = 400 ,width = 70),
                                        ),
                                        ),
                                 column(4,
                                  
                                        br(),
                                        ##put input boxes here
                                        div(style="display: inline-block;vertical-align:top; width: 200px;border: 2px solid black; padding: 1rem;",
                                            strong("Sampling Error:"),
                                            br(),
                                            br(),
                                            textOutput("se3"),
                                            br(),
                                            strong("Balanced Frequency:"),
                                            br(),
                                            br(),
                                            textOutput("bf1" ),
                                            br(),
                                            textOutput("bf2"),
                                            br(),
                                            textOutput("bf3" ),
                                            br(),
                                            textOutput("bf4"),
                                        ),
                                 )
                                 ),
                        fluidRow(
                          div(style="border: 2px solid black; padding: 1rem",
                          h3("Non-proportional Error"),
                          span(textOutput("emnp"), style = " color:red;font-weight: bold;font-size: 20px "))
                         
                        )
                                 ),
            
      ),
      tabItem(tabName = "TMUI",
              fluidPage(fluidRow(
                h2("Sample Size for Infinite Universes"),
                numericInput("pr4","Probability", value = 0.5, min = 0, max= 1),
                numericInput("er4","Error rate (%)", value =5),
                div(style="border: 2px solid black; padding: 1rem",
                h3("Sample size"),
                span(textOutput("tmui"), style = "color:red; font-weight: bold;font-size: 20px "))
              ))
              
      ),
      tabItem(tabName = "TMUF",
              fluidPage(fluidRow(
              h2("Sample Size for Finite Universes"),
              numericInput("pr5","Probability", value = 0.5, min = 0, max= 1),
              numericInput("er5","Error rate (%)", value =5),
              numericInput("uv5","Universe Size", value =1000),
              div(style="border: 2px solid black; padding: 1rem",
              h3("Sample size"),
              span(textOutput("tmuf"), style = "color:red;font-weight: bold;font-size: 20px "))
              )
              )
      ),
      tabItem(tabName = "CNPM",
              fluidPage(fluidRow(
                h2("Calculation of Number of Sample Points"),
                numericInput("uv6","Universe Size", value =1000),
                div(style="border: 2px solid black; padding: 1rem",h3("Sample Points"),
                span(textOutput("cnpm"), style = " color:red;font-weight: bold;font-size: 20px "))
              )
          )
      ),
      tabItem(tabName = "PMM",
              fluidPage(
                fluidRow(
                  h4("Significant Differences between Proportions of the Same Sample")
                  
                ),
                fluidRow(
                  h4("Confidence Level (One-Side)"),
                  HTML(
                    "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,04</td>
                    <td>1,28</td>
                    <td>1,64</td>
                    <td>2,33</td>
                    <td>2,58</td>
                    </tbody>
                    </table>"
                  )
                ),
                fluidRow(
                  h4("Confidence Level (Two-Sided)"),
                  HTML(
                    "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,44</td>
                    <td>1,64</td>
                    <td>1,96</td>
                    <td>2,58</td>
                    <td>2,81</td>
                    </tbody>
                    </table>"
                  )
                ),
                fluidRow(
                  column(4,
                         numericInput("ms7","Total Sample",value =150),
                         numericInput("prp71","Proportion 1 (%)",value = 92),
                         numericInput("prp72","Proportion 2 (%)",value = 79),
                         ),
                  column(4,
                         h4("Proportion 1"),
                         br(),
                         span(textOutput("prp71out"),style = "font-weight: bold;font-size: 20px "),
                         br(),
                         h4("Proportion 2"),
                         br(),
                         span(textOutput("prp72out"),style = "font-weight: bold;font-size: 20px "),
                         ),
                  column(4,
                         div(style="border: 2px solid black; padding: 1rem",
                         h4("Z-score"),
                         span(textOutput("z7"), style = "font-weight: bold;font-size: 20px "), br(),
                         h4("1-side Significance"),
                         span(textOutput("z17"), style = "color:red;font-weight: bold;font-size: 20px "),br(),
                         h4("2-sided significance"),
                         span(textOutput("z27"), style = "color:red;font-weight: bold;font-size: 20px "))
                         )
                
                )
                
              )
      ),
      tabItem(tabName = "PMI",
              fluidPage(
                fluidRow(
                  h4("Significant Differences between Proportions of the Same Sample"),
                  h4("Confidence Level (One-Side)"),
                  HTML(
                    "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,04</td>
                    <td>1,28</td>
                    <td>1,64</td>
                    <td>2,33</td>
                    <td>2,58</td>
                    </tbody>
                    </table>"
                  )
                ),
                fluidRow(
                  h4("Confidence Level (Two-Sided)"),
                  HTML(
                    "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,44</td>
                    <td>1,64</td>
                    <td>1,96</td>
                    <td>2,58</td>
                    <td>2,81</td>
                    </tbody>
                    </table>"
                  )
                ),
                fluidRow(
                  column(4,
                         numericInput("ms81","Proportional Base 1",value =150),
                         numericInput("ms82","Proportional Base 2",value =150),
                         numericInput("fr81","Frequency 1 (%)",value = 80),
                         numericInput("fr82","Frecuency 2 2 (%)",value = 62),
                  ),
                  column(4,
                         h4("Frequency 1"),
                         br(),
                         span(textOutput("fr81out"), style = "font-weight: bold;font-size: 20px "),
                         br(),
                         h4("Frequency 2"),
                         br(),
                         span(textOutput("fr82out"), style = "font-weight: bold;font-size: 20px "),
                  ),
                  column(4,
                         div(style="border: 2px solid black; padding: 1rem",
                         h4("Z-score"), 
                         span(textOutput("z8"), style = "font-weight: bold;font-size: 20px "),br(),
                         h4("1-sided significance"),
                        span(textOutput("z18"), style = "color:red;font-weight: bold;font-size: 20px "),br(),
                        h4("2-sided significance"),
                        span(textOutput("z28"), style = "color:red;font-weight: bold;font-size: 20px "))
                  )
                  
                )
                
              )
      ),
      tabItem(tabName = "DD",
              fluidPage(
                h4("Significant Differences Between Two Distributions."),
                p("By using the chi-square test for the degree of coincidence, it is possible to establish to what extent the distribution of a sample coincides with its theoretical distribution."),
                p("If the X2 value is greater than those on the table, the difference can be significant leaving only the percentage of probabilities to chance: 5% or 95%."),
                HTML(
                  "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> Degrees of Freedom </th>
                    <th> X2 max value at 5% </th>
                    <th> X2 max value at 10% </th>
                    
                    </tr>
                    </thead>
                    <tbody>
                    <tr>  
                    <td>1</td>
                    <td>3,84</td>
                    <td>0,004</td>
                    </tr>
                      <tr>  
                    <td>2</td>
                    <td>5,99</td>
                    <td>0,10</td>
                    </tr>
                      <tr>  
                    <td>3</td>
                    <td>7,81</td>
                    <td>0,35</td>
                    </tr>
                      <tr>  
                    <td>4</td>
                    <td>9,49</td>
                    <td>0,71</td>
                    </tr>
                      <tr>  
                    <td>5</td>
                    <td>11,07</td>
                    <td>1,15</td>
                    </tr>
                    </tbody>
                    </table>"
                ),
                fluidRow(
                  column(1,
                         div(style="display: inline-block;vertical-align:top;",
                         h5("Results by Distribution"),br(),
                         h5("1"),br(),
                         h5("2"),br(),
                         h5("3"),
                         )),
                  column(1,
                        
                         h5("Sample Frequency"),br(),
                         numericInput("sf121",NULL,value=10),
                         numericInput("sf122",NULL,value=30),
                         numericInput("sf123",NULL,value=60),
                         ),
                  column(1,
                         div(style="display: inline-block;vertical-align:top;",
                        h5("ID Sample for Comparison"),br(),
                         textOutput("imc1211"),br(),
                         textOutput("imc1212"),br(),
                         textOutput("imc1213")),
                  ),
                  column(1,
                         h5("Patron Sample Frequency"),br(),
                         numericInput("psf1211",NULL,value=10),
                         numericInput("psf1212",NULL,value=10),
                         numericInput("psf1213",NULL,value=40),
                  ),
                  column(1,
                         div(style="display: inline-block;vertical-align:top;",
                         h5("ID Sample for Comparison"),br(),
                         textOutput("imc1221"),br(),
                         textOutput("imc1222"),br(),
                         textOutput("imc1223")),
                  ),
                  column(1,
                         h5("Patron Sample Frequency"),br(),
                         numericInput("psf1221",NULL,value=20),
                         numericInput("psf1222",NULL,value=10),
                         numericInput("psf1223",NULL,value=10),
                  ),
                  column(1,
                         div(style="display: inline-block;vertical-align:top;",
                         h5("ID Sample for Comparison"),br(),
                         textOutput("imc1231"),br(),
                         textOutput("imc1232"),br(),
                         textOutput("imc1233")),
                  ),
                  column(2,
                         div(style="display: inline-block;vertical-align:top;",
                         h5("Total Sample by Distribution"),br(),
                         textOutput("ts121"),br(),
                         textOutput("ts122"),br(),
                         textOutput("ts123")),
                  ),
                  br(),
                
                ),
              fluidRow(
              
                column(4,
                       div(style="border: 2px solid black; margin-top: 1rem; padding:1rem",
                       h4("X2 Value"),
                       span(textOutput("xsq12"), style = "font-weight: bold;font-size: 20px "),br(),
                       h4("5% significance"),
                       span(textOutput("sig121"), style = "color:red;font-weight: bold;font-size: 20px "),br(),
                       h4("95% significance"),
                       span(textOutput("sig122"), style = "color:red;font-weight: bold;font-size: 20px ")
                       )
              ),    
              )
              )
             
      ),
      tabItem(tabName = "DMMD",
              fluidPage(
                fluidRow(
                  h4("Significant Differences Between Two Dependent Sample Means.")
                  
                ),
                fluidRow(
                  h4("Confidence Level (One-Side)"),
                  HTML(
                    "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,04</td>
                    <td>1,28</td>
                    <td>1,64</td>
                    <td>2,33</td>
                    <td>2,58</td>
                    </tbody>
                    </table>"
                  )
                ),
                fluidRow(
                  h4("Confidence Level (Two-Sided)"),
                  HTML(
                    "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,44</td>
                    <td>1,64</td>
                    <td>1,96</td>
                    <td>2,58</td>
                    <td>2,81</td>
                    </tbody>
                    </table>"
                  )
                ),
                fluidRow(
                  column(4,
                         numericInput("b10","Base",value =100),
                         numericInput("mn101","Mean 1",value =4.8),
                         numericInput("mn102","Mean 2",value =3.2),
                         numericInput("dv101","Std.Dev 1",value =1.5),
                         numericInput("dv102","Std.Dev 2",value =1),
                         ),
                  column(8,
                         div(style="border: 2px solid black; padding: 1rem",
                         h4("Degrees of Freedom"),
                         span(textOutput("dl10"),style = "font-weight: bold;font-size: 20px"), br(),
                         h4("Z-Score"),
                         span(textOutput("z10"), style = "font-weight: bold;font-size: 20px "),br(),
                         h4("1-sided significance"),
                         span(textOutput("z110"), style = "color:red;font-weight: bold;font-size: 20px "),br(),
                         h4("2-sided significance"),
                         span(textOutput("z210"), style = "color:red;font-weight: bold;font-size: 20px ")
                         ))
                )
              )
      ),
      tabItem(tabName = "DMMI",
              fluidPage( fluidRow(
                h4("Significant Differences Between Two Independent Sample Means.")
                
              ),
              fluidRow(
                h4("Confidence Level (One-Side)"),
                HTML(
                  "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,04</td>
                    <td>1,28</td>
                    <td>1,64</td>
                    <td>2,33</td>
                    <td>2,58</td>
                    </tbody>
                    </table>"
                )
              ),
              fluidRow(
                h4("Confidence Level (Two-Sided)"),
                HTML(
                  "<table class='table table-bordered'>
                    <thead class='thead-dark'>
                    <tr>
                    <th> 85% </th>
                    <th> 90% </th>
                    <th> 95% </th>
                    <th> 99% </th>
                    <th> 99,5% </th>
                    </tr>
                    </thead>
                    <tbody>
                    <td>1,44</td>
                    <td>1,64</td>
                    <td>1,96</td>
                    <td>2,58</td>
                    <td>2,81</td>
                    </tbody>
                    </table>"
                )
              ),
              fluidRow(
                column(4,
                       numericInput("b111","Base 1",value =384),
                       numericInput("mn111","Mean 1",value =4),
                       numericInput("dv111","Std.Dev 1",value =1.6),
                ),
                column(4,
                       numericInput("b112","Base 2",value =384),
                       numericInput("mn112","Mean 2",value =4.4),
                       numericInput("dv112","Std.Dev 2",value =1.6),
                ),
                column(4,
                       div(style="border: 2px solid black; padding: 1rem",
                       h4("Degrees of Freedom"),
                       span(textOutput("dl11"),style = "font-weight: bold;font-size: 20px"), br(),
                       h4("Z-Score"),
                       span(textOutput("z11"),style = "font-weight: bold;font-size: 20px"),br(),
                       h4("1-sided significance"),
                       span(textOutput("z111"),style = "color:red;font-weight: bold;font-size: 20px"),br(),
                       h4("2-sided significance"),
                       span(textOutput("z211"),style = "color:red;font-weight: bold;font-size: 20px"))
                )
              )
              )
      )
      
    )
    
  )
)
server <- function(input, output) {
  
 output$empui <- renderText({
   paste(round((2*sqrt((input$pr1*(1-input$pr1))/input$ms1)),digits = 4)*100,"%")
 })
 
 output$empuf <- renderText({
   paste(round((2*sqrt(((input$pr2*(1-input$pr2))/input$ms2)*((input$uv2-input$ms2)/(input$uv2-1)))),digits=4)*100,"%")
 })
 
output$tmui <- renderText({
   round((4*(input$pr4*(1-input$pr4)))/((input$er4/100)^2),digits = 0)
 })

output$tmuf <- renderText({
  round((4*((input$pr5*(1-input$pr5))*input$uv5))/(((input$er5/100)^2*(input$uv5-1))+(4*(input$pr5*(1-input$pr5)))),digits = 0)
})

output$cnpm <- renderText({
  abs(30+sqrt(input$uv6/1000)-150)
})

output$emnp <- renderText(
   {
     fe1 <- (input$ps31^2)/input$as31
     fe2 <- (input$ps32^2)/input$as32
     fe3 <- (input$ps33^2)/input$as33
     fe4 <- (input$ps34^2)/input$as34
     sumfe <- sum(fe1,fe2,fe3,fe4)
     paste(round(2*(sqrt(((input$pr3*(1-input$pr3))/(input$ts3^2))*(sumfe))),digits= 4) * 100, "%")
   }
 )
 
output$se3  <- renderText({
  paste(round((2*sqrt((input$pr3*(1-input$pr3))/input$ts3)),digits = 4)*100,"%")
})

output$bf1 <- renderText({
  (input$ps31^2)/input$as31
})

output$bf2 <- renderText({
  (input$ps32^2)/input$as32
})

output$bf3 <- renderText({
  (input$ps33^2)/input$as33
})

output$bf4 <- renderText({
  (input$ps34^2)/input$as34
})

output$prp71out <- renderText({
  round((input$prp71/100) * input$ms7 ,digits = 0)
})

output$prp72out <- renderText({
  round((input$prp72/100) * input$ms7 ,digits = 0)
})

output$z7 <- renderText({
  b <-  round((input$prp71/100) * input$ms7 ,digits = 0)
  c <-  round((input$prp72/100) * input$ms7 ,digits = 0)
  round(((abs(b-c))-1)/(sqrt(b+c)),digits = 2)
})

output$z17 <-renderText({
  b <-  round((input$prp71/100) * input$ms7 ,digits = 0)
  c <-  round((input$prp72/100) * input$ms7 ,digits = 0)
  z <- ((abs(b-c))-1)/(sqrt(b+c))
  paste(round((1-pnorm(z,lower.tail=FALSE)) * 100,digits = 2),"%")

})

output$z27<-renderText({
  b <-  round((input$prp71/100) * input$ms7 ,digits = 0)
  c <-  round((input$prp72/100) * input$ms7 ,digits = 0)
  z <- ((abs(b-c))-1)/(sqrt(b+c))
  paste(round((1-(2*pnorm(z,lower.tail=FALSE))) * 100,digits = 2),"%")
  
})

output$fr81out <- renderText({
  round((input$fr81/100) * input$ms81 ,digits = 0)
})

output$fr82out <- renderText({
  round((input$fr82/100) * input$ms82 ,digits = 0)
})

output$z8 <- renderText({
  d <-  input$fr81/100
  g <-  input$fr82/100
  z <- abs((d-g)/(sqrt((((d)*(1-d))/input$ms81)+(((g)*(1-g))/input$ms82))))
  round(z,digits = 2)
})

output$z18 <-renderText({
  d <-  input$fr81/100
  g <-  input$fr82/100
  z <- abs((d-g)/(sqrt((((d)*(1-d))/input$ms81)+(((g)*(1-g))/input$ms82))))
  paste(round((1-pnorm(z,lower.tail=FALSE)) * 100,digits = 2),"%")
  
})

output$z28 <-renderText({
  d <-  input$fr81/100
  g <-  input$fr82/100
  z <- abs((d-g)/(sqrt((((d)*(1-d))/input$ms81)+(((g)*(1-g))/input$ms82))))
  paste(round((1-(2*pnorm(z,lower.tail=FALSE))) * 100,digits = 2),"%")
  
})

output$dl10 <- renderText({
  input$b10 - 1
})

output$z10<- renderText({
  z <- abs((input$mn101-(input$dv101/(input$b10^0.5)))-(input$mn102-(input$dv102/(input$b10^0.5))))
  round(z,digits = 2)
})

output$z110 <-renderText({
  z <- abs((input$mn101-(input$dv101/(input$b10^0.5)))-(input$mn102-(input$dv102/(input$b10^0.5))))
  paste(round((1-pnorm(z,lower.tail=FALSE)) * 100,digits = 2),"%")
  
})

output$z210 <-renderText({
  z <- abs((input$mn101-(input$dv101/(input$b10^0.5)))-(input$mn102-(input$dv102/(input$b10^0.5))))
  paste(round((1-(2*pnorm(z,lower.tail=FALSE))) * 100,digits = 2),"%")
})

output$dl11 <- renderText({
  input$b111 + input$b112 - 1
})

output$z11<- renderText({
  z <- abs(input$mn111-input$mn112)/(sqrt(((input$dv111^2)/input$b111)+((input$dv112^2)/input$b112)))
  round(z,digits = 2)
})

output$z111 <-renderText({
  z <- abs(input$mn111-input$mn112)/(sqrt(((input$dv111^2)/input$b111)+((input$dv112^2)/input$b112)))
  paste(round((1-pnorm(z,lower.tail=FALSE)) * 100,digits = 2),"%")
  
})

output$z211 <-renderText({
  z <- abs(input$mn111-input$mn112)/(sqrt(((input$dv111^2)/input$b111)+((input$dv112^2)/input$b112)))
  paste(round((1-(2*pnorm(z,lower.tail=FALSE))) * 100,digits = 2),"%")
})


ts121 <- reactive({
  input$sf121+input$psf1211 + input$psf1221
})

ts122 <- reactive({
  input$sf122+input$psf1212 + input$psf1222
})

ts123 <- reactive({
  input$sf123+input$psf1213 + input$psf1223
})

tf121 <- reactive({
  input$sf121+input$sf122+input$sf123
})

tf122 <- reactive({
  input$psf1211+input$psf1212+input$psf1213
})

tf123 <- reactive({
  input$psf1221+input$psf1222+input$psf1223
})

ts <- reactive({
  ts121()+ts122()+ts123()
})

imc1211 <-reactive({
  (ts121()*tf121())/ts()
})

imc1212 <-reactive({
  (ts122()*tf121())/ts()
})

imc1213 <-reactive({
  (ts123()*tf121())/ts()
})

imc1221 <-reactive({
  (ts121()*tf122())/ts()
})

imc1222 <-reactive({
 (ts122()*tf122())/ts()
})

imc1223 <-reactive({
  (ts123()*tf122())/ts()
})

imc1231 <-reactive({
  (ts121()*tf123())/ts()
})

imc1232 <-reactive({
  (ts122()*tf123())/ts()
})

imc1233 <-reactive({
  (ts123()*tf123())/ts()
})

output$imc1211 <-renderText({
  imc1211()
})

output$imc1212 <-renderText({
  imc1212()
})

output$imc1213 <-renderText({
  imc1213()
})

output$imc1221 <-renderText({
  imc1221()
})

output$imc1222 <-renderText({
  imc1222()
})

output$imc1223 <-renderText({
  imc1223()
})

output$imc1231 <-renderText({
  imc1231()
})

output$imc1232 <-renderText({
  imc1232()
})

output$imc1233 <-renderText({
  imc1233()
})

output$ts121 <- renderText({
  ts121()
})

output$ts122 <- renderText({
  ts122()
})

output$ts123 <- renderText({
  ts123()
})

xsq12 <- reactive({
  x1 <- (((input$sf121-imc1211())^2)/imc1211())+(((input$psf1211-imc1221())^2)/imc1221())+(((input$psf1221-imc1231())^2)/imc1231())
  x2 <- (((input$sf122-imc1212())^2)/imc1212())+(((input$psf1212-imc1222())^2)/imc1222())+(((input$psf1222-imc1231())^2)/imc1232())
  x3 <- (((input$sf123-imc1213())^2)/imc1213())+(((input$psf1213-imc1223())^2)/imc1223())+(((input$psf1223-imc1233())^2)/imc1233())
  
  x1+x2+x3
})

output$xsq12 <-renderText({
 round(xsq12(),digits=2)
})

output$sig121 <-renderText({
  if (xsq12() > 9.5){
    "5%"
  }
  else{
    "NS"
  }
})

output$sig122 <-renderText({
  if (xsq12() > 0.7){
    "95%"
  }
  else{
    "NS"
  }
})

}

# Run the application 
shinyApp(ui = ui, server = server)
