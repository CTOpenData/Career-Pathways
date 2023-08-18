# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(shiny)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(DT)
library(htmlwidgets)
thematic::thematic_shiny(font = "auto")

source("carouselPanel.R")

## Setting the page theme ##
theme <- bs_theme(
  version = version_default(),
  bootswatch = "yeti",
  bg = "#ffffff",
  fg =  "#054266",
  primary = "#00548b",
  secondary = "#0071BB",
  base_font = font_google("Karla"),
  heading_font = font_google("Karla"),
  font_scale = 1
)


# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(
  navbarPage(
    title = img(src="CT_LOGO_white.png", height = "40px"), id = "navBar",
    theme = theme,
    collapsible = TRUE,
    inverse = TRUE,
    windowTitle = "Connecticut Career Pathways",
    position = "fixed-top",
    #footer = includeHTML("./www/include_footer.html"),
    header = tags$style(
    ".navbar-right {
    float: right !important;
    }",
    "body {padding-top: 55px;}"),
    
    tabPanel("HOME", value = "home",
             shinyjs::useShinyjs(),
             tags$head(tags$script(HTML('
             var fakeClick = function(tabName) {
             var dropdownList = document.getElementsByTagName("a");
             for (var i = 0; i < dropdownList.length; i++) {
             var link = dropdownList[i];
             if(link.getAttribute("data-value") == tabName) {
             link.click();
             };
             }
             };
                                        '))),
             fluidRow(
               style = "background-color:#054266; padding-bottom:20px; color:white;",
               shiny::HTML("<center> <h1>Discover Your Career Pathway</h1></center>")
               ),
             # WHAT
             fluidRow(column(3),
                      column(6,
                             shiny::HTML("<br><br><center> <h2>What you'll find here</h2> </center><br>"),
                              shiny::HTML("<h5>An interactive tool to help you explore the actual paths Connecticut's 
                                               workforce have taken during their careers. With information about the
                                               popularity of certain paths, employment outcomes, wage growth, and more, you can
                                               build your own path based on what is meaningful to you.</h5>")
                             ),
                      column(3)
                      ),
             
             fluidRow(
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # HOW
             fluidRow(column(3),
                      column(6,
                             shiny::HTML("<br><br><center> <h2>How it can help you</h2> </center><br>"),
                             shiny::HTML("<h5>With most things, the more you know, the better your decisions 
                                              will be. Connecticut Career Pathways empowers you to make better decisions 
                                              about your career and education by providing transparant information on workforce
                                              training and education programs outcomes.</h5>")
                             ),
                      column(3)
                      ),
             
             fluidRow(
               style = "height:50px;"),
             
             # PAGE BREAK
             
             tags$hr(),
             
             # WHERE
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h2>Where it came from</h2> </center><br>"),
                      shiny::HTML("<h5>Our team analyzed the workforce and education pathways of
                                       Connecticut residents over the past 11 years.</h5>")
                      ),
               column(3)
               ),
             
             fluidRow(
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # HOW TO START
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h2>How to get started</h2> </center><br>"),
                      ),
               column(3)
               ),
             
             fluidRow(
               column(3),
               column(2,
                      div(class="panel panel-default",
                          div(class="panel-body",  width = "600px",
                              align = "center",
                              div(
                                tags$img(src = "one.svg", 
                                         width = "50px", height = "50px")
                                ),
                              div(
                                h5(
                                "Pick a workforce training or education program
                                 to start your path. You may use a program you are currently enrolled in,
                                 a program you were previously enrolled in, or a program you're interested in exploring."
                                )
                                )
                              )
                          )
                      ),
               column(2,
                      div(class="panel panel-default",
                          div(class="panel-body",  width = "600px",
                              align = "center",
                              div(
                                tags$img(src = "two.svg", 
                                         width = "50px", height = "50px")
                                ),
                              div(
                                h5(
                                "Then from that program, review the set of next programs that people have moved 
                                 into during their careers. Review information about these choices and select your next career step."
                                )
                                )
                              )
                          )
                      ),
               column(2,
                      div(class="panel panel-default",
                          div(class="panel-body",  width = "600px",
                              align = "center",
                              div(
                                tags$img(src = "three.svg",
                                         width = "50px", height = "50px")),
                              div(h5(
                                "Plan up to five steps out in your career. When you're ready, you may save or print out your personalized report."
                                )
                                )
                              )
                          )
                      ),
               column(3)
               ),
             
             fluidRow(
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # AFTERWARD
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h2>How does it fit in the big picture?</h2> </center><br>"),
                      shiny::HTML("<h5>Building a career path is just one part of effective career 
                                       planning and development. You should also establish a career plan 
                                       to outline <i>how</i> you will achieve your professional goals. Our
                                       Career Planning Guide provides information to help you establish 
                                       a plan for making your career path a reality.</h5>")
                      ),
               column(3)
               ),
             fluidRow(
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             fluidRow(
             shiny::HTML("<br><br><center> <h2>Ready to Get Started?</h2> </center>
                         <br>"
                         )
             ),
             fluidRow(
               column(3),
               column(6,
                      tags$div(align = "center",
                               tags$a("Start", 
                                      onclick="fakeClick('careerPF')", 
                                      class="btn btn-primary btn-lg")
                               )
                      ),
               column(3)
               ),
             fluidRow(style = "height:25px;"
                      )
             ), # Closes the first tabPanel called "Home"
    
    tabPanel("CAREER PATHWAYS", value = "careerPF",
             sidebarLayout( 
               sidebarPanel(width = 3,
                            introjsUI(),
                            tags$div(
                              actionButton("help", "Take a Quick Tour"),
                              style = "height:50px;"
                              ),
                            useShinyjs(),
                            tags$div(
                              style = "height:50px;",
                              introBox(
                                tags$div(
                                  style = "height:50px;",
                                  actionLink("settings", "Settings",
                                             icon = icon("sliders", class = "fa-2x"))),
                                data.step = 6,
                                data.intro = "Settings is where you can set options that affect the graph and career statistics."
                                ),
                              radioButtons("selectData", 
                                           label = "How many years of data do you want to include?",
                                           choices = c("30 Years",
                                                       "15 Years"),
                                           inline = TRUE,
                                           width = "100%"
                              ),
                              selectizeInput("changeAvatar", "Change Icon:",
                                             choices = c(
                                               # "Traveler" = "traveler",  # not compatible with new FA
                                               "Map Marker" = "map-marker", 
                                               "Rocket" = "rocket", 
                                               # "Paper Plane" = "paper-plane",  # not compatible with new FA
                                               "Leaf" = "leaf"),
                                             selected = "rocket"
                                             ),
                              textInput("userName", "Add your name:", value = ""),
                              tags$div(
                                style = "height:50px;",
                                uiOutput("printInput1"),
                                uiOutput("printInput2"),
                                uiOutput("printInput3"),
                                uiOutput("printInput4"),
                                uiOutput("printInput5")
                                )
                              )
                            ),  # Closes sidebarPanel
               mainPanel( width = 8,
                          fluidRow(
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                                       ),
                            introBox(
                              panel_div(class_type = "default",
                                        content = tags$div(
                                          uiOutput("displayName"),
                                          visNetwork::visNetworkOutput("visTest", height = "200px")
                                          )
                                        ),
                              data.step = 4,
                              data.intro = "Your selections will be displayed here in a graph."
                              )
                            ),
                          fluidRow(
                            div(class="panel panel-default",
                                div(class="panel-body",
                                    width = "600px",
                                    tags$div(class = "wrap",
                                             div(class = "left", 
                                                 style="display: inline-block;vertical-align:top; width: 150px;",
                                                 uiOutput("stepNo")
                                                 ),
                                             div(class = "right",
                                                 style="display: inline-block;vertical-align:top; width: 150px;",
                                                 introBox(checkboxInput('returnpdf', 'Save as PDF?', FALSE),
                                                          data.step = 5, data.intro = "Stay on track with your plans by downloading your path."
                                                          ),
                                                 uiOutput("download")
                                                 ),
                                             div(class = "center",
                                                 style="display: inline-block;vertical-align:top; width: 150px;",
                                                 introBox(
                                                   actionButton("goBack",
                                                                label = "Back", 
                                                                icon = icon("arrow-circle-left", class = "fa-2x"),
                                                                width= "100px", height= "40px"),
                                                                data.step = 3,
                                                                data.intro = "Go back a step to edit your selection anytime."
                                                   )
                                                 ),
                                                                # div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                                #     uiOutput("clearBtns")
                                                                # ),
                                                                # actionButton("resetBtn", "Reset All", icon = icon("refresh", class = "fa-2x")),    
                                                                div(class = "center",
                                                                    style="display: inline-block;vertical-align:top; width: 150px;",
                                                                    introBox(
                                                                        actionButton("btn1", 
                                                                                     label = "Add", 
                                                                                     icon = icon("arrow-circle-right", class = "fa-2x"),
                                                                                     width= "100px", height= "40px"),
                                                                        data.step = 2,
                                                                        data.intro = "Confirm your selection by clicking here."
                                                                    )
                                                                )
                                                       ),
                                                       # Insert Table Output
                                                       introBox(
                                                           uiOutput("btns"),
                                                           data.step = 1, 
                                                           data.intro = "Start by selecting your first career choice from our list of over 2,000 current job classifications."
                                                       )
                                                   )
                                               ),
                                               plotOutput("myplot")
                                           )
                                )  # Closes the mainPanel
                            )  # Closes the sidebarLayout
                   ),  # Closes the second tabPanel called "Career PathFinder"
    navbarMenu(
      "PROGRAMS",
      tabPanel(
        "Higher Education",
        fluidPage(title="Higher Education Program Outcomes",
                  theme = theme
        ),
        mainPanel(
          p("")
        )
      ),
      tabPanel(
        "Workforce Training",
        fluidPage(title="Workforce Training Program Outcomes",
                  theme = theme
        ),
        mainPanel(
          p("")
        )
      )
    )
                
        
                   )
  )