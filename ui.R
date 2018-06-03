
require(shiny)
require(rhandsontable)

ui <- shinyUI(fluidPage(
  
  titlePanel("Race car dynamics"), br(),

  sidebarLayout(
    sidebarPanel(id = "sidebar",
                 fluidRow(
                   column(6, 
                          selectizeInput("choice", label = "Method of approach", 
                                         choices = c("STATIC", "TOP SPEED", "HIGH SPEED CORNER", "MAX BRAKING"))
                   )
                 ),
                 wellPanel(
                   fluidRow(tags$h5("SETUP")),
                   fluidRow(
                     column(3, h5(strong("PARAMETER"))),
                     column(2, h5(strong("UNIT"))),
                     column(3, h5(strong("FRONT"))),
                     column(3, h5(strong("REAR")))
                   ),
                   br(),
                   fluidRow(
                     column(3, tags$h5(strong("Ride height"))),
                     column(2, tags$h5("mm")),
                     column(3,
                            numericInput("front_ride_height", label = NULL, value = front_ride_height, min = 5, max = 50, step = 1)
                     ),
                     column(3,
                            numericInput("rear_ride_height", label = NULL, value = rear_ride_height, min = 5, max = 50, step = 1)
                     )
                   ),
                   fluidRow(
                     column(3, tags$h5(strong("Ride height max speed corner"))),
                     column(2, tags$h5("mm")),
                     column(3,
                            numericInput("front_ride_height_msc", label = NULL, value = f_ride_height_max_speed, min = 5, max = 50, step = 1)
                     ),
                     column(3,
                            numericInput("rear_ride_height_msc", label = NULL, value = r_ride_height_max_speed, min = 5, max = 50, step = 1)
                     )
                   ),
                   fluidRow(
                     column(3, tags$h5(strong("Spring"))),
                     column(2, tags$h5("lbs/in")),
                     column(3,
                            numericInput("front_spring", label = NULL, value = front_spring, min = 550, max = 1150, step = 100)
                     ),
                     column(3,
                            numericInput("rear_spring", label = NULL, value = rear_spring, min = 550, max = 1150, step = 100)
                     )
                   ),
                   fluidRow(
                     column(3, tags$h5(strong("ARB"))),
                     column(2, tags$h5("kg/mm")),
                     column(3,
                            selectizeInput("front_arb", label = NULL, choices = c(24,29,32,38,45,53,68,76,95,110), selected = front_arb)
                     ),
                     column(3,
                            selectizeInput("rear_arb", label = NULL, choices = c(12,18,seq(25,60,5)), selected = rear_arb)
                     )
                   ),
                   fluidRow(
                     column(3, tags$h5(strong("Roll centre height (at ref.)"))),
                     column(2, tags$h5("mm")),
                     column(3,
                            selectizeInput("front_roll_cntr", label = NULL, choices = c(5,-5), selected = front_roll_cntr)
                     ),
                     column(3,
                            selectizeInput("rear_roll_cntr", label = NULL, choices = c(15,25,38,55), selected = rear_roll_cntr)
                     )
                   ),
                   fluidRow(
                     column(3, tags$h5(strong("Tyre pressure"))),
                     column(2, tags$h5("bar")),
                     column(3,
                            numericInput("front_pressure", label = NULL, value = front_tyre_pressure_init, min = 1.4, max = 1.6, step = 0.1)
                     ),
                     column(3,
                            numericInput("rear_pressure", label = NULL, value = rear_tyre_pressure_init, min = 1.4, max = 1.6, step = 0.1)
                     )
                   )
                 ),
                 wellPanel(
                   fluidRow(tags$h5("WEATHER CONDITIONS")),
                   fluidRow(
                     column(6, h5(strong("BARCELONA RACE TRACK"))),
                     column(2, h5(strong("UNIT"))),
                     column(3, h5(strong("VALUE")))
                   ),
                   fluidRow(
                     column(6, h5(strong("Ambient temperature"))),
                     column(2, h5("deg")),
                     column(3,
                            numericInput("temperature", label = NULL, value = temperature, min = 10, max = 40))
                   ),
                   fluidRow(
                     column(6, h5(strong("Ambient pressure"))),
                     column(2, h5("mmb")),
                     column(3,
                            numericInput("pressure", label = NULL, value = pressure, min = 980, max = 1020))
                   ),
                   uiOutput("windspeed_plc")
                 ),
                 uiOutput("lap_cond_plc"),
                 wellPanel(
                   fluidRow(tags$h5("Damping")),
                   fluidRow(
                     column(3, tags$h5(strong("Bump"))),
                     column(2, tags$h5("Xi 0.7/0.7")),
                     column(3,
                            numericInput("front_bump", label = "Click", value = front_bump, 
                                         min = min(xl.damping$bump_click), max = max(xl.damping$bump_click), step = 1)
                     ),
                     column(3,
                            numericInput("rear_bump", label = "Click", value = rear_bump, 
                                         min = min(xl.damping$rebound_click), max = max(xl.damping$rebound_click), step = 1)
                     )
                   ),
                   fluidRow(
                     column(3),
                     column(2, tags$h5("")),
                     column(3,
                            verbatimTextOutput("front_bump_value"), 
                            tags$style(type='text/css', "#front_bump_value {background-color:#FFFFFF;vertical-align: middle; text-align:center;}")
                     ),
                     column(3,
                            verbatimTextOutput("rear_bump_value"),
                            tags$style(type='text/css', "#rear_bump_value {background-color:#FFFFFF;vertical-align: middle; text-align:center;}")
                     )
                   ),
                   fluidRow(
                     column(3, tags$h5(strong("Rebound"))),
                     column(2, tags$h5("Xi 0.7/0.3")),
                     column(3,
                            numericInput("front_rebound", label = "Click", value = front_rebound, min = 1, max = 20, step = 1)
                     ),
                     column(3,
                            numericInput("rear_rebound", label = "Click", value = rear_rebound, min = 1, max = 20, step = 1)
                     )
                   ),
                   
                   fluidRow(
                     column(3),
                     column(2, tags$h5("")),
                     column(3,
                            verbatimTextOutput("front_rebound_value"),
                            tags$style(type='text/css', "#front_rebound_value {background-color:#FFFFFF;vertical-align: middle; text-align:center;}")
                     ),
                     column(3,
                            verbatimTextOutput("rear_rebound_value"),
                            tags$style(type='text/css', "#rear_rebound_value {background-color:#FFFFFF;vertical-align: middle; text-align:center;}")
                     )
                   )
                 )
    ),
    mainPanel(id="mainbar",
              
              fluidRow(
                column(3, offset = 1, 
                       tags$h5(strong("FL Wheel"), align = "center"),
                       wellPanel(rHandsontableOutput("tbl_fl"), style = "width:240px;background-color:#4bacc6;"),
                       tags$style(type='text/css', "#tbl_fl { vertical-align: middle; text-align:center;}")
                ),
                column(3, 
                       tags$h5(strong("Front ride height"), align = "center"),
                       wellPanel(rHandsontableOutput("tbl_front_height"), style = "width:240px;background-color:#92cddc;"),
                       tags$style(type='text/css', "#tbl_front_height { vertical-align: middle; text-align:center;}")
                ),
                column(3, 
                       tags$h5(strong("FR Wheel"), align = "center"),
                       wellPanel(rHandsontableOutput("tbl_fr"), style = "width:240px;background-color:#4bacc6;"),
                       tags$style(type='text/css', "#tbl_fr { vertical-align: middle; text-align:center;}")
                )
              ),
              fluidRow(
                column(3, offset = 1, style = "width:245px;"),
                column(4, align = "center", 
                       wellPanel(img(src = "F3_top.png"), style = "width:200px;"))
              ),
              fluidRow(
                column(3, offset = 1, 
                       tags$h5(strong("RL Wheel"), align = "center"),
                       wellPanel(rHandsontableOutput("tbl_rl"), style = "width:240px;background-color:#4bacc6;"),
                       tags$style(type='text/css', "#tbl_rl { vertical-align: middle; text-align:center;}")
                ),
                column(3, 
                       tags$h5(strong("Rear ride height"), align = "center"),
                       wellPanel(rHandsontableOutput("tbl_rear_height"), style = "width:240px;background-color:#92cddc;"),
                       tags$style(type='text/css', "#tbl_rear_height { vertical-align: middle; text-align:center;}")
                ),
                column(3,
                       tags$h5(strong("RR Wheel"), align = "center"),
                       wellPanel(rHandsontableOutput("tbl_rr"), style = "width:240px;background-color:#4bacc6;"),
                       tags$style(type='text/css', "#tbl_rr { vertical-align: middle; text-align:center;}")
                )
              ),h1(),
              fluidRow(
                column(3, offset = 3, style = "padding-left:45px;",
                       tags$h5(strong("Control parameter"), align = "left"),
                       wellPanel(rHandsontableOutput("tbl_control"), style = "width:400px;", 
                                 helpText("Note: values for LLTD [%] will only be calculated when",
                                          "'HIGH SPEED CORNER' is selected.")
                       ),
                       tags$style(type='text/css', "#tbl_control { vertical-align: middle; text-align:left;}")
                )
              ),
              fluidRow(
                column(3, offset = 3, style = "padding-left:45px;",
                       tags$h5(strong("Damping calculation"), align = "left"),
                       wellPanel(rHandsontableOutput("tbl_damping"), style = "width:400px;"
                       ),
                       tags$style(type='text/css', "#tbl_control { vertical-align: middle; text-align:left;}")
                )
              )
              
    )
  )
) # END fixedPaige
)


