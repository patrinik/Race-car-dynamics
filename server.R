

server <- shinyServer(function(input, output, session){

  air_density <- reactive({
    
    if( input$choice != "TOP SPEED" ){
      wind_speed <- 0
    }else {
      wind_speed <- req(input$windspeed)
    }
    
    # density rho
    density <- 1.2255 *  (wind_speed + 273) / (input$temperature + 273) * (input$pressure/1000)
  })
  
  # linear assumption
  tyre_stiffness <- reactive({
    
    # stiffness kg/mm
    front_stiffness <- (front_tyre_stiff_high - front_tyre_stiff_low) / (tyre_pressure_high - tyre_pressure_low) * (input$front_pressure - tyre_pressure_low) + front_tyre_stiff_low
    rear_stiffness  <- (rear_tyre_stiff_high - rear_tyre_stiff_low) / (tyre_pressure_high - tyre_pressure_low) * (input$rear_pressure - tyre_pressure_low) + rear_tyre_stiff_low
    
    # stiffness N/m
    # front_stiffness <- front_stiffness * gravity * 1000
    # rear_stiffness  <- rear_stiffness * gravity * 1000
    
    out <- list(kt_front = front_stiffness, kt_rear = rear_stiffness)
  })
  
  vertical_rate <- reactive({
    
    # springs
    front_spring <- (input$front_spring * lb2kg) / inch2mm
    rear_spring  <- (input$rear_spring * lb2kg) / inch2mm
    
    # wheel rate
    WR_s_front <- front_spring / spring_MR_front^2
    WR_s_rear  <- rear_spring / spring_MR_rear^2
    
    # tyre stiffness
    tyre_data <- tyre_stiffness()
    front_tyre_stiff <- tyre_data$kt_front
    rear_tyre_stiff  <- tyre_data$kt_rear
    
    # ride rate
    RR_s_front <- (WR_s_front * front_tyre_stiff) / (WR_s_front + front_tyre_stiff)
    RR_s_rear  <- (WR_s_rear * rear_tyre_stiff) / (WR_s_rear + rear_tyre_stiff)
    
    # ride rate distribution
    RR_dist <- 100 * RR_s_front / (RR_s_front + RR_s_rear)  
    
    out <- list(WR_s_front = WR_s_front,
                WR_s_rear = WR_s_rear,
                RR_s_front = RR_s_front,
                RR_s_rear = RR_s_rear,
                RR_dist = RR_dist)
  })
  
  roll_stiffness_dist <- reactive({
    
    # vertical rate data
    tmp <- vertical_rate()
    
    WR_s_front <- tmp$WR_s_front
    WR_s_rear  <- tmp$WR_s_rear
    RR_s_front <- tmp$RR_s_front
    RR_s_rear  <- tmp$RR_s_rear
    RR_dist    <- tmp$RR_dist
    
    # tyre stiffnes
    tmp <- tyre_stiffness()
    
    tyre_stiff_front <- tmp$kt_front
    tyre_stiff_rear  <- tmp$kt_rear
    
    WR_arb_front <- as.numeric(input$front_arb) / ARB_MR_front^2
    WR_arb_rear  <- as.numeric(input$rear_arb) / ARB_MR_rear^2
    
    K_phi_s_front <- 0.5 * WR_s_front * track_front^2
    K_phi_t_front <- 0.5 * tyre_stiff_front * track_front^2
    K_phi_arb_front <- 0.5 * WR_arb_front * track_front^2
    
    K_phi_s_rear <- 0.5 * WR_s_rear * track_rear^2
    K_phi_t_rear <- 0.5 * tyre_stiff_rear * track_rear^2
    K_phi_arb_rear <- 0.5 * WR_arb_rear * track_rear^2
    
    K_phi_TOTAL_front <- 1 / (1 / (K_phi_s_front + K_phi_arb_front) + 1 / K_phi_t_front)
    K_phi_TOTAL_rear  <- 1 / (1 / (K_phi_s_rear + K_phi_arb_rear) + 1 / K_phi_t_rear)
    
    K_phi_dist_front <- 100 * K_phi_TOTAL_front / (K_phi_TOTAL_front + K_phi_TOTAL_rear)
    
    out <- K_phi_dist_front
  })
  
  longitudinal_load_transfer <- reactive({
    
    if( input$choice == "MAX BRAKING" ){
      long_accel <- req(input$long_g)
      
      longLT <- (total_weight * long_accel * CoG_height) / wheelbase
    }else{
      longLT <- long_accel <- 0
    }
    
    out <- list(longLT = longLT, long_accel = long_accel)
  })
  
  lateral_load_transfer <- reactive({
   
    if( input$choice == "HIGH SPEED CORNER" ){
      lateral_accel <- req(input$lateral_g)
    }
    
    # roll center height (at reference)
    RCH_front <- as.numeric(input$front_roll_cntr)
    RCH_rear  <- as.numeric(input$rear_roll_cntr)
    
    ride_height_f <- input$front_ride_height_msc
    ride_height_r <- input$rear_ride_height_msc
    
    # roll center height effective
    RCH_front_eff <- ride_height_f - ride_height_f_ref4RCH + RCH_front
    RCH_rear_eff  <- ride_height_r - ride_height_r_ref4RCH + RCH_rear
    
    # sprung mass
    c_s <- weight_dist_f * wheelbase
    b_s <- (1 - weight_dist_f) * wheelbase
    
    h_star <- CoG_height - ((b_s / wheelbase) * RCH_rear_eff) - ((c_s / wheelbase) * RCH_front_eff)
    
    weight_sprung_mass <- total_weight - (2 * (unsprung_mass_front + unsprung_mass_rear))
    
    # lateral unsprung
    LAT_f_u <- gravity * (2 * unsprung_mass_front * lateral_accel *  CoG_unsprung_mass_front) / track_front
    LAT_r_u <- gravity * (2 * unsprung_mass_rear * lateral_accel * CoG_unsprung_mass_rear) / track_rear
    
    # lateral geometric
    LAT_f_g <- gravity * (weight_sprung_mass * lateral_accel * (c_s / wheelbase) * RCH_front_eff) / track_front
    LAT_r_g <- gravity * (weight_sprung_mass * lateral_accel * (b_s / wheelbase) * RCH_rear_eff) / track_rear
    
    # lateral elastic
    K_phi_dist_front <- roll_stiffness_dist()
    K_phi_dist_front <- K_phi_dist_front / 100
    
    LAT_f_e <- gravity * (weight_sprung_mass * lateral_accel * h_star * K_phi_dist_front) / track_front
    LAT_r_e <- gravity * (weight_sprung_mass * lateral_accel * h_star * (1 - K_phi_dist_front)) / track_rear
    
    LAT_f_total <- LAT_f_u + LAT_f_g + LAT_f_e
    LAT_r_total <- LAT_r_u + LAT_r_g + LAT_r_e
    
    if( LAT_f_total != 0 & LAT_r_total != 0 ){
      LLT_dist <- 100 * LAT_f_total / (LAT_f_total + LAT_r_total)
    }else{
      LLT_dist <- 0
    }
    
    out <- list(LAT_f_total = LAT_f_total, LAT_r_total = LAT_r_total,  LLT_dist = LLT_dist, lateral_accel = lateral_accel) 
    
  })
  
  aerodynamic_force <- reactive({
    
    density <- air_density()

    if( input$choice == "STATIC" ){
      speed <- 0
    }else if( input$choice == "TOP SPEED" ){
      speed <- input$topspeed
    }else if( input$choice == "HIGH SPEED CORNER" ){
      speed <- input$corner_speed
    }else if( input$choice == "MAX BRAKING" ){
      speed <- input$topspeed
    }
    
    if( input$choice != "TOP SPEED" ){
      wind_speed <- 0
    }else {
      wind_speed <- req(input$windspeed)
    }
    
    total_aero_force <- 0.5 * density * total_lift_coeff_times_A * ((speed + wind_speed) / 3.6)^2
    
    front_aero_force <- total_aero_force * aero_weight_dist_f
    rear_aero_force  <- total_aero_force * (1 - aero_weight_dist_f)
    
    front_aero_weight <- front_aero_force / gravity
    rear_aero_weight  <- rear_aero_force / gravity
    
    out <- list(front_aero_force = front_aero_force, rear_aero_force = rear_aero_force)
  })
  
  heights_N_weights <- reactive({
    
    aero <- aerodynamic_force()
    #assign("aero", aero, envir = .GlobalEnv)
    front_aero_force <- aero$front_aero_force
    rear_aero_force  <- aero$rear_aero_force
    
    LLT <- lateral_load_transfer()
    #assign("LLT", LLT, envir = .GlobalEnv)
    LAT_f_total <- LLT$LAT_f_total
    LAT_r_total <- LLT$LAT_r_total
    
    lateral_accel <- LLT$lateral_accel
    
    if( lateral_accel != 0 ){
      # right hand corner
      mult_r <- ifelse( lateral_accel > 0, -1, ifelse( lateral_accel < 0, -1, 1))
      mult_l <- mult_r * (-1)
    }else{
      mult_r <- mult_l <- 1
    }
    
    tmp <- vertical_rate()
    #assign("tmp", tmp, envir = .GlobalEnv)
    RR_s_front <- tmp$RR_s_front
    RR_s_rear  <- tmp$RR_s_rear
    
    LLT <- longitudinal_load_transfer()
    
    longLT <- LLT$longLT
    long_accel <- LLT$long_accel
   
    # delta weights per wheel
    delta_w_FL <- (0.5 * front_aero_force + (mult_l * LAT_f_total)) / gravity
    delta_w_FL <- delta_w_FL + ifelse( long_accel >= 0, longLT, -longLT) 
    
    delta_w_FR <- (0.5 * front_aero_force + (mult_r * LAT_f_total)) / gravity
    delta_w_FR <- delta_w_FR + ifelse( long_accel >= 0, longLT, -longLT)
    
    delta_w_RL <- (0.5 * rear_aero_force + (mult_l * LAT_r_total)) / gravity
    delta_w_RL <- delta_w_RL + ifelse( long_accel >= 0, -longLT, longLT)
    
    delta_w_RR <- (0.5 * rear_aero_force + (mult_r * LAT_r_total)) / gravity
    delta_w_RR <- delta_w_RR + ifelse( long_accel >= 0, -longLT, longLT)
    
    weight_FL <- 0.5 * total_weight * weight_dist_f + delta_w_FL 
    weight_FR <- 0.5 * total_weight * weight_dist_f + delta_w_FR
    weight_RL <- 0.5 * total_weight * (1 - weight_dist_f) + delta_w_RL
    weight_RR <- 0.5 * total_weight * (1 - weight_dist_f) + delta_w_RR
    
    # delta heights per wheel
    delta_h_FL <- delta_w_FL / RR_s_front
    delta_h_FR <- delta_w_FR / RR_s_front
    
    delta_h_RL <- delta_w_RL / RR_s_rear
    delta_h_RR <- delta_w_RR / RR_s_rear
    
    real_h_FL <- input$front_ride_height - delta_h_FL  
    real_h_FR <- input$front_ride_height - delta_h_FR
    real_h_RL <- input$rear_ride_height - delta_h_RL
    real_h_RR <- input$rear_ride_height - delta_h_RR
    
    # delta heights axle
    delta_h_front <- 0.5 * (delta_h_FL + delta_h_FR)
    delta_h_rear  <- 0.5 * (delta_h_RL + delta_h_RR)
    
    ride_height_front <- input$front_ride_height - delta_h_front
    ride_height_rear  <- input$rear_ride_height - delta_h_rear
    
    ## prepare output
    ride_height <- data.frame(ride_height_front = ride_height_front, ride_height_rear = ride_height_rear) 
    
    real_h     <- data.frame(real_h_FL = real_h_FL, real_h_FR = real_h_FR, 
                             real_h_RL = real_h_RL, real_h_RR = real_h_RR)
    
    delta_h    <- data.frame(delta_h_FL = delta_h_FL, delta_h_FR = delta_h_FR, 
                             delta_h_RL = delta_h_RL, delta_h_RR = delta_h_RR,
                             delta_h_front = delta_h_front, delta_h_rear = delta_h_rear)
    
    real_w     <- data.frame(weight_FL = weight_FL, weight_FR = weight_FR, 
                             weight_RL = weight_RL, weight_RR = weight_RR)
    
    delta_w    <- data.frame(delta_w_FL = delta_w_FL, delta_w_FR = delta_w_FR, 
                             delta_w_RL = delta_w_RL, delta_w_RR = delta_w_RR)
    
    out <- list(ride_height = ride_height, real_h = real_h, delta_h = delta_h, real_w = real_w, delta_w = delta_w)
  })
  
  
  
  damping <- reactive({
    
    VR <- vertical_rate() 
    
    # WR [N/m]
    WR_f <- VR$WR_s_front * gravity * 1000
    WR_r <- VR$WR_s_rear * gravity * 1000
    
    SM_pw_f <- (total_weight - (2 * (unsprung_mass_front + unsprung_mass_rear))) * weight_dist_f * 0.5
    SM_pw_r <- (total_weight - (2 * (unsprung_mass_front + unsprung_mass_rear))) * (1 - weight_dist_f) * 0.5
    
    xi_bump_f <- 0.7
    xi_reb_f  <- 0.7
    xi_bump_r <- 0.7
    xi_reb_r  <- 0.3
    
    # omega front & rear
    w_f <- sqrt(WR_f/SM_pw_f)
    w_r <- sqrt(WR_r/SM_pw_r)
    
    C_crit_f <- 2 * sqrt(WR_f * SM_pw_f)
    C_crit_r <- 2 * sqrt(WR_r * SM_pw_r)
    
    C_W_f_bump <- 2 * w_f * SM_pw_f * xi_bump_f
    C_W_f_reb  <- 2 * w_f * SM_pw_f * xi_reb_f
    
    C_W_r_bump <- 2 * w_r * SM_pw_r * xi_bump_r
    C_W_r_reb  <- 2 * w_r * SM_pw_r * xi_reb_r
    
    C_D_f_bump <- C_W_f_bump * spring_MR_front^2
    C_D_f_reb  <- C_W_f_reb * spring_MR_front^2
    
    C_D_r_bump <- C_W_r_bump * spring_MR_rear^2
    C_D_r_reb  <- C_W_r_reb * spring_MR_rear^2 
    
    damping_f <- data.frame(C_D_f_bump = C_D_f_bump, C_D_f_reb = C_D_f_reb)
    damping_r <- data.frame(C_D_r_bump = C_D_r_bump, C_D_r_reb = C_D_r_reb)
    
    out <- list(damping_f = damping_f, damping_r = damping_r)
  })
  
  output$tbl_damping <- renderRHandsontable({
    
    res <- damping()
    
    f_bump <- paste0(format(round(res$damping_f$C_D_f_bump, 0), nsmall = 0), " Nm/s")
    f_reb  <- paste0(format(round(res$damping_f$C_D_f_reb, 0), nsmall = 0), " Nm/s")
    
    r_bump <- paste0(format(round(res$damping_r$C_D_r_bump, 0), nsmall = 0), " Nm/s")
    r_reb  <- paste0(format(round(res$damping_r$C_D_r_reb, 0), nsmall = 0), " Nm/s")
    
    disp <- as.data.frame(matrix(c("FRONT", f_bump, f_reb, 
                                   "REAR", r_bump, r_reb), ncol = 3, byrow = T))
    names(disp) <- c("DES","BUMP", "REBOUND")
    
    rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      hot_cols(colWidths = c(95, 130, 130))
  })
  
  output$front_bump_value <- renderText({
    
    click <- input$front_bump 
    bump_constant <- xl.damping[ xl.damping$bump_click == click, "bump_constant"]
    bump_constant
  })
  
  output$rear_bump_value <- renderText({
    
    click <- input$rear_bump 
    bump_constant <- xl.damping[ xl.damping$bump_click == click, "bump_constant"]
    bump_constant
  })
  
  output$front_rebound_value <- renderText({
    
    click <- input$front_rebound 
    reb_constant <- xl.damping[ xl.damping$rebound_click == click, "rebound_constant"]
    reb_constant
  })
  
  output$rear_rebound_value <- renderText({
    
    click <- input$rear_rebound 
    reb_constant <- xl.damping[ xl.damping$rebound_click == click, "rebound_constant"]
    reb_constant
  })
  
  output$windspeed_plc <- renderUI({
    
    if( input$choice == "TOP SPEED" ){
      list(
        fluidRow(
          column(6, h5(strong("Wind speed (+ headwind)"))),
          column(2, h5("km/h")),
          column(3,
                 numericInput("windspeed", label = NULL, value = wind_speed_init, min = -20, max = 20))
        )
      )
    }else{
      NULL
    }
  })
  
  output$lap_cond_plc <- renderUI({
   
    if( input$choice == "TOP SPEED" ){
      list(
        wellPanel(
          fluidRow(tags$h5("LAP CONDITIONS")),
          fluidRow(
            column(6, h5(strong("BARCELONA RACE TRACK"))),
            column(2, h5(strong("UNIT"))),
            column(3, h5(strong("VALUE")))
          ),
          fluidRow(
            column(6, h5(strong("Top speed"))),
            column(2, h5("km/h")),
            column(3,
                   numericInput("topspeed", label = NULL, value = topspeed, min = 0, max = 265))
          )
        )
      )
    }else if( input$choice == "MAX BRAKING" ){
      list(
        wellPanel(
          fluidRow(tags$h5("LAP CONDITIONS")),
          fluidRow(
            column(6, h5(strong("BARCELONA RACE TRACK"))),
            column(2, h5(strong("UNIT"))),
            column(3, h5(strong("VALUE")))
          ),
          fluidRow(
            column(6, h5(strong("Top speed"))),
            column(2, h5("km/h")),
            column(3,
                   numericInput("topspeed", label = NULL, value = topspeed, min = 0, max = 250))
          ),
          fluidRow(
            column(6, h5(strong("Longitudinal acceleration"))),
            column(2, h5("G's")),
            column(3,
                   numericInput("long_g", label = NULL, value = long_g, min = -1.5, max = 2, step = 0.05))
          )
        )
      )
    }else if( input$choice == "HIGH SPEED CORNER" ){
      list(
        wellPanel(
          fluidRow(tags$h5("LAP CONDITIONS")),
          fluidRow(
            column(6, h5(strong("BARCELONA RACE TRACK"))),
            column(2, h5(strong("UNIT"))),
            column(3, h5(strong("VALUE")))
          ),
          fluidRow(
            column(6, h5(strong("High speed corner"))),
            column(2, h5("km/h")),
            column(3,
                   numericInput("corner_speed", label = NULL, value = cornerspeed, min = 0, max = 250))
          ),
          fluidRow(
            column(6, h5(strong("Lateral acceleration"))),
            column(2, h5("G's")),
            column(3,
                   numericInput("lateral_g", label = NULL, value = lateral_g, min = -2.5, max = 2.5, step = 0.1))
          )
        )
      )
    }
  })
  
  ### OUTPUT WHEELS ###
  
  output$tbl_fl <- renderRHandsontable({
    
    res <- heights_N_weights()
    #assign("res", res, envir = .GlobalEnv)
    
    if( !is.null( res ) ){
      
      w_r <- paste0(format(round(res$real_w$weight_FL, 2), nsmall = 2), " kg")
      w_d <- paste0(format(round(res$delta_w$delta_w_FL, 2), nsmall = 2), " kg")
      
      h_r <- paste0(format(round(res$real_h$real_h_FL, 2), nsmall = 2), " mm")
      h_d <- paste0(format(round(res$delta_h$delta_h_FL, 2), nsmall = 2), " mm")
      
      disp <- as.data.frame(matrix(c(w_r, h_r, w_d, h_d), ncol = 2))
      names(disp) <- c("REAL", "DELTA")
      
      rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = 100)
    }
    
  })
  
  output$tbl_fr <- renderRHandsontable({
    
    res <- heights_N_weights()
    #assign("res", res, envir = .GlobalEnv)
    
    if( !is.null( res ) ){
      
      w_r <- paste0(format(round(res$real_w$weight_FR, 2), nsmall = 2), " kg")
      w_d <- paste0(format(round(res$delta_w$delta_w_FR, 2), nsmall = 2), " kg")
      
      h_r <- paste0(format(round(res$real_h$real_h_FR, 2), nsmall = 2), " mm")
      h_d <- paste0(format(round(res$delta_h$delta_h_FR, 2), nsmall = 2), " mm")
      
      disp <- as.data.frame(matrix(c(w_r, h_r, w_d, h_d), ncol = 2))
      names(disp) <- c("REAL", "DELTA")
      
      rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = 100)
    }
  })
  
  output$tbl_rl <- renderRHandsontable({
    
    res <- heights_N_weights()
    #assign("res", res, envir = .GlobalEnv)
    
    if( !is.null( res ) ){
      
      w_r <- paste0(format(round(res$real_w$weight_RL, 2), nsmall = 2), " kg")
      w_d <- paste0(format(round(res$delta_w$delta_w_RL, 2), nsmall = 2), " kg")
      
      h_r <- paste0(format(round(res$real_h$real_h_RL, 2), nsmall = 2), " mm")
      h_d <- paste0(format(round(res$delta_h$delta_h_RL, 2), nsmall = 2), " mm")
      
      disp <- as.data.frame(matrix(c(w_r, h_r, w_d, h_d), ncol = 2))
      names(disp) <- c("REAL", "DELTA")
      
      rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = 100)
    }
  })
  
  output$tbl_rr <- renderRHandsontable({
    
    res <- heights_N_weights()
    #assign("res", res, envir = .GlobalEnv)
    
    if( !is.null( res ) ){
      
      w_r <- paste0(format(round(res$real_w$weight_RR, 2), nsmall = 2), " kg")
      w_d <- paste0(format(round(res$delta_w$delta_w_RR, 2), nsmall = 2), " kg")
      
      h_r <- paste0(format(round(res$real_h$real_h_RR, 2), nsmall = 2), " mm")
      h_d <- paste0(format(round(res$delta_h$delta_h_RR, 2), nsmall = 2), " mm")
      
      disp <- as.data.frame(matrix(c(w_r, h_r, w_d, h_d), ncol = 2))
      names(disp) <- c("REAL", "DELTA")
      
      rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = 100)
    }
  })
  
  
  ### RIDE HEIGHT ###
  output$tbl_front_height <- renderRHandsontable({
    
    res <- heights_N_weights()
    #assign("res", res, envir = .GlobalEnv)
    
    if( !is.null( res ) ){
      
      h_r <- paste0(format(round(res$ride_height$ride_height_front, 2), nsmall = 2), " mm")
      h_d <- paste0(format(round(res$delta_h$delta_h_front, 2), nsmall = 2), " mm")
      
      disp <- as.data.frame(matrix(c(h_r, h_d), ncol = 2))
      names(disp) <- c("REAL", "DELTA")
      
      rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = 100)
    }
  })
  
  output$tbl_rear_height <- renderRHandsontable({
    
    res <- heights_N_weights()
    #assign("res", res, envir = .GlobalEnv)
    
    if( !is.null( res ) ){
      
      h_r <- paste0(format(round(res$ride_height$ride_height_rear, 2), nsmall = 2), " mm")
      h_d <- paste0(format(round(res$delta_h$delta_h_rear, 2), nsmall = 2), " mm")
      
      disp <- as.data.frame(matrix(c(h_r, h_d), ncol = 2))
      names(disp) <- c("REAL", "DELTA")
      
      rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = 100)
    }
  })
  
  output$tbl_control <- renderRHandsontable({
    
    VR  <- vertical_rate() 
    RR_dist <- paste0(format(round(VR$RR_dist, 2), nsmall = 2), " %")
    #RR_dist <- format(round(VR$RR_dist, 2), nsmall = 2)
    
    RS <- roll_stiffness_dist()
    RS_dist <- paste0(format(round(RS, 2), nsmall = 2), " %")
    #RS_dist <- format(round(RS, 2), nsmall = 2)
    
    LLT <- lateral_load_transfer()
    LLT_dist <- paste0(format(round(LLT$LLT_dist, 2), nsmall = 2), " %")
    #LLT_dist <- format(round(LLT$LLT_dist, 2), nsmall = 2)
    
    disp <- as.data.frame(matrix(c("Ride rate dist", RR_dist, " - ",#"49-55 %",
                                   "Roll stiff dist", RS_dist, "58-62 %",
                                   "LLTD", LLT_dist, "48-52 %"), ncol = 3, byrow = T))
    names(disp) <- c("DES","REAL", "TARGET")
    
    rhandsontable(disp, readOnly = TRUE, rowHeaders = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      hot_cols(colWidths = c(150, 90, 115))
  })
})



