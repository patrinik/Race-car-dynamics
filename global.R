
require(openxlsx)
xl.file <- "damping.xlsx"
xl.damping <- read.xlsx(paste0('./data/', xl.file))

## input parameter for sidebar ##

# setup
front_ride_height <- 20.2
rear_ride_height  <- 33.77

f_ride_height_max_speed <- 12
r_ride_height_max_speed <- 18

front_tyre_stiff_low <- 18.7
rear_tyre_stiff_low <- 19.9

front_tyre_stiff_high <- 20.3
rear_tyre_stiff_high <- 21.8

tyre_pressure_low <- 1.4
tyre_pressure_high <- 1.6

front_spring <- 650
rear_spring <- 550

front_arb <- 76
rear_arb <- 35

front_roll_cntr <- -5
rear_roll_cntr <- 55

ride_height_f_ref4RCH <- 15
ride_height_r_ref4RCH <- 35

front_tyre_pressure_init <- 1.6
rear_tyre_pressure_init <- 1.5

# weather
temperature <- 28
pressure <- 1015
wind_speed_init <- 0

# lap conditions
topspeed <- 223
cornerspeed <- 164
lateral_g <- 1.8
long_g <- 0.95

# damping
front_bump <- 10
rear_bump <- 10

front_rebound <- 16
rear_rebound  <- 7



## parameter for calculation ##
gravity  <- 9.81
lb2kg    <- 0.453592
inch2mm  <- 25.4

wheelbase <- 2765
track_front <- 1470
track_rear  <- 1455

spring_MR_front <- 1.11
spring_MR_rear  <- 1.27

ARB_MR_front <- 1.53
ARB_MR_rear  <- 1.62

total_weight <- 595

weight_dist_f <- 0.42

# per wheel
unsprung_mass_front <- 23
unsprung_mass_rear <- 26

CoG_unsprung_mass_front <- 265
CoG_unsprung_mass_rear  <- 280

CoG_height <- 290

total_lift_coeff_times_A <- 2.13

aero_weight_dist_f <- 0.415


lateral_accel <- 0


