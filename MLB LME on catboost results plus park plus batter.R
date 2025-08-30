# this code creates the logistic mixed effects model which is used to extract the whiff factor

library(tidyverse)
library(lme4)
library(RSQLite)
library(DBI)
library(tictoc)
library(catboost)
library(data.table)
library(baseballr)
library(future)

#### load in data, models and functions ####
# pull data from my local db
sdb <- 

MLB <- dbGetQuery(
      sdb, 
      "
      SELECT game_id,
      year, batter_id, batter_name, bat_side, pitcher_id, pitch_hand, 
      pitch_type, pitch_name, balls, strikes, plate_x, plate_z, 
      strike_zone_top, strike_zone_bottom, zone,
      release_speed, arm_angle,
      home_team, description, bat_score_diff,
      release_pos_x, release_pos_z, extension,
      ax, ay, az, vx0, vy0, vz0, release_spin_rate
      FROM 'MLB 20_24 w arm angle'
      WHERE year > 2021
      ;
      ")

dbDisconnect(sdb)

# load catboost models for controlling for pitch quality
rhp_fb <- catboost.load_model("final models/RHP FB.cbm")
rhp_breaking <- catboost.load_model("final models/RHP Breaking.cbm")
rhp_offspeed <- catboost.load_model("final models/RHP Offspeed.cbm")
lhp_fb <- catboost.load_model("final models/LHP FB.cbm")
lhp_breaking <- catboost.load_model("final models/LHP Breaking.cbm")
lhp_offspeed <- catboost.load_model("final models/LHP Offspeed.cbm")

# pitch type lists
fb_list <- c("4-Seam Fastball","Sinker","Cutter")
breaking_list <- c("Slider","Sweeper","Curveball","Slurve","Knuckle Curve")
offspeed_list <- c("Changeup","Split-Finger","Forkball","Screwball" )

# Function to transform accelerations relative to global vertical axis
# this is explained in the p of whiff script
transform_accelerations_global <- function(vx0, vy0, vz0, ax, ay, az) {
      #' Transforms acceleration components from global coordinates into a reference frame 
      #' aligned with the velocity vector and global vertical axis.
      #' 
      #' @param vx0,vy0,vz0 Initial velocity components in global coordinates (ft/s)
      #' @param ax,ay,az Acceleration components in global coordinates (ft/s²)
      #' @return List containing a_tang, a_horz, a_vert acceleration components
      
      # Velocity magnitude
      v_mag <- sqrt(vx0^2 + vy0^2 + vz0^2)
      
      # Tangential unit vector (along velocity)
      tang_x <- vx0 / v_mag
      tang_y <- vy0 / v_mag
      tang_z <- vz0 / v_mag
      
      # Use global vertical axis as reference
      temp_x <- 0
      temp_y <- 0
      temp_z <- 1
      
      # Calculate horizontal vector (cross product of temp and tangential)
      horz_x <- temp_y * tang_z - temp_z * tang_y
      horz_y <- temp_z * tang_x - temp_x * tang_z
      horz_z <- temp_x * tang_y - temp_y * tang_x
      
      # Normalize the horizontal vector to unit length
      horz_mag <- sqrt(horz_x^2 + horz_y^2 + horz_z^2)
      horz_x <- horz_x / horz_mag
      horz_y <- horz_y / horz_mag
      horz_z <- horz_z / horz_mag
      
      # Complete the right-handed coordinate system (cross product of tangential and horizontal)
      vert_x <- tang_y * horz_z - tang_z * horz_y
      vert_y <- tang_z * horz_x - tang_x * horz_z
      vert_z <- tang_x * horz_y - tang_y * horz_x
      
      # Project original acceleration vector onto the new coordinate system
      a_tang <- ax * tang_x + ay * tang_y + az * tang_z
      a_horz <- ax * horz_x + ay * horz_y + az * horz_z
      a_vert <- ax * vert_x + ay * vert_y + az * vert_z
      
      return(list(a_tang = a_tang, a_horz = a_horz, a_vert = a_vert))
}

# schedule so that I can have home venue instead of home_team

# list of mlb stadiums
mlb_stadiums <- {c(
      "Dodger Stadium",
      "Globe Life Field", 
      "Minute Maid Park",
      "Chase Field",
      "Petco Park",
      "Oakland Coliseum",
      "Nationals Park",
      "Tropicana Field",
      "Oracle Park",
      "Angel Stadium",
      "Oriole Park at Camden Yards",
      "Great American Ball Park",
      "Kauffman Stadium",
      "Guaranteed Rate Field",
      "loanDepot park",
      "T-Mobile Park",
      "Citi Field",
      "Citizens Bank Park",
      "Wrigley Field",
      "American Family Field",
      "Target Field",
      "Busch Stadium",
      "Yankee Stadium",
      "Comerica Park",
      "Coors Field",
      "PNC Park",
      "Truist Park",
      "Progressive Field",
      "Rogers Centre",
      "Fenway Park"
)} # get rid of international/weird venue games

# get the mlb schedule
schedule <- rbind(
      mlb_schedule(season = 2024, level_ids = "1") %>% 
            dplyr::select(game_pk, teams_home_team_name, venue_name, day_night),
      mlb_schedule(season = 2023, level_ids = "1") %>% 
            dplyr::select(game_pk, teams_home_team_name, venue_name, day_night),
      mlb_schedule(season = 2022, level_ids = "1") %>% 
            dplyr::select(game_pk, teams_home_team_name, venue_name, day_night)
) %>%
      # remove international and one off games in non mlb stadiums
      filter(venue_name %in% mlb_stadiums) %>%
      distinct()

# Define the retractable roof stadiums
retractable_roof_stadiums <- c(
      "Globe Life Field", 
      "Minute Maid Park", 
      "Chase Field", 
      "American Family Field",
      "loanDepot park", 
      "Rogers Centre",
      "T-Mobile Park"
)

# Get roof status for games at retractable roof stadiums
# Set up parallel processing
future::plan(multisession, workers = 4)

roof_status <- schedule %>%
      filter(venue_name %in% retractable_roof_stadiums) %>%
      distinct(game_pk) %>%
      mutate(other_weather = furrr::future_map_chr(game_pk, ~{
            tryCatch(
                  mlb_game_info(game_pk = .x)$other_weather,
                  error = function(e) NA_character_
            )
      }))

# turn off parallel processing
future::plan(sequential)

# join schedule and roof status - and get rid of duplicate game pk's that don't have day night info
schedule_plus_roof <- left_join(schedule, roof_status) %>%
      distinct(game_pk, .keep_all = TRUE) 

#### data editing and data cleaning ####

MLB_clean <-{
      MLB %>%
      filter(
            # filter down to just swings
            description %in% c(
                  "swinging_strike_blocked", "foul",
                  "foul_tip", "hit_into_play",
                  "swinging_strike"
            ),
            pitch_name %in% c(
                  # remove knuckle, slow curve, eephus, pitch out, etc...
                  "Sinker", "Sweeper", "Slider", "4-Seam Fastball", "Changeup",
                  "Cutter", "Curveball", "Split-Finger", "Knuckle Curve",
                  "Forkball", "Slurve", "Screwball"
            ),
            balls < 4 # one 4 ball count somehow...
      ) %>%
      # remove pitches less than 3 sd's slower than the mean, 
      # and pitches outside of the mean and sd of spin
      group_by(pitch_name) %>%
      filter(
            release_speed > (
                  mean(release_speed, na.rm = T) - 3 * sd(release_speed, na.rm = T)
            ),
            between(
                  release_spin_rate,
                  mean(release_spin_rate, na.rm = T) - 3 * sd(release_spin_rate, na.rm = T),
                  mean(release_spin_rate, na.rm = T) + 3 * sd(release_spin_rate, na.rm = T)
            )
      ) %>%
      ungroup() %>%
      # remove non competitive game situations
      filter(abs(bat_score_diff) < 10)
}

# do some calcs for more filtering
mean_rel_x = mean(abs(MLB_clean$release_pos_x), na.rm = T)
sd_rel_x = sd(abs(MLB_clean$release_pos_x), na.rm = T)
mean_rel_z = mean(MLB_clean$release_pos_z, na.rm = T)
sd_rel_z = sd(MLB_clean$release_pos_z, na.rm = T)
mean_ext = mean(MLB_clean$extension, na.rm = T)
sd_ext = sd(MLB_clean$extension, na.rm = T)
mean_arm = mean(MLB_clean$arm_angle, na.rm = T)
sd_arm = sd(MLB_clean$arm_angle, na.rm = T)

# do the final swings edit and final filtering for outlier release points
swings_for_modeling <-
      MLB_clean %>%
      filter(
            between(abs(release_pos_x), mean_rel_x - 3 * sd_rel_x, mean_rel_x + 3 * sd_rel_x),
            between(release_pos_z, mean_rel_z - 3 * sd_rel_z, mean_rel_z + 3 * sd_rel_z),
            between(extension, mean_ext - 3 * sd_ext, mean_ext + 3 * sd_ext),
            between(arm_angle, mean_arm - 3 * sd_arm, mean_arm + 3 * sd_arm)
      ) %>%
      mutate(
            # using batter term in new approach, everything else is handled by catboost model
            batter_term = paste0(batter_name, "_", bat_side, "_", year),
            SHH = case_when(
                  bat_side == pitch_hand ~ 1,
                  bat_side != pitch_hand ~ 0 
            ),
            # whiff binary
            is_whiff = case_when(
                  description %in% c("swinging_strike_blocked", "foul_tip", "swinging_strike") ~ 1,
                  T ~ 0
            )
      ) %>%
      # remove hitters that don't have at least 100 swings
      group_by(batter_term) %>%
      filter(n() >= 100) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(
            # accel transformations
            global_result = list(transform_accelerations_global(vx0, vy0, vz0, ax, ay, az)),
            a_tang_global = global_result$a_tang,
            a_horz_global = global_result$a_horz,
            a_vert_global = global_result$a_vert,
      ) %>%
      dplyr::select(-global_result) %>%
      ungroup() %>%
      filter(a_tang_global < 0) %>%
      mutate(
            # get a column for p of whiff from pitch quality
            pitch_quality_p_whiff = case_when(
                  pitch_hand == 'R' & pitch_name %in% fb_list ~
                        catboost.predict(
                              rhp_fb,
                              catboost.load_pool(.),
                              prediction_type = "Probability"
                        ),
                  pitch_hand == 'R' & pitch_name %in% breaking_list ~
                        catboost.predict(
                              rhp_breaking,
                              catboost.load_pool(.),
                              prediction_type = "Probability"
                        ),
                  pitch_hand == 'R' & pitch_name %in% offspeed_list ~
                        catboost.predict(
                              rhp_offspeed,
                              catboost.load_pool(.),
                              prediction_type = "Probability"
                        ),
                  pitch_hand == 'L' & pitch_name %in% fb_list  ~
                        catboost.predict(
                              lhp_fb,
                              catboost.load_pool(.),
                              prediction_type = "Probability"
                        ),
                  pitch_hand == 'L' & pitch_name %in% breaking_list  ~
                        catboost.predict(
                              lhp_breaking,
                              catboost.load_pool(.),
                              prediction_type = "Probability"
                        ),
                  pitch_hand == 'L' & pitch_name %in% offspeed_list ~
                        catboost.predict(
                              lhp_offspeed,
                              catboost.load_pool(.),
                              prediction_type = "Probability"
                        )
            )
      )
      
# join with roof and day/night game info to create stadium terms
swings_edit_roof <-
      swings_for_modeling %>%
      inner_join(., schedule_plus_roof, join_by(game_id == game_pk)) %>%
      mutate(
            stadium_term = case_when(
                  venue_name == 'Tropicana Field' ~ venue_name,
                  venue_name %in% retractable_roof_stadiums & other_weather == 'Roof Closed' ~ paste(venue_name, 'Roof Closed'),
                  T ~ paste(venue_name, day_night)
            )
      )

# check sample size of all stadium terms
sample_size <- swings_edit_roof %>%
      group_by(stadium_term) %>%
      summarise(n())

#### run the final logistic mixed effects ####
cat("Starting model fit at", format(Sys.time()), "\n")
tic("Model fitting")

model <-
      glmer(
            is_whiff ~ 
                  (1 | batter_term) + 
                  (1 | stadium_term) +
                  pitch_quality_p_whiff, 
            family = binomial, 
            data = swings_edit_roof,
            nAGQ = 0,
            control = glmerControl(optimizer = "bobyqa")
      )

toc()
cat("Model completed at", format(Sys.time()), "\n")

# summarize and park effects and save model
summary(model)

# Get team effects
day_night_park_effects <- ranef(model)$stadium_term %>%
      rownames_to_column(var = "Stadium term")

#saveRDS(model, "final models/MLB day night roof split mixed effects logistic.rds")

#write_csv(day_night_park_effects, "final models/MLB day night roof split effects.csv")
