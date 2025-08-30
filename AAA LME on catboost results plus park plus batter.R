# functionally the same as MLB version just slightly different column names

library(tidyverse)
library(lme4)
library(RSQLite)
library(DBI)
library(tictoc)
library(mixedup)
library(arm)
library(catboost)
library(data.table)
library(baseballr)

#### load in data, models and functions ####
sdb <-

AAA_query <- function(db, seasonID){
      dbGetQuery(
            sdb,
            paste(
                  "
                  SELECT game_pk,
                  `matchup.batter.id`, `matchup.batSide.code`, 
                  `matchup.pitcher.id`, `matchup.pitchHand.code`,
                  `details.type.description`, `count.balls.start`, `count.strikes.start`, 
                  `pitchData.coordinates.pX`, `pitchData.coordinates.pZ`,
                  `pitchData.startSpeed`, `home_team`, `home_parentOrg_name`, 
                  `details.description`,`result.homeScore`, `result.awayScore`, 
                  `pitchData.extension`, `pitchData.coordinates.x0`,
                  `pitchData.coordinates.z0`, `pitchData.coordinates.y0`,
                  `pitchData.coordinates.aX`, `pitchData.coordinates.aY`, `pitchData.coordinates.aZ`,
                  `pitchData.coordinates.vX0`, `pitchData.coordinates.vY0`, `pitchData.coordinates.vZ0`,
                  `pitchData.breaks.spinRate`
                  FROM
                  ",
                  db,
                  ";"
            )
      ) %>%
            mutate(year = seasonID)
}

AAA24 <- AAA_query('`AAA 24`', 2024)

AAA23 <- AAA_query('`AAA23`', 2023)

AAA22 <- AAA_query('`AAA22`', 2022)

dbDisconnect(sdb)

# load models
rhp_fb <- catboost.load_model("final models/RHP FB.cbm")
rhp_breaking <- catboost.load_model("final models/RHP Breaking.cbm")
rhp_offspeed <- catboost.load_model("final models/RHP Offspeed.cbm")
lhp_fb <- catboost.load_model("final models/LHP FB.cbm")
lhp_breaking <- catboost.load_model("final models/LHP Breaking.cbm")
lhp_offspeed <- catboost.load_model("final models/LHP Offspeed.cbm")

# pitch type lists
fb_list <- c("Four-Seam Fastball","Sinker","Cutter","Fastball","Two-Seam Fastball")
breaking_list <- c("Slider","Sweeper","Curveball","Slurve","Knuckle Curve")
offspeed_list <- c("Changeup","Splitter","Forkball","Screwball" )

# Function to transform accelerations relative to global vertical axis
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

schedule <- rbind(
      mlb_schedule(season = 2024, level_ids = "11") %>% 
            dplyr::select(game_pk, teams_home_team_name, venue_name, day_night),
      mlb_schedule(season = 2023, level_ids = "11") %>% 
            dplyr::select(game_pk, teams_home_team_name, venue_name, day_night),
      mlb_schedule(season = 2022, level_ids = "11") %>% 
            dplyr::select(game_pk, teams_home_team_name, venue_name, day_night)
) %>%
      mutate(
            venue_name = case_when(
                  venue_name == 'Frontier Field' ~ 'Innovative Field',
                  T ~ venue_name
            )
      )

#### data editing and data cleaning ####
AAA_clean <-
      rbind(AAA24, AAA23, AAA22) %>%
      # fix okc dodgers/baseball club/comets
      dplyr::mutate(
            home_team = case_when(
                  home_team %in% c(
                        "Oklahoma City Baseball Club", 
                        "Oklahoma City Dodgers"
                  ) ~ "Oklahoma City",
                  T ~ home_team
            )
      ) %>%
      inner_join(., schedule, join_by(game_pk)) %>%
      mutate(stadium_term = paste(venue_name, day_night)) %>%
      # maybe remove rows with na data?
      dplyr::filter(
            details.description %in% c( # filter for swings
                  "In play, out(s)", "Swinging Strike", "Foul",
                  "In play, no out", "Swinging Strike (Blocked)",
                  "In play, run(s)", "Foul Tip"
            ),
            !details.type.description %in% c(
                  # remove knuckle, slow curve, eephus, pitch out, etc...
                  'Slow Curve', 'Screwball', 'Eephus', 'Forkball', 'Knuckle Ball'
            ),
            count.strikes.start < 3,
            count.balls.start < 4
      ) %>%
      # remove all pitches with missing tracking data
      na.omit() %>%
      # remove the clunky AAA naming convention
      rename_with(~ gsub("pitchData\\.coordinates\\.", "", .x)) %>%
      # calculate release point
      mutate(
            release_pos_y = 60.5 - pitchData.extension,
            delta_t = (release_pos_y - y0) / vY0,
            release_pos_x = ifelse(
                  matchup.pitchHand.code == 'R',
                  (x0 + vX0 * delta_t + 0.5 * aX * delta_t^2) * -1,
                  x0 + vX0 * delta_t + 0.5 * aX * delta_t^2
            ),
            release_pos_z = z0 + vZ0 * delta_t + 0.5 * aZ * delta_t^2,
            # also get score difference calculated
            score_diff = abs(result.awayScore - result.homeScore)
      ) %>%
      # rename everything to have same names as mlb version
      rename(
            description = details.description,
            batter_id = matchup.batter.id, bat_side = matchup.batSide.code, 
            pitcher_id = matchup.pitcher.id, pitch_hand = matchup.pitchHand.code,
            pitch_name = details.type.description, balls = count.balls.start, 
            strikes = count.strikes.start, plate_x = pX, plate_z = pZ, 
            release_speed = pitchData.startSpeed, extension = pitchData.extension,
            ax = aX, ay = aY, az = aZ, vx0 = vX0, vy0 = vY0, vz0 = vZ0, 
            release_spin_rate = pitchData.breaks.spinRate
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
      filter(score_diff < 10)

# do some calcs for more filtering
mean_rel_x = mean(abs(AAA_clean$release_pos_x), na.rm = T)
sd_rel_x = sd(abs(AAA_clean$release_pos_x), na.rm = T)
mean_rel_z = mean(AAA_clean$release_pos_z, na.rm = T)
sd_rel_z = sd(AAA_clean$release_pos_z, na.rm = T)
mean_ext = mean(AAA_clean$extension, na.rm = T)
sd_ext = sd(AAA_clean$extension, na.rm = T)

# do the final swings edit and final filtering for outlier release points
swings_edit <- {
      AAA_clean %>%
            filter(
                  between(abs(release_pos_x), mean_rel_x - 3 * sd_rel_x, mean_rel_x + 3 * sd_rel_x),
                  between(release_pos_z, mean_rel_z - 3 * sd_rel_z, mean_rel_z + 3 * sd_rel_z),
                  between(extension, mean_ext - 3 * sd_ext, mean_ext + 3 * sd_ext)
            ) %>%
            mutate(
                  # using batter term in new approach, everything else is handled by catboost model
                  batter_term = paste0(batter_id, "_", bat_side, "_", year),
                  SHH = case_when(
                        bat_side == pitch_hand ~ 1,
                        bat_side != pitch_hand ~ 0 
                  ),
                  is_whiff = case_when(
                        description %in% c(
                              "Swinging Strike", "Swinging Strike (Blocked)", "Foul Tip"
                        ) ~ 1,
                        T ~ 0
                  )
            ) %>%
            group_by(batter_term) %>%
            filter(n() >= 100) %>%
            ungroup() %>%
            rowwise() %>%
            mutate(
                  # Global transformations
                  global_result = list(transform_accelerations_global(vx0, vy0, vz0, ax, ay, az)),
                  a_tang_global = global_result$a_tang,
                  a_horz_global = global_result$a_horz,
                  a_vert_global = global_result$a_vert,
            ) %>%
            dplyr::select(-global_result) %>%
            ungroup() %>%
            filter(a_tang_global < 0) %>%
            mutate(
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
            )}

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
            data = swings_edit,
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

saveRDS(model, "final models/AAA day night split mixed effects logistic.rds")
write_csv(day_night_park_effects, "final models/AAA day night split effects.csv")

