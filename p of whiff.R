# this script trains the models that predict the probability of a whiff
# the purpose of this to control for pitch movement and location in order to separate the effect of the pitch from the effect of the park

library(tidyverse)
library(RSQLite)
library(DBI)
library(catboost)
library(rsample)
library(pROC)
library(MLmetrics)
library(data.table)
library(ParBayesianOptimization)
library(furrr)

#### data pull ####
# connecting to and pulling from my local DB
sdb <- dbConnect(drv = SQLite(), "C:/Users/gabe2/Desktop/Data analysis projects/statcast db/statcast_db.sqlite")

MLB <- dbGetQuery(
      sdb, 
      "
      SELECT 
      year, batter_id, batter_name, bat_side, pitcher_id, pitch_hand, 
      pitch_type, pitch_name, balls, strikes, plate_x, plate_z, 
      strike_zone_top, strike_zone_bottom, zone,
      release_speed, 
      home_team, description, bat_score_diff,
      release_pos_x, release_pos_z, extension,
      ax, ay, az, vx0, vy0, vz0, release_spin_rate, spin_axis, arm_angle
      FROM 'MLB 20_24 w arm angle'
      WHERE year > 2021
      ;
      ")

dbDisconnect(sdb)

#### data cleaning + outlier filtering ####
MLB_clean <-
      MLB %>%
      filter(
            # get just pitches that resulted in swings
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
      # get rid of game situations that are noncompetitive
      filter(abs(bat_score_diff) < 10)

# keep things clean and fast by removing large objects
rm(MLB)

# do some calcs for more filtering
# will use this to remove points that are so weird they throw off the model
mean_rel_x = mean(abs(MLB_clean$release_pos_x), na.rm = T)
sd_rel_x = sd(abs(MLB_clean$release_pos_x), na.rm = T)
mean_rel_z = mean(MLB_clean$release_pos_z, na.rm = T)
sd_rel_z = sd(MLB_clean$release_pos_z, na.rm = T)
mean_ext = mean(MLB_clean$extension, na.rm = T)
sd_ext = sd(MLB_clean$extension, na.rm = T)
mean_arm = mean(MLB_clean$arm_angle, na.rm = T)
sd_arm = sd(MLB_clean$arm_angle, na.rm = T)

# do the filtering
MLB_clean <- 
      MLB_clean %>%
      filter(
            # apply the 3 sd filters
            between(abs(release_pos_x), mean_rel_x - 3 * sd_rel_x, mean_rel_x + 3 * sd_rel_x),
            between(release_pos_z, mean_rel_z - 3 * sd_rel_z, mean_rel_z + 3 * sd_rel_z),
            between(extension, mean_ext - 3 * sd_ext, mean_ext + 3 * sd_ext),
            between(arm_angle, mean_arm - 3 * sd_arm, mean_arm + 3 * sd_arm)
      ) %>%
      mutate(
            # create a shh binary
            SHH = case_when(
                  bat_side == pitch_hand ~ 1,
                  bat_side != pitch_hand ~ 0 
            ),
            # create a whiff binary
            is_whiff = case_when(
                  description %in% c("swinging_strike_blocked", "foul_tip", "swinging_strike") ~ 1,
                  T ~ 0
            ),
            # create a rhp binary
            rhp = pitch_hand == "R"
      )



#### transform movement using acceleration ####
# Code and idea from Josh Hejka - https://gist.github.com/hedgertronic/ace4513eafec0021d7dcfe843af92c8a
# I wanted to use acceleration instead of raw movement to separate shape from velocity
# but acceleration is biased towards location
# this code takes acceleration and transforms it relative to initial trajectory (velocity in 3 component directions) to account for that

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

# run the transformations on the dataframe
MLB_clean <- MLB_clean %>%
  rowwise() %>%
  mutate(
    # Global transformations
    global_result = list(transform_accelerations_global(vx0, vy0, vz0, ax, ay, az)),
    a_tang_global = global_result$a_tang,
    a_horz_global = global_result$a_horz,
    a_vert_global = global_result$a_vert,
  ) %>%
  select(-global_result) %>%
  ungroup() %>%
  # remove a pitch that misread as negative acceleration
  filter(a_tang_global < 0)

#### separate dataframes by pitcher hand and pitch type ####
# I wanted to do this to create 6 different models
# RHP FB, RHP breaking, RHP offspeed
# LHP FB, LHP breaking, LHP offspeed

fb_list <- c("4-Seam Fastball","Sinker","Cutter")
breaking_list <- c("Slider","Sweeper","Curveball","Slurve","Knuckle Curve")
offspeed_list <- c("Changeup","Split-Finger","Forkball","Screwball" )

rhp_fb <- MLB_clean %>%
      filter(
            pitch_hand == 'R',
            pitch_name %in% fb_list
      )

rhp_breaking <- MLB_clean %>%
      filter(
            pitch_hand == 'R',
            pitch_name %in% breaking_list
      )

rhp_offspeed <- MLB_clean %>%
      filter(
            pitch_hand == 'R',
            pitch_name %in% offspeed_list
      )

lhp_fb <- MLB_clean %>%
      filter(
            pitch_hand == 'L',
            pitch_name %in% fb_list
      )

lhp_breaking <- MLB_clean %>%
      filter(
            pitch_hand == 'L',
            pitch_name %in% breaking_list
      )

lhp_offspeed <- MLB_clean %>%
      filter(
            pitch_hand == 'L',
            pitch_name %in% offspeed_list
      )

#### the model, a function to make it and then code that runs it on all the datasets ####

# this function trains the model, 
# it tunes hyper parameters using bayesian optimization with crossfold validation
# then the model is trained on a train set and is evaluated on a test set
train_catboost_model <- function(df){
      # feature columns
      feature_cols <- c("release_pos_x", "release_pos_z", "extension",
                        "a_horz_global", "a_vert_global", "release_speed", 
                        "release_spin_rate", "plate_x", "plate_z", "SHH")
      
      
      # Create CatBoost pool
      data_pool <- catboost.load_pool(
            data = as.data.table(df)[, ..feature_cols], 
            label = df$is_whiff
      )
      
      # Define objective function for Bayesian Optimization using CV
      catboost_cv_bayes <- function(depth, learning_rate, l2_leaf_reg, border_count) {
            params <- list(
                  loss_function = 'Logloss',
                  eval_metric = 'Logloss',  # Or use 'AUC'
                  iterations = 100,
                  depth = as.integer(depth),
                  learning_rate = learning_rate,
                  l2_leaf_reg = l2_leaf_reg,
                  border_count = as.integer(border_count),
                  task_type = 'CPU',
                  thread_count = 2
            )
            
            cv_results <- catboost.cv(
                  params = params,
                  pool = data_pool,
                  fold_count = 5,
                  type = 'Classical',
                  partition_random_seed = 123,
                  early_stopping_rounds = 10
            )
            
            # Use the minimum logloss from CV
            best_logloss <- min(cv_results$test.Logloss.mean)
            
            # Bayesian optimization maximizes the Score, so return -logloss
            list(Score = -best_logloss)
      }
      
      # Define hyperparameter bounds
      bounds <- list(
            depth = c(3L, 10L),
            learning_rate = c(0.01, 0.3),
            l2_leaf_reg = c(1, 10),
            border_count = c(32L, 255L)
      )
      
      # Run Bayesian Optimization
      opt_result <- bayesOpt(
            FUN = catboost_cv_bayes,
            bounds = bounds,
            initPoints = 10,
            iters.n = 20,
            acq = "ei",  # Expected Improvement
            verbose = 1
      )
      
      # Best parameters
      best_params <- getBestPars(opt_result)
      print(best_params)
      
      # save parameters
      final_params <- list(
            loss_function = 'Logloss',
            eval_metric = 'Logloss',
            iterations = 300,
            depth = as.integer(best_params$depth),
            learning_rate = best_params$learning_rate,
            l2_leaf_reg = best_params$l2_leaf_reg,
            border_count = as.integer(best_params$border_count),
            task_type = 'CPU',
            thread_count = 2,           # Or your preferred number of threads
            random_seed = 123           # For reproducibility
      )
      
      # split training and testing
      data_split <- initial_split(df,
                                  prop = 0.8,
                                  strata = is_whiff)  # This preserves the ratio
      train_data <- training(data_split) %>% as.data.table()
      test_data <- testing(data_split) %>% as.data.table()
      
      # Extract the target for evaluation
      test_target <- test_data$is_whiff
      
      # pools for catboost
      train_pool <- catboost.load_pool(
            data = train_data[, ..feature_cols],
            label = train_data$is_whiff
      )
      
      test_pool <- catboost.load_pool(
            data = test_data[, ..feature_cols],
            label = test_data$is_whiff
      )
      
      # train model
      final_model <- catboost.train(
            learn_pool = train_pool,
            params = final_params
      )
      
      # Predict probabilities on test set
      pred_probs <- catboost.predict(
            final_model,
            test_pool,
            prediction_type = "Probability"
      )
      
      # Evaluate LogLoss
      logloss <- LogLoss(y_pred = pred_probs, y_true = test_target)
      print(paste("LogLoss on test set:", round(logloss, 5)))
      
      # Evaluate AUC
      roc_obj <- roc(test_target, pred_probs)
      auc_val <- auc(roc_obj)
      print(paste("AUC on test set:", round(auc_val, 5)))
      
      metrics_df <- data.frame(
            LogLoss = logloss,
            AUC = as.numeric(auc_val)
      )
      
      importance <- catboost.get_feature_importance(
            model = final_model,
            pool = train_pool,
            type = 'FeatureImportance'
      ) %>% as.data.frame()
      
      # return the stuff
      return(list(
            model = final_model,
            metrics = metrics_df,
            feature_importance = importance
      ))
}

# list of data frames
dataset_list <- list(
      rhp_fb = rhp_fb,
      rhp_breaking = rhp_breaking,
      rhp_offspeed = rhp_offspeed,
      lhp_fb = lhp_fb,
      lhp_breaking = lhp_breaking,
      lhp_offspeed = lhp_offspeed
)

# open threads to run faster
plan(multisession, workers = 4)

# run through 6 different options and make the models
results_list <- future_map(dataset_list, train_catboost_model, .options = furrr_options(seed = TRUE))

# switch back to sequential
plan(sequential)

# save the models
catboost.save_model(results_list$rhp_fb$model, "final models/RHP FB.cbm")
catboost.save_model(results_list$rhp_breaking$model, "final models/RHP Breaking.cbm")
catboost.save_model(results_list$rhp_offspeed$model, "final models/RHP Offspeed.cbm")
catboost.save_model(results_list$lhp_fb$model, "final models/LHP FB.cbm")
catboost.save_model(results_list$lhp_breaking$model, "final models/LHP Breaking.cbm")
catboost.save_model(results_list$lhp_offspeed$model, "final models/LHP Offspeed.cbm")