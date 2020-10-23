library(rsample)
library(parsnip)
library(recipes)
library(tidyr)
library(workflows)
library(tune)
library(yardstick)
library(broom)
library(dials)
library(cfbscrapR)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(vip)
library(ranger)


AllPBPclean <-read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PlayByPlay/Allpbp.csv")  %>%
  select(
    game_id, drive_id, new_id, offense_play, defense_play, home,
    away, period, clock.minutes, clock.seconds, offense_score, defense_score, play_type_general,
    TimeSecsRem, Under_two, down, distance, Goal_To_Go, off_timeouts_rem_before, def_timeouts_rem_before,
    yards_to_goal, start_yardline, start_yards_to_goal, home_wp, away_wp, penalty_flag
  )%>%
  filter(
    play_type_general %in% c("Rush", "Pass"),
    penalty_flag == 0,
    !is.na(down),
  ) %>%
  mutate(
    in_red_zone = if_else(start_yards_to_goal <= 20, 1, 0),
    in_fg_range = if_else(start_yards_to_goal <= 35, 1, 0),
    two_min_drill = if_else(clock.minutes < 2, 1, 0)
  ) %>%
  collect()
write.csv(AllPBPclean, paste0("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PlayByPlay/AllpbpCleaned.csv" )) 

# glimpse(AllPBPclean)
# all_plays <- AllPBPclean %>%
#   group_by(game_id, offense_play) %>%
#   mutate(
#     run = if_else(play_type_general == "Rush", 1, 0),
#     pass = if_else(play_type_general == "Pass", 1, 0),
#     total_runs = if_else(play_type_general == "rush", cumsum(run) - 1, cumsum(run)),
#     total_pass = if_else(play_type_general == "Pass", cumsum(pass) - 1, cumsum(pass)),
#     previous_play = if_else(offense_play == lag(offense_play),
#                             lag(play_type_general), "First play of Drive"
#     ),
#     previous_play = if_else(is.na(previous_play),
#                             replace_na("First play of Drive"), previous_play
#     )
#   ) %>%
#   ungroup() %>%
#   mutate_at(vars(
#     play_type_general, offense_play, defense_play, down, period,
#     off_timeouts_rem_before, def_timeouts_rem_before, in_red_zone,
#     in_fg_range, previous_play, Goal_To_Go, two_min_drill
#     
#   ), as.factor) %>%
#   select(-run, -pass)
# 
# write.csv(all_plays, paste0("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PlayByPlay/AllpbpCleaned.csv" )) 

all_plays <-read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PlayByPlay/AllpbpCleaned.csv") 
glimpse(all_plays)
split_pbp <- initial_split(all_plays, 0.75, strata = play_type_general)
split_pbp
# separate the training data
train_data <- training(split_pbp)
# separate the testing data
test_data <- testing(split_pbp)

train_data %>% 
  count(play_type_general) %>% 
  mutate(ratio = n/sum(n))

test_data %>% 
  count(play_type_general) %>% 
  mutate(ratio = n/sum(n))


pbp_rec <- recipe(play_type_general ~ ., data = train_data) %>% 
  # ignore these vars for train/test, but include in data as ID
  update_role(game_id, new_role = "ID") %>% 
  # removes vars that have large absolute correlations w/ other vars
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec)   # recipe from recipes

pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data) # fit the model against the training data

print("Done with training!")
pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(play_type_general))
glimpse(pbp_pred_lr)

pbp_pred_lr %>% 
  # calculate ROC curve
  roc_curve(truth = play_type_general, 
            .pred_Pass) %>% 
  # ggplot2 autoplot for AB line 
  # and the path of ROC curve
  autoplot()

pbp_pred_lr %>% 
  # get Area under Curve
  roc_auc(truth = play_type_general, 
          .pred_Pass)
pbp_pred_lr %>% 
  # collect and report metrics
  metrics(truth = play_type_general, 
          .pred_class)



rf_mod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")
rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe
pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest
# Get predictions and check metrics
pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(play_type_general)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))

`pbp_pred_rf` %>% # Random Forest predictions
  metrics(truth = play_type_general, .pred_class)
`pbp_pred_lr` %>% # Logistic Regression predictions
  metrics(truth = play_type_general, .pred_class)

pbp_fit_rf %>%
  pull_workflow_fit() %>% 
  vip(num_features = 20, aesthetics = list(fill = topo.colors(20)) )


roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = play_type_general, .pred_Pass) %>% 
  mutate(model = "Random Forest")
roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = play_type_general, .pred_Pass) %>% 
  mutate(model = "Logistic Regression")
full_plot <- bind_rows(roc_rf, roc_lr) %>% 
  # Note that autoplot() would also work here!
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  scale_color_manual(values = rainbow(2)) +
  theme(legend.position = "top")
full_plot
