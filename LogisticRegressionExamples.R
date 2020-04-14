### Lab Examples of Logistic Regression https://homepages.uc.edu/~lis6/Teaching/ML19Spring/Lab/lab7_logit.html

# Packages needed
library(dplyr)
library(ROCR)

# Set a working directory
setwd("~/NCAA_Bball/MDataFiles_Stage1")
file_list <- list.files(path = "~/NCAA_Bball/MDataFiles_Stage1")

# Year variable to filter datasets
first_year <- 2007

# Team Names
teams <- read.csv('MTeams.csv') %>%
  as_tibble() %>%
  filter(LastD1Season >= first_year)

# Compact Tournament Results
tourney_comp <- read.csv('MNCAATourneyCompactResults.csv') %>%
  as_tibble() %>% # Convert to a tibble - tibbles are types of tables
  filter(Season >= first_year) %>% # filter for only data greater than first_year
  select(Season, WTeamID, LTeamID) # Select these columns

# Compact Tournament Seeds
tourney_seeds <- read.csv('MNCAATourneySeeds.csv') %>%
  as_tibble() %>%
  filter(Season >= first_year) %>%
  mutate(Seed = as.numeric(substr(Seed, 2, 3)))

# Compact Season Results
season_comp <- read.csv('MRegularSeasonCompactResults.csv') %>%
  as_tibble() %>%
  filter(Season >= first_year)

# Total Points by Team by Year for Wins
win_points <- season_comp %>%
  select(Season, WTeamID, WScore) %>%
  group_by(Season, WTeamID) %>% # Group by the season & the WTeamID
  summarize(WPoints = sum(WScore)) # Calculate the total scores for the winning times

# Total Points by Team by Year for Losses
loss_points <- season_comp %>%
  select(Season, LTeamID, LScore) %>%
  group_by(Season, LTeamID) %>%
  summarize(LPoints = sum(LScore)) 

# Total Points by Team by Year
  # This won't get teams that had 0 wins - not needed for March Madness, so this works
total_team_points <- win_points %>%
  left_join(loss_points, by = c("Season", "WTeamID" = "LTeamID")) %>% # Join the winning & losing points
  mutate(TeamID = WTeamID,
         WPoints = if_else(is.na(WPoints), as.integer(0), WPoints), 
         LPoints = if_else(is.na(LPoints), as.integer(0), LPoints), 
         TeamPts = WPoints + LPoints) %>%
  select(Season, TeamID, TeamPts)

# Check for NA values
total_team_points[rowSums(is.na(total_team_points)) > 0,]

# Develop base dataset
base_data <- tourney_comp %>% # Take the tourney compact results
  inner_join(tourney_seeds, by = c("Season", "WTeamID" = "TeamID")) %>% # Join the seeds
  mutate(WTeamSeed = Seed) %>% # Add the Winning Team's Seed
  select(-Seed) %>% # Drop Seed
  inner_join(tourney_seeds, by = c("Season", "LTeamID" = "TeamID")) %>% # Join the seeds
  mutate(LTeamSeed = Seed) %>% # Add the losing Team's Seed
  select(-Seed) %>%
  inner_join(total_team_points, by = c("Season", "WTeamID" = "TeamID")) %>% # Join total points for the winning team
  mutate(WTeamPts = TeamPts) %>%
  select(-TeamPts) %>%
  inner_join(total_team_points, by = c("Season", "LTeamID" = "TeamID")) %>% # Join total points for the losing team
  mutate(LTeamPts = TeamPts) %>%
  select(-TeamPts)

# Check for NA values
base_data[rowSums(is.na(base_data)) > 0,]


# Change winners & losers into 2 separate dataframes & merge back together
# Complete this for model training
team_win <- base_data %>%
  mutate(
    Team1 = WTeamID,
    Team1Seed = WTeamSeed,
    Team1Pts = WTeamPts,
    Team2 = LTeamID,
    Team2Seed = LTeamSeed,
    Team2Pts = LTeamPts
  ) %>%
  select(Team1, Team2, Team1Seed, Team2Seed, Team1Pts, Team2Pts) %>%
  mutate(Result = 1) # Team1 in this dataframe won, so give them a 1

team_lose <- base_data %>%
  mutate(
    Team1 = LTeamID,
    Team1Seed = LTeamSeed,
    Team1Pts = LTeamPts,
    Team2 = WTeamID,
    Team2Seed = WTeamSeed,
    Team2Pts = WTeamPts
  ) %>%
  select(Team1, Team2, Team1Seed, Team2Seed, Team1Pts, Team2Pts) %>%
  mutate(Result = 0) # Team1 in this dataframe lost, so give them a 0

# Merge the datasets back together
team_combined <- team_win %>%
  union_all(team_lose) %>%
  mutate(SeedDiff = Team1Seed - Team2Seed, 
         TeamPtsDiff = Team1Pts - Team2Pts)


# Separate training & testing data
index <- sample(nrow(team_combined), nrow(team_combined)*.7) # Get a random sample of rows
train <- team_combined[index,]
test <- team_combined[-index,]


# Sample Model
model1 <- glm(Result ~ SeedDiff + TeamPtsDiff, family=binomial, data = train)

# Model Summary Results
summary(model1)

# Predictions 
model1_pred <- predict(model1, data = train, type = "response")

# Take the results of the games for the training set to compare to the predictions
y_train <- train$Result

# Check the area under the curve
# Read more about this on the link in the first line or google
pred <- prediction(predictions =  model1_pred, labels = y_train)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

# Get the AUC
# Read more about this on the link in the first line or google
unlist(slot(performance(pred, "auc"), "y.values"))


### Out of sample testing

# Predictions on the testing dataset
pred_test <- predict(model1, newdata = test, type="response")

# Sames as above
pred <- prediction(pred_test, test$Result)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

# Testing AUC
unlist(slot(performance(pred, "auc"), "y.values"))

# Add the results back to the testing dataset
test$ModResult <- pred_test

# Convert the win probabilites to 1 or 0
test <- test %>%
          mutate(ModResultBin = if_else(ModResult > .5, 1, 0))

# See where predictions were wrong != 0
test$Result - test$ModResultBin
