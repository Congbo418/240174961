football_scorer<-read.csv("C:/Users/Administrator/Downloads/archive/goalscorers.csv")
View(football_scorer)
#Read out all the players in the file who scored goals in the match
football_results<-read.csv("C:/Users/Administrator/Downloads/archive/results.csv")
View(football_results)
#Result Data Frames
football_shootout<-read.csv("C:/Users/Administrator/Downloads/archive/shootouts.csv")
View(football_shootout)  #Penalty Shootout Information
#The three datasets, which make up the three data frames, represent goal scorers/match results/penalty shoot-outs respectively

summary(football_results)
summary(football_scorer)
summary(football_shootout)
#Summary of the three data frames

unique(football_results$tournament)  #Check out what types of ball games are available
world_cup<-subset(football_results, subset = (tournament=="FIFA World Cup")) 
View(world_cup)  #Exporting data frames for World Cup results
print(world_cup)
world_cup$date <- gsub("-", "", world_cup$date) #Remove the - from the date
world_cup$date <- as.numeric(world_cup$date)  #Date changed to numeric
summary(world_cup)  
world_cup<-subset(world_cup,subset = (date>19940101)) #Selection of post-1994 World Cups for analysis
summary(world_cup)    #Found out the game went from 964 to 500.


install.packages("dplyr")

library(dplyr)

install.packages("tidyr")

library(tidyr)
install.packages("tidyverse")
library(tidyverse)

View(world_cup)
summary(world_cup)
is.na(world_cup)  #There's no missing data. There's no need for missing data.
sum(is.na(world_cup)) 


par(mfrow=c(1,2))
hist(x=world_cup$home_score, col="blue",main="team score", xlab="home score",ylab="count",ylim=c(0,300),xlim=c(0,8))
hist(x=world_cup$away_score, col="red",main="team score", xlab="home score",ylab="count",ylim=c(0,300),xlim=c(0,8))
#Put the two charts together and compare them 

plot(world_cup$home_score,world_cup$away_score)  
boxplot(world_cup$home_score)   
lm(world_cup$home_score~1/world_cup$away_score)  
abline(lm(world_cup$home_score ~ 1 / world_cup$away_score), col = "red") 
lines(world_cup$home_score,world_cup$away_score,col="blue")  

freq_table <- table(world_cup$home_team)   #View World Cup Teams - Generate Table
print(freq_table)
common_elements <- intersect(world_cup$home_team, world_cup$away_team) #Finding repeating teams in World Cup
common_counts <- table(common_elements)
install.packages("dplyr")
library(dplyr)
element_counts1 <- table(world_cup$home_team)  
print(element_counts1)
element_counts2 <- table(world_cup$away_team)  
print(element_counts2)
total_count<- element_counts1+element_counts2 
print(total_count)
View(total_count)   
good_team<- total_count[total_count>15]   
View(good_team)
barplot(good_team)   
good_team2<-total_count[total_count>20]
View(good_team2)
barplot(good_team2,las = 2,xlab="country", ylab="number of matches", col="blue",main="good team",cex.names = 0.5)   #Histogram of awesome World Cup teams generated 



##############The questions analyzed in these lines are more skewed toward the home side of the game
world_cup$home_win <- ifelse(world_cup$home_score > world_cup$away_score, 1, 0)
world_cup$away_win <- ifelse(world_cup$home_score < world_cup$away_score, 1, 0)  #Added two rows to the World Cup data to show win/loss relationships
ggplot(world_cup, aes(x = home_score)) + geom_histogram(binwidth = 1, fill = "black") +ggtitle("home team goals")


#####Analyzing only the strongest teams from here on out
wcgh_team<-subset(world_cup,subset=(home_team%in%c("Argentina","Brazil","Czech Republic","France","Germany","Italy","Morocco","Netherlands","South Korea","Spain","Switzerland","United States","Uruguay")))
View(wcgh_team) 
wcga_team<-subset(world_cup,subset=(away_team%in%c("Argentina","Brazil","Czech Republic","France","Germany","Italy","Morocco","Netherlands","South Korea","Spain","Switzerland","United States","Uruguay")))
View(wcga_team)
good_world_cup<-rbind(wcgh_team, wcga_team)   
View(good_world_cup)  #To the current position Split up the matches of what we consider to be the strongest teams, from 1994-2022
summary(good_world_cup)  #As you can see by the SUMMARY, there were 391 games in total
View(good_world_cup)
ggplot(good_world_cup, aes(x = home_score)) + geom_histogram(binwidth = 1, fill = "black") +ggtitle("home team score")

ggplot(good_world_cup, aes(x = away_score)) + geom_histogram(binwidth = 1, fill = "black") +ggtitle("away team score")
par(mfrow=c(1,2))
hist(x=good_world_cup$home_scor,col="blue",main="home team score", xlab="home score",ylab="count",ylim=c(0,150))
hist(x=good_world_cup$away_score,col="red",main="away team score", xlab="home score",ylab="count",ylim=c(0,150)) 



good_world_cup$result <- ifelse(good_world_cup$home_score > good_world_cup$away_score, "home team wins",
                         ifelse(good_world_cup$home_score < good_world_cup$away_score, "away team wins", "draw"))  #Add a new column - home team win or away team win
print(good_world_cup) 

home_wins <- good_world_cup %>%
  group_by(home_team) %>%
  summarise(Home_winning_percentage = mean(result == "home team wins"))
#Stats on each team's home winning percentage
print(home_wins)
summary(home_wins)
View(home_wins)
good_home_wins<-subset(home_wins,subset=(home_team%in%c("Argentina","Brazil","Czech Republic","France","Germany","Italy","Morocco","Netherlands","South Korea","Spain","Switzerland","United States","Uruguay")))
View(good_home_wins)
#Only strong teams
top10_home_wins <- good_home_wins %>%
  top_n(10, Home_winning_percentage)
ggplot(top10_home_wins, aes(x = reorder(home_team, Home_winning_percentage), y = Home_winning_percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Winning Percentage", x = "team", y = "percentage")

good_world_cup$goal_difference <- good_world_cup$home_score - good_world_cup$away_score


#Shorten the World Cup years
number_str <- as.character(good_world_cup$date)

first_four_digits_str <- substr(number_str, 1, 4)

good_world_cup$date <- as.numeric(first_four_digits_str)

#Analyzing only four teams
best_team<-subset(good_world_cup,subset=(home_team%in%c("Argentina","Brazil","France","Germany")))
View(best_team)
summary(best_team)
ggplot(best_team, aes(x = date, y = goal_difference, colour = home_team)) +
  geom_point(size = 2)


#Figure out the rankings of the four strongest teams per World Cup
rank_data <- data.frame(
  Year = c(1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022),
  Brazil = c(1, 2, 1, 5, 6, 4, 6, 7),
  Argentina = c(10, 6, 18, 6, 5, 2, 16, 1),
  France = c(17, 1, 28, 2, 25, 7, 1, 2),
  Germany = c(5, 7, 2, 1, 3, 1, 22, NA)
)
# Convert data from wide format to long format
long_data <- rank_data %>%
  pivot_longer(cols = -Year, names_to = "Country", values_to = "Rank") %>%
  drop_na()  # Remove missing values

# To visualize the ranking trend more, you can invert the rankings
long_data <- long_data %>%
  mutate(Rank = -Rank)

ggplot(long_data, aes(x = Year, y = Rank, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = -32:-1, labels = 32:1) +  # Show original ranking
  labs(title = "Ranking trends（1994-2022）",
       x = "year",
       y = "rank",
       color = "country") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#This chart sees how the teams from the four countries are ranked, and you can see that Brazil is relatively stable


---------------------------------------------------------------------------------------------------------------------
set.seed(123)   #Setting up random sampling
train_index <- sample(1:nrow(best_team), 0.8 * nrow(best_team))  #80% training set 20% test set
train_data <- best_team[train_index, ]   
test_data <- best_team[-train_index, ]   

#########################################################################
model <- glm(home_win ~ home_score, data = train_data, family = "binomial")   #The first logistic regression used only the home team's score for prediction.
#Specify the model as binomial distribution 
summary(model)  #View logistic regression simulation results

plot(train_data$home_score, train_data$home_win) #View a scatterplot between the home team's score and the training set

# Predicting test set results
predictions <- predict(model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# accuracy
accuracy <- mean(predicted_class == test_data$home_win)
cat("Model accuracy：", accuracy, "\n")   
# confusion matrix
table(Predicted = predicted_class, Actual = test_data$home_win)  


next_world_cup <- data.frame(
  home_team = c("Brazil", "Germany"),
  away_team = c("Argentina", "France"),
  fifa_rank_home = c(3, 1),
  fifa_rank_away = c(4, 2),
  home_score = c(1, 1),   # Assuming the home team scores 1
  away_score = c(1, 1)    # Assuming the away team scores 1
)
# Predicting Winning Percentage
predictions <- predict(model, newdata = next_world_cup, type = "response")
predictions

-------------------------------------------------------------------------------------------------------------------


# Using the average number of goals scored by home and away teams as a feature：
home_avg <- good_world_cup %>%
  group_by(home_team) %>%
  summarise(home_avg_score = mean(home_score))

away_avg <- good_world_cup %>%
  group_by(away_team) %>%
  summarise(away_avg_score = mean(away_score))

model1_data <- good_world_cup %>%
  left_join(home_avg, by = "home_team") %>%
  left_join(away_avg, by = "away_team")
View(model1_data)

# Training logistic regression models
model1 <- glm(home_win ~ home_avg_score + away_avg_score, data = model1_data, family = binomial)

summary(model1)

ggplot(data=model1_data,aes(x=home_avg_score,y=home_win))+geom_point()

cor.test(model1_data$home_avg_score,model1_data$home_win)
#correlation analysis：
--------------------------------------------------------------------------------------------------------------------------
#With the above logistic regression, we can predict the winner of the game, but we can't be sure who is the best team, and finally in the test of the best team
  team_stats <- good_world_cup %>%
  group_by(home_team) %>%
  summarise(
    home_win=home_win,
    total_matches = n(),
    wins = sum(result == "主队胜"),
    win_rate = wins / total_matches,
    total_goals = sum(home_score),
    total_conceded = sum(away_score),
    goal_difference = total_goals - total_conceded
  )
View(team_stats)
#Plotting Winning Percentage
ggplot(team_stats, aes(x = reorder(home_team, -win_rate), y = win_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Winning percentage of each team", x = "team", y = "percentage") +
  theme_minimal()
#Plotting the number of goals scored
ggplot(team_stats, aes(x = reorder(home_team, -total_goals), y = total_goals)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Total goals scored by teams", x = "team", y = "total") +
  theme_minimal()
#Chart of goal difference
ggplot(team_stats, aes(x = reorder(home_team, -goal_difference), y = goal_difference)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "every team goal difference", x = "team", y = "goal difference") +
  theme_minimal()
#Relationship between winning percentage and goal difference
ggplot(team_stats, aes(x = win_rate, y = goal_difference, label = home_team)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5) +
  labs(title = "Winning percentage vs. goal difference", x = "percentage", y = "goal difference") +
  theme_minimal()
#Relationship between goals scored and goals conceded
ggplot(team_stats, aes(x = total_goals, y = total_conceded, label = home_team)) +
  geom_point(color = "red", size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5) +
  labs(title = "Total goals scored vs. total goals conceded", x = "score", y = "loss") +
  theme_minimal()


set.seed(123)   
train_index <- sample(1:nrow(team_stats), 0.8 * nrow(team_stats)) 
train_data <- team_stats[train_index, ]   
test_data <- team_stats[-train_index, ]   


model <- glm(home_win ~ goal_difference, data = train_data, family = "binomial") 
summary(model)  
#Output interpretation: goal difference has a significant impact！

# Predicting test set results
predictions <- predict(model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)
# accuracy
accuracy <- mean(predicted_class == test_data$home_win)
cat("acuuracy：", accuracy, "\n")

table(Predicted = predicted_class, Actual = test_data$home_win) 

model1 <- glm(home_win ~ goal_difference+total_goals, data = train_data, family = "binomial")
summary(model1)

model2 <- glm(home_win ~ win_rate, data = train_data, family = "binomial")
summary(model2)


