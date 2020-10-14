library(readr)
wnba <- read_csv("wnba.csv")
colnames(wnba)

library(tidyverse)
#Sampling Error
View(wnba)
set.seed(1)
population <-wnba$Games_Played
parameter <- max(population, na.rm = T)


sample <- sample(wnba$Games_Played, size = 30)
statistic <- max(sample, na.rm = T)

sampling_error <- parameter - statistic
sampling_error

# Simple Randomig Sampling
set.seed(1)
sample(wnba$Games_Played, size = 5)

set.seed(1)
sample <- sample(wnba$Games_Played, size = 30)
sample <- mean(sample(wnba$Games_Played, size = 30))


set.seed(1)
sample_1 <- mean(sample(wnba$PTS, size = 10))
sample_2 <- mean(sample(wnba$PTS, size = 10))
sample_3 <- mean(sample(wnba$PTS, size = 10))

sample_1
sample_2
sample_3


#Generating Numerous Random Samples
set.seed(1)
mean_points <- replicate(100,mean(sample(wnba$PTS, size = 10)))
minimum <- min(mean_points)
maximum <- max(mean_points)

minimum
maximum

#Visualizing Random Samples
mean_points
x <- 1:100
df <- tibble(x, mean_points)
df

ggplot(data = df, aes(x = x, y = mean_points))+geom_point()+
  geom_hline(yintercept = mean(wnba$PTS),color = "blue")+ylim(90,310)


#The Importance of Sample Size
set.seed(1)
mean_points <- replicate(100,mean(sample(wnba$PTS, size = 100)))

x <- 1:100
df <- tibble(x, mean_points)

ggplot(data = df, aes(x = x, y = mean_points))+geom_point()+
  geom_hline(yintercept = mean(wnba$PTS),color = "blue")+ylim(90,310)


#Stratified Sampling
unique(wnba$Pos)

#Sampling Rows
set.seed(1)
wnba_sampled <- sample_n(wnba, size = 10)
head(wnba_sampled)
mean(wnba_sampled$PTS)

set.seed(1)
wnba_sampled <- sample(wnba$PTS, size = 10)
mean(wnba_sampled)

set.seed(1)
thirty_samples <- sample_n(wnba, size = 30)
mean_age <- mean(thirty_samples$Age)
mean_games <- mean(thirty_samples$Games_Played)

mean_age
mean_games


#Creating and Analyzing Strata with dplyr
set.seed(1)
wnba %>% group_by(Pos) %>% sample_n(10) %>% summarise(mean_pts = mean(PTS))

set.seed(1)
wnba <- wnba %>%
  mutate(pts_game = PTS/Games_Played)
total_points_estimates <- wnba %>%
  group_by(Pos) %>% sample_n(10) %>%
  summarise(mean_pts_season = mean(PTS),mean_pts_game = mean(pts_game)) %>%
  arrange(Pos)

total_points_estimates

#Proportional Stratified Sampling

min(wnba$Games_Played)
max(wnba$Games_Played)

wnba %>%
  mutate(games_stratum = cut(Games_Played,breaks = 3)) %>% 
  group_by(games_stratum) %>% 
  summarise(n = n()) %>% mutate(percentage = n/sum(n) * 100) %>% arrange(desc(percentage))

set.seed(1)
under_12 <- wnba %>% filter(Games_Played <= 12) %>% sample_n(1)
btw_13_22<- wnba %>% filter(Games_Played > 12 & Games_Played <= 22) %>% sample_n(2)
over_22 <- wnba %>% filter(Games_Played > 22) %>% sample_n(7)
combined <- bind_rows(under_12,btw_13_22,over_22)

mean(combined$PTS)


#Many Proportional Stratified Samples

set.seed(1)
mean_points <- replicate(n = 100, expr = mean(sample(wnba$PTS, size = 10)))

sample_number <- 1:100
df <- tibble(x = sample_number, y = mean_points)

ggplot(data = df, aes(x = sample_number, y = mean_points)) + geom_point()+ geom_hline(yintercept = mean(wnba$PTS),color = "blue")+ylim(90,310)


set.seed(1)
sample_mean <- function(x){
  
  under_12 <- wnba %>% filter(Games_Played <= 12) %>% sample_n(1)
  btw_13_22<- wnba %>% filter(Games_Played > 12 & Games_Played <= 22) %>% sample_n(2)
  over_22 <- wnba %>% filter(Games_Played > 22) %>% sample_n(7)
  combined <- bind_rows(under_12,btw_13_22,over_22)
  
  mean(combined$PTS)
  
}
sample_number <- 1:100
mean_points_season <- map_dbl(sample_number, sample_mean)
df <- tibble(sample_number,  mean_points_season)
ggplot(data = df, aes(x = sample_number, y = mean_points_season)) + geom_point()+ geom_hline(yintercept = mean(mean_points_season),color = "blue")+ylim(80,320)



#Alternative Approach

over_22 <- wnba %>% filter(Games_Played > 22) %>% sample_frac(0.25)
over_22


set.seed(1)
wnba %>% group_by(Pos) %>% 
  sample_n(10) %>% 
  summarise(mean_pts = mean(PTS))



wnba %>% group_by(Pos) %>% 
  sample_frac(0.07) %>% 
  summarise(mean_pts = mean(PTS))


wnba <- wnba %>% mutate(games_stratum = cut(Games_Played, breaks = 3))

set.seed(1)
sample_mean <- function(x){
  sample <- wnba %>% group_by(games_stratum) %>% sample_frac(0.07)
  
  mean(sample$PTS)
}
sample_number <- 1:100
mean_points_season <- map_dbl(sample_number, sample_mean)
df <- tibble(sample_number, mean_points_season)
ggplot(data = df, aes(x = sample_number, y = mean_points_season)) +
  geom_point()+ 
  geom_hline(yintercept = mean(mean_points_season),color = "blue")+ylim(80,320)

#Choosing the right strata
#Cluster Sampling

unique(wnba$Team)
set.seed(10)
clusters <- unique(wnba$Team) %>% sample(size = 4)

sample <- wnba %>% filter(Team %in% clusters)
sample_height <- mean(sample$Height)
sample_age <- mean(sample$Age)
sample_game <- mean(sample$Games_Played)
sample_total <- mean(sample$PTS)


sampling_error_height <- mean(wnba$Height) - sample_height
sampling_error_age <- mean(wnba$Age) - sample_age
sampling_error_games <- mean(wnba$Games_Played) - sample_game
sampling_error_points <- mean(wnba$PTS) - sample_total

sampling_error_age
sampling_error_games
sampling_error_points

#Qualitative and Quantitative Variables 

view(wnba)
quantitative_vars <- sort( c("Three_PA",'Age',"AST","Birthdate","BMI","DREB","Experience",
                       "FGA","FGM","FT_perc","FIA","FTM","Games_Played","Height","MIN",
                       "OREB","PTS","REB","Weight"))
qualitative_vars <- sort(c("Birth_Place","College","Name","Team","Pos"))

quantitative_vars
qualitative_vars

#The Nominal Scale
wnba <- wnba %>% mutate(Height_labels = case_when(
  Height <= 170 ~ "short",
  Height > 170 & Height <= 180 ~ "medium",
  Height > 180 ~ "tall"
))
wnba %>% select(Height,Height_labels) %>% head(10)

nominal_scale <- sort(c("Birth_Place","College","Name","Team","Pos","Height_labels"))
nominal_scale


#The ordinal Scale
question_1 <- TRUE
question_2 <- FALSE
question_3 <- FALSE
question_4 <- TRUE
question_5 <- FALSE
question_6 <- FALSE

#The Difference between Ratio and Interval Scales
wnba <- wnba %>% mutate(Weight_deviation = Weight - mean(Weight, na.rm = TRUE))

wnba %>% select(Name, Weight, Weight_deviation) %>% sample_n(size = 5)


interval_scale <- sort(c("Birthdate","Weight_deviation"))
ratio_scale <- sort(c("Three_PA","Age","AST","BMI","DREB",
                      "Experience","FGA","FGM","FT_perc","FTA",
                      "FTM","Games_Played","Height","MIN",
                      "OREB","PTS","REB","Weight"))


#Discrete and Non Discrete values 
discrete <- sort(c("Three_PA","AST","DREB","FGA","FGM","FTA",
                   "FTM","Games_Played","OREB","PTS","REB"))

continous <- sort(c("BMI","Experience","FT_perc","Height","MIN","Weight","Age"))


#Limits

wnba$Weight <- as.integer(wnba$Weight)
head(wnba$Weight)
typeof(wnba$Weight)
21.2 - 20.5 21.5

#Simplifying Data
table(wnba$Pos)

#summary of th df
str(wnba)
glimpse(wnba)

#Frequency distribution tables
pos_freq <- table(wnba$Pos)
pos_freq

as.data.frame(pos_freq) %>% arrange(-Freq)

freq_dist_pos <- wnba %>% group_by(Pos) %>% summarise(Freq = n())
freq_dist_height <- wnba %>% group_by(Height) %>% summarise(Freq = n())

#Arranging Frequency Distribution Table
wnba %>% group_by(Height) %>% summarise(Freq = n()) %>% arrange(-Freq)

age_ascending <- wnba %>% group_by(Age) %>% summarise(Freq = n()) %>% arrange(Age)
age_descending <- wnba %>% group_by(Age) %>% summarise(Freq = n()) %>% arrange(desc(Age))

age_ascending %>% filter(Age > 30) %>% summarise(sum(Freq))

#Sorting Tables For Ordinal Variables
wnba %>% select(Height, Height_labels) %>% head(10)

wnba %>% group_by(Height_labels) %>% summarise(Freq = n()) %>% arrange(Height_labels)


wnba %>% group_by(Height_labels) %>% summarise(Freq = n()) %>% arrange(desc(Height_labels))

height_levels <- c('short','medium','tall')
wnba_factor <- wnba %>% mutate(Height_labels = factor(Height_labels,levels = height_levels))
view(wnba)

wnba_factor %>% group_by(Height_labels) %>% summarise(Freq = n()) %>% arrange(Height_labels)


wnba %>% group_by(Height_labels) %>% summarise(Freq = n()) %>% arrange(factor(Height_labels, levels = height_levels))

wnba <- wnba %>% mutate(Points_labels = case_when(
  PTS <= 118.4 ~ 'well below average',
  PTS > 118.4 & PTS <= 234.8 ~ 'below average',
  PTS > 234.8 & PTS <= 351.2 ~ 'average points',
  PTS > 351.2 & PTS <= 467.6 ~ 'above average',
  PTS > 467.6 ~ 'well above average'
))


point_levels <- c('well below average','below average','average points','above average','well above average')

points_ordinal <- wnba %>% group_by(Points_labels) %>%
  summarise(Freq = n()) %>% arrange(factor(Points_labels, levels = point_levels))

points_ordinal

#Proportion and Percentages
wnba %>% group_by(Pos) %>% summarize(Freq = n()) %>% arrange(desc(Freq))

# Proportion

wnba %>% group_by(Pos) %>% 
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / nrow(wnba)) %>% 
  arrange(desc(Freq))

#Percentage
wnba %>% group_by(Pos) %>% 
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / nrow(wnba)) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100) %>%
  arrange(desc(Freq))

wnba %>% 
  filter(Pos == "G") %>% 
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / nrow(wnba)) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100) %>%
  arrange(desc(Freq))


age_25 <- wnba %>%
  filter(Age == 25) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / nrow(wnba)) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100) %>%
  arrange(desc(Freq))

age_23_or_under <- wnba %>% 
  filter(Age <= 23) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / nrow(wnba)) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100) %>%
  arrange(desc(Freq))

age_30_or_older <- wnba %>%
  filter(Age >= 30)  %>%
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / nrow(wnba)) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100) %>%
  arrange(desc(Freq))

data.frame(age_25, age_23_or_under, age_30_or_older)


#Percentile and Percentile Ranks
mean(wnba$Age <= 23)

mean(wnba$Age <= 23) * 100

#Age older than 30
percentile_50_or_less <- mean(wnba$Games_Played <= 17) * 100

percentile_above_50 <- mean(wnba$Games_Played > 17) * 100 

#Finding Percentiles with R

summary(wnba$Age)

quantile(wnba$Age)

quantile(wnba$Age, probs = c(0,0.1,0.25,0.33,0.5,0.66,0.75,0.9,1))

wnba %>%
  mutate(cume_dist_age = cume_dist(Age)) %>%
  select(Name,Age,cume_dist_age) %>% head(n = 15)

quantile(wnba$Age)

age_upper_quartile <- quantile(wnba$Age, probs = c(0.75))
age_middle_quartile <- quantile(wnba$Age, probs = c(0.50))
age_95th_percentile <- quantile(wnba$Age, probs = c(0.95))

a <- quantile(wnba$Age, probs = c(0.50,0.75,0.95))


wnba_age_percentiles <- wnba %>%
  mutate(cume_dist_age = cume_dist(Age)) %>%
  select(Name,Age,cume_dist_age) %>% arrange((Age))
view(wnba_age_percentiles)

#Grouped Frequency distribution tables
wnba %>% group_by(Weight) %>% summarise(Freq = n())

wnba <- wnba %>% mutate(weight_categories = cut(Weight,breaks = 10, dig.lab = 4))
view(wnba)

wnba %>% 
  group_by(weight_categories) %>%
  summarise(Freq = n()) %>%
  drop_na()

wnba <- wnba %>% mutate(points_categories = cut(PTS,breaks = 10,dig.lab = 4))  
pts_freq_table <- wnba %>%
  group_by(PTS) %>% 
  summarise(Freq = n()) %>%
  drop_na()
pts_grouped_freq_table <- wnba %>%
  group_by(points_categories) %>%
  summarise(Freq = n()) %>%
  mutate(Percentage = Freq/ nrow(wnba) * 100) %>%
  arrange(desc(points_categories))

pts_freq_table


#Information loss
wnba %>% group_by(points_categories) %>% summarise(Freq = n())

wnba <- wnba %>% mutate(points_categories = cut(PTS,breaks = 5,dig.lab = 4))

pts_freq_table <- wnba %>%
  group_by(points_categories) %>% 
  summarise(Freq = n()) %>%
  drop_na()

pts_freq_table


#Readability for grouped Frequency Tables
wnba <- wnba %>% 
  mutate(points_categories = cut(PTS,breaks = c(0,100,200,300,400,500,600),dig.lab = 4))

wnba %>% group_by(points_categories)%>% 
  summarise(Freq = n()) %>%
  mutate(Percentage = Freq/nrow(wnba)* 100)

wnba <- wnba %>% 
  mutate(min_categories = cut(MIN, breaks = c(0,150,300,450,600,750,900,1050),dig.lab = 4))
min_grouped_freq_table <- wnba %>% group_by(min_categories)%>%
  summarise(Freq = n())%>%
  mutate(Percentage = Freq/nrow(wnba)*100) %>% arrange(min_categories)
min_grouped_freq_table

#BarCharts

ggplot(data = wnba, aes(x = Pos))+geom_bar()

ggplot(data = wnba, aes(x = Pos, fill = Pos))+geom_bar()+ theme(legend.position = "none")

#To generate a horizontal barchart
ggplot(data = wnba, aes(x = Pos, fill = Pos))+
  geom_bar()+ coord_flip()+
  theme(legend.position = "none")

wnba <- wnba_1 %>% mutate(Exp_ordinal = case_when(
  Experience == 0 ~ "Rookie",
  Experience > 0 & Experience <= 3 ~ "Little experience",
  Experience > 3 & Experience <= 5 ~ "Experienced",
  Experience > 5 & Experience <= 10 ~ "Very experienced",
  Experience > 10 ~ "Veteran"
  
))
wnba_1 <- wnba 
wnba_1$Experience = as.numeric(wnba_1$Experience)
wnba_1$Experience[is.na(wnba_1$Experience)] <- 0

wnba_1$Experience

view(wnba)


ggplot(data = wnba, aes(x = Exp_ordinal, fill = Exp_ordinal))+geom_bar()+ 
  theme(legend.position = "none")

ggplot(data = wnba, aes(x = Exp_ordinal, fill = Exp_ordinal))+geom_bar()+ coord_flip()+ 
  theme(legend.position = "none")

typeof(wnba$Experience)


#Proportions with bar charts
ggplot(data = wnba, aes(x = Pos, y = ..prop.. ,group = 1))+geom_bar()+theme(legend.position = "none")

ggplot(data = wnba, aes(x = Pos, y = ..prop..*100 ,group = 1))+
  geom_bar()+theme(legend.position = "none")+ 
  labs(x = "Position", y = "Percentage")

ggplot(data = wnba, aes(x = Pos, y = ..prop..*100 ,group = 1, fill = factor(..x..)))+
  geom_bar()+theme(legend.position = "none")+ 
  labs(x = "Position", y = "Percentage")

ggplot(data = wnba, aes(x = Exp_ordinal, y = ..prop..*100 ,group = 1, fill = factor(..x..)))+
  geom_bar()+theme(legend.position = "none")+ 
  labs(x = "Experience Level", y = "Percentage")

#Stacked Bar Charts
pos_prop <- wnba %>% 
  group_by(Pos) %>%
  summarise(Prop = n()/nrow(wnba))

pos_prop

ggplot(data = pos_prop, aes(x = "", y = Prop, fill = Pos))+geom_bar(stat = "identity")

ggplot(data =  pos_prop, aes(x = "", y = Prop, fill = Pos )) + 
  geom_bar(stat = "identity", width = 0.25) + coord_flip()


library(stringr)
ggplot(data = pos_prop,
       aes(x = '', y = Prop, fill = Pos))+
  geom_bar(stat = 'identity', width = 0.25)+
  coord_flip()+
  geom_text(aes(label = str_c(round(Prop * 100), '%')),position = position_stack(vjust = 0.5))+
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       title = 'Player Distribution by Position')+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

exp_prop<- wnba %>% group_by(Exp_ordinal)%>%
  summarise(Prop = n()/nrow(wnba))


ggplot(data = exp_prop, aes(x = '', y = Prop, fill = Exp_ordinal))+
  geom_bar(stat = "identity", width = 0.25)+
  coord_flip()+
  geom_text(aes(label = str_c(round(Prop * 100), '%')),position = position_stack(vjust = 0.5))+
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       title = 'Player Distribution by Experience Level')+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

#Pie Charts
ggplot(data = pos_prop, aes(x = '', y = Prop, fill = Pos))+
  geom_bar(stat = "identity")+coord_polar(theta = "y")


ggplot(data = exp_prop, aes(x = '', y = Prop, fill = Exp_ordinal))+
  geom_bar(stat = "identity", width = 0.25)+
  coord_polar(theta = "y")+
  geom_text(aes(label = str_c(round(Prop * 100), '%')),position = position_stack(vjust = 0.5))+
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       title = 'Player Distribution by Experience Level')+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

#Histogram
summary(wnba$PTS)
ggplot(data = wnba,aes(x = PTS))+geom_histogram()

ggplot(data = wnba, aes(x = Games_Played)) + geom_histogram()


#Binning Histogram
ggplot(data = wnba,aes(x = PTS))+geom_histogram(bins = 10)


ggplot(data = wnba, aes(x = Games_Played)) + geom_histogram(bins = 4)
ggplot(data = wnba, aes(x = Games_Played)) + geom_histogram(bins = 60)

#The statistics behind histogram
wnba %>% group_by(points_categories)%>%
  summarise(Freq = n())


ggplot(data = wnba,aes(x = PTS))+geom_histogram(bins = 6)

pts_binwidth <- (max(wnba$PTS) - min(wnba$PTS)) / 6

ggplot(data = wnba,aes(x = PTS))+geom_histogram(boundary = min(wnba$PTS),
                                                binwidth = pts_binwidth)

wnba <- wnba %>% 
  mutate(games_categories = cut(Games_Played,breaks = 10,dig.lab = 4))
wnba%>% 
  group_by(games_categories) %>%
  summarise(Freq = n())

games_played_binwidth <- (max(wnba$Games_Played) - min(wnba$Games_Played))/10
ggplot(data = wnba, aes(x = Games_Played))+
  geom_histogram(boundary = min(wnba$Games_Played),
                 binwidth = games_played_binwidth)

# Comparing Frequency Distributions
head(wnba)
view(wnba)

  pos_by_exp <- wnba %>% group_by(Exp_ordinal,Pos) %>% summarise(Freq = n())
  exp_by_pos
  
  exp_by_pos <- wnba %>% group_by(Pos,Exp_ordinal) %>% summarise(Freq = n())
  pos_by_exp

  
# Grouped by Charts
ggplot(data = wnba, aes(x = Exp_ordinal, fill = Pos))+
  geom_bar(position = "dodge")+labs(x = "Experience Level", y = "Frequency" )

ggplot(data = exp_by_pos, aes(x = Exp_ordinal, y = Freq, fill = Pos))+
  geom_bar(position = "dodge", stat = "identity")+labs(x = "Exprience Level", y = "Frequency")


ggplot(data = exp_by_pos, aes(x = Pos, y = Freq, fill = Exp_ordinal))+
  geom_bar(position = "dodge", stat = "identity")+labs(x = "Position", y = "Frequency")


ggplot(data = wnba, aes(x = Pos, fill = Exp_ordinal))+
  geom_bar(position = "dodge")+labs(x = "Position", y = "Frequency" )


#Challenge: Do Older Player play less


#on the average a wnba player plays approximately
mean(wnba$MIN)

mean(wnba$Age)

wnba <- wnba %>% 
  mutate(age_relative = if_else(Age >= 27, "old","young")) %>%
  mutate(age_relative = factor(age_relative ,levels = c("young","old")))

wnba <- wnba %>% 
  mutate(min_relative = 
           if_else(MIN >= 497, "average or above","below average")) %>%
  mutate(min_relative = factor(min_relative, levels = c("below average","average or above")))


view(wnba)

wnba %>% select(Name, Age, age_relative,MIN,min_relative)%>% head()

ggplot(data = wnba, aes(x = age_relative, fill = min_relative))+geom_bar(position = "dodge")

#Comparing Histogram
ggplot(data = wnba, aes(x = MIN, fill = age_relative ))+
  geom_histogram(bins = 10, position = "identity",alpha = 0.5)


#Visualizing Mean with Histogram
ggplot(data = wnba, aes(x = MIN, fill = age_relative ))+
  geom_histogram(bins = 10, position = "identity",alpha = 0.5)+
  geom_vline(aes(xintercept = mean(wnba$MIN), linetype = "Average Minutes"),color = 'black')

ggplot(data = wnba, aes(x = MIN, fill = age_relative ))+
  geom_histogram(bins = 10, position = "identity",alpha = 0.5)+
  geom_vline(aes(xintercept = mean(wnba$MIN), linetype = "Average Minutes"),color = 'black')+
  facet_wrap(~ age_relative)

ggplot(data = wnba, aes(x = PTS, fill = age_relative ))+
  geom_histogram(bins = 10, position = "identity",alpha = 0.5)+
  geom_vline(aes(xintercept = mean(wnba$PTS), linetype = "Average Points"),color = 'black')+
  facet_wrap(~ age_relative)

ggplot(data = wnba, aes(x = PTS, fill = age_relative ))+
  geom_histogram(bins = 10, position = "identity",alpha = 0.5)+
  geom_vline(aes(xintercept = mean(wnba$PTS), linetype = "Average Points"),color = 'black')


#Frequency Polygons and Kernel Density Estimate Plots
ggplot(data = wnba, aes(x = MIN, color = age_relative ))+
  geom_freqpoly(bins = 10, position = "identity")+
  geom_vline(aes(xintercept = mean(wnba$MIN), linetype = "Average Minutes"),color = 'black')


ggplot(data = wnba, 
       aes(x = MIN, color = age_relative))+geom_density()+
  geom_vline(aes(xintercept = mean(wnba$MIN), linetype = "Average Minutes"),color = 'black')

ggplot(data = wnba, 
       aes(x = PTS, color = age_relative))+geom_density()+
  geom_vline(aes(xintercept = mean(wnba$PTS), linetype = "Average Points"),color = 'black')

#Drawbacks of kernel density plots

ggplot(data = wnba, aes(x = Height, color = Pos))+geom_density()


#Scatter Plots
ggplot(data = wnba, aes(x = Pos, y = Height, color = Pos))+geom_point()

#jitter helps spread the points

ggplot(data = wnba, aes(x = Pos, y = Height, color = Pos))+geom_point()+geom_jitter()

ggplot(data = wnba, aes(x = Pos, y = Weight, color = Pos))+geom_point()+geom_jitter()

#Box Plots
guards <- wnba %>% filter(Pos == "G") 
summary(guards$Height)

ggplot(data = wnba, aes(x = Pos, y = Height, color = Pos))+geom_boxplot()

ggplot(data = wnba, aes(x = Pos, y = Weight, color = Pos))+geom_boxplot()


#Outliners
centers <- wnba %>% filter(Pos == "C")
summary(centers$Height)
ggplot(data = centers, aes(x = Pos, y = Height, color = Pos))+geom_boxplot()

ggplot(data = wnba,
       aes(x = Pos, y = Height, color = Pos))+
  geom_boxplot(coef = 4)

ggplot(data = wnba, aes(x = "", y = Games_Played, group = 1))+ geom_boxplot()

summary(wnba$Games_Played)
iqr <- 29.00 - 22.00
lower_bound <- 22.00 - (iqr * 1.5)
upper_bound <- 29.00 + (iqr * 1.5)
outliers_low <- 7
outliers_high <- 0



