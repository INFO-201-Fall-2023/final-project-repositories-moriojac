library(dplyr)
library(stringr)

df_1 <- read.csv("injuries_2010-2020.csv") 
df_2 <- read.csv("Seasons_Stats.csv") 

inj_df <- df_1
sts_df <- df_2

#Filtering Stats Dataframe to include data from 2010-2017
sts_df <- filter(sts_df, sts_df$Year >= 2010)

#Filtering injury Dataframe to inlcude data from only 2010-2017
inj_df <- filter(inj_df, inj_df$Date <= 2018)

#Combining "Relinquished" and "Acquired" columns to a new column since we only need the players name.
missing_relinquished <- inj_df$Acquired[which(inj_df$Relinquished == "")]
inj_df$Relinquished[which(inj_df$Relinquished == "")] <- missing_relinquished
colnames(inj_df)[colnames(inj_df) == "Relinquished"] = "Player"

#Removing duplicate columns/culumns we dont need
inj_df$Acquired <- NULL
sts_df$X <- NULL

#Changing the inj_df "Team" column to the teams abbriviation to match sts_df

team_abv <- c("ATL", "NJN", "BRK", "BOS", "CHA", "CHO", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NOH", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
team_name <- c("Hawks", "Nets", "Nets", "Celtics", "Hornets", "Hornets", "Bulls", "Cavaliers", "Mavericks", "Nuggets", "Pistons", "Warriors", "Rockets", "Pacers", "Clippers", "Lakers", "Grizzlies", "Heat", "Bucks", "Timberwolves", "Pelicans", "Pelicans", "Knicks", "Thunder", "Magic", "76ers", "Suns", "Blazers", "Kings", "Spurs", "Raptors", "Jazz", "Wizards")
teams <- sts_df$Tm
teams <- team_name[match(teams, team_abv)]
sts_df$Tm <- teams

#Changing inj_df date column to only hold the year to match sts_df
inj_yr <- substr(inj_df$Date, start = 1, stop = 4)
inj_yr <- as.integer(inj_yr)
inj_df$Date <- inj_yr

#Joining dataframes
df <- merge(inj_df, sts_df, by.x = c("Date", "Player", "Team"), by.y = c("Year", "Player", "Tm"))

#Cleaning and organizing Dataframe even more
colnames(df)[colnames(df) == "Date"] = "Year"
df$blanl <- NULL
df$blank2 <- NULL

#Creating a collumn that gives unique values to each row
num <- 0
num_vec <- c()
for(i in 1:nrow(df)){
num <- num + 1
num_vec <- append(num_vec, num)
}
df <- cbind(num_vec, df)
colnames(df)[colnames(df) == "num_vec"] = "NUM"

#Creating a column that includes the amount of injuries a player had in a certain year
find_injuries_in_yr <- function(nm, yr){
total_inj <- sum(df$Player == nm & df$Year == yr)
return(total_inj)
}

inj_in_yr <-c()
for(i in 1:nrow(df)){
  inj_in_yr <- append(inj_in_yr, find_injuries_in_yr(df$Player[i], df$Year[i]))
}
df$INJ_IN_YR <- inj_in_yr

#Creating a column that includes the total amount of injuries for a player from 2010-2017
find_total_inj <- function(nm){
tot <- sum(df$Player == nm)
return(tot)
}

tot_inj <-c()
for(i in 1:nrow(df)){
tot_inj <- append(tot_inj, find_total_inj(df$Player[i]))
}
df$TOT_INJ <- tot_inj

#Creating a column that includes the ratio of usage rate to amount of injuries in that year
find_inj_to_usg <- function(num){
usg <- df$USG.[which(df$NUM == num)]
inj <- df$INJ_IN_YR[which(df$NUM == num)]
ratio <- round(inj / usg * 100, 3)
return(ratio)
}

inj_to_usg <- c()
for(i in 1:nrow(df)){
inj_to_usg <- append(inj_to_usg, find_inj_to_usg(df$NUM[i]))
}
df$INJ_TO_USG <- inj_to_usg

#Creating a column that includes minutes per game
find_min_per <- function(num){
gp <- df$G[which(df$NUM == num)]
mins <- df$MP[which(df$NUM == num)]
mpg <- round(mins / gp, 2)
return(mpg)
}

mins_per_game <- c()
for(i in 1:nrow(df)){
mins_per_game <- append(mins_per_game, find_min_per(df$NUM[i]))
}
df$MPG <- mins_per_game

#Creating a column that includes the ratio of minutes played per game to amount of injuries in that year
find_inj_to_min <- function(num){
min <- df$MPG[which(df$NUM == num)]
inj <- df$INJ_IN_YR[which(df$NUM == num)]
ratio <- round(inj / min, 2)
return(ratio)
}

inj_to_min <- c()
for(i in 1:nrow(df)){
inj_to_min <- append(inj_to_min, find_inj_to_min(df$NUM[i]))
}
df$INJ_TO_MIN <- inj_to_min



#Creating a summary Dataframe of the new columns created
df_summary <- select(df, "Year", "Player", "Pos", "Notes", "Age", "TOT_INJ", "INJ_IN_YR", "MP", "MPG", "USG.", "INJ_TO_MIN", "INJ_TO_USG")
df_summary_copy <- df_summary
df_summary_copy$Notes <- NULL
#Creating another summary Dataframe that includes each player only once and amount of injuries they had from 2010-2017
df_summary2 <- select(df, "Player", "Pos", "Age",  "TOT_INJ",)
df_summary2 <- df_summary2 %>% distinct()

df_summary3 <- df_summary_copy %>% distinct()

df_year <- group_by(df_summary3, Year)
df_year <- summarize(df_year, total = sum(INJ_IN_YR))

df_age <- group_by(df, Age)
df_age <- summarize(df_age, mean_inj = mean(INJ_IN_YR))

df_position <- group_by(df, Pos)
df_position <- summarize(df_position, mean_inj = mean(INJ_IN_YR), inj_tot = sum(INJ_IN_YR))

df_mpg <- group_by(df, MPG)
df_mpg <- summarize(df_mpg, mean_inj = mean(INJ_IN_YR))

find_mpg_grp <- function(min){
  if(min > 0 & min < 10){
  return("0-9 MPG")
  }else if(min >= 10 & min < 20){
    return("10-19 MPG")
  }else{
    return("20+ MPG")
  }
}


group <-c()
for(i in 1:nrow(df_mpg)){
  group <- append(group, find_mpg_grp(df_mpg$MPG[i]))
}
df_mpg$GROUP <- group
df_mpg$num <- 1

df_mpg <- group_by(df_mpg, GROUP)
df_mpg <- summarize(df_mpg, tot_inj = sum(num))


df_pts <- subset(df, df$PER > 0)

find_ppg_grp <- function(pts){
  if(pts > 0 & pts < 4){
    return("0-4 PPG")
  }else if(pts >= 5 & pts < 10){
    return("5-9 PPG")
  }else if(pts >= 10 & pts < 15){
    return("10-14 PPG")
  }else{
    return("15+ PPG")
  }
}

group2 <-c()
for(i in 1:nrow(df_pts)){
  group2 <- append(group2, find_ppg_grp(df_pts$PER[i]))
}
df_pts$GROUP <- group2
df_pts$num <- 1

df_pts <- group_by(df_pts, GROUP)
df_pts <- summarize(df_pts, tot_inj = sum(num))

x <- c("0-4 PPG", "5-9 PPG", "10-14 PPG", "15+ PPG")
df_pts %>% arrange(tot_inj) 

total_injuries <- sum(df_summary2$TOT_INJ)
age19_to_29 <- sum(df_summary2$TOT_INJ[which(df_summary2$Age >= 18 & df_summary2$Age <= 29)])
age19_to_29_total <- sum(df_summary2$Age >= 18 & df_summary2$Age <= 29)
age30_to_40 <- sum(df_summary2$TOT_INJ[which(df_summary2$Age >= 30)])
age30_to_40_total <- sum(df_summary2$Age >= 30)
                                     
young_ratio <- age19_to_29 / age19_to_29_total
old_ratio <- age30_to_40 / age30_to_40_total
