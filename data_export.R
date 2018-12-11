library(tidyverse)
library(ggplot2)
library(dplyr)
library(usmap)

alco_df <- read.csv(file="2015.csv", header=TRUE)
alco_df = sample_n(alco_df, 10000, replace=FALSE, weight=NULL, .env = NULL)
variabls <- c("X_STATE","SEX","MAXDRNKS","X_AGEG5YR", "X_AGE65YR", "X_AGE80", "AVEDRNK2","X_AGE_G","DRNKANY5", "DROCDY3_", "X_RFBING5", "X_DRNKWEK", "X_RFDRHV5","TOLDHI2" )
alco_df <- alco_df[variabls]
alco_df$X_RFDRHV5[alco_df$X_RFDRHV5 == 1] <- 'No'
alco_df$X_RFDRHV5[alco_df$X_RFDRHV5 == 2] <- 'Yes'
alco_df$X_RFDRHV5[alco_df$X_RFDRHV5 == 9] <- 'Unknown'
alco_df$TOLDHI2[alco_df$TOLDHI2 == 1] <- 'Yes'
alco_df$TOLDHI2[alco_df$TOLDHI2 == 2] <- 'No'
alco_df$TOLDHI2[alco_df$TOLDHI2 == 7] <- 'Unknown'
alco_df$TOLDHI2[alco_df$TOLDHI2 == 9] <- 'Refused'

#number of people in each state
state_date <- alco_df%>% group_by(X_STATE)%>% 
  summarize(total_state = n())


#number of heavy drinkers in each state
percent_drinker_state <- alco_df[alco_df$X_RFDRHV5 =='Yes',] %>% group_by(X_STATE) %>% 
  summarize(n_heavy_drinkers = n())
percent_drinker_state

#number of high chlostoral in each state
percent_chlos_state <- alco_df[alco_df$TOLDHI2 =='Yes',] %>% group_by(X_STATE) %>% 
  summarize(n_high_chlos = n())
percent_chlos_state

#combine the num of drinkers and number people in each state 
join_table <- percent_drinker_state  %>% left_join(state_date,by=c("X_STATE" = "X_STATE") ) 
join_table$heavy_drinker_percent<- 100*(percent_drinker_state$n_heavy_drinkers /join_table$total_state)
View(join_table)

#combine the num of high chlostoral and number people in each state 
join_table2<- percent_chlos_state %>% left_join(state_date,by=c("X_STATE" = "X_STATE") ) 
join_table2$high_chlos_percent<- 100*(percent_chlos_state$n_high_chlos /join_table2$total_state)
join_table2<-join_table2 %>% drop_na(high_chlos_percent)
View(join_table2)

join_table$high_chlo_percent<-join_table2$high_chlos_percent

View(join_table)

statepop$fips <- as.integer(statepop$fips)
overall_df<- join_table %>% left_join(statepop,by=c("X_STATE" = "fips") ) 
colnames(overall_df)[colnames(overall_df) == 'full'] <- 'state'
overall_df


write.csv(overall_df,'heavy_drinker.csv')

new_df<- read.csv(file="heavy_drinker.csv", header=TRUE)
fruit_df <- read.csv(file="Fruit_hater.csv",header=TRUE )
smoke_df <- read.csv(file="heavy_smoker.csv",header=TRUE )
new_df1 <- new_df%>% left_join(smoke_df,by=c("X_STATE" = "X_STATE") ) 
new_df2 <- new_df1%>% left_join(fruit_df,by=c("X_STATE" = "X_STATE") ) 

write.csv(new_df2,'new_df.csv')
