llll
---
title: "cyclistic_capstone1"
author: "Krithajna J"
date: '2022-04-20'
output: html_document
---
#                                                     Loading packages 
```{r}
library(tidyverse) # for data import and wrangling
library(janitor)   # for cleaning empty rows and columns
library(dplyr)     # for visualization
library(readr)     # to read files
```
#                                         Loading all the csv files of last 12 months 
```{r}
df1 <- read_csv("jan.csv")
df1 <- remove_empty(df1, which=c("cols")) # to have consistent column so removing empty columns
df2 <- read.csv("feb.csv")
df3 <- read.csv("mar.csv")
df3 <- remove_empty(df3, which=c("cols"))
df4 <- read.csv("apr.csv") 
df5 <- read.csv("may.csv")
df6 <- read.csv("jun.csv")
df7 <- read.csv("july.csv")
df7 <- remove_empty(df7, which=c("cols"))
df8 <- read.csv("aug.csv")
df9 <- read.csv("sep.csv")
df10 <- read.csv("oct.csv")
df11 <- read.csv("nov.csv")
df12 <- read.csv("dec.csv")
df12 <- remove_empty(df12, which=c("cols"))
```

#                                                         Data wrangling 

### 1) Concatenating all the CSV files into a single data frame
```{r}
binded_df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
```
### 2) Removing any empty rows or columns present 
```{r}
binded_df <- remove_empty(binded_df, which=c("cols"))
binded_df <- remove_empty(binded_df, which=c("rows"))  
```
### 3) Omitting NA values in the entire data frame 
```{r}
binded_df <- na.omit(binded_df) 
```
### 4) Dimensions and summary of new data frame
```{r echo=TRUE}
dim(binded_df) 
glimpse(binded_df)
summary(binded_df)
```
### 5) Removing duplicates 
```{r echo=TRUE}
print(nrow(binded_df))
binded_df<- binded_df[!duplicated(binded_df$ride_id), ]
print(nrow(binded_df)) 
```
### 6) Format for date time format 
```{r}
binded_df$started_at <- lubridate::dmy_hm(binded_df$started_at)
binded_df$ended_at <- lubridate::dmy_hm(binded_df$ended_at)
```
### 7) Rename column member_casual to usertype and deleting start_lat,start_lng,end_lat,end_lng columns 
```{r}
binded_df <- binded_df %>% 
  rename(usertype=member_casual) %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))
```
### 8) Reassign member as subscriber and casual as customer in usertype column 
```{r echo=TRUE}
binded_df <- binded_df %>% 
  mutate(usertype = recode(usertype,"member" = "subscriber"))
table(binded_df$usertype)
```
### 9) Add new columns month,year,weekday from startd_at column 
```{r echo=TRUE}
binded_df$month <- format(as.Date(binded_df$started_at), "%B")
table(binded_df$month)
binded_df$year <- format(as.Date(binded_df$started_at), "%Y")
table(binded_df$year)
binded_df$day_of_week <- format(as.Date(binded_df$started_at), "%A")
table(binded_df$day_of_week)
table(binded_df$rideable_type)
```
### 10) Add new column as ride_length in sec 
```{r}
binded_df$ride_length <- difftime(binded_df$ended_at,binded_df$started_at)
binded_df <- binded_df %>% 
  mutate(ride_length_hour=as.numeric((ride_length/60),format = "%h")) %>% 
  select(-c(ride_length))
clean_df <- binded_df
```

#                                        Data Analysis 

### 1) Cleaning negative value in ride_length 
```{r echo=TRUE}
table(clean_df$ride_length_hour<0)
filter(clean_df, ride_length_hour<0)
clean_df <- clean_df[!(clean_df$ride_length_hour<0),] # removing negative values
```
### 2) Inspect the table values 
```{r echo=TRUE}
mean(clean_df$ride_length_hour) 
median(clean_df$ride_length_hour) #midpoint number in the ascending array of ride lengths
max(clean_df$ride_length_hour) 
max(clean_df$ride_length_hour)
filter(clean_df, ride_length_hour==55944) # maximum user is casual user
min(clean_df$ride_length_hour) 
clean_df <-  mutate(clean_df, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type)) 
```
### 3) Set the days of the week and year in order.
```{r}
clean_df$day_of_week <- ordered(clean_df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
clean_df$month <- ordered(clean_df$month, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
```
### 4) Inspect the table details
```{r echo=TRUE}
colnames(clean_df)  #List of column names
nrow(clean_df)  #How many rows are in data frame?
dim(clean_df)  #Dimensions of the data frame?
head(clean_df)  #See the first 6 rows of data frame
str(clean_df)  #See list of columns and data types (numeric, character, etc)
summary(clean_df)  #Statistical summary of data. Mainly for numeric
clean_df1 <- clean_df
clean_df1$ride_length_hour <- as.numeric(as.character(clean_df1$ride_length_hour))
```
### 5) Compare subscriber and casual users 
```{r echo=TRUE}
aggregate(clean_df1$ride_length_hour ~ clean_df1$usertype, FUN = mean)
aggregate(clean_df1$ride_length_hour~ clean_df1$usertype, FUN = median)
aggregate(clean_df1$ride_length_hour ~ clean_df1$usertype, FUN = max)
aggregate(clean_df1$ride_length_hour ~ clean_df1$usertype, FUN = min)
```
### 6) See the average ride length by each day for subscriber vs casual users 
```{r echo=TRUE}
aggregate(clean_df1$ride_length_hour ~ clean_df1$usertype + clean_df1$day_of_week, FUN = mean)
table(clean_df1$day_of_week)
```
### 7) The average ride length by each day for subscriber vs casual users 
```{r echo=TRUE}
aggregate(clean_df1$ride_length_hour ~ clean_df1$usertype + clean_df1$day_of_week, FUN = mean)
```
### 8) Analyze ridership data by type and weekday
```{r echo=TRUE}
clean_df1 %>% 
  group_by(usertype,day_of_week) %>%                                             
  summarise(number_of_rides = n(),average_duration = mean(ride_length_hour)) %>% #calculates the number of rides and average duration and calculates the average duration
  arrange(usertype,day_of_week)		
```

#                                                      Data visualization 

### 1) Visualize the number of rides by user type in day of the week 
```{r echo=TRUE}
df <- clean_df1 
df %>% 
  group_by(usertype,day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_hour)) %>% 
  arrange(usertype,day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")
```

### 2) Visualize the number of rides by user type in month 
```{r echo=TRUE}
df <- clean_df1 
df %>% 
  group_by(usertype,month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_hour)) %>% 
  arrange(usertype,month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) # avoid overlaping titles in x axis
```

### 3) Let's create a visualization for average duration vs day of week  
```{r echo=TRUE}
df %>% 
  group_by(usertype,day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_hour)) %>% 
  arrange(usertype,day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")
```

### 4) Let's create a visualization for average duration vs month 
```{r echo=TRUE}
df %>% 
  group_by(usertype,month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_hour)) %>% 
  arrange(usertype,month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")+theme(axis.text.x = element_text(angle = 45, hjust=1))
```

### 5) EXPORT SUMMARY FILE FOR FURTHER ANALYSIS 
```{r}
counts <- aggregate(df$ride_length_hour ~ df$usertype + df$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/DELL/Desktop/excel_project/avg_ride_length.csv')
```
