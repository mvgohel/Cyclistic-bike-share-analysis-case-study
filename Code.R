# Project
require(tidyverse)
require(lubridate)
require(dplyr)
require(here)
require(skimr)
require(janitor)
require(hablar)
require(gapminder)
library(scales)
library(ggrepel)
options(warn=-1)

# Step 2 : Load the data

divvy_tips_2019_Q2<-read.csv("Divvy_Trips_2019_Q2.csv")
divvy_tips_2019_Q3<-read.csv("Divvy_Trips_2019_Q3.csv")
divvy_tips_2019_Q4<-read.csv("Divvy_Trips_2019_Q4.csv")
divvy_tips_2020_Q1<-read.csv("Divvy_Trips_2020_Q1.csv")

# Step 3: Data Format

colnames(divvy_tips_2019_Q2)
colnames(divvy_tips_2019_Q3)
colnames(divvy_tips_2019_Q4)
colnames(divvy_tips_2020_Q1)


# Column names and col position need to be uniform across the file before joining them
# change the column name in the divvy_tips_2019_Q2 and divvy_tips_2020_Q1

divvy_tips_2019_Q2<-divvy_tips_2019_Q2 %>% 
  rename(Rental_ID=X01...Rental.Details.Rental.ID,
         From_Start_Date_Time=X01...Rental.Details.Local.Start.Time,
         To_End_Date_Time=X01...Rental.Details.Local.End.Time,
         Rental_Bike_ID=X01...Rental.Details.Bike.ID,
         Rental_Duration_Second=X01...Rental.Details.Duration.In.Seconds.Uncapped,
         Start_Station_ID=X03...Rental.Start.Station.ID,
         Start_Station_Name=X03...Rental.Start.Station.Name,
         End_Station_ID=X02...Rental.End.Station.ID,
         End_Station_Name=X02...Rental.End.Station.Name,
         User_Type=User.Type,
         Gender=Member.Gender,
         Birth_Year=X05...Member.Details.Member.Birthday.Year)

divvy_tips_2019_Q3<-divvy_tips_2019_Q3 %>% 
  rename(Rental_ID=trip_id,
         From_Start_Date_Time=start_time,
         To_End_Date_Time=end_time,
         Rental_Bike_ID=bikeid,
         Rental_Duration_Second=tripduration,
         Start_Station_ID=from_station_id,
         Start_Station_Name=from_station_name,
         End_Station_ID=to_station_id,
         End_Station_Name=to_station_name,
         User_Type=usertype,
         Gender=gender,
         Birth_Year=birthyear)

divvy_tips_2019_Q4<-divvy_tips_2019_Q4 %>% 
  rename(Rental_ID=trip_id,
         From_Start_Date_Time=start_time,
         To_End_Date_Time=end_time,
         Rental_Bike_ID=bikeid,
         Rental_Duration_Second=tripduration,
         Start_Station_ID=from_station_id,
         Start_Station_Name=from_station_name,
         End_Station_ID=to_station_id,
         End_Station_Name=to_station_name,
         User_Type=usertype,
         Gender=gender,
         Birth_Year=birthyear)

divvy_tips_2020_Q1<-divvy_tips_2020_Q1 %>% 
  rename(Rental_ID=ride_id,
         From_Start_Date_Time=started_at,
         To_End_Date_Time=ended_at,
         Start_Station_Name=start_station_name,
         Start_Station_ID=start_station_id,
         End_Station_Name=end_station_name,
         End_Station_ID=end_station_id,
         User_Type=member_casual)

# Remove the columns that are not required

divvy_tips_2019_Q2<-divvy_tips_2019_Q2 %>% 
  select(-c(Rental_ID,Rental_Bike_ID,Rental_Duration_Second,
            Gender, Birth_Year))

divvy_tips_2019_Q3<-divvy_tips_2019_Q3 %>% 
  select(-c(Rental_ID,Rental_Bike_ID,Rental_Duration_Second,
            Gender,Birth_Year))

divvy_tips_2019_Q4<-divvy_tips_2019_Q4 %>% 
  select(-c(Rental_ID,Rental_Bike_ID,Rental_Duration_Second,
            Gender,Birth_Year))

divvy_tips_2020_Q1<-divvy_tips_2020_Q1 %>% 
  select(c(From_Start_Date_Time,To_End_Date_Time,
           Start_Station_ID,Start_Station_Name,End_Station_ID,
           End_Station_Name,User_Type))

# Data structure of the dataframe

str(divvy_tips_2019_Q2)
str(divvy_tips_2019_Q3)
str(divvy_tips_2019_Q4)
str(divvy_tips_2020_Q1)


# Convert the "Rental_ID from int to Chr

# divvy_tips_2019_Q2<-divvy_tips_2019_Q2 %>% convert(chr(Rental_ID))

# divvy_tips_2019_Q3<-divvy_tips_2019_Q3 %>%  convert(chr(Rental_ID))

# divvy_tips_2019_Q4<-divvy_tips_2019_Q4 %>%  convert(chr(Rental_ID))

# Check number of Rows and columns

nrow(divvy_tips_2019_Q2)
nrow(divvy_tips_2019_Q3)
nrow(divvy_tips_2019_Q4)
nrow(divvy_tips_2020_Q1)

ncol(divvy_tips_2019_Q2)
ncol(divvy_tips_2019_Q3)
ncol(divvy_tips_2019_Q4)
ncol(divvy_tips_2020_Q1)


# divvy_tips_2019_Q2 %>% distinct(User_Type)
unique(divvy_tips_2019_Q2$User_Type)
unique(divvy_tips_2019_Q3$User_Type)
unique(divvy_tips_2019_Q4$User_Type)
unique(divvy_tips_2020_Q1$User_Type)


# The membership_type should be uniform 
# Customer and or Casual = Not Member
# Subscriber/member = Member

divvy_tips_2019_Q2$User_Type<-recode(divvy_tips_2019_Q2$User_Type,
                                     "Subscriber" ="Member",
                                     "Customer" ="Not Member")

divvy_tips_2019_Q3$User_Type<-recode(divvy_tips_2019_Q3$User_Type,
                                     "Subscriber" ="Member",
                                     "Customer" ="Not Member")

divvy_tips_2019_Q4$User_Type<-recode(divvy_tips_2019_Q4$User_Type,
                                     "Subscriber" ="Member",
                                     "Customer" ="Not Member")

divvy_tips_2020_Q1$User_Type<-recode(divvy_tips_2020_Q1$User_Type,
                                     "member" ="Member",
                                     "casual" ="Not Member")

#Combined data

divvy_tips_2019_2020<-bind_rows(divvy_tips_2019_Q2,divvy_tips_2019_Q3,
                                divvy_tips_2019_Q4,divvy_tips_2020_Q1)

# Date formating from chr to Date and time
divvy_tips_2019_2020$From_Start_Date_Time<-ymd_hms(divvy_tips_2019_2020$From_Start_Date_Time)
divvy_tips_2019_2020$To_End_Date_Time<-ymd_hms(divvy_tips_2019_2020$To_End_Date_Time)


str(divvy_tips_2019_2020)
nrow(divvy_tips_2019_2020)
ncol(divvy_tips_2019_2020)
str(divvy_tips_2019_2020)
glimpse(divvy_tips_2019_2020)

# write.csv(divvy_tips_2019_Q2,"divvy_tips_2019_Q2G.csv",row.names = FALSE)
# write.csv(divvy_tips_2019_Q3,"divvy_tips_2019_Q3G.csv",row.names = FALSE)
# write.csv(divvy_tips_2019_Q4,"divvy_tips_2019_Q4G.csv",row.names = FALSE)
# write.csv(divvy_tips_2020_Q1,"divvy_tips_2020_Q1G.csv",row.names = FALSE)
# write.csv(divvy_tips_2019_2020,"divvy_tips_2019_2020.csv",row.names = FALSE)

# Create new columns 
divvy_tips_2019_2020$Ride_Length_min<-
as.numeric(as.character(difftime(divvy_tips_2019_2020$To_End_Date_Time,divvy_tips_2019_2020$From_Start_Date_Time)/60))

divvy_tips_2019_2020<-filter(divvy_tips_2019_2020, Ride_Length_min>0)

summary(divvy_tips_2019_2020)

require(ggplot2)

ggplot(data=divvy_tips_2019_2020)+ 
  geom_boxplot(mapping = aes(x=User_Type,y=Ride_Length_min))+
  scale_y_continuous(label = scales::comma)+
  labs(title = "Data Distribution",
       subtitle= "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x = "Member Type",
       y ="Ride Length in Minutes")+
  theme(plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"))

# Add Column Day of Week, Day Name, Month Number and Month Name

divvy_tips_2019_2020<- divvy_tips_2019_2020 %>%
  mutate(Day_of_Week = wday(From_Start_Date_Time), 
         Day_Name = wday(From_Start_Date_Time,label = TRUE, abbr = FALSE),
         Month_Number=month(From_Start_Date_Time),
         Month_Name =months(From_Start_Date_Time),
         quarter_Number = quarters(From_Start_Date_Time),
         Year = year(From_Start_Date_Time),
         Hour= hour(From_Start_Date_Time))
  

# To detect outliers

unique(divvy_tips_2019_2020$User_Type)

Q1<-divvy_tips_2019_2020 %>% group_by(User_Type) %>% 
  summarize(Q1= quantile(Ride_Length_min,probs=0.25))

View(Q1)

Q3<-divvy_tips_2019_2020 %>% group_by(User_Type) %>% 
  summarize(Q3= quantile(Ride_Length_min,probs=0.75))

View(Q3)

IQR<-divvy_tips_2019_2020 %>% group_by(User_Type) %>% 
  summarize(IQR= quantile(Ride_Length_min,probs=0.75)-quantile(Ride_Length_min,probs=0.25))

View(IQR)

Lower_Bound<-divvy_tips_2019_2020 %>% group_by(User_Type) %>% 
  summarize(Lower_Bound= quantile(Ride_Length_min,probs=0.25)-
              1.5*(quantile(Ride_Length_min,probs=0.75)-quantile(Ride_Length_min,probs=0.25)))

View(Lower_Bound)

Upper_Bound<-divvy_tips_2019_2020 %>% group_by(User_Type) %>% 
  summarize(Upper_Bound= quantile(Ride_Length_min,probs=0.75)+
              1.5*(quantile(Ride_Length_min,probs=0.75)-quantile(Ride_Length_min,probs=0.25)))

View(Upper_Bound)

Outlier_prm<-cbind(Lower_Bound,Upper_Bound)
Outlier_prm<-Outlier_prm %>% select(-3)

View(Outlier_prm)

divvy_tips_2019_2020<- merge(divvy_tips_2019_2020,Outlier_prm, by ="User_Type", all=TRUE)

View(divvy_tips_2019_2020)


glimpse(divvy_tips_2019_2020)

# Developed column True and False
divvy_tips_2019_2020$Outliers <- 
  divvy_tips_2019_2020$Ride_Length_min<= divvy_tips_2019_2020$Lower_Bound | divvy_tips_2019_2020$Ride_Length_min>= divvy_tips_2019_2020$Upper_Bound
  
# divvy_tips_2019_2020$Outliers<-as.numeric(divvy_tips_2019_2020$Outliers)

divvy_tips_2019_2020$Outliers[divvy_tips_2019_2020$Outliers== TRUE]<- "Yes"
divvy_tips_2019_2020$Outliers[divvy_tips_2019_2020$Outliers== FALSE]<- "No"

unique(divvy_tips_2019_2020$Outliers)

table(divvy_tips_2019_2020$Outliers) 
length(divvy_tips_2019_2020$Outliers)

sum(divvy_tips_2019_2020$Outliers=="Yes")
sum(divvy_tips_2019_2020$Outliers=="No")

sum(divvy_tips_2019_2020$Outliers=="Yes" | divvy_tips_2019_2020$Outliers=="No")

#outliers % within member and non member group 

divvy_tips_2019_2020 %>% 
  count(User_Type,Outliers) %>% 
  group_by(User_Type) %>% 
  mutate(percnt=prop.table(n)*100) %>% 
  ggplot(mapping=aes(User_Type, percnt, fill=Outliers,
                     label = paste0(round(percnt,2), "%")))+
  geom_bar(stat="identity")+
  geom_text(position = position_stack(vjust = .5))+
  labs(title = "Percent Outliers observed",
       subtitle= "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x = "Member Type",
       y ="Number of Outliers (%)")+
  theme(legend.title=element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"))


#write.csv(divvy_tips_2019_2020,"divvy_tips_2019_2020.csv",row.names = FALSE)

divvy_tips_2019_2020<-filter(divvy_tips_2019_2020, Outliers =="No")


#ggplot(data=divvy_tips_2019_2020, mapping = aes(x=User_Type,y=Ride_Length_min))+ 
#  geom_boxplot()+ stat_summary(fun=mean, geom = "point", col="black")
  
ggplot(data=divvy_tips_2019_2020,
       mapping = aes(x=Day_Name,y=Ride_Length_min, fill=User_Type))+ 
  geom_boxplot(outlier.shape = NA)+ 
  stat_summary(fun=mean, geom = "point", size=2,
               position=position_dodge(width=0.75), color="black")+
  stat_summary(fun = mean, geom = "text", col = "black",    
               vjust = -3.5, 
               aes(label = paste("Mean-", round(..y.., digits = 2))))+
  labs(title = "Trip Length in Weekdays",
       subtitle= "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x = "Weekdays",
       y ="Ride Length in Min",
       fill="User Type")+
  theme(legend.title=element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
       plot.subtitle = element_text(size=9),
       plot.caption = element_text(size=7),
       axis.title.x = element_text(size=9, face="bold"),
       axis.title.y= element_text(size=9, face="bold"))


# divvy_tips_2019_2020 %>% group_by(Hour) %>%summarize(appearances = n()) 

# number of trips in 24hours
divvy_tips_2019_2020 %>% group_by(Hour,User_Type) %>%
  summarise(Total_Rides = n()) %>% 
  ggplot(mapping = aes(x=Hour, y=Total_Rides, 
                       group=User_Type, color=User_Type))+
  geom_line()+ scale_y_continuous(label = scales::comma)+
  scale_color_manual(values=c("blue", "orangered"))+
  scale_size_manual(values=c(1.5, 1.5))+
  labs(title = "Number of Trips in 24 hours",
       subtitle= "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x = "Hour",
       y ="Number of Trips",
       color="User Type")+ 
  theme(legend.position=c(0.88,0.82),
        legend.title=element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"))
  
  
# Total Trips in Minutes in 24 hours

divvy_tips_2019_2020 %>% group_by(Hour, User_Type) %>% 
  summarise(Total_Trip_Minutes =sum(Ride_Length_min)) %>% 
  ggplot(mapping = aes(x=Hour, y=Total_Trip_Minutes,
                       group=User_Type, color=User_Type))+
  geom_line()+ 
  scale_y_continuous(label = scales::comma)+
  scale_color_manual(values = c("blue","orangered"))+
  scale_size_manual(values=c(1.5, 1.5))+
  labs(title="Total Trips in Minutes in 24 hours",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Hour",
       y ="Total Trips in Minutes",
       color="User Type")+
  theme(legend.position = c(0.88,0.82),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"))
  
  
# Mean Trip Minutes in 24 hours


divvy_tips_2019_2020 %>% group_by(Hour, User_Type) %>% 
  summarise(AVG_Trip_Minutes =mean(Ride_Length_min)) %>% 
  ggplot(mapping = aes(x=Hour, y=AVG_Trip_Minutes,
                       group=User_Type, color=User_Type))+
  geom_line()+ scale_color_manual(values = c("blue","orangered"))+
  scale_size_manual(values=c(1.5, 1.5))+
  labs(title="Mean Trip Minutes in 24 hours",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Hour",
       y ="AVG  Trips in Minutes",
       color="User Type")+
   theme(legend.position = c(0.89,0.42),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"))
  
# Number of Trips in Weekdays

divvy_tips_2019_2020 %>% group_by(Day_Name,User_Type) %>% 
  summarise(Total_rides_wk=n()) %>% 
  ggplot(mapping = aes(x=Day_Name, y=Total_rides_wk,
                       fill=User_Type))+
  geom_bar(stat = "identity")+ 
  scale_y_continuous(label = scales::comma)+
  scale_fill_manual(values = c("blue","orangered"))+
  geom_text(aes(label = paste0(comma(round(Total_rides_wk,2)))),
            position = position_stack(vjust = .5),
            size=2,color="white", fontface="bold")+
  labs(title="Number of Trips in Weekdays",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Hour",
       y ="Number of Trips",
       fill="User Type")+
  theme(legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=7,face="bold"))

# Total Trips Minutes in Weekdays

divvy_tips_2019_2020 %>% group_by(Day_Name, User_Type) %>% 
  summarise(Total_trips_min_wkd =sum(Ride_Length_min)) %>% 
  ggplot(mapping=aes(x=Day_Name,y=Total_trips_min_wkd, fill=User_Type))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::comma)+
  scale_fill_manual(values=c("blue","orangered"))+
  geom_text(aes(label = paste0(comma(round(Total_trips_min_wkd ,2)))),
          position = position_stack(vjust = .5),
          size=2,color="white",fontface="bold")+
  labs(title="Total Trips Minutes in Weekdays",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Weekdays",
       y ="Total Trips in Minutes",
       fill="User Type")+
  theme(legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=7,face="bold"))


# Mean Trip Minutes in Weekdays

divvy_tips_2019_2020 %>% group_by(Day_Name,User_Type) %>% 
  summarise(AVG_Trip_Min_wkd=mean(Ride_Length_min)) %>% 
  ggplot(mapping = aes(x=Day_Name, y=AVG_Trip_Min_wkd,
                       fill=User_Type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("blue","orangered"))+
  geom_text(aes(label= paste0(round(AVG_Trip_Min_wkd,2))),
            position = position_stack(vjust = 0.5),
            size=3,color="white")+
  labs(title="Mean Trip Minutes in Weekdays",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Weekdays",
       y ="AVG Trip in Minutes",
       fill="User Type")+
  theme(legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=7,face="bold"))

# Number of Trips in Year-Month-Quarter

divvy_tips_2019_2020$Year_Month<-divvy_tips_2019_2020$Year*100+divvy_tips_2019_2020$Month_Number

unique(divvy_tips_2019_2020$Year_Month)

divvy_tips_2019_2020$Year_Month_Quarter<-paste0(divvy_tips_2019_2020$Year_Month,divvy_tips_2019_2020$quarter_Number)

divvy_tips_2019_2020 %>%group_by(Year_Month_Quarter, User_Type) %>% 
  summarise(Total_Number_Trips_ymq=n()) %>% 
  ggplot(mapping = aes(x=Year_Month_Quarter,y=Total_Number_Trips_ymq,
                       group=User_Type,
                       color=User_Type))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("blue","orangered"))+
  scale_size_manual(values=c(1.5, 1.5))+
    labs(title="Number of Trips in Year-Month-Quarter",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Year-Month-Quarter",
       y ="Totall Number of Trips",
       color="User Type")+
  theme(legend.position = c(0.88,0.80),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=5.4,face="bold"))

# Total Trips in Minutes in Year-Month-Quarter

divvy_tips_2019_2020 %>% group_by(Year_Month_Quarter,User_Type) %>% 
  summarise(Total_Ride_Mins_ymq= sum(Ride_Length_min)) %>% 
  ggplot(mapping=aes(x=Year_Month_Quarter, y=Total_Ride_Mins_ymq,
                     group=User_Type,
                     color=User_Type))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("blue","orangered"))+
  scale_size_manual(values=c(1.5,1.5))+
  labs(title="Total Trips in Minutes in Year-Month-Quarter",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Year-Month-Quarter",
       y ="Total Trips in Minutes",
       color="User Type")+
  theme(legend.position = c(0.88,0.82),
       legend.title = element_text(size=8),
       legend.text = element_text(size=7),
       plot.title = element_text(size=12,face="bold"),
       plot.subtitle = element_text(size=9),
       plot.caption = element_text(size=7),
       axis.title.x = element_text(size=9, face="bold"),
       axis.title.y= element_text(size=9, face="bold"),
       axis.text.x=element_text(size=5.4,face="bold"))  

# Mean Trip Minutes in Year-Month-Quarter

divvy_tips_2019_2020 %>% group_by(Year_Month_Quarter,User_Type) %>% 
  summarise(AVG_Ride_Min_ymq= mean(Ride_Length_min)) %>% 
  ggplot(mapping=aes(x=Year_Month_Quarter, y=AVG_Ride_Min_ymq,
                     group=User_Type,
                     color=User_Type))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("blue","orangered"))+
  scale_size_manual(values=c(1.5,1.5))+
  labs(title="Average Rides in Minutes in Year-Month-Quarter",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Year-Month-Quarter",
       y ="Average Ride in Minutes",
       color="User Type")+
  theme(legend.position = c(0.20,0.45),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=5.4,face="bold"))
 

# Number of Trips From Start Station
# pivot_wider(names_from = User_Type,values_from = Number_of_Trips_sid) %>% 
# arrange(desc(`Not Member`)) %>%
  
#divvy_tips_2019_2020 %>% group_by(Start_Station_ID, User_Type) %>% 
  #summarise(Number_of_Trips_sid=n()) %>% 
   # ggplot()+ aes(x=Start_Station_ID, y=Number_of_Trips_sid,
     #                         group=User_Type,
      #                        color=User_Type,
       #                       alpha=User_Type)+
 # geom_point(size=1)+
  # geom_hline(yintercept=15000,
   #           linetype="dashed", color = "orangered", size=.3)+
   #geom_text(divvy_tips_2019_2020 %>% group_by(Start_Station_ID, User_Type) %>%
   #            summarise(Number_of_Trips_sid=n()) %>% 
    #           filter(User_Type=="Not Member", Number_of_Trips_sid>15000),
     #        mapping=aes(x=Start_Station_ID, y=Number_of_Trips_sid,
       #                           label=Start_Station_ID),
        #    size=3,
         #   vjust=1.5)+guides(User_Type = FALSE) +
  #scale_y_continuous(labels = scales::comma)+
 # scale_color_manual(values = c("blue","orangered"))+
  #labs(title="Number of Trips From Start Station",
  #     subtitle = "Comparision between members and non-members",
   #    caption = "Data from 2019_Q2 to 2020_Q1",
    #   x= "Start Station ID",
     #  y ="Total Number of Trips",
      # color="User Type")+
  #theme(legend.title = element_text(size=8),
  #      legend.text = element_text(size=7),
  #      plot.title = element_text(size=12,face="bold"),
 #       plot.subtitle = element_text(size=9),
 #       plot.caption = element_text(size=7),
 #       axis.title.x = element_text(size=9, face="bold"),
 #       axis.title.y= element_text(size=9, face="bold"),
 #       axis.text.x=element_text(size=5.4,face="bold"))


# Number of Trips From Start Station
divvy_tips_2019_2020 %>% 
  group_by(Start_Station_ID, User_Type, Start_Station_Name) %>% 
  summarise(Number_of_Trips_sid=n()) %>% 
  ggplot()+ aes(x=Start_Station_ID, y=Number_of_Trips_sid,
                group=User_Type,
                color=User_Type,
                alpha=User_Type)+
  geom_point(size=1)+
  geom_hline(yintercept=15000,
             linetype="dashed", color = "orangered", size=.3)+
  geom_text_repel(data=divvy_tips_2019_2020 %>% 
                    group_by(Start_Station_ID, User_Type, Start_Station_Name) %>%
                    summarise(Number_of_Trips_sid=n()) %>% 
                    filter(User_Type=="Not Member", Number_of_Trips_sid>15000),
                  mapping=aes(x=Start_Station_ID, y=Number_of_Trips_sid,
                              label=print(paste0(Start_Station_ID, " - ", Start_Station_Name))), size=3, vjust=1, hjust=1, fontface ="bold", point.padding = 0.2)+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("blue","orangered"))+
  labs(title="Number of Trips From Start Station",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Start Station ID",
       y ="Total Number of Trips",
       color="User Type",
       alpha="User Type")+
  theme(legend.position = c(0.88,0.82),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=6,face="bold"))

colnames(divvy_tips_2019_2020)
# Total Trip in Minutes From Start Station

divvy_tips_2019_2020 %>% group_by(Start_Station_ID,User_Type,Start_Station_Name) %>% 
  summarise(Total_trip_Min_sid=sum(Ride_Length_min)) %>% 
  ggplot()+
  aes(x=Start_Station_ID,y=Total_trip_Min_sid,
      group=User_Type,
      color=User_Type,
      alpha=User_Type)+
  geom_point(size=1)+
  geom_hline(yintercept = 500000,linetype="dashed", 
             color = "orangered", size=.3)+
  geom_text_repel(data=divvy_tips_2019_2020 %>% 
                    group_by(Start_Station_ID,User_Type,Start_Station_Name) %>% 
                    summarise(Total_trip_Min_sid=sum(Ride_Length_min)) %>% 
                    filter(User_Type=="Not Member", Total_trip_Min_sid>500000),
                  mapping = aes(x=Start_Station_ID, 
                                y=Total_trip_Min_sid,
                                label=print(paste0(Start_Station_ID," - ", Start_Station_Name))), size =3, vjust=1,hjust=1, fontface ="bold", point.padding = 0.2)+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("blue","orangered"))+
  labs(title="Total Trip in Minutes From Start Station",
       subtitle = "Comparision between members and non-members",
       caption = "Data from 2019_Q2 to 2020_Q1",
       x= "Start Station ID",
       y ="Total Tips in Minutes",
       color="User Type",
       alpha="User Type")+
  theme(legend.position = c(0.88,0.82),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption = element_text(size=7),
        axis.title.x = element_text(size=9, face="bold"),
        axis.title.y= element_text(size=9, face="bold"),
        axis.text.x=element_text(size=6,face="bold"))

