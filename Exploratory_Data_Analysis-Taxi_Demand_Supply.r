library(stringr)
library(ggplot2)

######################################## Data Importing ########################################

#Loading the Uber Data Set Provided into the R-Environment
uber_req_data<-read.csv("Uber Request Data.csv", stringsAsFactors = F, header = T)

######################################## Data Cleaning ########################################

# Check For Duplicated Rows
any(duplicated(uber_req_data))
# This Command retuens FALSE, indicating, there is no duplicated rows in this data frame.

# Chech for validity of "NA"
sum(is.na(uber_req_data$Request.id))
sum(is.na(uber_req_data$Pickup.point))
sum(is.na(uber_req_data$Driver.id))
sum(is.na(uber_req_data$Status))
sum(is.na(uber_req_data$Request.timestamp))
sum(is.na(uber_req_data$Drop.timestamp))
# From the above cmds, we see that, only the 'Driver.id' column and 'Drop.timestamp' column have 
#   "NA" values in it. So we need to chech the validity of those "NA" values.
any(is.na(uber_req_data$Driver.id) & (uber_req_data$Status=="Trip Completed" | uber_req_data$Status=="Cancelled"))
# The 'Driver.id' is not supposed to be "NA", for the rides with 'Status' as "Trip Completed" 
#   or "Cancelled". The above cmd, checks for that and returns FALSE, meaning that no "NA" for  
#     rides with 'Status' as "Trip Completed" or "Cancelled".

any(is.na(uber_req_data$Drop.timestamp) & uber_req_data$Status=="Trip Completed")
# The 'Drop.timestamp' is supposed to be NA for the rides, that did not take plce.(Rides with rides with 'Status' as 
#   "Cancelled" or "No Cars Available". The above cmd checks of there are any rows, that have 'Status' 
#     as "Trip Completed" and 'Drop.timestamp' as "NA". This returns FALSE.

# Converting 'Request.timestamp' and 'Drop.timestamp' to proper DateTime Objects

typeof(uber_req_data$Request.timestamp)
typeof(uber_req_data$Drop.timestamp)

# As we can see from above, the 'Request.timestamp' and 'Drop.timestamp' 
#   are of type "Character" and not "Date".
# Hence, the below function, date_conv() will 
#   Get a date in character type column from the dataframe as an argument,
#   Convert the character string into uniform date format,
#   Convert that into date object, and
#   Return the Date Object list.

date_conv<-function(DateTime_Arg)
{
  DateTime_list<-str_replace_all(DateTime_Arg, "[/]",  "-")
  DateTime_list<- as.POSIXlt(DateTime_list, format="%d-%m-%Y %H:%M")
  return(DateTime_list)
}

uber_req_data$Request_DateTime<-date_conv(uber_req_data$Request.timestamp)
uber_req_data$Drop_DateTime<-date_conv(uber_req_data$Drop.timestamp)
typeof(uber_req_data$Request_DateTime)
typeof(uber_req_data$Drop_DateTime)

######################################## Deriving Data ########################################

# Getting the date
uber_req_data$Date<-format(uber_req_data$Request_DateTime,"%d")
# Gettng the day of the week
uber_req_data$Day<-weekdays(uber_req_data$Request_DateTime)
# Getting the hour of the day in which the request is placed
uber_req_data$Req_Hour<-format(uber_req_data$Request_DateTime,"%H")
# Getting the droping hour of the day
uber_req_data$Drop_Hour<-format(uber_req_data$Drop_DateTime,"%H")
# Converting the hour to number
uber_req_data$Req_Hour<-as.numeric(uber_req_data$Req_Hour)
uber_req_data$Drop_Hour<-as.numeric(uber_req_data$Drop_Hour)

Time_Period_Segmentation<-function(request_hour)
{
  ifelse((request_hour>=3 & request_hour<7),"Early Morning",
         ifelse((request_hour>=7 & request_hour<11),"Morning",
                       ifelse((request_hour>=11 & request_hour<16),"Afternoon",
                              ifelse((request_hour>=16 & request_hour<19),"Evening",
                                     ifelse((request_hour>=19 & request_hour<23),"Night",
                                            ifelse((request_hour>=23 | request_hour<3),"Mid Night",NA))))))
  
}

# Segmenting the hour in which into request is placed into time frames
uber_req_data$Time_Period<-Time_Period_Segmentation(uber_req_data$Req_Hour)

# Cleaned and Segmented Data Frame
View(uber_req_data)

######################################## Exploratory Data Analysis ########################################

# Frequency of request that get Cancelled (or) No Cars Available
req_count_vs_status_plot<-ggplot(uber_req_data,aes(x=Status))+ geom_bar(fill="#FF512C")+ geom_text(stat='count',aes(label=..count..),vjust=-0.5) + xlab("Status of Request") + ylab("No of Requests")  
req_count_vs_status_plot
# Cancelled Request: 1264
# No Cars Available: 2650
# A total of 3914 requests are lost due to Trip Cancellation or Non-availability of cars.
# This results in financial loss.

# Most problematic place of request
req_count_vs_pickup_pt_statuswise_plot<-ggplot(uber_req_data,aes(x=Pickup.point, fill=Status)) + geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Pickup Point", y="Number of Request",fill="Status of Request") + geom_text(stat='count',aes(label=..count..),vjust=-0.5,position = position_dodge(width = 1))
req_count_vs_pickup_pt_statuswise_plot
req_count_vs_pickup_pt_statuswise_plot_all_days <- req_count_vs_pickup_pt_statuswise_plot + facet_wrap( ~ uber_req_data$Date, nrow =5, ncol = 1)
req_count_vs_pickup_pt_statuswise_plot_all_days
# No of Request, that dos not result in trips in Airport:
nrow(subset(uber_req_data, uber_req_data$Pickup.point == "Airport" & (uber_req_data$Status == "Cancelled" | uber_req_data$Status == "No Cars Available")))
# No of Request, that dos not result in trips in City:
nrow(subset(uber_req_data, uber_req_data$Pickup.point == "City" & (uber_req_data$Status == "Cancelled" | uber_req_data$Status == "No Cars Available")))
# From above graphs and the count, we can definitely say that the most Request that 
#   originate in the CITY are either cancelled or there are no cars are available to 
#   complete the request. 
# Hence the problematic place of request is in CITY.
# No of Request, that were CANCELLED from CITY:
nrow(subset(uber_req_data, uber_req_data$Pickup.point == "City" & uber_req_data$Status == "Cancelled"))
# No of Request, where there were NO CARS from CITY:
nrow(subset(uber_req_data, uber_req_data$Pickup.point == "City" & uber_req_data$Status == "No Cars Available"))


# Frequency of request VS the hours of the day, categoried by Status
req_count_per_hr_plot<-ggplot(uber_req_data,aes(x=as.factor(Req_Hour),fill = Status)) + geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Hour of the day", y="Number of Request",fill="Status of Request")
req_count_per_hr_plot
req_count_per_hr_plot_all_days<- req_count_per_hr_plot + facet_wrap(~uber_req_data$Date, nrow =5, ncol = 1)
req_count_per_hr_plot_all_days
# Frequency of request and the status of the request for each part of the day.
req_count_vs_time_period_statuswise_plot<- ggplot(uber_req_data,aes(x=Time_Period, fill=Status)) + geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Part of the Day", y="Number of Request",fill="Status of Request")
req_count_vs_time_period_statuswise_plot
req_count_vs_time_period_statuswise_plot_all_days<-req_count_vs_time_period_statuswise_plot + facet_wrap( ~ uber_req_data$Date, nrow =5, ncol = 1)
req_count_vs_time_period_statuswise_plot_all_days
#Inference:
# It can be concurred from the above graphs that,
#   Problem_1: At Morning, there is highest number of trip cancellation.
#   Problem_2: At Evening & Night, there are more number of trip request, but very less 
#               cars to full fill the request.

# Frequency of request VS the hours of the day, categoried by Pickup Point
req_count_vs_pickup_point_plot<-ggplot(uber_req_data,aes(x=as.factor(Req_Hour),fill=Pickup.point))+ geom_bar(position = "dodge") + scale_fill_manual(values=c("Red", "#0bd60f")) + labs(x="Hour of the day", y="Number of Request",fill="Pickup Point")
req_count_vs_pickup_point_plot
req_count_vs_pickup_point_plot_all_days<-req_count_vs_pickup_point_plot + facet_wrap( ~ uber_req_data$Date, nrow =5, ncol = 1)
req_count_vs_pickup_point_plot_all_days
# Frequency of request and the orign of request for each part of the day
req_count_vs_time_period_pickup_wise_plot<- ggplot(uber_req_data,aes(x=Time_Period, fill=Pickup.point)) + geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Part of the Day", y="Number of Request",fill="Pickup Point")
req_count_vs_time_period_pickup_wise_plot
req_count_vs_time_period_pickup_wise_plot_all_days<-req_count_vs_time_period_pickup_wise_plot + facet_wrap( ~ uber_req_data$Date, nrow =5, ncol = 1)
req_count_vs_time_period_pickup_wise_plot_all_days
# Inference:
# It can be concurred from the above graphs that,
#   The number of cab request from City to Airport is the highest in the Morning.
#   The number of cab request from Airport to City is the highest in the Evening and night.

Overall_plot<-ggplot(uber_req_data,aes(x=Pickup.point, fill=Status))+ geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Pickup Point", y="Number of Request",fill="Status of Request")+facet_wrap( ~ uber_req_data$Time_Period, nrow =3, ncol = 3) + geom_text(stat='count',aes(label=..count..),vjust=-0.5,position = position_dodge(width = 1))
Overall_plot

#-------------------------------------------------------------------------------------#

# Problem_1: 
# Problem Statement: High Cancellation in the Morning
# It is identified that Cancellation is the highest in the morning time
EMorn_Morn_Data<-subset(uber_req_data,uber_req_data$Time_Period=="Early Morning" | uber_req_data$Time_Period=="Morning")
EMorn_Morn_Plot<-ggplot(EMorn_Morn_Data,aes(x=Pickup.point, fill=Status)) + geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Pickup Point", y="Number of Request",fill="Status of Request") + geom_text(stat='count',aes(label=..count..),vjust=-0.5,position = position_dodge(width = 1))
EMorn_Morn_Plot
#Inferece:
# From the EMorn_Morn_Plot, it is understood that rides from CITY 
#   booked at the time frame of approximately 3m to 12 noon are cancelled more.

# Demand
demand_Emorn_and_morn_from_city<-nrow(subset(EMorn_Morn_Data,EMorn_Morn_Data$Pickup.point=="City" ))
demand_Emorn_and_morn_from_city
# Suppy
supply_Emorn_and_morn_from_city<-nrow(subset(EMorn_Morn_Data,EMorn_Morn_Data$Pickup.point=="City" & EMorn_Morn_Data$Status=="Trip Completed"))
supply_Emorn_and_morn_from_city
# Supply Demand Gap
Emorn_and_morn_supply_demand_gap<-demand_Emorn_and_morn_from_city-supply_Emorn_and_morn_from_city
Emorn_and_morn_supply_demand_gap
Emorn_and_morn_supply_demand_gap_percent<-(Emorn_and_morn_supply_demand_gap/demand_Emorn_and_morn_from_city)*100
Emorn_and_morn_supply_demand_gap_percent

# Reason for Supply Demand Gap
#   Of the Supply Demand Gap, Cancellation of Trip results for approximately 65%. 
#   i.e.., here the cars are there, but the drivers are reluctant in accepting the trip
#     request to airport. 
#   This can be attributed to the fact that, Airports usually situated in the outskirts
#     of the city. Although, there is a high demand for going to Airport from City,
#     there is very less demand for cabs from Airport to city ( refer EMorn_Morn_Plot )
#     in this time frame. So it is possible that the drivers fear they may not get 
#     the ride back to city from airport, and they may end up waiting for long time 
#     in airport for the ride or comeback to city at their own expense, both incurring 
#     financial loss to Driver.
#   Moreover, drivers usually get many rides within city at this time frame and therefore, 
#     they don't mind cancelling the rides to airport.

# Recommanded ways to solve the problem
# As the main problem here is the cancellation by drivers, that has to be averted
#   but at the same time, financial loss of drivers has to be compensated
# A fine of some kind, incentive for drivers, who don't cancel their rides 
#   can be announced to discourage the drivers from cancelling the rides.
# At the same time, if a driver takes up a ride to Airport, and does not get a 
#   ride back to city and he faces time loss (due to waiting in airport for 
#   ride) , or financial loss ( due to riding back to city without any customer ) 
#   then the drivers can be compensated on common grounds without 
#   any harm to company or driver.
# By, these ways, we can control the excess cancellation in Airport 
#   during Early Morning and Morning 

#-------------------------------------------------------------------------------------#

# Problem_2 
# Problem Statement: At Evening & Night, there are more number of trips, but no cars.

# It is identified that, there is a huge demand for cab rides at evening and night.
Evening_Night_Data<-subset(uber_req_data,uber_req_data$Time_Period=="Evening" | uber_req_data$Time_Period=="Night")
Evening_Night_Plot<-ggplot(Evening_Night_Data,aes(x=Pickup.point, fill=Status)) + geom_bar(position = "dodge") + scale_fill_manual(values=c("#0762f1", "Red", "#0bd60f")) + labs(x="Pickup Point", y="Number of Request",fill="Status of Request") + geom_text(stat='count',aes(label=..count..),vjust=-0.5,position = position_dodge(width = 1))
Evening_Night_Plot
#Inferece:
# From the Evening_Night_Plot,it is inferred that, rides from Airport to City in the 
#    evening-night time frame is not getting cars (No cars Available) for the request.

#Demand
demand_Evening_Night_from_airport<-nrow(subset(Evening_Night_Data,Evening_Night_Data$Pickup.point=="Airport" ))
demand_Evening_Night_from_airport
#Supply
supply_Evening_Night_from_airport<-nrow(subset(Evening_Night_Data,Evening_Night_Data$Pickup.point=="Airport" & Evening_Night_Data$Status=="Trip Completed"))
supply_Evening_Night_from_airport
#Demand-Supply Gap
Evening_Night_supply_demand_gap<-demand_Evening_Night_from_airport-supply_Evening_Night_from_airport
Evening_Night_supply_demand_gap
Evening_Night_supply_demand_gap_percent<-(Evening_Night_supply_demand_gap/demand_Evening_Night_from_airport)*100
Evening_Night_supply_demand_gap_percent

# Reason for Demand Supply Gap
# The main reason for the shortage of cars from airport, when there is high 
#   demand is that, there is very less demand for cabs from city to airport. There
#   is very less Supply (City to Airport, total no of request is ~ 761). As the 
#   airport is situated at the outskirts of the city, no driver will come to airport
#   to just pick up the passenger. It is not feasible for the drivers.
# Even if the full demand of City to Airport is met, that is will in no match 
#   to the demand from Airport. The Demand-Supply % is at whopping 76%. Hence
#   obviously, the total number of cars is very less.   

# Recommanded ways to sove the problem
# The most obvious solution is to increase the number of cars operating under Uber. 
#   Yet, Just for closing in the demand supply gap, we can't get new cars. It will 
#   be financially feasible, only if the demand in other area in other time are also 
#   high. Or else, the supply will become more and the demand will less, resulting in 
#   loss.
# Other Feasible solution is that, making the drivers get the request with long 
#   distance route, so that the drivers are will compensated for the loss incurred 
#   for driving to airport from city without customer. Cabs that reached airport 
#   for dropping the customer can be given, short route request.
