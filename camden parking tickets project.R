#Load libraries

library(dplyr)
library(ggplot2)


#Read in and view all of the data files separately 

Parking_Services_Penalty_Charge_Notices_2010.11 <- read.csv("~/Desktop/Parking PCN Data Files/Parking_Services_Penalty_Charge_Notices_2010-11.csv")
View(Parking_Services_Penalty_Charge_Notices_2010.11)

Parking_Services_Penalty_Charge_Notices_2011.12 <- read.csv("~/Desktop/Parking PCN Data Files/Parking_Services_Penalty_Charge_Notices_2011-12.csv")
View(Parking_Services_Penalty_Charge_Notices_2011.12)

Parking_Services_Penalty_Charge_Notices_2012.13 <- read.csv("~/Desktop/Parking PCN Data Files/Parking_Services_Penalty_Charge_Notices_2012-13.csv")
View(Parking_Services_Penalty_Charge_Notices_2012.13)

Parking_Services_Penalty_Charge_Notices_2013.14 <- read.csv("~/Desktop/Parking PCN Data Files/Parking_Services_Penalty_Charge_Notices_2013-14.csv")
View(Parking_Services_Penalty_Charge_Notices_2013.14)

Parking_Services_Penalty_Charge_Notices_2014.15 <- read.csv("~/Desktop/Parking PCN Data Files/Parking_Services_Penalty_Charge_Notices_2014-15.csv")
View(Parking_Services_Penalty_Charge_Notices_2014.15)

Parking_Services_Penalty_Charge_Notices_2015.16 <- read.csv("~/Desktop/Parking PCN Data Files/Parking_Services_Penalty_Charge_Notices_2015-16.csv")
View(Parking_Services_Penalty_Charge_Notices_2015.16)


##########################################################################################################################


#Create a new dataframe (train_data) and merge the files together using rbind()

train_data <- rbind(Parking_Services_Penalty_Charge_Notices_2010.11, Parking_Services_Penalty_Charge_Notices_2011.12)
train_data <- rbind(train_data, Parking_Services_Penalty_Charge_Notices_2012.13)
train_data <- rbind(train_data, Parking_Services_Penalty_Charge_Notices_2013.14)
train_data <- rbind(train_data, Parking_Services_Penalty_Charge_Notices_2014.15)

str(train_data)
head(train_data, n=30)
glimpse(train_data)

#Create a backup of the combined dataset
traindata_backup <- train_data

#Create new dataframe for the test data (15-16)

test_data <- Parking_Services_Penalty_Charge_Notices_2015.16
  
#Check data with head()
head(train_data$Contravention.Date)


#Convert Contravention.Date to date format with as.Date
as.Date(train_data$Contravention.Date, format = "%d/%m/%Y")

#Create a placeholder date field to check new field observations against
train_data$Placeholder.Contravention.Date <- as.Date(train_data$Contravention.Date, format = "%d/%m/%Y")

#Create new fields for Hour / Day / Month / Year
train_data$Contravention_Year <- format(train_data$Placeholder.Contravention.Date, "%Y")
train_data$Contravention_Month <- format(train_data$Placeholder.Contravention.Date, "%m")
train_data$Contravention_Day <- format(train_data$Placeholder.Contravention.Date, "%d")

#Calculate and insert the day of the week based on Date
train_data$Contravention_Day_of_Week <- format(train_data$Placeholder.Contravention.Date, "%A")

#Create Weekday / Weekend Variable

dayType <- function(x){
  if(x == 'Saturday')
    return("Weekend")
  if(x == 'Sunday')
    return("Weekend")
  if(x == 'Monday')
    return("Weekday")
  if(x == 'Tuesday')
    return("Weekday")
  if(x == 'Wednesday')
    return("Weekday")
  if(x == 'Thursday')
    return("Weekday")
  if(x == 'Friday')
    return("Weekday")
}
  
train_data$Contravention_Day_Type <- lapply(train_data$Contravention_Day_of_Week, dayType) #replaced with the sapply version below
train_data$Contravention_Day_Type <- sapply(train_data$Contravention_Day_of_Week, dayType) #applies the dayType function to populate the new column: Weekday v Weekend



##########################################################################################################################
#Data quality - review of NA entries in the data
##########################################################################################################################  


sum(is.na(train_data$Contravention.Date))
sum(is.na(train_data$Contravention.In.Last.7.Days))
sum(is.na(train_data$Ticket.Type))
sum(is.na(train_data$Ticket.Description))
sum(is.na(train_data$Contravention.Code))
sum(is.na(train_data$Contravention.Code.Suffix))
sum(is.na(train_data$Contravention.Code.Description))
sum(is.na(train_data$Ticket.Issued.Via.CCTV.Camera))
sum(is.na(train_data$Controlled.Parking.Zone.Area))
sum(is.na(train_data$Street))
sum(is.na(train_data$Vehicle.Category))
sum(is.na(train_data$Vehicle.Removed))
sum(is.na(train_data$Status.Of.Case))
sum(is.na(train_data$Charging.Band.Description))
sum(is.na(train_data$Civil.Enforcement.Officer.Error))
sum(is.na(train_data$Penalty.Charge.Notice.Cancelled))
sum(is.na(train_data$Penalty.Charge.Notice.Written.Off))
sum(is.na(train_data$Cancellation.Reason))
sum(is.na(train_data$Foreign.Vehicle))
sum(is.na(train_data$Country.Vehicle.Registered.To))
sum(is.na(train_data$Ward.Code))
sum(is.na(train_data$Ward.Name))
sum(is.na(train_data$Easting))
sum(is.na(train_data$Northing))
sum(is.na(train_data$Longitude))
sum(is.na(train_data$Latitude))
sum(is.na(train_data$Location))
sum(is.na(train_data$Spatial.Accuracy))
sum(is.na(train_data$Last.Uploaded))
sum(is.na(train_data$Socrata.ID))
sum(is.na(train_data$Placeholder.Contravention.Date))
sum(is.na(train_data$Contravention_Year))
sum(is.na(train_data$Contravention_Month))
sum(is.na(train_data$Contravention_Day))
sum(is.na(train_data$Contravention_Day_of_Week))
sum(is.na(train_data$Contravention_Day_Type))

#Drill down into Variables with NA values: Easting / Northing / Logitude / Latitude

sum(!is.na(train_data$Easting))
sum(!is.na(train_data$Northing))
sum(!is.na(train_data$Longitude))
sum(!is.na(train_data$Latitude))

#Review variables for Blank observations

sum(train_data$Contravention.Date == "")
sum(train_data$Contravention.In.Last.7.Days == "")
sum(train_data$Ticket.Type == "")
sum(train_data$Ticket.Description == "")
sum(train_data$Contravention.Code == "")
sum(train_data$Contravention.Code.Suffix == "")
sum(train_data$Contravention.Code.Description == "")
sum(train_data$Ticket.Issued.Via.CCTV.Camera == "")
sum(train_data$Controlled.Parking.Zone.Area == "")
sum(train_data$Street == "")
sum(train_data$Vehicle.Category == "")
sum(train_data$Vehicle.Removed == "")
sum(train_data$Status.Of.Case == "")
sum(train_data$Charging.Band.Description == "")
sum(train_data$Civil.Enforcement.Officer.Error == "")
sum(train_data$Penalty.Charge.Notice.Cancelled == "")
sum(train_data$Penalty.Charge.Notice.Written.Off == "")
sum(train_data$Cancellation.Reason == "")
sum(train_data$Foreign.Vehicle == "")
sum(train_data$Country.Vehicle.Registered.To == "")
sum(train_data$Ward.Code == "")
sum(train_data$Ward.Name == "")
sum(train_data$Easting == "")
sum(train_data$Northing == "")
sum(train_data$Longitude == "")
sum(train_data$Latitude == "")
sum(train_data$Location == "")
sum(train_data$Spatial.Accuracy == "")
sum(train_data$Last.Uploaded == "")
sum(train_data$Socrata.ID == "")
sum(train_data$Placeholder.Contravention.Date == "")
sum(train_data$Contravention_Year == "")
sum(train_data$Contravention_Month == "")
sum(train_data$Contravention_Day == "")
sum(train_data$Contravention_Day_of_Week == "")
sum(train_data$Contravention_Day_Type == "")

#Overview of Data variations
table(train_data$Charging.Band.Description)

table(train_data$Contravention.Date)
table(train_data$Contravention.In.Last.7.Days)
table(train_data$Ticket.Type)
table(train_data$Ticket.Description)
table(train_data$Contravention.Code)
table(train_data$Contravention.Code.Suffix)
table(train_data$Contravention.Code.Description)
table(train_data$Ticket.Issued.Via.CCTV.Camera)
table(train_data$Controlled.Parking.Zone.Area)
table(train_data$Street)
table(train_data$Vehicle.Category)
table(train_data$Vehicle.Removed)
table(train_data$Status.Of.Case)
table(train_data$Charging.Band.Description)
table(train_data$Civil.Enforcement.Officer.Error)
table(train_data$Penalty.Charge.Notice.Cancelled)
table(train_data$Penalty.Charge.Notice.Written.Off)
table(train_data$Cancellation.Reason)
table(train_data$Foreign.Vehicle)
table(train_data$Country.Vehicle.Registered.To)
table(train_data$Ward.Code)
table(train_data$Ward.Name)
table(train_data$Easting)
table(train_data$Northing)
table(train_data$Longitude)
table(train_data$Latitude)
table(train_data$Location)
table(train_data$Spatial.Accuracy)
table(train_data$Last.Uploaded)
table(train_data$Socrata.ID)
table(train_data$Placeholder.Contravention.Date)
table(train_data$Contravention_Year)
table(train_data$Contravention_Month)
table(train_data$Contravention_Day)
table(train_data$Contravention_Day_of_Week)
table(train_data$Contravention_Day_Type)


##########################################################################################################################
#EDA 
##########################################################################################################################  

#Contravention Year v Day of Week
prop.table(table(train_data$Contravention_Year, train_data$Contravention_Day_of_Week),1)

# Tickets by Year / Status
table(train_data$Contravention_Year, train_data$Status.Of.Case)

# Tickets by Month / Year
table(train_data$Contravention_Year, train_data$Contravention_Month)

##########################################################################################################################
#EDA Plots
##########################################################################################################################  
  
#EDA plot: Ticket Type v Parking Zone
ggplot(train_data, aes(x = train_data$Ticket.Type, col = train_data$Controlled.Parking.Zone.Area)) +
  geom_bar(na.rm = TRUE)

#EDA plot: Zone v Count
ggplot(train_data, aes(x = train_data$Controlled.Parking.Zone.Area, )) +
  geom_bar()

#Total tickets by Month
ggplot(train_data, aes(x = train_data$Contravention_Month, y = count(as.factor(train_data$Controlled.Parking.Zone.Area, col = train_data$Contravention_Year))) +
  geom_line(aes(color = train_data$Controlled.Parking.Zone.Area))
