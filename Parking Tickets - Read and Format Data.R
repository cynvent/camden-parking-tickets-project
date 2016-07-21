# 1 - Getting data in to R and binding together to create a single data frame

#list of data file names
filelist <- list.files(path = "/Users/mw/Desktop/Parking Tickets Project/Parking Tickets/Data Files")

#Read in and bind the separate data files
tickets <- do.call(rbind,lapply(filelist,read.csv))

#Reformatting leveles of specific Variable factors
tickets$Contravention_Day_of_Week <- factor(tickets$Contravention_Day_of_Week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
tickets$Contravention_Month <- factor(tickets$Contravention_Month, levels=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
tickets$Ticket.Paid <- factor(tickets$Ticket.Paid, levels=c("0", "1"))
tickets$Contravention_Year <- factor(tickets$Contravention_Year, levels=c("2010", "2011", "2012", "2013", "2014", "2015", "2016"))
tickets$Contravention_Day_Type <- factor(tickets$Contravention_Day_Type, levels=c("Weekday", "Weekend"))
tickets$Contravention_Day <- factor(tickets$Contravention_Day, levels=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12","13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"))


# 2 - Review and create new Variables in the dataset

#Update the Contravention Date to a Date Format
tickets$Contravention.Date <- as.Date(tickets$Contravention.Date, format = "%d/%m/%Y")

#Create new Variables for Hour / Day / Month / Year
tickets$Contravention_Year <- format(tickets$Contravention.Date, "%Y")
tickets$Contravention_Month <- format(tickets$Contravention.Date, "%m")
tickets$Contravention_Day <- format(tickets$Contravention.Date, "%d")

#Calculate and insert the day of the week based on Date
tickets$Contravention_Day_of_Week <- format(tickets$Contravention.Date, "%A")

#Create Weekday / Weekend Variable > Simplified version of the original custom function
dayType <- function(x) {
  if(x == 'Saturday') {
    return("Weekend")
    if(x == 'Sunday')
      return("Weekend")
  } else {
    return("Weekday")
  }
}

#applies the dayType function to populate the new column: Weekday v Weekend
tickets$Contravention_Day_Type <- sapply(tickets$Contravention_Day_of_Week, dayType)

#Adding in a new Feature to group vehicle types into Commercial and Private vehicles
tickets$Vehicle.Class <- "Commercial"
tickets$Vehicle.Class[tickets$Vehicle.Category == "(Private) Car"] <- "(Private) Car"

#New field to aggregate Status.Of.Case into binary Paid/Not Paid outcome
tickets$Ticket.Paid <- 0
tickets$Ticket.Paid[tickets$Status.Of.Case == "Paid/Closed"] <- 1

