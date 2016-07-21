# 1 - Basic total number of tickets overviews

table(tickets$Status.Of.Case) #total tickets by status
table(tickets$Contravention_Year) #total tickets by year
table(tickets$Contravention_Month, tickets$Contravention_Year) #totals by month/year
table(tickets$Ticket.Type, tickets$Contravention_Year) #ticket type totals by year

#Zone reviews
table(tickets$Controlled.Parking.Zone.Area)
table(tickets$Controlled.Parking.Zone.Area, tickets$Contravention_Year)
table(tickets$Controlled.Parking.Zone.Area, tickets$Status.Of.Case)


#Paid v Unpaid Totals
table(tickets$Contravention_Year, tickets$Ticket.Paid)
prop.table(table(tickets$Contravention_Year, tickets$Ticket.Paid),1)
prop.table(table(tickets$Controlled.Parking.Zone.Area, tickets$Ticket.Paid),1)

#Reasons for cancellation
table(tickets$Cancellation.Reason, tickets$Contravention_Year)

#Wards overview
table(tickets$Ward.Name, tickets$Contravention_Year)
table(tickets$Ward.Name, tickets$Ticket.Paid)


# 2 - EDA Charts/Plots

ggplot(data = tickets) +
  geom_bar(mapping = aes(x = Contravention_Year))

#Histogram plots

#7 day binwidth
ggplot(data = tickets, aes(x = Contravention.Date)) + 
  geom_histogram(binwidth = 7, col = "white")
  

#30 day binwidth (rough months)
ggplot(data = tickets, aes(x = Contravention.Date)) + 
  geom_histogram(binwidth = 30, col = "white") 

#30 day binwidth with Year facet (rough months)
ggplot(data = tickets, aes(x = as.date(Contravention_Month))) + 
  geom_histogram(binwidth = 30, col = "white") +
  facet_wrap(~ Contravention_Year)

#90 day binwidth (rough quarters)
ggplot(data = tickets, aes(x = Contravention.Date)) + 
  geom_histogram(binwidth = 90, col = "white")

#30 day binwidth (rough months) with vehicle types
ggplot(data = tickets, aes(x = Contravention.Date, fill = Vehicle.Category)) + 
  geom_histogram(binwidth = 30, col = "white")

#30 day binwidth (rough months) with vehicle class
ggplot(data = tickets, aes(x = Contravention.Date, fill = Vehicle.Class)) + 
  geom_histogram(binwidth = 30, col = "white", position = "stack")

#30 day binwidth (rough months) with Paid status (proportion)
ggplot(data = tickets, aes(x = Contravention.Date, fill = ContraventionDay_Type)) + 
  geom_histogram(binwidth = 30, col = "white", position = "fill")

#30 day binwidth (rough months) with Ticket Type
ggplot(data = tickets, aes(x = Contravention.Date, fill = Ticket.Type)) + 
  geom_histogram(binwidth = 30, col = "white", position = "stack")
  

#total tickets / month by vehicle class with facet view by Parking Zone
ggplot(tickets, aes(x = month(Contravention.Date), fill = Vehicle.Class)) +
  geom_bar() +
  facet_wrap(~ Controlled.Parking.Zone.Area)