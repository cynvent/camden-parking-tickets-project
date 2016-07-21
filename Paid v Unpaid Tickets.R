#Create data frame with Unpaid tickets
unpaid.tickets <- tickets %>% 
  select(1:37) %>% 
  filter(Ticket.Paid == "0")
  
#Create data frame with Unpaid tickets
paid.tickets <- tickets %>% 
  select(1:37) %>% 
  filter(Ticket.Paid == "1")

#Unpaid ticket histograms

ggplot(unpaid.tickets, aes(x = unpaid.tickets$Contravention.Date)) +
  geom_histogram(fill = unpaid.tickets$Ticket.Type)

#Facet plot of unpaid tickets by zone
ggplot(unpaid.tickets, aes(unpaid.tickets$Controlled.Parking.Zone)) +
  geom_bar() +
  facet_wrap(~ Contravention_Year) +
  theme(axis.text.x = element_text(angle = 90))

#Facet plot of unpaid tickets by 
ggplot(unpaid.tickets, aes(unpaid.tickets$Contravention_Month)) +
  geom_bar() +
  facet_wrap(~ Contravention_Year) +
  theme(axis.text.x = element_text(angle = 90))
  
#Facet plot comparing Paid v Unpaid by Month/Year
ggplot(tickets, aes(x = Contravention_Month, fill = Contravention_Year)) +
  geom_bar()+
  facet_wrap(~ Ticket.Paid)

#Unpaid tickets - Forgeign cars v UK   
ggplot(unpaid.tickets, aes(unpaid.tickets$Foreign.Vehicle)) +
  geom_bar() +
  facet_wrap(~ Contravention_Year)

#Unpaid tickets - reasons for cancellation
ggplot(unpaid.tickets, aes(x = Contravention_Year, fill = Cancellation.Reason)) +
  geom_bar()

summary(unpaid.tickets$Cancellation.Reason)

#Unpaid tickets - Commercial v Private Vehicles   
ggplot(unpaid.tickets, aes(unpaid.tickets$Vehicle.Class, fill = Status.Of.Case)) +
  geom_bar()

