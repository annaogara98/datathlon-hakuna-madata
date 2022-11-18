#load packages
library(tidyverse)
library(dbplyr)

#read in hospital stay data and clean to show X countries of interest
hospital_days <- read_csv("hospital_days.csv")
glimpse(hospital_days)

clean_hospital_days <- hospital_days %>%  
  select(Country, Year, Value) %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands" | Country == "Finland" | Country == "Denmark" | Country == "Iceland")

view(clean_hospital_days)

#plot hospital stay duration by country over time
plot_compare <- clean_hospital_days %>% 
  ggplot(aes(Year, Value, color = Country)) +
  geom_point() +
  geom_line()

plot_compare

#read in alcohol consumption data and clean to show X countries of interest
alcohol_consumption_raw <- read_csv("alc_consumption.csv")

clean_alcohol_consumption <- alcohol_consumption_raw %>% 
  select(Country, Year, Value, Measure) %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands" | Country == "Finland" | Country == "Denmark" | Country == "Iceland") %>% 
  filter(Measure == "Litres per capita (15+)")

#plot alcohol consumption by country over time
plot_consumption <- clean_alcohol_consumption %>% 
  ggplot(aes(Year, Value, color = Country)) +
  geom_point() +
  geom_line()



#next want to combine to compare both 
#why have stays dropped but consumption stabilised in some places but not others?

joined <- left_join(clean_hospital_days, clean_alcohol_consumption, by = c("Country" = "Country", "Year" = "Year")) %>% 
  rename(days = Value.x,
         consumption = Value.y)


joined_plot <- joined %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands") %>% 
  pivot_longer(days:consumption, names_to = "dayconsump", values_to = "value")%>% 
  ggplot(aes(Year, value, group = paste0(dayconsump, Country), color = Country, shape = dayconsump)) +
  geom_point() +
  geom_line()
joined_plot  



#want to join on the discharges per 100k of the population to compare if discharges and duration differ
#read in discharge data
discharge_per_100k_pop <- read_csv("hospital_discharges_per_100k_pop.csv")

#clean
clean_discharge_data <- discharge_per_100k_pop %>% 
  select(Country, Year, Value) %>% 
  rename(discharges = Value) %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands" | Country == "Finland" | Country == "Denmark" | Country == "Iceland")

#join discharge data
joined_days_discharges_consumption <- left_join(joined, clean_discharge_data, by = c("Country" = "Country", "Year" = "Year"))

#plotting - this is messy and needs a checking over. Interesting comparison is possible, just not sure how best to visualise?
days_discharges_consumption_plot <- joined_days_discharges_consumption %>% 
  #filter(Country == "United Kingdom" | Country == "Netherlands") %>%
  ggplot() +
  geom_point(aes(Year, days, color = Country)) +
  geom_line(aes(Year, days, color = Country))+
  geom_point(aes(Year, consumption, color = Country)) +
  geom_line(aes(Year, consumption, color = Country)) +
  geom_line(aes(Year, discharges, colour = Country))

days_discharges_consumption_plot  
