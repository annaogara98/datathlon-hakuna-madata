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
  filter(Country == "Norway" | Country == "Ireland" | Country == "France" | Country == "Estonia" | Country == "Belgium" | Country == "Austria" | Country == "United Kingdom" | Country == "Netherlands" | Country == "Finland" | Country == "Denmark" | Country == "Sweden") %>% 
  filter(Measure == "Litres per capita (15+)")


#plot alcohol consumption by country over time
plot_consumption <- clean_alcohol_consumption %>% 
  ggplot(aes(Year, Value, color = Country)) +
  scale_y_continuous(name = "Alcohol Consumption (Litres)") +
  geom_point() +
  geom_line() +
  ggtitle("Alcohol Consumption Over Time") +
  #gghighlight::gghighlight(Country == "United Kingdom" | Country == "Netherlands", keep_scales = TRUE)+
  theme_minimal()


plot_consumption


#next want to combine to compare both 
#why have stays dropped but consumption stabilised in some places but not others?

joined <- left_join(clean_hospital_days, clean_alcohol_consumption, by = c("Country" = "Country", "Year" = "Year")) %>% 
  rename(days = Value.x,
         consumption = Value.y) 


#%>% 
 # select(Country:consumption)


joined_plot <- joined %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands") %>% 
  pivot_longer(days:consumption, names_to = "dayconsump", values_to = "value")%>% 
  ggplot(aes(Year, value, group = paste0(dayconsump, Country), color = Country, shape = dayconsump)) +
  geom_point() +
  geom_line()
joined_plot  

days_discharges_consumption_plot <- joined_days_discharges_consumption %>% 
  #filter(Country == "United Kingdom" | Country == "Netherlands") %>%
  ggplot() +
  geom_point(aes(Year, days, color = Country)) +
  geom_line(aes(Year, days, color = Country))+
  geom_point(aes(Year, consumption, color = Country)) +
  geom_line(aes(Year, consumption, color = Country)) +
  geom_line(aes(Year, discharges, colour = Country))

days_discharges_consumption_plot  

coeff <- 1

joined_plot2 <- joined %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands") %>% 
  ggplot(aes(x=Year)) +
  geom_line(aes(y=days, color = Country)) +
  geom_point(aes(y=consumption, color = Country)) +
  scale_y_continuous(
    name = "Hospital Stay (days)",
    sec.axis = sec_axis(~.*coeff, name = "Alcohol Consumption (litres)")
  ) +
  ggtitle("Alcohol consumption and length of hospital stay over time")

joined_plot2


p <-joined_plot2 + ggnewscale::new_scale_colour() +
  geom_line(aes(y=days, colour = Country)) +
  scale_colour_manual("Days", values = c("red", "skyblue"))

p
