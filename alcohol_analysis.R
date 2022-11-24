#load packages
library(tidyverse)
library(dbplyr)
library(ggthemes)

#read in hospital stay data and clean to show X countries of interest
hospital_days <- read_csv("hospital_days.csv")
glimpse(hospital_days)

clean_hospital_days <- hospital_days %>%  
  select(Country, Year, Value)

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
  filter(Country == "France" | Country == "Germany" | Country == "United Kingdom" | Country == "Netherlands" | Country == "Switzerland" | Country == "Denmark" | Country == "Sweden") %>% 
  filter(Measure == "Litres per capita (15+)")



#plot alcohol consumption by country over time. # is for visualisation, highlighting countries of interest.
plot_consumption <- clean_alcohol_consumption %>% 
  ggplot(aes(Year, Value, color = Country)) +
  scale_y_continuous(name = "Alcohol Consumption (Litres)") +
  geom_point() +
  geom_line() +
  ggtitle("Alcohol Consumption Over Time") +
  #gghighlight::gghighlight(Country == "United Kingdom" | Country == "Netherlands", keep_scales = TRUE)+
  theme_clean()
  
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")


plot_consumption_colour <- plot_consumption + scale_color_manual(values = cbp1)
plot_consumption_colour

#next want to combine to compare both 
#why have stays dropped but consumption stabilised in some places but not others?

#Join hospital duration and alcohol consumption data
joined <- full_join(clean_hospital_days, clean_alcohol_consumption, by = c("Country" = "Country", "Year" = "Year")) %>% 
  rename(days = Value.x,
         consumption = Value.y) 

#make pretty colours for plot
library(RColorBrewer)
#define custom color scale
myColors <- brewer.pal(3, "GnBu")
names(myColors) <- levels(joined$Country)
custom_colors <- scale_colour_manual(values = myColors)

#Plot hospital duration and alcohol consumption data for UK and Netherlands
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
  ggtitle("Alcohol consumption and length of hospital stay over time") +
  theme_clean()+
  custom_colors

joined_plot2
#Fix legend
p <-joined_plot2 + ggnewscale::new_scale_colour() +
  geom_line(aes(y=days, colour = Country)) +
  scale_colour_manual("Days", values = c("red", "skyblue"))
p


# work with the discharge data
#read in hospital discharged per 100k of population
hospital_discharged <- read_csv("hospital_discharges_per_100k_pop.csv")

#clean hospital discharges and only select UK and Netherlands
clean_hospital_discharged <- hospital_discharged %>% 
  select(Country, Year, Value, Measure) %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands")
clean_hospital_discharged


#plot hospital discharges
plot_hospital_discharged <- clean_hospital_discharged %>% 
  ggplot(aes(Year, Value, color = Country)) +
  scale_y_continuous(name = "Hospital discharges per 100,000 of the population") +
  geom_point() +
  geom_line() +
  ggtitle("Hospital discharges per 100,000 of the population over time") +
  #gghighlight::gghighlight(Country == "United Kingdom" | Country == "Netherlands", keep_scales = TRUE)+
  theme_clean()

plot_hospital_discharged

#combine to have discharges, duration, and consumption on one graph
joined_days_duration_consumption <- joined %>% 
  full_join(clean_hospital_discharged, by = c("Country" = "Country", "Year" = "Year")) #%>% 
joined_days_duration_consumption
view(joined_days_duration_consumption)
#plot

coeff2 <- 10
joined_plot3 <- joined_days_duration_consumption %>% 
  filter(Country == "United Kingdom" | Country == "Netherlands") %>% 
  ggplot(aes(x=Year)) +
  geom_line(aes(y=Value/coeff2, color = Country)) +
  geom_point(aes(y=consumption, color = Country)) +
  scale_colour_manual(name="Consumption", values = c("#009E73", "#D55E00")) +
  scale_y_continuous(
    name = "Alcohol Consumption (litres)",
    sec.axis = sec_axis(~.*coeff2, name = "Hospital Discharges per 100,000 population")
  ) +
  ggtitle("Alcohol consumption and hospital discharges over time") +
  theme_clean()
joined_plot3
#fix legend
p3 <-joined_plot3 + ggnewscale::new_scale_colour() +
  geom_line(aes(y=Value/coeff2, colour = Country)) +
  scale_colour_manual("Discharges", values = c("#009E73", "#D55E00"))

p3

