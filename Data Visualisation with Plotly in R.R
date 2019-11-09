# R00181392, Svetlna Ivanov
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  
#       Visualisation of gapminder Data using plotly library in R
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loading packages


library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(readr)
library(choroplethr)
library(choroplethrMaps)

#install.packages("choroplethr")
#install.packages("choroplethrMaps")

# 
path = "C:/Users/User/Documents/R/CIT/Visualisation/Project1/GAPminder/"

# Reading data from files

countries_gdp <-read_csv(file = paste(path, "income_per_person_gdppercapita_ppp_inflation_adjusted.csv", sep=""))
countries_population <-read_csv(file=paste(path, "population_total.csv", sep=""))
countries_food <-read_csv(file = paste(path, "food_supply_kilocalories_per_person_and_day.csv", sep=""))
countries_region <- read_csv(file = paste(path,"countries by region.csv", sep = ""))
countries_lexp <- read_csv(file = paste(path, "life_expectancy_years.csv", sep = ""))
countries_health <- read_csv(file = paste(path, "government_health_spending_of_total_gov_spending_percent.csv", sep = ""))
countries_growth <- read_csv(file =paste(path,"population_growth_annual_percent.csv", sep = ""))
countries_codes<- read_csv(file =paste(path,"Country_codes.csv", sep = ""))


# drop_columns function

drop_columns<- function(df_name, years=c(1995:2010)){
  
  df_name = subset(df_name, select=c("country", years))
}


#droping columns from data frames and selecting only years 1995-2010 for data consistency


countries_gdp <- drop_columns(countries_gdp)
countries_lexp <- drop_columns(countries_lexp)
countries_health <- drop_columns(countries_health)
countries_growth <- drop_columns(countries_growth)
countries_food <- drop_columns(countries_food)
countries_population <-drop_columns(countries_population)

# Gathering years under 2 columns
library(tidyr)

countries_gdp <- gather(countries_gdp, year, gdp, -country)
countries_lexp <- gather(countries_lexp, year, lexp, -country)
countries_health <- gather(countries_health, year, health, -country)
countries_growth <- gather(countries_growth,year, growth, -country)
countries_food <- gather(countries_food, year, food, -country)
countries_population <- gather(countries_population, year, pop, -country)

# Merging all data into one data frame

df <- merge(countries_gdp, countries_lexp, by = c("country", "year"), all=TRUE)
df1 <- merge(df, countries_health, by = c("country", "year"), all=TRUE)
df2 <- merge(df1, countries_growth, by = c("country", "year"), all=TRUE)
df3 <- merge(df2, countries_food, by = c("country", "year"), all=TRUE)
df4 <- merge(df3, countries_region, by.x =  "country", by.y="Country", all=TRUE)
df5 <- merge(df4, countries_population, by = c("country", "year"), all=TRUE)
df_total <- merge(df5, countries_codes, by.x =  "country", by.y="Country", all=TRUE)



summary(df_total)

#   Starting from simple
#    Boxplot by regions

df_total %>%
  filter(year==2010)%>%
  group_by(Region)%>%
  plot_ly(x= ~Region, y = ~ lexp, color = ~ Region) %>%
  add_boxplot()%>%
  layout(xaxis=list(title = "Regions"),
         yaxis=list(title = "Life Expectancy"),
         title = "Life expectancy by regions in 2010")

  
# Simple scatterplot with hoverinfo
# Income vs Life expectancy by government spending for health

df_total %>%
  filter(year == 2010) %>%
  plot_ly(x=~gdp, y=~lexp, color=~health,
          hoverinfo = "text",
          text = ~paste("Country:", country, "<br>",
                        "Region:", Region, "<br>",
                        "GDPperCapita:", gdp, "<br>",
                        "Life Expectancy :", lexp, "<br>",
                        "Government health spending :", health )) %>%
  add_markers()%>%
  layout(xaxis=list(title = "Income per person, inflation adjusted", type="log"),
         yaxis=list(title = "Life Expectancy"),
         title = "Income vs Life expectancy by government spending for health in 2010")



# Static bubble chart
# Food supply versus income by countries and regions

df_total %>%
  filter(year == 2010) %>%
  plot_ly(x = ~gdp, y = ~food, 
          hoverinfo =  "text", 
          text = ~paste("Country:", country,
                  "<br> Income per person:", gdp,
                  "<br> kcal per person :", food,
                  "<br> Region :", Region)) %>%
  add_markers(size = ~pop, color = ~Region,
              marker = list(opacity = 0.6,
              sizemode = "diameter",
              sizeref = 2)) %>%
  layout(xaxis=list(title = "Income per person, inflation adjusted", type="log"),
        yaxis=list(title = "Food supply kilocalories per person and day"),
        title = "Food supply versus income for 2010 by countries and regions")

#        Animations
#
#Keyframe animation : Static graphics united in a film.
#Each frame is a static scatterplot
# Frame wil be defined by year
#
# add_markers(frame = ~year) %>%
# layout(xaxis = list(type ="log"), yaxis = list(type ="log"))
#
# For object consistancy will define ids
#
# add_markers(frame = ~year, ids = ~country, showlegend = FALSE) %>%
# layout(xaxis = list(type ="log"), yaxis = list(type ="log"))
#
#
df_total %>%
  plot_ly(x = ~gdp, y = ~food,
          hoverinfo =  "text", 
          text = ~paste("Country:", country,
                        "<br> Income per person:", gdp,
                        "<br> kcal per person :", food,
                        "<br> Region :", Region)) %>%
  add_markers(frame = ~year, 
                     size = ~pop, 
                     color = ~Region,
              marker = list(opacity = 0.6,
                            sizemode = "diameter",
                            sizeref = 2)) %>%
  layout(xaxis = list(title = "Income per person, inflation adjusted",type ="log"), 
         yaxis = list(title = "Food supply kilocalories per person and day", type ="log"),
         title = "Food supply versus income by countries and regions")

# Customisation

ani <- df_total %>%
  plot_ly(x = ~gdp, y = ~food,
          hoverinfo =  "text", 
          text = ~paste("Country:", country,
                        "<br> Income per person:", gdp,
                        "<br> kcal per person :", food,
                        "<br> Region :", Region)) %>%
  add_text(x = 20000, y = 1700, text = ~year, frame = ~year,
           textfont = list(color = toRGB("gray80"), size = 100)) %>%
  add_markers(frame = ~year, 
              size = ~pop, 
              color = ~Region,
              marker = list(opacity = 0.6,
                            sizemode = "diameter",
                            sizeref = 2)) %>%
  layout(xaxis = list(title = "Income per person, inflation adjusted",type ="log"), 
         yaxis = list(title = "Food supply kilocalories per person and day", type ="log"),
         title = "Food supply versus income by countries and regions")%>%
  animation_slider(hide = TRUE)

##

ani %>%
  animation_opts(frame = 1000,
                transition = 300,
                easing = "back",
                redraw = TRUE)

# Basic easing options:
# "linear" , "quad" , "cubic", "sin", "exp", "circle", "elastic", "back", "bounce"

# Linked charts: Crosstalk

#install.packages("crosstalk")
library(crosstalk)

shared_df <- SharedData$new(df_total)

p1 <- shared_df %>%
  plot_ly(x = ~food, y = ~lexp, color=~Region) %>%
  add_markers()%>%
  layout(xaxis = list(type ="log"), title="Life expectation vs Food")



p2 <- shared_df %>%
  plot_ly(x = ~health, y = ~lexp, color = ~Region) %>%
  add_markers( title="Life expectation vs Health", showlegend = FALSE)

bscols(filter_checkbox(id ="Region", label = "Region",
                       sharedData = shared_df, group = ~Region),p1)


bscols(filter_select(id = "country", label = "Country",
                       sharedData = shared_df, group = ~country),p1)

bscols(filter_slider(id ="food", label = "kCal",
                     sharedData = shared_df, column = ~food),p1)

bscols(widths = c(2, 5, 5),
       list(filter_checkbox(id ="Region", label = "Region",
                            sharedData = shared_df, group = ~Region),
            filter_select(id = "country", label = "Country",
                          sharedData = shared_df, group = ~country),
            filter_slider(id ="lexp", label = "Years",
                            sharedData = shared_df, column = ~lexp)),
       p1,p2)


subplot(p1, p2, titleX = TRUE, shareY = TRUE) %>% hide_legend()%>%
  highlight(on = "plotly_selected")
  


#       Choropleth maps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~



 # Function for map show
  map_show <- function(df, col_new, year_new, title_new, unit_new, pal){
  df_new <- df %>%
    filter(year == year_new) 
  df_new <- subset(df_new, select=c("country", col_new))
  names(df_new)<-c("region", "value")
  
  plotdata <- df_new %>%
    mutate(region = tolower(region))
  
  
  country_choropleth(plotdata, num_colors=9) +
    scale_fill_brewer(palette=pal) +
    labs(title = title_new,
         subtitle = paste("Gapminder data ", year_new),
         caption = "source: https://www.gapminder.org",
         fill = unit_new)
  }
  
  map_show(df_total, "lexp", 2010, "Life expectancy by country", "Years", "YlOrRd")
  map_show(df_total, "gdp", 2010, "Income per person by country", "$", "Blues")
  map_show(df_total, "food", 2010, "Food supply kCal per person", "kcal", "BuGn")
  map_show(df_total, "health", 2010, "Governement health spending", "%", "BuPu")
  map_show(df_total, "growth", 2010, "Population growth annual percent", "%", "PuBuGn")
  map_show(df_total, "pop", 2010, "Population of the world ", " ", "Paired")
  