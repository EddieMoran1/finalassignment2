# Package installations
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gapminder")
install.packages("ggthemes")
install.packages("viridis")
install.packages("gridExtra")

#package libraries
library("tidyverse")
library("plotly")
library("dplyr")
library("ggplot2")
library("gapminder")
library("ggthemes")
library("viridis")
library("gridExtra")

#importing of files
unicef_indicator_1_1 <- read_csv("unicef_indicator_1 (1).csv")
unicef_metadata_1_ <- read_csv("unicef_metadata (1).csv")

#Data join
data_join <- full_join(unicef_metadata_1_, unicef_indicator_1_1, by =c("country" = "country"))

data_join <- unicef_metadata_1_ %>%
  full_join(unicef_indicator_1_1, by = c("country", "year" = "time_period"))

#Map
installed.packages("maps")
library(maps)

map_world <- map_data("world")

# Number 2 - MAP
 
ggplot(map_data_join) + 
  aes(x = long, y = lat, group = group) + 
  geom_polygon(aes(fill = obs_value)) + 
  geom_polygon(color = "black", fill = NA, size = 0.25) + 
  scale_fill_gradient(low = "gold", high = "red", na.value = "white", name = "% Overweight") +
  labs(x = "Longitude", y = "Latitude", title = "Proportion of 10-19 year olds that are overweight") + 
  theme_void() +
  theme(plot.background = element_rect(color = "black", size = 0.25, fill = NA)) 
  

# Creating the time series plot

countries_of_interest <- c("Brazil", "China", "Nigeria", "France", "Australia")
life_expectancy_data <- unicef_metadata_1_ %>%
  filter(country %in% countries_of_interest,
         !is.na(`Life expectancy at birth, total (years)`)) %>%
  select(country, year, `Life expectancy at birth, total (years)`)

# Creating the time series plot
ggplot(life_expectancy_data, aes(x = year, y = `Life expectancy at birth, total (years)`, color = country)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Life Expectancy at Birth Over Time",
       x = "Year",
       y = "Life Expectancy (years)",
       color = "Country") +
  theme(legend.title = element_blank())

# Number 3

vis_3 <- data_join %>%
  filter(year == 2019) %>%
  group_by(country) %>%
  summarise(
    Avg_GDP_per_capita = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE),
    Obs_value_avg = mean(obs_value, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Obs_value_avg, y = Avg_GDP_per_capita, color = country)) +
  geom_point(size = 2.5, alpha = 0.8) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue", linetype = "solid") +
  scale_color_viridis_d() + 
  theme_tufte() + 
  labs(
    y = "Average GDP per capita (constant 2015 US$)",
    x = "Overweight Value",
    title = "Average GDP per Capita by Country for 2019"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"), 
        text = element_text(size = 12), 
        legend.position = "none") 

ggplotly(vis_3)


vis_3 <- data_join %>%
  filter(year == 2019) %>%
  group_by(country) %>%
  summarise(
    Avg_Life_Expectancy = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE), # Change here
    Obs_value_avg = mean(obs_value, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Obs_value_avg, y = Avg_Life_Expectancy, color = country)) + # Adjusted y aesthetic
  geom_point(size = 2.5, alpha = 0.8) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue", linetype = "solid") +
  scale_color_viridis_d() + 
  theme_tufte() + 
  labs(
    y = "Average Life Expectancy", # Updated label
    x = "Overweight Value",
    title = "Average Life Expectancy by Country for 2019" # Updated title
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"), 
        text = element_text(size = 12), 
        legend.position = "none") # If you want to hide the legend

# Convert to an interactive plot with ggplotly
ggplotly(vis_3)

# Number 4 

colors <- c("Australia" = "#FFD700", # Gold
            "Brazil" = "#FFB14E", # A shade between gold and red
            "France" = "#FF8C00", # Darker shade towards red
            "China" = "#FF4500", # Red-Orange
            "Nigeria" = "#FF0000") # Red

# Assuming 'data_join' is pre-filtered and summarized as needed
vis_4 <- data_join %>%
  filter(year == 2019, country %in% c("Brazil", "China", "Nigeria", "France", "Australia")) %>%
  group_by(country) %>%
  summarise(Avg_obs_value = mean(obs_value, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(country, -Avg_obs_value), y = Avg_obs_value, fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
  scale_fill_manual(values = colors) + # Use the custom color palette
  theme_minimal() +
  labs(y = "Average Obs Value", x = "Country", title = "% Overweight by Country for 2019") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Times New Roman", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.title.x = element_blank())

# Convert to ggplotly
ggplotly(vis_4)

