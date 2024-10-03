
# Project Topic: U.S. Electricity Price 

# By:  Namrata Adhikari

#____________________________________________


# Install and Load the packages to be used. 

install.packages("finalfit")
install.packages("viridis")
install.packages("reshape2")
library(reshape2)
library(viridis)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(finalfit)



# Load and view the dataset.
Electricity <- clean_data
View(Electricity)



# Exploratory Data Analysis.
#_____________________________

## Identify Missing values ###########

glimpse(Electricity)

missing_glimpse(Electricity)  # To identify missing values in each variables.

missing_plot(Electricity)    # To visualize missing values in the data set.

Electricity %>% filter(is.na(customers)) # Another way to look for missing values 



# I doubt if 2024 consists complete data since it's only April. Let's check!

year <- Electricity %>%                   
  group_by(year) %>%
  summarize(observations = n())

print(year)


# Descriptive Statistics. 
summary(Electricity) 



# Visualization: Histogram 

Electricity%>%
  ggplot(aes(x = price)) +
  geom_histogram( fill = "coral1", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of U.S. Electricity Price (2001-2023)",
       x = "PRICE/kWh in cents",
       y = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  


Electricity %>%
  ggplot(aes(x = sales)) +
  geom_histogram( fill = "cornflowerblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of U.S. Electricity Sales (2001-2023)",
       x = "Sales in millions kWh",
       y = NULL)+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  



Electricity %>%
  ggplot(aes(x = revenue)) +
  geom_histogram( fill = "darkolivegreen3", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of U.S. Electricity Revenue (2001-2023)",
       x = "Revenue in millions $",
       y = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  






######  Data Preparation #######
#________________________________

# Handle Missing Values 

Electricity <- Electricity %>% select(-customers)  # Remove the customers column

Electricity <- Electricity %>%       # Filter out observations for the year 2024
  filter(year < 2024)

View(Electricity)


# Rename variables  
Electricity <- Electricity %>% 
  rename(state = stateDescription, sector = sectorName)


# Change the data type. 
Electricity$state <- as.factor(Electricity$state)
Electricity$sector <- as.factor(Electricity$sector)


# Create a new Season variable. 

Electricity <-  Electricity %>%
  mutate(season = case_when(
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    month %in% c(12, 1, 2 ) ~ "Winter"))


Electricity$season <- as.factor(Electricity$season)


# Re-code the values for month variable 
Electricity$month <- month.name[Electricity$month]
Electricity$month <- as.factor(Electricity$month)


# Handle Inconsistencies 
#____________________________

## Convert price from Cents to dollars. 
Electricity <- Electricity %>% mutate(price_dollars_kWh = price/100)


## Round the values. 
Electricity <- Electricity %>% 
  mutate(REVENUE = round(revenue, 2), SALES = round(sales, 2), PRICE = round(price_dollars_kWh,2))


##Modify the sector column

# Re code the sector column to include transportation in "other"

Electricity <- Electricity %>%
  mutate(sector = case_when(
    sector == "transportation" ~ "other",
    TRUE ~ as.character(sector)
  ))

Electricity$sector <- as.factor(electricity_data$sector)

summary(Electricity$sector)



## Select and keep only the required variables. 
U.S_Electricty <- Electricity %>%
  filter(state %in% c("New England", "Middle Atlantic", "South Atlantic",
                      "East North Central", "East South Central", "West North Central",
                      "West South Central", "Mountain", "Pacific Contiguous", 
                      "Pacific Noncontiguous", "District of Columbia")) %>%
  select(year, season, state, sector, PRICE, SALES, REVENUE)



U.S_Electricity_allSectors <- U.S_Electricty %>% filter( sector == "all sectors")




## DATA is ready for USE and ANALYSIS!!! ###########################################

View(U.S_Electricty)
View(U.S_Electricity_allSectors)



## Step 9: Save the final data set!!! ##############################################

write.csv(U.S_Electricty, "Final_U.S_Electricty.csv") # Final data set

write.csv(U.S_Electricity_allSectors, "Final_U.S_Electricity_allSectors.csv") 

write.csv(Electricity, "Electricity.csv") # Data preparation data set






###### Data Modeling #######
#____________________________



# Data Exploration  ############################################################

summary(U.S_Electricity_allSectors)
summary(U.S_Electricty)
glimpse(U.S_Electricty)



# Box plot ############################################################

U.S_Electricity_allSectors %>%
  ggplot(aes(PRICE)) +
  geom_boxplot(fill = "coral1") +
  theme_minimal() +
  labs(title = "Boxplot of U.S. Electricity Price (2001-2023)",
       x = "PRICE ($/kWh)",
       y = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  


U.S_Electricity_allSectors %>%
  ggplot(aes(SALES)) +
  geom_boxplot(fill = "cornflowerblue") +
  theme_minimal() +
  labs(title = "Boxplot of U.S. Electricity Sales (2001-2023)",
       x = "Sales (million kWh)",
       y = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  



U.S_Electricity_allSectors %>%
  ggplot(aes(REVENUE)) +
  geom_boxplot(fill = "darkolivegreen3") +
  theme_minimal() +
  labs(title = "Boxplot of U.S. Electricity Revenue (2001-2023)",
       x = "Revenue (million $)",
       y = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  




# Correlation ###########################################################################

correlation_price_sales <- cor(U.S_Electricity_allSectors$PRICE, U.S_Electricity_allSectors$SALES)



# Scatter plot of PRICE vs. SALES ####################################################

U.S_Electricity_allSectors %>% 
  ggplot(aes(x = PRICE, y = SALES)) +
  geom_point(shape = 15) + 
  geom_smooth(method = "lm", color = "red") +
  #facet_wrap(~sector)
  theme_minimal() +
  labs( x = "Electricity Price $/kWh", y = "Electricity Sales (million kWh)",
        title = "Relationship between U.S. Electricity Price and Sales",
        subtitle = "2001-2023")




# Regression Analysis ################################################################

regression <- lm(SALES ~ PRICE, data = U.S_Electricity_allSectors)

summary(regression)


multiple_regression <- lm(REVENUE ~ PRICE + SALES, data = U.S_Electricity_allSectors)

summary(multiple_regression)



# PRICE, SALES, and REVENUE trend over the years #####################################


TREND <- U.S_Electricity_allSectors %>%
  group_by(year) %>%
  summarise(AVG_PRICE =  round(mean(PRICE), 3),
            TOTAL_SALES =  round(sum(SALES), 3), 
            TOTAL_REVENUE =  round(sum(REVENUE), 3))



# Time-series plot for Average Price #########

TREND %>% 
  ggplot(aes(year, AVG_PRICE)) +
  geom_line() +
  geom_point(color ="brown", size = 3) +
  
  labs(title = "U.S. Electricity Price Trend (2001 - 2023)",
       x = NULL,
       y = "Price/kWh in $") +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  




# Market share / Electricity Consumption ########################################
#_________________________________________________________________________________

# Time-series plot for Total Sales/Consumption ############

TREND %>% 
  ggplot(aes(year, TOTAL_SALES)) +
  geom_line() +
  geom_point(color ="cornflowerblue", size = 3) +
  labs(title = "U.S. Electricity Consumption Trend (2001 - 2023)",
       x = NULL,
       y = "Sales in millions kWh") +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  



# Consumption by (Regions) #################

Market_share_state <- U.S_Electricity_allSectors %>%
  group_by(state) %>%
  summarise(total_sales = round(sum(SALES), 3)) %>%
  mutate(percentage_of_total_sales = round((total_sales / sum(total_sales)) * 100, 3)) %>%
  arrange(desc(percentage_of_total_sales))


# Visualization
ggplot(Market_share_state, aes(x = reorder(state, percentage_of_total_sales), y = percentage_of_total_sales, fill = percentage_of_total_sales)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = paste0(percentage_of_total_sales, "%")), color = "black", size = 3, vjust = -0.5) +
  scale_fill_gradient(low = "beige", high = "cornflowerblue", na.value = "white") +  
  theme_minimal() +
  labs(x = NULL, y = NULL, 
       title = "U.S. Electricity Consumption by Region (2001-2023)") +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  # Bold title, center it, and add space after




# Consumption by (Regions/Sector) #################

Market_share_state <- U.S_Electricty %>%
  group_by(state, sector) %>%
  filter(sector %in% c("commercial", "industrial", "other", "residential")) %>%
  summarise(total_sales = round(sum(SALES), 3)) %>%
  mutate(percentage_of_total_sales = round((total_sales / sum(total_sales)) * 100, 3)) 
#arrange(desc(percentage_of_total_sales))


# Visualization
ggplot(Market_share_state, aes(x = reorder(sector, -percentage_of_total_sales), y = percentage_of_total_sales, fill = sector, label = paste0(percentage_of_total_sales, "%"))) +
  geom_bar(stat = "identity") +  
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  
  scale_fill_manual(values = c("darkgray", "burlywood3", "azure3", "cornflowerblue")) +  
  facet_wrap(~ state, scales = "free") +  
  labs(x = NULL, y = NULL,  
       title = "\nElectricity Consumption by U.S. Region",
       subtitle = "\nSector-wise Analysis\n") +  # Centered subtitle
  theme(axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  # Centered subtitle




# Consumption by (Season) ##################

U.S_Electricity_allSectors %>%
  group_by(season) %>%
  summarise(total_sales = sum(SALES),
            average_sales = mean(SALES)) %>%
  mutate(percentage_of_total_sales = (total_sales / sum(total_sales)) * 100) %>%
  ggplot(aes(reorder(season, -percentage_of_total_sales), percentage_of_total_sales, fill = season)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(aes(label = paste0(round(percentage_of_total_sales, 2), "%")), 
            position = position_dodge(width = 0.5), 
            vjust = -0.5, 
            size = 3,
            fontface = "bold") + 
  geom_text(aes(label = round(average_sales, 2)), 
            position = position_dodge(width = 0.5), 
            vjust = 3, 
            size = 2.5) +
  scale_fill_brewer(palette = "Blues") +  
  theme_minimal() +
  labs(x = NULL, y = NULL, 
       title = "U.S. Electricity Consumption by Season (2001-2023)", 
       subtitle = "With Average Sales (million kWh) figure") +
  theme(plot.subtitle = element_text(margin = margin(b = 20))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill = FALSE)





# Consumption by (Season/Sector) ###################

U.S_Electricty %>%
  group_by(season, sector) %>%
  summarise(total_sales = sum(SALES),
            average_sales = mean(SALES)) %>%
  mutate(percentage_of_total_sales = (total_sales / sum(total_sales)) * 100) %>%
  filter(sector %in% c("commercial", "industrial", "other", "residential")) %>%
  ggplot(aes(reorder(sector, -percentage_of_total_sales), percentage_of_total_sales, fill = sector)) +
  geom_bar(show.legend = FALSE, stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(round(percentage_of_total_sales, 2), "%")), 
            position = position_dodge(width = 0.5), 
            vjust = -0.5, 
            size = 2,
            fontface = "bold") + 
  geom_text(aes(label = round(average_sales, 2)), 
            position = position_dodge(width = 0.5), 
            vjust = 1.5, 
            size = 2) +
  scale_fill_manual(values = c("darkgray", "burlywood3", "azure3", "cornflowerblue")) + 
  facet_wrap(~ season, scales = "free_y") +
  theme_bw() +
  labs(x = NULL, y = NULL, 
       title = "Electricity Consumption by Season", 
       subtitle = "Sector-wise Analysis") +
  theme(plot.subtitle = element_text(margin = margin(b = 20))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill = FALSE)





# Electricity Revenue ############################################################
#___________________________________________________________________________________

# Time-series plot for Total Revenue #############

TREND %>% 
  ggplot(aes(year, TOTAL_REVENUE)) +
  geom_line() +
  geom_point(color ="darkolivegreen3", size = 3) +
  labs(title = "U.S. Electricity Revenue Trend (2001 - 2023)",
       x = NULL,
       y = "Revenue in million $") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)))  



# Average revenue by Season ####################
U.S_Electricity_allSectors %>%
  group_by(season) %>%
  summarise(AVG_revenue = mean(REVENUE)) %>%
  ggplot(aes(reorder(season, -AVG_revenue), AVG_revenue, fill = season)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(aes(label = sprintf("%.2f", AVG_revenue)), 
            position = position_dodge(width = 0.5), 
            vjust = -0.5, 
            size = 3,
            fontface = "bold") + 
  scale_fill_brewer(palette = "Greens") +  
  theme_minimal() +
  labs(x = NULL, y = NULL, 
       title = "U.S. Electricity Revenue by Season (2001-2023)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.subtitle = element_text(margin = margin(b = 20))) +
  guides(fill = FALSE)  # Remove legend



# Average revenue by (Season/sector) ###################
U.S_Electricty %>%
  group_by(season, sector) %>%
  summarise(AVG_revenue = mean(REVENUE)) %>%
  filter(sector %in% c("commercial", "industrial", "other", "residential")) %>%
  ggplot(aes(reorder(sector, -AVG_revenue), AVG_revenue, fill = sector)) +
  geom_bar(show.legend = FALSE, stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", AVG_revenue)), 
            position = position_dodge(width = 0.5), 
            vjust = -0.3, 
            size = 2,
            fontface = "bold") + 
  scale_fill_manual(values = c("darkkhaki", "darkseagreen", "azure3", "darkolivegreen3")) + 
  facet_wrap(~ season, scales = "free_y") +
  theme_bw() +
  labs(x = NULL, y = NULL, 
       title = "Electricity Revenue by Season", 
       subtitle = "Sector-wise Analysis") +
  theme(plot.subtitle = element_text(margin = margin(b = 20))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill = FALSE)




# Highest revenue/kWh by (Region) ##########
Revenue_Sales_State <- U.S_Electricty %>%
  group_by(state) %>%
  summarise(total_sales = round(sum(SALES), 3),
            Revenue_kWh = sum(REVENUE) / sum(SALES)) %>%
  mutate(percentage_of_total_sales = round((total_sales / sum(total_sales)) * 100, 3)) %>%
  arrange(desc(Revenue_kWh)) %>%
  mutate(Revenue_kWh = round(Revenue_kWh, 3))



# Visualization revenue/kWh vs % of total sales


# Filter data to include only the highest and lowest points
highest_lowest_points <- Revenue_Sales_State %>%
  filter(percentage_of_total_sales == max(percentage_of_total_sales) | percentage_of_total_sales == min(percentage_of_total_sales))

ggplot(Revenue_Sales_State) +
  geom_bar(aes(x = state, y = Revenue_kWh, fill = "Revenue_kWh"), stat = "identity", 
           width = 0.5, alpha = 0.7) +
  geom_line(aes(x = state, y = percentage_of_total_sales * max(Revenue_Sales_State$Revenue_kWh) / max(Revenue_Sales_State$percentage_of_total_sales), 
                group = 1, color = "Percentage_of_Total_Sales"), size = 1.5) +
  geom_text(data = highest_lowest_points, aes(x = state, y = percentage_of_total_sales * max(Revenue_Sales_State$Revenue_kWh) / max(Revenue_Sales_State$percentage_of_total_sales), 
                                              label = paste0(round(percentage_of_total_sales, 2), "%")), vjust = -0.5, size = 3, color = "blue") +  
  geom_text(aes(x = state, y = Revenue_kWh, label = Revenue_kWh), vjust = -0.5, size = 3, color = "black") +
  scale_fill_manual(values = "darkolivegreen3") +
  scale_color_manual(values = "cornflowerblue") +
  labs(x = NULL, y = "Revenue ($)",
       fill = NULL, color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y = element_blank(),  # Remove y-axis label
        plot.title = element_text(hjust = 0.5, margin = margin(b = 20), face = "bold")) +  # Center title, add margin, and make it bold
  scale_y_continuous(sec.axis = sec_axis(~./max(Revenue_Sales_State$Revenue_kWh) * max(Revenue_Sales_State$percentage_of_total_sales), name = "Percentage of Total Sales")) +
  theme(legend.position = "bottom") +
  labs(title = "Electricity Revenue($) VS Electricity Consumption Rate by U.S. Regions")





# Electricity Price #########################################################
#_____________________________________________________________________________

# Electricity Price distribution across regions ##########

U.S_Electricty %>%
  ggplot(aes(x = state, y = PRICE)) +
  geom_boxplot(fill = "coral1", outlier.color = "brown") +
  theme_minimal() +
  labs(title = "Electricity Price distribution across U.S. Regions", 
       x = "", y = "Price ($/kWh)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20))) +
  coord_flip()



# Average Price/kWh by Season ##########
U.S_Electricity_allSectors %>%
  group_by(season) %>%
  summarise (Average_price_kWh = round(mean(PRICE), 3)) %>%
  ggplot(aes(reorder(season, -Average_price_kWh), Average_price_kWh, fill = season)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0("$", sprintf("%.3f", Average_price_kWh))), 
            hjust = 0.5, 
            vjust = -0.5,
            color = "black", size = 3) +
  theme_minimal() +
  scale_fill_brewer(palette = "Oranges") +
  theme(panel.border = element_blank()) +
  labs(x = NULL, y = NULL, 
       title = "U.S. Electrictiy Price by Season (2001-2023)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20))) 



# Average price by (Season/sector) ###################
U.S_Electricty %>%
  group_by(season, sector) %>%
  summarise(AVG_price = mean(PRICE)) %>%
  filter(sector %in% c("commercial", "industrial", "other", "residential")) %>%
  ggplot(aes(reorder(sector, -AVG_price), AVG_price, fill = sector)) +
  geom_bar(show.legend = FALSE, stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = paste0("$", sprintf("%.3f", AVG_price))), 
            hjust = 0.5, 
            vjust = -0.5,
            color = "black", size = 2) +
  scale_fill_manual(values = c("bisque", "darksalmon", "darkorange3", "coral1")) + 
  facet_wrap(~ season, scales = "free_y") +
  theme_bw() +
  labs(x = NULL, y = NULL, 
       title = "Electricity Price by Season", 
       subtitle = "Sector-wise Analysis") +
  theme(plot.subtitle = element_text(margin = margin(b = 20))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill = FALSE)





