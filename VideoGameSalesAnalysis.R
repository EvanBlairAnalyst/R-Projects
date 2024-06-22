# Loading in packages

library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(shiny)
library(tibble)
library(stringr)
library(here)
library(skimr)
library(janitor)
library(ggpubr)
library(plotly)



-------------------------------------------------------------------------

#Importing data from the csv file

vg_data <- read_csv("~/RStudio/PortfolioProject/vgsales.csv")

glimpse(vg_data)

------------------------------------------------------------------------
#Looking at top global sales by platform

vg_topsales <- vg_data %>%
    group_by(Platform) %>%
    summarise(Total_sales = sum(Global_Sales)) %>%
    arrange(desc(Total_sales))

view(vg_topsales)

#Looking at what systems had the most game releases

vg_systemreleases <- table(vg_data$Platform) %>%
  as.data.frame() %>%
  arrange(desc(Freq))

view(vg_systemreleases)

#The Nintendo DS had the most game releases with 2163 games, the PS2 is a close second with 2161 game releases.

-------------------------------------------------------------------------

# Looking at North America sales versus Global Sales

NA_percent <- sum(vg_data$NA_Sales)/sum(vg_data$Global_Sales)

view(NA_percent)

# North America accounted for 49 percent of global sales.
-------------------------------------------------------------------------
# Looking at Best Platform Sales by Country

# North America Sales

 vg_NAtopsales <- vg_data %>%
  group_by(Platform) %>%
  summarise(Total_sales = sum(NA_Sales)) %>%
  arrange(desc(Total_sales))

# Japan Sales

vg_JPtopsales <- vg_data %>%
  group_by(Platform) %>%
  summarise(Total_sales = sum(JP_Sales)) %>%
  arrange(desc(Total_sales))

# Europe Sales

vg_EUtopsales <- vg_data %>%
  group_by(Platform) %>%
  summarise(Total_sales = sum(EU_Sales)) %>%
  arrange(desc(Total_sales))



-------------------------------------------------------------------------
#Looking at total sales per year by Platform

vg_yearsales <- vg_data %>%
  filter(Year %in% 1980:2017) %>%
  group_by(Platform, Year) %>%
  summarise(Total_sales = sum(Global_Sales)) %>%
  spread(Year, Total_sales)

vg_yearsales[is.na(vg_yearsales)] <- 0

view(vg_yearsales)

-------------------------------------------------------------------------
# Looking at Sales by Country

# North American Total Sales by Platform

vg_NAsales <- vg_data %>%
  filter(Year %in% 1980:2017) %>%
  group_by(Platform, Year) %>%
  summarise(NA_Sales_Total = sum(NA_Sales)) %>%
  spread(Year, NA_Sales_Total)

vg_NAsales[is.na(vg_NAsales)] <- 0

view(vg_NAsales)

# Japan Total Sales by Platform

vg_JPsales <- vg_data %>%
  filter(Year %in% 1980:2017) %>%
  group_by(Platform, Year) %>%
  summarise(JP_Sales_Total = sum(JP_Sales)) %>%
  spread(Year, JP_Sales_Total)

vg_JPsales[is.na(vg_JPsales)] <- 0

view(vg_JPsales)

# Europe Total Sales by Platform

vg_EUsales <- vg_data %>%
  filter(Year %in% 1980:2017) %>%
  group_by(Platform, Year) %>%
  summarise(EU_Sales_Total = sum(EU_Sales)) %>%
  spread(Year, EU_Sales_Total)

vg_EUsales[is.na(vg_EUsales)] <- 0

view(vg_EUsales)

-------------------------------------------------------------------------

# Looking at Global Sales by Genre

vg_genre <- vg_data %>%
  group_by(Genre) %>%
  summarise(Global_Genre_Sales = sum(Global_Sales)) %>%
  arrange(desc(Global_Genre_Sales))

view(vg_genre)

# Now lets look at Genre Sales by Country

# North America Genre Sales

vg_NAgenre <- vg_data %>%
  group_by(Genre) %>%
  summarise(Global_Genre_Sales = sum(NA_Sales)) %>%
  arrange(desc(Global_Genre_Sales))

view(vg_NAgenre)

# Japan Genre Sales

vg_JPgenre <- vg_data %>%
  group_by(Genre) %>%
  summarise(Global_Genre_Sales = sum(JP_Sales)) %>%
  arrange(desc(Global_Genre_Sales))

view(vg_JPgenre)

# Europe Genre Sales

vg_EUgenre <- vg_data %>%
  group_by(Genre) %>%
  summarise(Global_Genre_Sales = sum(EU_Sales)) %>%
  arrange(desc(Global_Genre_Sales))

view(vg_EUgenre)

-------------------------------------------------------------------------
vg_yearbreakdown <- vg_yearsales %>%
  pivot_longer(cols = 2:39, names_to = "Year", values_to = "Sales") %>%
  group_by(Platform,Year)


# Double Check data

vg_data %>%
  group_by(Platform, Year) %>%
  summarise(Sales = sum(Global_Sales)) %>%
  filter(Platform==2600) %>%
  spread(Year,Sales)
------------------------------------------------------------------------
# Lets Visualize the data

# Line Graph of Total Global Sales by Platform

platform_G <- vg_yearsales %>%
  pivot_longer(cols = 2:39, names_to = "Year", values_to = "Sales_Millions_USD") %>%
  ggplot(vg_yearsales, mapping = aes(x = Year, y = Sales_Millions_USD, color = Platform, group = Platform)) +
  geom_line(linewidth=2) + geom_label_repel(aes(label = Platform)) + ggtitle("Global Yearly Sales by Platform") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold"))

# Label Graph of Total Global Sales by Platform

platform_G2 <- vg_yearsales %>%
  pivot_longer(cols = 2:39, names_to = "Year", values_to = "Sales_Millions_USD") %>%
  ggplot(vg_yearsales, mapping = aes(x = Year, y = Sales_Millions_USD, color = Platform, group = Platform)) +
  geom_label_repel(aes(label = Platform)) + ggtitle("Global Yearly Sales by Platform") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold"))

# Line Graph of Total North American Sales by Platform

platform_NA <- vg_NAsales %>%
  pivot_longer(cols = 2:39, names_to = "Year", values_to = "Sales_Millions_USD") %>%
  ggplot(vg_NAsales, mapping = aes(x = Year, y = Sales_Millions_USD, color = Platform, group = Platform)) +
  geom_line(linewidth=2) + geom_label_repel(aes(label = Platform)) + ggtitle("North American Yearly Sales by Platform") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold"))

# Line Graph of Total Japan Sales by Platform

platform_JP <- vg_JPsales %>%
  pivot_longer(cols = 2:39, names_to = "Year", values_to = "Sales_Millions_USD") %>%
  ggplot(vg_JPsales, mapping = aes(x = Year, y = Sales_Millions_USD, color = Platform, group = Platform)) +
  geom_line(linewidth=2) + geom_label_repel(aes(label = Platform)) + ggtitle("Japan Yearly Sales by Platform") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold"))

# Line Graph of Total European  Sales by Platform

platform_EU <- vg_EUsales %>%
  pivot_longer(cols = 2:39, names_to = "Year", values_to = "Sales_Millions_USD") %>%
  ggplot(vg_EUsales, mapping = aes(x = Year, y = Sales_Millions_USD, color = Platform, group = Platform)) +
  geom_line(linewidth=2) + geom_label_repel(aes(label = Platform)) + ggtitle("Europe Yearly Sales by Platform") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold"))

# Visualization of best selling Platforms

# Best selling Platform Global

vgsales_G <- ggplot(vg_topsales,aes(x = Platform, y = Total_sales, fill = Platform)) + labs(y="Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Total Video Game Sales by Platform (Global)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Platform North America

vgsales_NA <- ggplot(vg_NAtopsales,aes(x = Platform, y = Total_sales, fill = Platform)) + labs(y="Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Total Video Game Sales by Platform (North America)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Platform Japan

vgsales_JP <- ggplot(vg_JPtopsales,aes(x = Platform, y = Total_sales, fill = Platform)) + labs(y="Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Total Video Game Sales by Platform (Japan)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Platform Europe

vgsales_EU <- ggplot(vg_EUtopsales,aes(x = Platform, y = Total_sales, fill = Platform)) + labs(y="Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Total Video Game Sales by Platform (Europe)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Genre Globally

genre_G <-ggplot(vg_genre,aes(x= Genre, Global_Genre_Sales, fill = Genre)) + labs(y = "Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Sales by Genre (Global)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Genre North America

genre_NA <-ggplot(vg_NAgenre,aes(x= Genre, Global_Genre_Sales, fill = Genre)) + labs(y = "Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Sales by Genre (North America)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Genre Japan

genre_JP <-ggplot(vg_JPgenre,aes(x= Genre, Global_Genre_Sales, fill = Genre)) + labs(y = "Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Sales by Genre (Japan)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

# Best Selling Genre Europe

genre_EU <-ggplot(vg_EUgenre,aes(x= Genre, Global_Genre_Sales, fill = Genre)) + labs(y = "Total Sales in Millions (USD)") +
  geom_bar(stat= "identity") +ggtitle("Sales by Genre (Europe)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size=10)) + theme(plot.title = element_text(size = 30, face = "bold")) + theme(axis.title = element_text(size = 12, face = "bold"))

------------------------------------------------------------------------------------------------------------------------------

# Combining all charts for total video game sales by platform

ggarrange(vgsales_G, vgsales_NA, vgsales_JP, vgsales_EU, legend = "none")

# Based on this data We can see that in the global video game market the PS2 had the most video game sales with 1.26 Billion USD.
# In the North American market the Xbox 360 had the most video game sales with 600 Million USD, closely followed by the PS2.
# In the Japan Market the Nintendo DS had the most video game sales with 175 million USD.
# In the European Market the PS3 had the most video game sales with 345 million USD, closely followed by the PS2.

------------------------------------------------------------------------------------------------------------------------------

# Combining all charts for sales by Genre

ggarrange(genre_G, genre_NA, genre_JP, genre_EU, legend = "none")

# Action was the most popular genre, with the most sales in the Global, North American (826 Million USD), and European markets(520 Million USD). Total global sales of 1.75 Billion.
# The most popular Genre in Japan was Role-Playing, with total sales of 352 million.

------------------------------------------------------------------------------------------------------------------------------

# Combining all yearly sales by platform charts

ggarrange(platform_G, platform_NA, platform_JP, platform_EU, legend = "none")

# With this data we can see yearly sales of different platforms in the global, north american, japan, and European markets.
# In the global market PS2 peaked sales in 2004 with 211.78 Million USD, the Wii peaked sales in 2009 with 210.44 Million USD, and Xbox 360 peaked sales in 2010 with 171.05 Million USD.
# In the North american market the PS2 peaked sales in 2004 with 96.78 Million USD, the Wii peaked sales in 2009 with 116.54 Million USD, and the Xbox 360 peaked sales in 2010 with 107.21 Million USD
# In the Japan market the PS2 was not as popular, peaking in 2002 with sales of 21.85  Million USD. The Wii peaked sales in 2009 with sales of 16.57 million, but was overthrown by the DS in sales which peaked in 2006 with 38.56 Million USD.
# In the European market the PS2 did very well with video game sales peaking in 2002 with 65.27 million USD. The Wii peaked in 2009 with 59.36 million USD in sales. And the PS3 peaked in 2011 with sales of 58.11 Million USD.

------------------------------------------------------------------------------------------------------------------------------

# Finding the best selling video games between 1980-2017

vg_topgamesales <- vg_data %>%
  filter(Year %in% 1980:2017) %>%
  group_by(Name)

view(vg_topgamesales)

# Top Selling game globally is Wii Sports with 82.74 Million USD in sales
# Wii Sports is also the best selling in North America (41.49 Million USD) and Europe (29.02 Million USD).
# Top selling game in Japan was Pokemon Red/Pokemon Blue with sales of 10.22 Million USD.

------------------------------------------------------------------------------------------------------------------------------

# Visualize best selling video games globally

topgame_global <- aggregate(list(Global_Sales = vg_data$Global_Sales), list(Name = vg_data$Name), sum)
topgame_global <- topgame_global[order(topgame_global$Global_Sales, decreasing = T), ]

topgames <- ggplot(data = head(topgame_global, 10), mapping = aes(x = Name, y = Global_Sales)) +
  geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), linewidth = 1.1, alpha = .7) +
  geom_label(mapping = aes(label = Global_Sales), size = 6, fontface = "bold") +
  xlab("") +
  ylab("Sales Globally In Millions (USD)") +
  ggtitle("The 10 Best Selling Games Globally") +
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face = "bold", hjust = .5),
        axis.text.x = element_text(size = 15, face = "bold", angle = 20),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 20))
