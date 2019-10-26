# Install necessary packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggmap)
library(maps)
library(ggrepel)
library(RColorBrewer)

# Reading in Crash Table and Vision Zero Table
Crashes <- read.csv("Crashes_in_DC.csv", sep = "\t", stringsAsFactors = F)
VisionZero <- read.csv("Vision_Zero_Safety.csv", sep = "\t", stringsAsFactors = F)
# Transforming Crashes and VisionZero tables to dataframes
Crashes <- as.data.frame(Crashes)
VisionZero <- as.data.frame(VisionZero)

# Using Lubridate to transform Crashes and Vision Zero date fields to POSIX datetimes
VisionZero$REQUESTDATE <- ymd_hms(VisionZero$REQUESTDATE)
Crashes$DATE <- ymd_hms(Crashes$DATE)

# Selecting most important columns of Crashes Table
Crashes <- Crashes %>% select(1:27)
str(VisionZero)
# Filtering out dates until there was a full year of data (2009)
Crashes <- Crashes %>% filter(DATE > ymd_hms("2009-01-01 00:00:00"))
# Making 3 Separate Tables from Crashes table for crashes where there were bikes involved,
# bike injuries involved, and bike fatalities involved.
Bike_Crashes <- Crashes %>% filter(TOTAL_BICYCLES > 0)
Bike_Injuries <- Crashes %>% filter(MAJORINJURIES_BICYCLIST > 0)
Bike_Fatalities <- Crashes %>% filter(FATAL_BICYCLIST > 0)

# Making Bike_VisionZero table for both only "Biking_Request_Issue" reports and only
# reports submitted by bikers (ended up using the second table because I am assuming that
# Vision Zero reports submitted by bikers are going to be the ones where the issue is most
# dangerous to bikers)
Biking_Request_Issues <- c("Blocking the bikebox", "Other Biking Issue", "Red light running", "Stop sign running")

Bike_VisionZero <- VisionZero %>% filter(REQUESTTYPE %in% Biking_Request_Issues)
Bike_VisionZero2 <- VisionZero %>% filter(USERTYPE == "Biker")


# Yearly frequency table of bike crashes
Yearly_Freq <- Bike_Crashes %>% group_by(year(DATE)) %>% summarize(number = n())



# Getting ggmap API key and Registering DC map
register_google(key = "Your-Key-Here")
DC_map <- ggmap(get_map(location = c(lon = -77.019, lat = 38.9072), zoom = 12, maptype = "roadmap", scale = 2, color = 'color'))

# Reading in table with Intersection I think are the most dangerous from the Vision Zero Dotplot
Vision_Zero_Intersections <- read_csv("Vision_Zero_Intersection.csv")
# Adding in last intersection
Vision_Zero_Intersections <- add_row(Vision_Zero_Intersections, LONGITUDE.I = -77.034548, LATITUDE.I = 38.905646, Intersection = "15th St. b/t Rhode Island Ave. & I St. NW")

# Adding in extra "Ward" column
Ward <- c("One", "Three", "Two", "Six", "Six", "Six", "Two", "Two", "Two", "Two", "Two", "Two")
Vision_Zero_Intersections <- cbind(Vision_Zero_Intersections, Ward)

# Converting the Ward column to factors for the scale_shape_manual() function in plot
Vision_Zero_Intersections$Ward <- as.factor(Vision_Zero_Intersections$Ward)

# Releveling the factor order of the Wards
Vision_Zero_Intersections$Ward <- fct_relevel(Vision_Zero_Intersections$Ward, "One", "Two", "Three", "Six")

# Defining Plot Base Theme
theme_dcmap <- function() {
  theme_minimal() +
    theme(text = element_text(family = "Garamond", color = "gray25"),
          plot.title = element_text(size = 24, face = "bold"), 
          plot.subtitle = element_text(size = 18),
          plot.caption = element_text(color = "gray30", size = 10),
          plot.background = element_rect(fill = "gray95"),
          plot.margin = unit(c(2, 13, 2, 13), "lines"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.position = "right",
          legend.text = element_text(color = "gray30", size = 10)
    )
}


Vision_Zero_Dotmap <- DC_map + geom_point(data = Bike_VisionZero2, aes(x = LONGITUDE.VZ, y = LATITUDE.VZ), color = "blue", alpha = 0.25) +
  geom_point(data = Vision_Zero_Intersections, aes(x = LONGITUDE.I, y = LATITUDE.I, shape = Intersection, stroke = 7, color = Ward), size = 7) +
  theme_dcmap() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = LETTERS[1:12]) +
  xlab('') +
  ylab('') + labs(title = "Vision Zero Submissions by Bikers", subtitle = "2015-2019", caption = "Data from https://opendata.dc.gov/datasets")

  
# Importing the Intersections that I think are additionally dangerous based on the
# total crashes data
Crash_Intersections <- read_csv("Crash_Intersections.csv")

# Adding in Ward numbers for Crash_Intersections
Ward <- c("One", "Three", "Five", "Six", "Three", "Seven", "Seven", "Seven", "Eight", "Eight")
Crash_Intersections <- cbind(Crash_Intersections, Ward)

# Making Crash Intersections factors so the scale_shape_manual can detect them
Crash_Intersections$Intersection <- as.factor(Crash_Intersections$Intersection)
# Releveing the factor order of the Wards
Crash_Intersections$Ward <- fct_relevel(Crash_Intersections$Ward, "One", "Three", "Five", "Six", "Seven", "Eight")
# Combining two dataframes into one dual intersection dataframe
All_Intersections <- rbind.data.frame(Vision_Zero_Intersections, Crash_Intersections)
# Releveling the factor order of the Wards
All_Intersections$Ward <- fct_relevel(All_Intersections$Ward, "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")

Crash_Dotmap <- DC_map + stat_density2d(
  aes(x = LONGITUDE.C, y = LATITUDE.C, fill = ..level.., alpha = ..level..),
  size = 0.01, bins = 100, data = Bike_Crashes,
  geom = "polygon", show.legend = F
  ) + theme_dcmap() + 
  geom_density2d(data = Bike_Crashes, aes(x = LONGITUDE.C, y = LATITUDE.C), size = 0.3, show.legend = F) +
  geom_point(data = Bike_Fatalities, aes(x = LONGITUDE.C, y = LATITUDE.C), color = "red") + 
  geom_point(data = Bike_Injuries, aes(x = LONGITUDE.C, y = LATITUDE.C), color = "orange", alpha = 0.3) +
  geom_point(data = Crash_Intersections, aes(x = LONGITUDE.I, y = LATITUDE.I, stroke = 6, color = Ward, shape = Intersection), size = 6) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = LETTERS[1:10]) +
  xlab('') +
  ylab('') + labs(color = "Ward", shape = "Intersection", title = "Bike Crashes in DC", subtitle = "2009-2019", caption = "Red Dots = Fatality, Yellow Dots = Major Injury, Data from https://opendata.dc.gov/datasets")

# Customized colors for the time-heatmap
col1 = "#d8e1cf" 
col2 = "#438484"
# Filtering out dates until there was a full year of data (2009)
bikeCrashesDatesFiltered <- Bike_Crashes %>% filter(DATE > ymd_hms("2009-01-01 00:00:00"))

# Add time variables with Lubridate
bikeCrashesDatesFiltered$month <- month(bikeCrashesDatesFiltered$DATE, label = T)
bikeCrashesDatesFiltered$year <- year(bikeCrashesDatesFiltered$DATE)

# Year/Month Heatmap
yearMonth <- ddply(bikeCrashesDatesFiltered, c("year", "month"), summarize, N = length(DATE))

# reverse order of months for easier grouping
yearMonth$month <- factor(yearMonth$month, levels = rev(levels(yearMonth$month)))

# Adding in average of Jan - Jul 2014 & 2016 for missing 2015 data from those months
str(yearMonth)
yearMonth2 <- yearMonth %>% filter(year == "2014" | year == "2016") %>% group_by(month) %>% summarize(N = mean(N))
yearMonth2 <- yearMonth2[6:12,]
yearMonth2$year <- rep(2015, 7)
yearMonth2 <- yearMonth2[c(3,1,2)]
yearMonth <- rbind(yearMonth, yearMonth2) %>% arrange(year, month)
yearMonth <- yearMonth[-1,]
# overall summary
Seasonality_Summary <- ggplot(yearMonth, aes(year, month)) + geom_tile(aes(fill = N), color = 'white') + scale_fill_gradient(low = col1, high = col2) +
  xlab("Year") + ylab("Month") + labs(fill = "# of Crashes", title = "Seasonality of DC Bike Crashes", subtitle = "2012-2019", caption = "Data from https://opendata.dc.gov/datasets") +
  theme_minimal() +
  theme(text = element_text(family = "Garamond", color = "gray25"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(color = "gray30"),
        plot.background = element_rect(fill = "gray95"),
        plot.margin = unit(c(2, 13, 2, 13), "lines"),
        legend.position = "right",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_blank()
  )

# Heatmap 2: Group/Hour

groupSummary <- ddply(bikeCrashesDatesFiltered, c("WARD", "month"), summarize, N = length(DATE))
groupSummary <- groupSummary[-1,]
# overall ward by month of year summary
Ward_Summary <- ggplot(groupSummary, aes(month, WARD)) + geom_tile(aes(fill = N), color = "white") + scale_fill_gradient(low = col1, high = col2) +
  xlab("Month") + ylab("Ward") + labs(fill = "# of Crashes", title = "DC Bike Crashes by Ward", subtitle = "2012-2019", caption = "Data from https://opendata.dc.gov/datasets") +
  theme_minimal() +
  theme(text = element_text(family = "Garamond", color = "gray25"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(color = "gray30"),
        plot.background = element_rect(fill = "gray95"),
        plot.margin = unit(c(2, 13, 2, 13), "lines"),
        legend.position = "right",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_blank()
  )
