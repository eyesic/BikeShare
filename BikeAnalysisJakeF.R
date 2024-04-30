library(readr)
library(tidyverse)
hour <- read_csv("Documents/Personal Files/R/BikeSharing/hour.csv")
day <- read_csv("Documents/Personal Files/R/BikeSharing/day.csv")

names(day)

dayGrouped <- day %>%
  group_by(season) %>%
  summarize(
    Casualusers = sum(casual),
    Registeredusers = sum(registered),
    AvgTemp = round(mean(temp) *100, 2),
    Users = sum(casual) + sum(registered)
  )
dayGrouped_long <- dayGrouped %>%
  pivot_longer(cols = c(Casualusers, Registeredusers), names_to = "UserType", values_to = "Count")

ggplot(dayGrouped_long, aes(x = season, y = Count, fill = UserType)) +
  geom_col(position = "stack") +
  theme_minimal() +
  coord_cartesian(ylim = c(300000, 1150000)) +
  scale_fill_manual(values = c("Casualusers" = "lightblue", "Registeredusers" = "lightgray")) +
  labs(fill = "User Type")

ggplot(dayGrouped) +
  geom_col(aes(season, Users, fill=as.factor(AvgTemp))) +
  coord_cartesian(ylim = c(300000, 1150000)) +
  theme_minimal() +
  scale_fill_brewer(palette="OrRd")

dayGroupedTwice <- day %>%
  group_by(yr, season) %>%
  summarize(
    Casualusers = sum(casual),
    Registeredusers = sum(registered),
    AvgTemp = round(mean(temp) * 100, 2),
    Users = sum(casual) + sum(registered)
  )

ggplot(dayGroupedTwice) +
  geom_col(aes(season, Users, fill=as.factor(AvgTemp))) +
  coord_cartesian(ylim = c(100000, 650000)) +
  theme_minimal() +
  scale_fill_brewer(palette="OrRd") +
  facet_wrap(~yr)

ggplot(day) +
  geom_point(aes(cnt,temp,color=as.factor(weekday))) +
  theme_minimal() +
  facet_wrap(~workingday) +
  scale_color_brewer(palette = "Dark2")


data <- mutate(day, yr = ifelse(yr == 0, 2011, 2012),
               mnth = case_when(
                 mnth == 1 ~ "January",
                 mnth == 2 ~ "February",
                 mnth == 3 ~ "March",
                 mnth == 4 ~ "April",
                 mnth == 5 ~ "May",
                 mnth == 6 ~ "June",
                 mnth == 7 ~ "July",
                 mnth == 8 ~ "August",
                 mnth == 9 ~ "September",
                 mnth == 10 ~ "October",
                 mnth == 11 ~ "November",
                 mnth == 12 ~ "December"
               ))

# Convert weekday column to actual day names
data <- mutate(data, weekday = case_when(
  weekday == 0 ~ "Sunday",
  weekday == 1 ~ "Monday",
  weekday == 2 ~ "Tuesday",
  weekday == 3 ~ "Wednesday",
  weekday == 4 ~ "Thursday",
  weekday == 5 ~ "Friday",
  weekday == 6 ~ "Saturday"
))

# Convert 'dteday' column to date format
data$dteday <- as.Date(data$dteday)

# Define important event dates
important_dates <- c(
  "2011-01-20",  # Presidential Inauguration of Barack Obama
  "2011-04-09",  # Dedication of the Martin Luther King Jr. Memorial
  "2011-06-30",  # End of the "Don't Ask, Don't Tell" policy
  "2011-08-23",  # East Coast Earthquake
  "2011-10-30",  # Dedication of the Franklin Delano Roosevelt Memorial
  "2011-12-15",  # Last convoy of U.S. troops leaves Iraq
  "2012-01-21",  # Second Presidential Inauguration of Barack Obama
  "2012-05-20",  # Dedication of the Martin Luther King Jr. National Memorial Library
  "2012-06-28",  # Supreme Court upholds the Affordable Care Act
  "2012-08-06",   # International AIDS Conference
  "2011-01-18",  # Martin Luther King Jr. Day
  "2011-07-04",  # Independence Day Celebrations
  "2011-09-24",  # National Book Festival
  "2012-02-11",  # "Portraits of African American Heroes" exhibition at the Smithsonian
  "2012-11-06",  # United States Presidential Election
  "2011-05-01",  # Death of Osama bin Laden
  "2011-09-11"   # 10th Anniversary of the September 11 Attacks
)

# Create binary variable indicating whether it's an important date
data$is_important <- ifelse(data$dteday %in% as.Date(important_dates), 1, 0)

# Define cherry blossom festival dates
cherry_blossom_dates <- c(seq(from = as.Date("2011-03-26"), to = as.Date("2011-04-10"), by = "day"),
                        seq(from = as.Date("2012-03-20"), to = as.Date("2012-04-27"), by = "day"))


# Create binary variable indicating whether it's a peak blossom day
data$is_cherry_blossom <- ifelse(data$dteday %in% as.Date(cherry_blossom_dates), 1, 0)

# Create binary variable indicating whether it's both an important date and a peak blossom day
data$is_important_and_cherry <- ifelse(data$is_important == 1 & data$is_cherry_blossom == 1, 1, 0)

# Print the updated dataset
head(data)

sorted_data <- data[order(data$casual), ]

# Select the top 5 and bottom 5 rows
top_5_least <- sorted_data[1:5, ]
bottom_5_most <- sorted_data[(nrow(sorted_data) - 4):nrow(sorted_data), ]

# Create a combined dataset for display
combined_data <- rbind(top_5_least, bottom_5_most)

# Convert 'dteday' column to Date format
combined_data$dteday <- as.Date(combined_data$dteday)

# Create a copy of the dataset with formatted date
formatted_combined_data <- combined_data
formatted_combined_data$dteday <- format(formatted_combined_data$dteday, "%Y-%m-%d")
formatted_combined_data <- formatted_combined_data %>%
  select(dteday,
         weekday,
         holiday,
         workingday,
         temp,
         atemp,
         hum,
         windspeed,
         cnt,
         is_important,
         is_cherry_blossom,
         is_important_and_cherry) %>%
  mutate(temp = round(temp * 100),
         atemp = round(atemp * 100),
         hum = round(hum * 100),
         windspeed = round(windspeed * 100))

binary_to_yes_no <- function(x) {
  ifelse(x == 0, "No", "Yes")
}

# Modify the binary variables to Yes or No
tableData <- formatted_combined_data %>%
  mutate_at(vars(is_important, is_cherry_blossom, is_important_and_cherry, holiday, workingday), binary_to_yes_no) %>%
  rename(
    Date = dteday,
    DayOfWeek = weekday,
    Holiday = holiday,
    WorkingDay = workingday,
    HeatIndex = atemp,
    Temperature = temp,
    Humidity = hum,
    WindSpeed = windspeed,
    Users = cnt,
    ImportantDate = is_important,
    CherryFestivalDay = is_cherry_blossom,
    ImportantAndPeakBlossomDay = is_important_and_cherry
  )

# Display the combined dataset with kable
library(kableExtra)

# Display the combined dataset with kable and color formatting
kable(tableData, caption = "Top 5 Days with Least and Most Casual Users", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(5, color = "white",
              background = spec_color(tableData$Temperature[1:10], end = 0.7)) 


ggplot(data, aes(x=atemp, y=cnt, color=as.factor(is_cherry_blossom))) +
  geom_point() +  
  theme_minimal() +
  labs(x = "Heat Index (atemp)", y = "Total Users") +
  scale_color_manual(values = c("lightblue", "red"), name = "Cherry Blossom", labels = c("Not Cherry Blossom", "Cherry Blossom"))

p1 <- ggplot(day, aes(x = temp, y = casual + registered, color = temp)) +
  geom_point() +
  labs(x = "Temperature", y = "Total Users") +
  scale_color_continuous(name = "Temperature")

# Plot for Humidity
p2 <- ggplot(day, aes(x = hum, y = casual + registered, color = hum)) +
  geom_point() +
  labs(x = "Humidity", y = "Total Users") +
  scale_color_continuous(name = "Humidity")

# Plot for WindSpeed
p3 <- ggplot(day, aes(x = windspeed, y = casual + registered, color = windspeed)) +
  geom_point() +
  labs(x = "Wind Speed", y = "Total Users") +
  scale_color_continuous(name = "Wind Speed", trans = "reverse")

p4 <- ggplot(day, aes(x = atemp, y = casual + registered, color = atemp)) +
  geom_point() +
  labs(x = "Heat Index", y = "Total Users") +
  scale_color_continuous(name = "Heat Index")

p5 <- ggplot(day, aes(x = factor(weathersit), y = casual + registered, color = factor(weathersit))) +
  geom_boxplot() +
  labs(x = "Weather Situation", y = "Total Users") +
  scale_color_discrete(name = "Weather Situation")

# Combine plots into one picture
combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

# Display the combined plot
print(combined_plot)

