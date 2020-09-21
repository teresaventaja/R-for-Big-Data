#Dataset 1

# Read csv Airbnb file

airbnb <- read.csv(file="airbnb_berlin_cl.csv",head=TRUE,sep=",")
airbnb

# Objective 1

#Install package so I can select different categories of listings

install.packages('dplyr')

# Create a data frame with the price for objective 1

airbnb_price <- data.frame(airbnb$listing_id, airbnb$price)
airbnb_price

# Create a vector including only different categories of listings
# Need to do it because there are multiple duplicate listings
# Because each row of the dataset is for one review, not for one listing

library(dplyr)
single_prices <- distinct(airbnb_price)
single_prices

# Write a CSV file that I can manipulate

write.csv(single_prices,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\price_listing.csv", row.names = FALSE)

# Create a calculator that displays whether a price is cheap, expensive or average

price <- as.integer(readline(prompt="Enter a price(number): "))
for (val in price) {
if (price > 54) {
  print("This Airbnb listing is too expensive");
  print("Watch out your savings!");
} else if (price < 27) {
  print("This Airbnb listing is specially cheap");
  print("Be careful with quality standards!");
} else {
  print("This price is average for an Airbnb listing in Berlin");
  print("Good selection!");
}
}
price <- as.integer(readline(prompt="Enter a price(number): "))

# Objective 2

# Create a data frame for objective 2

response_time <- data.frame(airbnb$listing_id, airbnb$host_response_time, airbnb$is_superhost)
response_time

# Create a vector including only different categories of listings
# Need to do it because there are multiple duplicate listings
# Because each row of the dataset is for one review, not for one listing

remove_duplicate_times <- distinct(response_time)
remove_duplicate_times

# Create subset with different categories
# Creating subsets is going to tell me in the environment how many observations
# there are for each subset. I will use these numbers to create a matrix

superhostNA <- subset(remove_duplicate_times, airbnb.is_superhost=="TRUE" 
                & airbnb.host_response_time=="N/A",
      select=airbnb.listing_id:airbnb.is_superhost)

summary(remove_duplicate_times$airbnb.is_superhost=='TRUE' 
        & remove_duplicate_times$airbnb.host_response_time=="N/A")

superhostFH <- subset(remove_duplicate_times, airbnb.is_superhost=="TRUE" 
                      & airbnb.host_response_time=="within a few hours",
                      select=airbnb.listing_id:airbnb.is_superhost)

superhostAH <- subset(remove_duplicate_times, airbnb.is_superhost=="TRUE" 
                      & airbnb.host_response_time=="within an hour",
                      select=airbnb.listing_id:airbnb.is_superhost)

superhostAD <- subset(remove_duplicate_times, airbnb.is_superhost=="TRUE" 
                      & airbnb.host_response_time=="within a day",
                      select=airbnb.listing_id:airbnb.is_superhost)

superhostFD <- subset(remove_duplicate_times, airbnb.is_superhost=="TRUE" 
                      & airbnb.host_response_time=="a few days or more",
                      select=airbnb.listing_id:airbnb.is_superhost)

hostNA <- subset(remove_duplicate_times, airbnb.is_superhost=="FALSE" 
                      & airbnb.host_response_time=="N/A",
                      select=airbnb.listing_id:airbnb.is_superhost)

hostFH <- subset(remove_duplicate_times, airbnb.is_superhost=="FALSE" 
                      & airbnb.host_response_time=="within a few hours",
                      select=airbnb.listing_id:airbnb.is_superhost)

hostAH <- subset(remove_duplicate_times, airbnb.is_superhost=="FALSE" 
                      & airbnb.host_response_time=="within an hour",
                      select=airbnb.listing_id:airbnb.is_superhost)

hostAD <- subset(remove_duplicate_times, airbnb.is_superhost=="FALSE" 
                      & airbnb.host_response_time=="within a day",
                      select=airbnb.listing_id:airbnb.is_superhost)

hostFD <- subset(remove_duplicate_times, airbnb.is_superhost=="FALSE" 
                      & airbnb.host_response_time=="a few days or more",
                      select=airbnb.listing_id:airbnb.is_superhost)

# Creating a matrix

times_HS <- matrix(c(266, 0, 733, 1870, 2136, 4088, 328, 1999, 6, 411), nrow=2, ncol=5)

rownames (times_HS) <- c("Superhost", "Host")
colnames (times_HS) <- c("No_response", "Few_hours", "One_hour", "One_day", "More_than_one_day")

times_HS

# Write a CSV file that I can manipulate

write.csv(times_HS,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\time_host_superhost.csv", row.names = TRUE)

# Objective 3.1

# Create a data frame for objective 3.1

locations <- data.frame(airbnb$listing_id, airbnb$neighborhood_group, airbnb$location_rating)
locations

# Create a vector including only different categories of listings
# Need to do it because there are multiple duplicate listings
# Because each row of the dataset is for one review, not for one listing

remove_duplicate_locations <- distinct(locations)
remove_duplicate_locations

# Create subset with different neighbourhood
# Creating subsets is going to help me to calculate the average rating per neighbourhood

CW <- subset(remove_duplicate_locations, 
                      airbnb.neighborhood_group=="Charlottenburg-Wilm.",
                      select=airbnb.listing_id:airbnb.location_rating)

FK <- subset(remove_duplicate_locations, 
                      airbnb.neighborhood_group=="Friedrichshain-Kreuzberg",
                      select=airbnb.listing_id:airbnb.location_rating)

LI <- subset(remove_duplicate_locations, 
                      airbnb.neighborhood_group=="Lichtenberg",
                      select=airbnb.listing_id:airbnb.location_rating)

MH <- subset(remove_duplicate_locations, 
                      airbnb.neighborhood_group=="Marzahn - Hellersdorf",
                      select=airbnb.listing_id:airbnb.location_rating)

MI <- subset(remove_duplicate_locations, 
                airbnb.neighborhood_group=="Mitte",
                select=airbnb.listing_id:airbnb.location_rating)

NE <- subset(remove_duplicate_locations, 
                    airbnb.neighborhood_group=="NeukÃ¶lln",
                    select=airbnb.listing_id:airbnb.location_rating)

PA <- subset(remove_duplicate_locations, 
                    airbnb.neighborhood_group=="Pankow",
                    select=airbnb.listing_id:airbnb.location_rating)

RE <- subset(remove_duplicate_locations, 
                    airbnb.neighborhood_group=="Reinickendorf",
                    select=airbnb.listing_id:airbnb.location_rating)

SP <- subset(remove_duplicate_locations, 
                  airbnb.neighborhood_group=="Spandau",
                  select=airbnb.listing_id:airbnb.location_rating)

SZ <- subset(remove_duplicate_locations, 
                  airbnb.neighborhood_group=="Steglitz - Zehlendorf",
                  select=airbnb.listing_id:airbnb.location_rating)

TS <- subset(remove_duplicate_locations, 
                   airbnb.neighborhood_group=="Tempelhof - SchÃ¶neberg",
                   select=airbnb.listing_id:airbnb.location_rating)

TK <- subset(remove_duplicate_locations, 
                   airbnb.neighborhood_group=="Treptow - KÃ¶penick",
                   select=airbnb.listing_id:airbnb.location_rating)

# Create a vector for the overall_rating column of each subset

Rating_CW <- CW$airbnb.location_rating

Rating_FK <- FK$airbnb.location_rating

Rating_LI <- LI$airbnb.location_rating

Rating_MH <- MH$airbnb.location_rating

Rating_MI <- MI$airbnb.location_rating

Rating_NE <- NE$airbnb.location_rating

Rating_PA <- PA$airbnb.location_rating

Rating_RE <- RE$airbnb.location_rating

Rating_SP <- SP$airbnb.location_rating

Rating_SZ <- SZ$airbnb.location_rating

Rating_TS <- TS$airbnb.location_rating

Rating_TK <- TK$airbnb.location_rating

# Calculate the mean with 2 decimal places

CW_mean <- round(mean(Rating_CW), digits=2)

FK_mean <- round(mean(Rating_FK), digits=2)

LI_mean <- round(mean(Rating_LI), digits=2)

MH_mean <- round(mean(Rating_MH), digits=2)

MI_mean <- round(mean(Rating_MI), digits=2)

NE_mean <- round(mean(Rating_NE), digits=2)

PA_mean <- round(mean(Rating_PA), digits=2)

RE_mean <- round(mean(Rating_RE), digits=2)

SP_mean <- round(mean(Rating_SP), digits=2)

SZ_mean <- round(mean(Rating_SZ), digits=2)

TK_mean <- round(mean(Rating_TK), digits=2)

TS_mean <- round(mean(Rating_TS), digits=2)

# Since FK_mean and PA_mean returned NA, I am exporting them as a CSV file to troubleshoot

write.csv(Rating_FK,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\FK.csv", row.names = FALSE)
write.csv(Rating_PA,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\PA.csv", row.names = FALSE)

# Remove 1 NA value in Excel on each file

# Import new data

FK_new <- read.csv(file="FK.csv",head=FALSE,sep=",")
PA_new <- read.csv(file="PA.csv",head=FALSE,sep=",")

# Replace faulty vectors with my new data

Rating_FK <- FK_new$V1
Rating_PA <- PA_new$V1

# Calculate the mean

FK_mean <- round(mean(Rating_FK), digits=2)
PA_mean <- round(mean(Rating_PA), digits=2)

# Creating a matrix

rating_location <- matrix(c(9.55, 9.7, 9.14, 8.84, 9.53, 9.46, 9.64, 9.11, 8.88, 9.5, 9.29, 9.53), byrow=TRUE, nrow=1, ncol=12)

rownames (rating_location) <- c("Mean Location Rating")
colnames (rating_location) <- c("Charlottenburg-Wilmersdorf", "Friedrichshain-Kreuzberg", 
                                "Lichtenberg", "Marzahn-Hellersdorf", "Mitte", 
                                "Neukölln", "Pankow", "Reinickendorf", "Spandau", 
                                "Steglitz-Zehlendorf", "Tempelhof-Schöneberg", "Treptow-Köpenick")

rating_location

# Creating a Mat Plot with my matrix, where I can compare the results
# Since I have too many neighbourhood with long names, a leyend does not look good
# This is why I am adding a complementary Excel graph on the report

matplot(t(rating_location), type="b", pch=15:18, col=c(4),  
        xlab = 'Neighbourhood', ylab = 'Rating')

# Write a CSV file to add a complementary graph

write.csv(rating_location,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\rating_by_neighborhood.csv", row.names = TRUE)

# Objective 3.2

# Create a data frame for objective 3.2

ratings <- data.frame(airbnb$listing_id, airbnb$overall_rating, airbnb$location_rating)
ratings

# Create a vector including only different categories of listings
# Need to do it because there are multiple duplicate listings
# because each row of the dataset is for one review, not for one listing
# Note that the ratings showing are not the ones rated by each review
# but the ratings by listing

library('dplyr')

rem_duplicate_ratings <- distinct(ratings)
rem_duplicate_ratings

# Create vectors for overall_rating and location_rating columns

rating_data <- subset(rem_duplicate_ratings, select=airbnb.overall_rating:airbnb.location_rating)
rating_location2 <- rating_data$airbnb.location_rating
rating_overall2 <- rating_data$airbnb.overall_rating

# t-test

testing_ratings <-t.test(rating_location2, rating_overall2)
testing_ratings

# Objective 4

# Create a data frame for objective 4

review_date <- data.frame(airbnb$review_date)
review_date

# Write a CSV file that I can manipulate
# I will convert date into weekday

write.csv(review_date,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\review_dates.csv", row.names = FALSE)

# Load csv with data converted

review_weekday <- read.csv(file="review_dates.csv",head=FALSE,sep=",")
review_weekday

# Obtaining the total observations for each weekday

summary(review_weekday)

# Creating a matrix based on summary observations

review_per_weekday <- matrix(c(82746, 58545, 50721, 51244, 56254, 53364, 99250), byrow=TRUE, nrow=1, ncol=7)

rownames (review_per_weekday) <- c("Number of Reviews")
colnames (review_per_weekday) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

review_per_weekday

# Write a CSV file that I can manipulate

write.csv(review_per_weekday,"C:\\Users\\Owner\\Desktop\\datasets\\airbnb\\review_weekday.csv", row.names = TRUE)

#Dataset 2

# Read csv Airbnb file

video_games <- read.csv(file="video_games_clean.csv",head=TRUE,sep=",")
video_games

# Objective 1

# I identified the videogames that I want to find on my dataset
# I know the year of release and the name of each of them,
# I got this info from the literature review
# Now I am going to filter on my dataset to find the sales for each of them

Space_game <- video_games$Name == 'Space Invaders'
video_games[Space_game,]

# We have 2.53 Million Units for Space Invaders sold globally
# For the release year 1978

Kong_game <- video_games$Name == 'Donkey Kong'
video_games[Kong_game,]

# We have 3.07 Million Units for Donkey Kong sold globally
# For the release year 1981

Pac_game <- video_games$Name == 'Pac-Man'
video_games[Pac_game,]

# We have 7.81 Million Units for Pac-Man sold globally
# For the release year 1982

Mario_game <- video_games$Name == 'Super Mario Bros.'
video_games[Mario_game,]

# We have 40.24 Million Units for Super Mario Bros. sold globally
# For the release year 1985

Zelda_game <- video_games$Name == 'The Legend of Zelda'
video_games[Zelda_game,]

# We have 6.51 Million Units for The Legend of Zelda sold globally
# For the release year 1986

Metroid_game <- video_games$Name == 'Metroid'
video_games[Metroid_game,]

# We have 2.73 Million Units for Metroid sold globally
# For the release year 1986

Castle_game <- video_games$Name == 'Castlevania'
video_games[Castle_game,]

# We have 1.23 Million Units for Castlevania sold globally
# For the release year 1986

Sonic_game <- video_games$Name == 'Sonic the Hedgehog'
video_games[Sonic_game,]

# We have 4.34 Million Units for Sonic the Hedgehog sold globally
# For the release year 1991

Dragon_game <- video_games$Name == 'Dragon Quest V: Tenkuu no Hanayome'
video_games[Dragon_game,]

# We have 2.79 Million Units for Dragon Quest V: Tenkuu no Hanayome sold globally
# For the release year 1992

Tetris_game <- video_games$Name == 'Tetris'
video_games[Tetris_game,]

# We have 30.26 Million Units for Tetris sold globally
# For the release year 1992

Street_game <- video_games$Name == 'Street Fighter II: The World Warrior'
video_games[Street_game,]

# We have 6.3 Million Units for Street Fighter II: The World Warrior sold globally
# For the release year 1992

Kombat_game <- video_games$Name == 'Mortal Kombat'
video_games[Kombat_game,]

# We have 2.67 Million Units for Mortal Kombat sold globally
# For the release year 1992

Flight_game <- video_games$Name == 'Microsoft Flight Simulator'
video_games[Flight_game,]

# We have 5.12 Million Units for Microsoft Flight Simulator sold globally
# For the release year 1996

Mario2_game <- video_games$Name == 'Super Mario 64'
video_games[Mario2_game,]

# We have 11.89 Million Units for Super Mario 64 sold globally
# For the release year 1996

Spyros_game <- video_games$Name == 'Skylanders: Spyros Adventure'
video_games[Spyros_game,]

# We have 5.37 Million Units for Skylanders: Spyros Adventure sold globally
# For the release year 2011

Disney_game <- video_games$Name == 'Disney Infinity'
video_games[Disney_game,]

# We have 5 Million Units for Disney Infinity sold globally
# For the release year 2013

# Create a bar chart in R

# Create the data for the chart
H <- c(2.53,3.07,7.81,40.24,6.51, 2.73, 1.23, 4.34, 
       2.79, 30.26, 6.3, 2.67, 5.12, 11.89, 5.37, 5)
M <- c("SI 78","DK 81","PM 82","MB 85","Lz 86",
       "Me 86", "CV 86", "So 91",
       "DQ 92", "Te 92", "SF 92", "MK 92", 
       "FS 96", "M64 96", "Sp 11", "DI 13"
       )

# Give the chart file a name
png(file = "historical_videogames.png")

# Plot the bar chart 
barplot(H, names.arg=M ,xlab="Game", ylab="Units (Millions)", col="blue",
        main="Historical Video Games", border="red")

# Objective 2

# I am going to calculate the total sales for all genres
# Then I will get the total sales for each genre
# And compare the results

# Total sales for all genres

Total_sales <- sum(video_games$Global_Sales)

# Total_sales gives me a result of 8987.1 Million units as of Jan 2017

# Getting the total sum of sales for Action games

Action_games <- video_games$Genre == 'Action'
video_games[Action_games,10]
Action_sales <- sum(video_games[Action_games,10])
Action_sales

# Getting the total sum of sales for Adventure games

Adventure_games <- video_games$Genre == 'Adventure'
video_games[Adventure_games,10]
Adventure_sales <- sum(video_games[Adventure_games,10])
Adventure_sales

# Getting the total sum of sales for Fighting games

Fighting_games <- video_games$Genre == 'Fighting'
video_games[Fighting_games,10]
Fighting_sales <- sum(video_games[Fighting_games,10])
Fighting_sales

# Getting the total sum of sales for Misc games

Misc_games <- video_games$Genre == 'Misc'
video_games[Misc_games,10]
Misc_sales <- sum(video_games[Misc_games,10])
Misc_sales

# Getting the total sum of sales for Platform games

Platform_games <- video_games$Genre == 'Platform'
video_games[Platform_games,10]
Platform_sales <- sum(video_games[Platform_games,10])
Platform_sales

# Getting the total sum of sales for Puzzle games

Puzzle_games <- video_games$Genre == 'Puzzle'
video_games[Puzzle_games,10]
Puzzle_sales <- sum(video_games[Puzzle_games,10])
Puzzle_sales

# Getting the total sum of sales for Racing games

Racing_games <- video_games$Genre == 'Racing'
video_games[Racing_games,10]
Racing_sales <- sum(video_games[Racing_games,10])
Racing_sales

# Getting the total sum of sales for Role-Playing games

Role_games <- video_games$Genre == 'Role-Playing'
video_games[Role_games,10]
Role_sales <- sum(video_games[Role_games,10])
Role_sales

# Getting the total sum of sales for Shooter games

Shooter_games <- video_games$Genre == 'Shooter'
video_games[Shooter_games,10]
Shooter_sales <- sum(video_games[Shooter_games,10])
Shooter_sales

# Getting the total sum of sales for Simulation games

Simulation_games <- video_games$Genre == 'Simulation'
video_games[Simulation_games,10]
Simulation_sales <- sum(video_games[Simulation_games,10])
Simulation_sales

# Getting the total sum of sales for Sports games

Sports_games <- video_games$Genre == 'Sports'
video_games[Sports_games,10]
Sports_sales <- sum(video_games[Sports_games,10])
Sports_sales

# Getting the total sum of sales for Strategy games

Strategy_games <- video_games$Genre == 'Strategy'
video_games[Strategy_games,10]
Strategy_sales <- sum(video_games[Strategy_games,10])
Strategy_sales

# Action - 1757.7 Million units sold as of Jan 2017, percentage:
acpe <- (1757.7*100)/8987.1
acpe # 19.56
# Adventure - 241.55 Million units sold as of Jan 2017, percentage:
adpe <- (241.55*100)/8987.1
adpe # 2.69
# Fighting - 449.17 Million units sold as of Jan 2017, percentage:
fipe <- (449.17*100)/8987.1
fipe # 5
# Miscellany - 808.8 Million units sold as of Jan 2017, percentage:
mipe <- (808.8*100)/8987.1
mipe # 9
# Platform - 831.74 Million units sold as of Jan 2017, percentage:
plpe <- (831.74*100)/8987.1
plpe # 9.25
# Puzzle - 243.76 Million units sold as of Jan 2017, percentage:
pupe <- (243.76*100)/8987.1
pupe # 2.71
# Racing - 731.67 Million units sold as of Jan 2017, percentage:
rape <- (731.67*100)/8987.1
rape # 8.14
# Role-Playing - 945.85 Million units sold as of Jan 2017, percentage:
rppe <- (945.85*100)/8987.1
rppe # 10.52
# Shooter - 1067.3 Million units sold as of Jan 2017, percentage:
shpe <- (1067.3*100)/8987.1
shpe # 11.88
# Simulation - 392.84 Million units sold as of Jan 2017, percentage:
sipe <- (392.84*100)/8987.1
sipe # 4.37
# Sports - 1341 Million units sold as of Jan 2017, percentage:
sppe <- (1341*100)/8987.1
sppe # 14.92
# Strategy - 175.72 Million units sold as of Jan 2017, percentage:
stpe <- (175.72*100)/8987.1
stpe # 1.96

# Creating a matrix with the data

sales_by_genre <- matrix(c(19.56, 2.69, 5, 9, 9.25, 2.71, 8.14, 10.52,
                               11.88, 4.37, 14.92, 1.96), byrow=TRUE, nrow=1, ncol=12)

rownames (sales_by_genre) <- c("% units sold")
colnames (sales_by_genre) <- c("Action", "Adventure", "Fighting", 
                                   "Miscellany", "Platform", "Puzzle", 
                                   "Racing", "Role-Playing", "Shooter",
                                  "Simulation", "Sports", "Strategy")

sales_by_genre

# Write a CSV file that I can manipulate

write.csv(sales_by_genre,"C:\\Users\\Owner\\Desktop\\datasets\\sales_genre.csv", row.names = TRUE)

# Objective 3

# I need to filter games whose year of release is greater or equal to 2005

Modern_games <- video_games$Year_of_Release >= '2005'

# Next I get only the columns I need (Platform, Global_sales)

Sales_platform <- video_games[Modern_games, c(2, 10)]

# I now need to calculate the total global sales by platform
# I am going to discard Nintendo 64 and Dreamcast games
# Because these consoles are too old for this analysis
# I am also discarding PC because it is not a single platform 'per se'

# Nintendo 3DS

TDS_sales <- Sales_platform$Platform == "3DS"
TDS_total_sales <- sum(Sales_platform[TDS_sales,2])
TDS_total_sales # Total of 270.94

# Nintendo DS

DS_sales <- Sales_platform$Platform == "DS"
DS_total_sales <- sum(Sales_platform[DS_sales,2])
DS_total_sales # Total of 791.52

# Game Boy Advanced

GBA_sales <- Sales_platform$Platform == "GBA"
GBA_total_sales <- sum(Sales_platform[GBA_sales,2])
GBA_total_sales # Total of 43.82

# Nintendo Game Cube

GC_sales <- Sales_platform$Platform == "GC"
GC_total_sales <- sum(Sales_platform[GC_sales,2])
GC_total_sales # Total of 39.95

# PlayStation 2

PS2_sales <- Sales_platform$Platform == "PS2"
PS2_total_sales <- sum(Sales_platform[PS2_sales,2])
PS2_total_sales # Total of 432.86

# PlayStation 3

PS3_sales <- Sales_platform$Platform == "PS3"
PS3_total_sales <- sum(Sales_platform[PS3_sales,2])
PS3_total_sales # Total of 941.27

# PlayStation 4

PS4_sales <- Sales_platform$Platform == "PS4"
PS4_total_sales <- sum(Sales_platform[PS4_sales,2])
PS4_total_sales # Total of 340.79

# PSP

PSP_sales <- Sales_platform$Platform == "PSP"
PSP_total_sales <- sum(Sales_platform[PSP_sales,2])
PSP_total_sales # Total of 288.46

# PS Vita

PSV_sales <- Sales_platform$Platform == "PSV"
PSV_total_sales <- sum(Sales_platform[PSV_sales,2])
PSV_total_sales # Total of 57.03

# Wii

Wii_sales <- Sales_platform$Platform == "Wii"
Wii_total_sales <- sum(Sales_platform[Wii_sales,2])
Wii_total_sales # Total of 910.18

# WiiU

WiiU_sales <- Sales_platform$Platform == "WiiU"
WiiU_total_sales <- sum(Sales_platform[WiiU_sales,2])
WiiU_total_sales # Total of 84.93

# X-box

X_sales <- Sales_platform$Platform == "X"
X_total_sales <- sum(Sales_platform[X_sales,2])
X_total_sales # Total of 60.43

# X-box 360

X360_sales <- Sales_platform$Platform == "X360"
X360_total_sales <- sum(Sales_platform[X360_sales,2])
X360_total_sales # Total of 973.39

# X-box One

XOne_sales <- Sales_platform$Platform == "XOne"
XOne_total_sales <- sum(Sales_platform[XOne_sales,2])
XOne_total_sales # Total of 173.82

# Creating a data frame with results

Platforms <- c('3DS', 'DS', 'GBA', 'GC', 'PS2', 'PS3','PS4', 
               'PSP', 'PSV', 'Wii', 'WiiU', 'X-box','X-360', 'X-One')

PSales <- c(270.94, 791.52, 43.82, 39.95, 432.86, 941.27, 340.79, 288.46,
            57.03, 910.18, 84.93, 60.43, 973.39, 173.82)

platform_data <- data.frame(Platforms, PSales)

platform_data

# Creating a lollipop plot

library('ggplot2')

ggplot(platform_data, aes(x=Platforms, y=PSales)) +
  geom_point() + 
  geom_segment( aes(x=Platforms, xend=Platforms, y=0, yend=Sales))

# Objective 4

# Getting the total sales by region

NA_sales <- sum(video_games[,6])
NA_sales # Result 4429.49

EU_sales <- sum(video_games[,7])
EU_sales # Result 2448.77

JP_sales <- sum(video_games[,8])
JP_sales # Result 1305.2

Other_sales <- sum(video_games[,9])
Other_sales # Result 798.76

# Install package to create a Treemap

install.packages('treemap')

# Use package

library('treemap')

# Create data

group <- c("NA","EU","JP", "Other")
UnitsSold <- c(4429.49, 2448.77, 1305.2,798.76)
Region_sales <- data.frame(group,UnitsSold)

# Treemap

treemap(Region_sales,
        index="group",
        vSize="UnitsSold",
        type="index"
)