# @author: Andre Duarte
# @date: June 1st, 2016
# @purpose: Globo.com presentation
# @confidential: True

# Read in files
g1c <- read.csv("g1c.tsv", header = FALSE, sep="\t", stringsAsFactors = FALSE)
g1u <- read.csv("g1u.tsv", header = FALSE, sep="\t", stringsAsFactors = FALSE)
# Remove last row (incomplete)
g1u <- g1u[-nrow(g1u),]

# Set column names
colnames(g1c) <- c("Gender", "DOB", "State", "UserId")
colnames(g1u) <- c("URL", "ts", "Device", "Datetime", "UserId")

# Merge the two tables (inner join) on UserId
merged <- merge(x = g1c, y = g1u, by = "UserId")

# Clean the data a little bit more
merged$UserId <- as.factor(merged$UserId)
# if gender is missing, set it as NA
merged$Gender <- ifelse(merged$Gender == " ", NA, merged$Gender)
merged$Gender <- as.factor(merged$Gender)
# if DOB is missing, set it as NA
merged$DOB <- as.Date(merged$DOB, "%m/%d/%Y")
merged$DOB_y <- year(merged$DOB)
# one DOB is way off into the future. We can remove it
#head(sort(unique(merged$DOB), decreasing = T))
# more generally, we can remove all of whom have DOB > today
merged <- merged[-which(with(merged, DOB > today())),]
# Get the year of birth as a separate value
#merged$DOB_y <- factor(year(merged$DOB), ordered = TRUE)
merged$DOB_y <- year(merged$DOB)
# if state is missing or not informed, set it as NA
merged$State <- ifelse(merged$State %in% c(" ", "Nao Informado"), "NA", merged$State)
merged$State <- as.factor(merged$State)
# Get the root of the URLs for easier grouping
merged$URL_split <- as.factor(vapply(strsplit(merged$URL, "/"), `[`, 1, FUN.VALUE=character(1)))
merged$Device <- factor(merged$Device,
                        levels = c("mob", "pc"),
                        labels = c("Mobile", "PC"))
# break down the datetime into year, month, day, hour (minutes and seconds seems too granular)
#merged$Year <- year(merged$Datetime) # 2016 here
#merged$Month <- month(merged$Datetime) # 5 here
merged$Day <- day(merged$Datetime)
merged$Hour <- hour(merged$Datetime)
# keep the date as a separate variable (no hours, minutes, seconds)
merged$Date <- as.Date(merged$Datetime)
merged$Datetime <- as.POSIXct(merged$Datetime, tz = "Brazil/East")
# Get the day of the week as well
merged$DOW <- weekdays(merged$Date)
merged$DOW <- factor(merged$DOW,
                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                            "Saturday", "Sunday"),
                     ordered = T)