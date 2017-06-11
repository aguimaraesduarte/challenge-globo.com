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

############################
### FILE TRANSFORMATIONS ###
############################
df <- g1u
df$URL_split <- as.factor(vapply(strsplit(df$URL, "/"), `[`, 1, FUN.VALUE=character(1)))
df$URL <- NULL
df$ts <- NULL
df$Device <- NULL
df$Datetime <- NULL
df$UserId <- as.factor(df$UserId)
df$URL_split <- as.factor(df$URL_split)
UrlIds <- levels(df$URL_split)

##### One-hot encode URL_split
df <- dummy.data.frame(df, names=c("URL_split"), sep="_")
# Add the columns by UserId, just have yes/no if visited the URL or not
df <- aggregate(. ~ UserId, df, function(x) as.integer(sum(x)>0))
#df <- aggregate(. ~ UserId, df, sum)

# Merge the two tables (inner join) on UserId
merged <- merge(x = g1c, y = df, by = "UserId")

#### Prepare for recommender system
m <- merged
m$DOB <- NULL
m$State <- NULL
m$UserId <- NULL
m$Gender <- NULL