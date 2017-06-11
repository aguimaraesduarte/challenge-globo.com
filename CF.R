# @author: Andre Duarte
# @date: June 2016
# @purpose: Globo.com presentation
# @confidential: True

# Set working directory (change to local)
setwd('~/Desktop/globo/')

###########################
######### IMPORTS #########
###########################
source("clean_session.R")
source("clean_session.R")
list.of.packages <- c("dummies", "recommenderlab", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

###########################
######## READ FILES #######
###########################
source("read_tables_CF.R")

###########################
######## FUNCTIONS ########
###########################
source("functions_CF.R")

############################
#### Recommender System ####
############################
# Inspired from 'Building a Recommendation System with R',
# by Suresh K. Gorakala and Michele Usuelli.

# Convert data frame to binary rating matrix for user based collaborative filtering.
m <- as.matrix(m)
m <- as(m, "binaryRatingMatrix")

# Look at the matrix (very sparse).
image(m[1:200,], main = "Binary rating matrix")
# We can see patterns (URLs that are very visited, users that have super low activity, etc).

# Look at distribution of users per URL,
n_users <- colCounts(m)
qplot(n_users[n_users < 5000]) +
  stat_bin(binwidth = 100) +
  ggtitle("Distribution of the number of users")
# Lots of users only visited one or very few URLs!

# keep only URLs that have been visited by at least 5 people
# keep only users that have visited at least 5 URLs
m <- m[, colCounts(m) >= 5]
m <- m[rowCounts(m) >= 5, ]

# Look at the matrix now (a bit better).
image(m[1:100,], main = "Binary rating matrix")

# Set up 10-fold cross-validation.
scheme <- evaluationScheme(m,                                  # data matrix (binaryRatingMatrix)
                           method = "cross-validation",        # k-fold cross-validation
                           given = -1,                         # all-but-one evaluation
                           k = 10                              # k-fold cross-validation
                          )

scheme

# First, let's compare some usual recommender system algorithms (RANDOM, POPULAR, UBCF, IBCF).
# ROC and precision/recall curves will allow us to find the best suited one here.
compare_algos(scheme)

# We can see that IBCF performs best. Let's compare different similarity methods (Jaccard, Cosine, Pearson).
# Again, ROC and precision/recall curves will allow us to find the best suited one here.
compare_similarity(scheme)

# We can see that Jaccard performs best. Let's compare different values for k (number of neighbors).
# Again, ROC and precision/recall curves will allow us to find the best suited one here.
compare_k(scheme)

# Best model (IBCF, Jaccard, k = 50).
recc_model <- Recommender(data = m,
                          method = "IBCF",
                          parameter = list(method = "Jaccard",
                                           k = 50))

# Make recommendations for all users.
# Note: this is fairly fast for IBCF (but we'd limit to ~100 if using UBCF).
npreds <- 10
recc_predicted <- predict(object = recc_model,
                          newdata = m,
                          n = npreds)

# Transform into matrix for easier visualization.
recc_matrix <- sapply(recc_predicted@items, function(x){
  UrlIds[x]
})

# Print recommendations for first n users.
recc_matrix[, 1:4]

# Look at them (change XX to profile number).
merged[5, 1:4]
