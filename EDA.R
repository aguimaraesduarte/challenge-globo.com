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
list.of.packages <- c("ggplot2", "lubridate", "scales", "plyr", "dplyr", "magrittr", "cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

###########################
####### READ FILES ########
###########################
source("read_tables.R")
# how many unique URLs visited?
#length(unique(merged$URL)) # 7828
# how many main sections visited?
#length(unique(merged$URL_split)) # 185

######################################################
##################### FUNCTIONS ######################
######################################################
source("functions.R")

######################################################
#################### PAGE VISITS #####################
######################################################
# Plot page visits per day, with regression line
p_line_visits <- plot_by_var(merged, regress = TRUE, xlab_ = "Data", ylab_ = "Visitas", title_ = "Visitas por dia")
plot(p_line_visits)

# Plot page visits per day by device, with regression line and custom colors/labels
plot_by_var(merged, var1_ = "Device", collabels_ = c("Mobile", "PC"),
            colvalues_ = c("#FF9922", "#9922FF"), colname_ = "Dispositivo", regress = TRUE,
            xlab_ = "Data", ylab_ = "Visitas", title_ = "Visitas por dia por dispositivo")

# Plot page visits by gender, with regression line and custom colors/labels
plot_by_var(merged, var1_ = "Gender", collabels_ = c("F", "M", "NA"),
            colvalues_ = c("#FF9922", "#9922FF", "#22FF99"), colname_ = "Sexo", regress = TRUE,
            xlab_ = "Data", ylab_ = "Visitas", title_ = "Visitas por dia por sexo")

# Plot interaction between gender and device
plot_by_var(merged, var1_ = "Device", var2_ = "Gender", collabels_ = c("Mobile", "PC"),
            colvalues_ = c("#FF9922", "#9922FF"), colname_ = "Dispositivo", regress = TRUE,
            xlab_ = "Data", ylab_ = "Visitas", title_ = "Visitas por dia por dispositivo e sexo", facet = TRUE)

# Plot interaction between device and gender
plot_by_var(merged, var1_ = "Gender", var2_ = "Device", collabels_ = c("F", "M", "NA"),
            colvalues_ = c("#FF9922", "#9922FF", "#22FF99"), colname_ = "Sexo", regress = TRUE,
            xlab_ = "Data", ylab_ = "Visitas", title_ = "Visitas por dia por sexo e dispositivo", facet = TRUE)

# Plot day of week effect on page visits per hour of day, non-faceted
p_line_DOW_visits <- plots_DOW(merged, xlab_ = "Hora", ylab_ = "Visitas",
                        title_ = "Visitas por hora por dia da semana", facet = FALSE)

# Plot page visits by state (histogram) + highlight NAs
p_hist_state_visits <- prepare_hist(merged, "State") %>% 
  hist_by_var(x_ = "State", y_ = "count", fill_ = "Color", xlab_ = "Estado",
              ylab_ = "Visitas", title_ = "Distribuição de visitas por estado", switchAxes = TRUE)

# Plot page visits by gender (histogram) + highlight NAs
p_hist_gender_visits <- prepare_hist(merged, "Gender") %>%
  hist_by_var(x_ = "Gender", y_ = "count", fill_ = "Color", ylimits = c(0, 600000),
              xlab_ = "Sexo", ylab_ = "Visitas", 
              title_ = "Distribuição de visitas por sexo")

# Plot page visits by device (histogram)
p_hist_device_visits <- prepare_hist(merged, "Device", color_ = FALSE) %>%
  hist_by_var(x_ = "Device", y_ = "count", xlabels_ = c("Mobile", "PC"),
              xlab_ = "Dispositivo", ylab_ = "Visitas", title_ = "Distribuição de visitas por dispositivo",
              angledLabels = FALSE)

# Plot page visits by year of birth (remove NAs)
p_hist_yob_visits <- prepare_hist(merged, "DOB_y", factor_ = FALSE) %>%
  subset(!is.na(DOB_y)) %>%
  hist_by_var(x_ = "DOB_y", y_ = "count", xbreaks_ = seq(1900, 2016, 8), xlab_ = "Ano de nascimento",
              ylab_ = "Visitas", title_ = "Distribuição de visitas por ano de nascimento", angled = TRUE,
              switchAxes = FALSE)

# Page visits by root URL (only top20)
p_hist_URL_visits <- activity_by_URL <- merged %>%
  prepare_hist("URL_split", color_ = FALSE) %>%
  prepare_top_n(n = 20) %>%
  hist_by_var(x_ = "URL_split", y_ = "count", xlab_ = "URL", ylab_ = "Visitas",
              title_ = "Distribuição de visitas por URL (top 20)", angledLabels = FALSE, switchAxes = TRUE)

######################################################
################### USER ACTIVITY ####################
######################################################
# We looked at raw page visits. But what about profile activity?
#merged %>%
#  group_by(UserId) %>%
#  summarise(count = n()) %>%
#  as.data.frame() %>%
#  summary()
# Min: 1, 1st Qu.: 2, Median: 5, Mean: 24.5, 3rd Qu.: 16, Max: 6100
# Most users have low activity, but some have absurdly high activity.
# This will result in a terrible raw histogram.

# Plot profile activity per day, with regression line
p_line_profiles <- merged %>% 
  group_by(UserId, Date) %>%
  summarise(count = n()) %>%
  plot_by_var(xlab_ = "Data", ylab_ = "Perfis", title_ = "Atividade por dia", regress = TRUE)
plot(p_line_profiles)

# Plot page visits per day by device, with regression lines
merged %>% 
  group_by(UserId, Date, Device) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  plot_by_var(var1_ = "Device", collabels_ = c("Mobile", "PC"),
              colvalues_ = c("#FF9922", "#9922FF"), colname_ = "Dispositivo", regress = TRUE,
              xlab_ = "Data", ylab_ = "Perfis", title_ = "Atividade por dia por dispositivo")

# Plot page visits per day by gender, with regression lines
merged %>% 
  group_by(UserId, Date, Gender) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  plot_by_var(var1_ = "Gender", collabels_ = c("F", "M", "NA"),
              colvalues_ = c("#FF9922", "#9922FF", "#22FF99"), colname_ = "Sexo", regress = TRUE,
              xlab_ = "Data", ylab_ = "Perfis", title_ = "Atividade por dia por sexo")

# Plot interaction between gender and device
merged %>% 
  group_by(UserId, Date, Device, Gender) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  plot_by_var(var1_ = "Device", var2_ = "Gender", collabels_ = c("Mobile", "PC"),
              colvalues_ = c("#FF9922", "#9922FF"), colname_ = "Dispositivo", regress = TRUE,
              xlab_ = "Data", ylab_ = "Perfis", title_ = "Atividade por dia por dispositivo e sexo",
              facet = TRUE)

# Plot interaction between device and gender
merged %>% 
  group_by(UserId, Date, Gender, Device) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  plot_by_var(var1_ = "Gender", var2_ = "Device", collabels_ = c("F", "M", "NA"),
              colvalues_ = c("#FF9922", "#9922FF", "#22FF99"), colname_ = "Sexo", regress = TRUE,
              xlab_ = "Data", ylab_ = "Perfis", title_ = "Atividade por dia por sexo e dispositivo",
              facet = TRUE)

# Plot day of week effect on profile activity per day, non-faceted
p_line_DOW_profiles <- users_by_day_hour <- merged %>% 
  group_by(UserId, Day, Hour) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  plots_DOW(xlab_ = "Hora", ylab_ = "Perfis", title_ = "Atividade por hora por dia da semana", facet = FALSE)

# Plot profile activity by device (histogram)
# This function cannot be simplified, since it is somewhat different from the others here.
# A profile can be active both on Mobile and PC, so we must account for that.
users_by_device <- merged %>%
  group_by(UserId, Device) %>%
  summarise(count = n()) %>%
  as.data.frame()
# get number of intersections < = > number of profiles that used both mobile and pc
cross.area = nrow(users_by_device)-length(unique(users_by_device$UserId))

users_by_device <- users_by_device %>%
  group_by(Device) %>%
  summarise(count = n()) %>%
  as.data.frame()
users_by_device$cross = rep(cross.area, 2)

p_hist_device_profiles <- users_by_device %>%
  ggplot() +
  geom_bar(aes(Device, count, fill = "Exclusivo"),
           stat = "identity") +
  geom_bar(aes(Device, cross, fill = "Ambos"),
           stat = "identity") +
  scale_fill_manual(name = "Atividade",
                    values = c("gray30", "gray60")) +
  scale_x_discrete(labels = c("Mobile", "PC")) +
  scale_y_continuous(labels = comma,
                     limits = c(0, 30000)) +
  xlab("Dispositivo") +
  ylab("Perfis") +
  ggtitle("Distribuição de perfis por dispositivo")
plot(p_hist_device_profiles)

# Plot profile activity by gender (histogram)
p_hist_gender_profiles <- users_by_gender <- merged %>%
  group_by(UserId, Gender) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  prepare_hist("Gender") %>%
  hist_by_var(x_ = "Gender", y_ = "count", fill_ = "Color", xlab_ = "Sexo", ylab_ = "Perfis",
              title_ = "Distribuição de perfis por sexo")

# Plot profile activity by state (histogram)
p_hist_state_profiles <- users_by_state <- merged %>% 
  group_by(UserId, State) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  prepare_hist("State") %>% 
  hist_by_var(x_ = "State", y_ = "count", fill_ = "Color", xlab_ = "Estado",
              ylab_ = "Perfis", title_ = "Distribuição de perfis por estado", switchAxes = TRUE)

# Plot profile activity by year of birth (histogram)
p_hist_yob_profiles <- users_by_yearbirth <- merged %>% 
  group_by(UserId, DOB_y) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  prepare_hist("DOB_y", factor_ = FALSE) %>%
  subset(!is.na(DOB_y)) %>%
  hist_by_var(x_ = "DOB_y", y_ = "count", xbreaks_ = seq(1900, 2016, 8), xlab_ = "Ano de nascimento",
              ylab_ = "Perfis", title_ = "Distribuição de perfis por ano de nascimento",
              angled = TRUE)

# Plot profile activity by root URL (histogram, top 20)
p_hist_URL_profiles <- users_by_URL <- merged %>% 
  group_by(UserId, URL_split) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  prepare_hist("URL_split", color_ = FALSE) %>%
  prepare_top_n(n = 20) %>%
  hist_by_var(x_ = "URL_split", y_ = "count", xlab_ = "URL", ylab_ = "Perfis",
              title_ = "Distribuição de perfis por URL (top 20)", angledLabels = FALSE, switchAxes = TRUE)

##############################
####### COMBINED PLOTS #######
##############################
# Main activity
plot_grid(p_line_visits, p_line_profiles)

# DOW effect
plot_grid(p_line_DOW_visits$plot, p_line_DOW_profiles$plot)

# Histograms by gender
plot_grid(p_hist_gender_visits$plot, p_hist_gender_profiles$plot)

# Histograms by device
plot_grid(p_hist_device_visits$plot, p_hist_device_profiles)

# Histograms by state
plot_grid(p_hist_state_visits$plot, p_hist_state_profiles$plot)

# Histograms by URL
plot_grid(p_hist_URL_visits$plot, p_hist_URL_profiles$plot)

# Histograms by year of birth
plot_grid(p_hist_yob_visits$plot, p_hist_yob_profiles$plot)
