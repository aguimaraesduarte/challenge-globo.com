# @author: Andre Duarte
# @date: June 1st, 2016
# @purpose: Globo.com presentation
# @confidential: True

# Set global theme for plots
theme_set(theme_bw(base_size = 14))
theme_update(plot.title = element_text(hjust = 0.5))

# Line plot by 0, 1, or 2 variables.
# The input dataframe <df> is grouped by <var_default_>, which is "Date" if no changes have been made to
# the *read_tables.R* file. This is the variable used as the x-axis.
# <var1_> and <var2> are optional and define the other variables to plot by.
# If both are specified, it is important to also set <facet> to TRUE to get the desired effect (or else
# ggplot won't be able to plot correctly).
# By default, <var1_> is used as the color variable, and we facet by <var2_> horizontally.
# Custom labels and values can be set by the user using <collabels_>, <colvalues_>, and <colname_>.
# <regress> allows to plot the OLS regression fit, and standard error curves can be set with <se_>.
plot_by_var <- function(df, var_default_ = "Date", var1_ = NULL, var2_ = NULL,
                        collabels_ = NULL, colvalues_ = NULL, colname_ = NULL,
                        regress = TRUE, se_ = FALSE, facet = FALSE,
                        xlab_ = "Date", ylab_ = NULL, title_ = NULL){
  df %>%
  {if(is.null(var1_)){
    group_by_(., var_default_)
    } else if (is.null(var2_)){
      group_by_(., var_default_, var1_)
    } else {
      group_by_(., var_default_, var1_, var2_)
    } } %>%
    summarise(count = n()) %>%
    {if(is.null(var1_)){
      ggplot(., aes_string(var_default_, "count"))
      } else{
        ggplot(., aes_string(var_default_, "count", color = var1_))
      } } +
    geom_line(size = 1) +
    {if(regress) geom_smooth(method = 'lm',
                             se = se_,
                             alpha = 0.5,
                             size = 1,
                             linetype = 'dashed')} +
    {if(!is.null(var2_) & facet) facet_wrap(~.[[var2_]])} +
    scale_y_continuous(labels = comma) +
    scale_x_date(breaks = pretty_breaks(15)) +
    {if(! ((is.null(collabels_)) | (is.null(colvalues_))) ){
      scale_colour_manual(labels = collabels_,
                          values = colvalues_,
                          name = colname_,
                          na.value = "grey")
    } } +
    labs(color = var1_) +
    ylab(ylab_) +
    xlab(xlab_) +
    ggtitle(title_) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Line plot of hourly activity by day of week in a single plot.
# The input dataframe <df> is sent to ggplot, where the x-axis is defined by default (and immutable) as Hour.
# <yinter_> specifies the month average of user activity.
# A plot with the seven days activity will be produced.
plot_by_DOW <- function(df, yinter_ = NULL, xlab_ = NULL, ylab_ = NULL, title_ = NULL){
  df %>%
    ggplot(aes(Hour, count, color = DOW, group = DOW)) +
    geom_hline(mapping = aes(yintercept = yinter_, alpha = "Média mensal"),
               size = 1) +
    stat_summary(geom = "line",
                 fun.y = "mean",
                 size = 1) +
    scale_y_continuous(labels = comma) +
    scale_color_discrete(name = "Dia da semana") +
    scale_alpha_manual(name = "Atividade",
                       values = 1,
                       guide = guide_legend(override.aes = list(size = 1))) +
    ylab(ylab_) +
    xlab(xlab_) +
    ggtitle(title_)
}

# Line plot of hourly activity faceted by day of week in a single plot.
# The input dataframe <df> is sent to ggplot, where the x-axis is defined by default (and immutable) as Hour.
# <dataDOW> specifies a data frame containing each day of week's average user activity.
# <yinter_> specifies the month average of user activity.
# A plot with the seven days activity will be produced.
plot_by_DOW_facet <- function(df, dataDOW = NULL, yinter_ = NULL, xlab_ = NULL, ylab_ = NULL, title_ = NULL){
  df %>%
    ggplot(aes(Hour, count)) +
    geom_hline(data = dataDOW,
               mapping = aes(yintercept = avgvalue, color = "Média do dia"),
               size = 1,
               alpha = 0.5) +
    geom_hline(mapping = aes(yintercept = yinter_, color = "Média mensal"),
               size = 1,
               alpha = 0.5) +
    stat_summary(geom = "line",
                 fun.y = "mean",
                 mapping = aes(group = DOW),
                 color = "gray50",
                 size = 1) +
    facet_grid(DOW~.) +
    scale_y_continuous(labels = comma) +
    scale_color_discrete(name = "Atividade") +
    ylab(ylab_) +
    xlab(xlab_) +
    ggtitle(title_)
}

# This function ties together the day-of-week effect plots.
# The user can decide whether to plot the single plot, or the faceted one.
plots_DOW <- function(df, xlab_ = NULL, ylab_ = NULL, title_ = NULL, facet = F){
  tmp <- df %>% 
    group_by(Day, Hour) %>%
    summarise(count = n()) %>%
    as.data.frame()
  
  tmp$DOW <- factor(weekdays(as.Date(paste("2016-05-", tmp$Day, sep = ""))),
                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                               "Saturday", "Sunday"),
                    labels = c("Segunda", "Terça", "Quarta", "Quinta", "Sexta",
                               "Sábado", "Domingo"),
                    ordered = TRUE)
  mean_per_month <- as.integer(mean(tmp$count))
  mean_per_DOW <- ddply(tmp, .(DOW), summarize, avgvalue = mean(count))
  
  p <- plot_by_DOW(tmp, mean_per_month, xlab_, ylab_, title_)
  
  if(facet){
    p <- plot_by_DOW_facet(tmp, mean_per_DOW, mean_per_month, xlab_, ylab_, title_)
  }
  print(p)
}

# This function prepares the data prior to building a histogram by grouping along a given variable.
# The dataframe <df> is grouped by the variable <groupBy_>.
# If specified, this grouping variable can be factored and ordered by number of counts (to have an ordered histogram).
# If specified, a color variable is appended to the dataframe (to showcase NAs for example).
# This function returns a dataframe.
prepare_hist <- function(df, groupBy_ = NULL, factor_ = T, color_ = T){
  tmp <- df %>%
    group_by_(groupBy_) %>%
    summarise(count = n()) %>%
    as.data.frame()
  
  if(factor_) tmp[[groupBy_]] <- factor(tmp[[groupBy_]],
                                        levels = tmp[order(tmp$count), groupBy_])
  if(color_) tmp$Color <- ifelse( (tmp[[groupBy_]] ==  "NA") | (is.na(tmp[[groupBy_]])) , "yes", NA)
  
  tmp
}

# This function filters an input dataframe <df> to only the top <n> observations by count.
# Returns the filtered dataframe, which can be passed to the hist_by_var() function.
prepare_top_n <- function(df, n = 20){
  top_n <- levels(df$URL_split)[(length(df$URL_split)-(n-1)):(length(df$URL_split))]
  df <- df[df$URL_split %in% top_n,]
  df$URL_split <- factor(df$URL_split,
                         levels = df[order(df$count), "URL_split"])
  df
}

# This function creates and plots a histogram of the input data.
# The input dataframe <df> can come from the prepare_hist() function.
# The user can specify the X and Y variables to plot, as well as the variable to use for filling the color.
# The user can specify custom labels, tick breaks, or limits to the plotting region.
# The user can specify whether the x-axis labels should be angled (useful when many labels).
# The user can specify whether the axes should be switched (useful when the x labels are long to remain horizontal).
hist_by_var <- function(df, x_, y_, fill_ = NULL, xlabels_ = NULL, xbreaks_ = NULL, ylimits_ = NULL,
                        xlab_ = NULL, ylab_ = NULL, title_ = NULL, angledLabels = FALSE, switchAxes = FALSE){
  X = df[[x_]]
  Y = df[[y_]]
  if(!is.null(fill_)){
    FILL = df[[fill_]]
    p <- df %>%
      ggplot(mapping = aes(X, Y, fill = FILL)) +
      scale_fill_discrete(guide = FALSE)
  } else{
    p <- df %>%
      ggplot(mapping = aes(X, Y, fill = "grey60")) +
      scale_fill_manual(guide = FALSE,
                        values = "grey60")
  }
  if(!is.null(xlabels_)){
    p <- p + scale_x_discrete(labels = xlabels_)
  }
  if(!is.null(xbreaks_)){
    p <- p + scale_x_continuous(breaks = xbreaks_)
  }
  p <- p +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = comma,
                       limits = ylimits_) +
    xlab(xlab_) +
    ylab(ylab_) +
    ggtitle(title_)
  if(angledLabels){
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  if(switchAxes){
    p <- p + coord_flip()
  }
  print(p)
}
