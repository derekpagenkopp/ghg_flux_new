#------------------------------------------
# Load packages
library(tidyverse)
library(here)
library(gridExtra)


#-------------------------------------------
# Read in data

ch4_standards <- read_csv(here::here("data",
                                     "ch4_standard_raw.csv"))

#-------------------------------------------
# Use lm(), linear model function, to create regression model

### Change dates in name and filter

#fit_ch4_20190830 <- lm(ppm ~ area, data = ch4_standards %>% filter(date == "2019_08_30"))

#-------------------------------------------
#Use summary function to get summary information about linear model

### Changes dates

#summary(fit_ch4_20190830)

#--------------------------------------------
#Function to create standard curve


ggplot_regression <- function (fit, date) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[1], y = names(fit$model)[2])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(date, " CH4 Standard Curve"), 
         caption = paste("Adj R2 = ",
                         signif(summary(fit)$adj.r.squared, 5),
                         "Intercept =",signif(fit$coef[[1]],5 ),
                         " Slope =",signif(fit$coef[[2]], 5),
                         " P =",
                         signif(summary(fit)$coef[2,4], 5)))
}

dates_list_ch4 <- unique(ch4_standards$date) %>% 
  sort() %>% 
  as.list()

reg_list_ch4 <- map(dates_list_ch4, 
                ~ lm(ppm ~ area, data = ch4_standards %>% filter(date == .x))) %>% 
  set_names(dates_list_ch4)

reg_ggplot_list_ch4 <- map2(reg_list_ch4, 
                        dates_list_ch4, 
                        ~ ggplot_regression(fit = .x, date = .y))

### access plots by list element index or name
reg_ggplot_list_ch4[[4]]
reg_ggplot_list_ch4[["2019_08_30"]]


#DIsplay all plots
n <- length(reg_ggplot_list_ch4)

nCol <- floor(sqrt(n))

do.call("grid.arrange", c(reg_ggplot_list_ch4, ncol=nCol))

