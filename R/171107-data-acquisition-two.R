################################################################################
#                                                    
# ikashnitsky.github.io 2017-11-07
# Data acquisition in R - Part 2/4
# https://ikashnitsky.github.io/2017/data-acquisition-two
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#                                                  
################################################################################

# load required packages
library(tidyverse)      # data manipulation and viz


# Eurostat ---------------------------------------------------------------------

library(tidyverse) 
library(eurostat) 
library(lubridate)
library(viridis)

search_eurostat("life expectancy")

# download the selected dataset
e0 <- get_eurostat("demo_mlexpec")

e0 %>% 
        filter(! sex == "T",
               age == "Y65", 
               geo %in% c("DE", "FR", "IT", "RU", "ES", "UK")) %>% 
        ggplot(aes(x = time %>% year(), y = values, color = sex))+
        geom_path()+
        facet_wrap(~ geo, ncol = 3)+
        labs(y = "Life expectancy at age 65", x = NULL)+
        theme_minimal(base_family = "mono")

ggsave("eurostat.png", width = 8, height = 5)



# World Bank -------------------------------------------------------------------
library(tidyverse) 
library(wbstats)

# search for a dataset of interest
wbsearch("fertility") %>% View

# fetch the selected dataset
df_wb <- wb(indicator = "SH.MMR.RISK.ZS", startdate = 2000, enddate = 2015)

# have look at the data for one year
df_wb %>% filter(date == 2015) %>% View

df_wb %>% 
        filter(iso2c %in% c("V4", "V1", "1W")) %>% 
        ggplot(aes(x = date %>% as.numeric(), y = value, color = country))+
        geom_path(size = 1)+
        scale_color_brewer(NULL, palette = "Dark2")+
        labs(x = NULL, y = NULL, title = "Lifetime risk of maternal death (%)")+
        theme_minimal(base_family = "mono")+
        theme(panel.grid.minor = element_blank(),
              legend.position = c(.8, .9))

ggsave("worldbank.png", width = 8, height = 5)

# OECD -------------------------------------------------------------------------
library(tidyverse) 
library(viridis)
library(OECD)


search_dataset("unemployment") %>% View

df_oecd <- get_dataset("AVD_DUR")
names(df_oecd) <- names(df_oecd) %>% tolower()

df_oecd %>% 
        filter(country %in% c("EU16", "EU28", "USA"), sex == "MEN", ! age == "1524") %>% 
        ggplot(aes(obstime, age, fill = obsvalue))+
        geom_tile()+
        scale_fill_viridis("Months", option = "B")+
        scale_x_discrete(breaks = seq(1970, 2015, 5) %>% paste)+
        facet_wrap(~ country, ncol = 1)+
        labs(x = NULL, y = "Age groups", 
             title = "Average duration of unemployment in months, males")+
        theme_minimal(base_family = "mono")

ggsave("oecd.png", width = 8, height = 5)


# WID --------------------------------------------------------------------------
library(tidyverse) 

#install.packages("devtools")
devtools::install_github("WIDworld/wid-r-tool")
library(wid)

?wid_series_type
?wid_concepts


df_wid <- download_wid(
        indicators = "shweal", # Shares of personal wealth
        areas = c("FR", "GB"), # In France an Italy
        perc = c("p90p100", "p99p100") # Top 1% and top 10%
)


df_wid %>% 
        ggplot(aes(x = year, y = value, color = country)) +
        geom_path()+
        labs(title = "Top 1% and top 10% personal wealth shares in France and Great Britain",
             y = "top share")+
        facet_wrap(~ percentile)+
        theme_minimal(base_family = "mono")

ggsave("wid.png", width = 8, height = 5)