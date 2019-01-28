################################################################################
#                                                    
# ikashnitsky.github.io 2017-10-17
# Data acquisition in R - Part 1/4
# https://ikashnitsky.github.io/2017/data-acquisition-one
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#                                                  
################################################################################

# load required packages
library(tidyverse)      # data manipulation and viz


# built-in ---------------------------------------------------------------------
# Swiss Fertility and Socioeconomic Indicators (1888) Data. Let's check the difference in fertility based of rurality and domination of Catholic population.

swiss %>% 
        ggplot(aes(x = Agriculture, y = Fertility, 
                   color = Catholic > 50))+
        geom_point()+
        stat_ellipse()+
        theme_minimal(base_family = "mono")

ggsave("swiss.png", width = 8, height = 5)


# gapminder --------------------------------------------------------------------

library(gapminder)

gapminder %>% 
        ggplot(aes(x = year, y = lifeExp, 
                   color = continent))+
        geom_jitter(size = 1, alpha = .2, width = .75)+
        stat_summary(geom = "path", fun.y = mean, size = 1)+
        theme_minimal(base_family = "mono")

ggsave("gapminder.png", width = 8, height = 5)


# URL --------------------------------------------------------------------------

library(tidyverse)

galton <- read_csv("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/HistData/Galton.csv")

galton %>% 
        ggplot(aes(x = father, y = height))+
        geom_point(alpha = .2)+
        stat_smooth(method = "lm")+
        theme_minimal(base_family = "mono")

ggsave("galton.png", width = 8, height = 5)


# UNZIP ------------------------------------------------------------------------

# create a directory for the unzipped data
ifelse(!dir.exists("unzipped"), dir.create("unzipped"), "Directory already exists")

# specify the URL of the archive
url_zip <- "http://www.nyc.gov/html/nypd/downloads/zip/analysis_and_planning/citywide_historical_crime_data_archive.zip"

# download, unzip and read data
f <- tempfile()
download.file(url_zip, destfile = f)
unzip(f, exdir = "unzipped/.")

# if we want to keep the .zip file
path_unzip <- "unzipped/data_archive.zip"
ifelse(!file.exists(path_unzip), 
       download.file(url_zip, path_unzip, mode="wb"), 
       'file alredy exists')
unzip(path_unzip, exdir = "unzipped/.")

library(readxl)

murder <- read_xls("unzipped/Web Data 2010-2011/Seven Major Felony Offenses 2000 - 2011.xls",
                   sheet = 1, range = "A5:M13") %>% 
        filter(OFFENSE %>% substr(1, 6) == "MURDER") %>% 
        gather("year", "value", 2:13) %>% 
        mutate(year = year %>% as.numeric())

# plot
murder %>% 
        ggplot(aes(year, value))+
        geom_point()+
        stat_smooth(method = "lm")+
        theme_minimal(base_family = "mono")+
        labs(title = "Murders in New York")


ggsave("new-york.png", width = 8, height = 5)


# Figshare ---------------------------------------------------------------------

library(rfigshare)

# find the dataset
# fs_search("ice hockey players") # not working

url <- fs_download(article_id = "3394735")

hockey <- read_csv(url)

hockey %>% 
        ggplot(aes(x = year, y = height))+
        geom_jitter(size = 2, color = "#35978f", alpha = .1, width = .25)+
        stat_smooth(method = "lm", size = 1)+
        ylab("height, cm")+
        xlab("year of competition")+
        scale_x_continuous(breaks = seq(2005, 2015, 5), labels = seq(2005, 2015, 5))+
        theme_minimal(base_family = "mono")

ggsave("ice-hockey.png", width = 8, height = 5)