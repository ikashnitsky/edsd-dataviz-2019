#===============================================================================
# 2019-01-28 -- EDSD dataviz
# ggplot2 examples
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# load the packages
library(tidyverse)
library(janitor)
library(ggthemes)



# base --------------------------------------------------------------------


# automatic plots for linear model
obj <- lm(data = swiss, Fertility ~ Education)
plot(obj)

# blank map
library(rgdal)
shapefile <- readOGR("data/.", "shape-france")
plot(shapefile)



# ggplot2 -----------------------------------------------------------------

# The logic


ggplot()

swiss %>% View

?swiss



# geom_point
ggplot(data = swiss,
       aes(x = Agriculture, y = Fertility)) +
        geom_point()

gg <- last_plot()

# saving a plot
ggsave(filename = "test.png", plot = gg)


# stat_smooth
gg + stat_smooth(method = "lm", se = F, col = "red")


# stat_ellipse
swiss %>% 
        ggplot(aes(x = Agriculture, y = Fertility, 
                   color = Catholic > 50))+
        geom_point()+
        stat_ellipse()+
        theme_minimal(base_family = "mono")

gg <- last_plot()


# line / path
df_aq <- airquality %>% 
        clean_names() %>% 
        mutate(date = paste(day, month, "1973", sep = "-") %>%  lubridate::dmy(),
               month = month %>% factor) 

        

ggplot(df_aq)+
        geom_line(aes(x = date, y = temp))


df_aq %>% 
        ggplot(aes(x = date, y = temp))+
        geom_path()


df_aq %>% 
        ggplot(aes(x = day, y = temp, group = month))+
        geom_line()

df_aq %>% 
        ggplot(aes(x = day, y = temp, color = month))+
        geom_line()

df_aq %>% 
        ggplot(aes(x = date, y = temp, color = month))+
        geom_line()



library(gapminder)

gapminder %>% 
        select(1:4) %>% 
        group_by(continent, year) %>% 
        summarise(avg_e0 = lifeExp %>% mean) %>% 
        ungroup() %>% 
        ggplot(aes(x = year, y = avg_e0, 
                   color = continent))+
        geom_path(size = 1)+
        theme_minimal(base_family = "mono")
        

gapminder %>% 
        ggplot(aes(x = year, y = lifeExp, 
                   color = continent))+
        geom_jitter(size = 1, alpha = .2, width = .75)+
        stat_summary(geom = "path", fun.y = mean, size = 1)+
        theme_minimal(base_family = "mono")



load("data/Denmark.Rdata")

df %>% 
        filter(year == "y2004", sex == "m",!age %in% c("total", "open")) %>% 
        ggplot()+
        geom_line(aes(x = age, y = mx, group = region, color = region))+
        scale_y_continuous(trans = "log", breaks = c(.0001,.001,.01))+
        theme_few()



# density / ecdf (airquality)

ggplot(airquality)+
        geom_density(aes(x = Temp, color = factor(Month)), size=1)+
        scale_color_viridis_d(option = "D",end = .8)+
        theme_minimal()+
        theme(legend.position = c(.1,.8))

ggplot(airquality)+
        stat_ecdf(aes(x = Temp, color = factor(Month)), size=1)+
        scale_color_viridis_d(option = "B", end = .8)+
        theme_fivethirtyeight()+
        theme(legend.position = c(.1,.8))


# boxplot
ggplot(airquality) +
        geom_boxplot(aes(x = factor(Month), y = Temp))

# violin
ggplot(airquality) +
        geom_violin(aes(x = factor(Month), y = Temp, fill = factor(Month)))

# jitter
ggplot(airquality) +
        geom_jitter(aes(x = factor(Month), y = Temp, color = factor(Month)),
                    width = .2)


# faceting
ggplot(airquality)+
        stat_ecdf(aes(x = Temp, color = factor(Month)), size=1)+
        scale_color_viridis_d(option = "B", end = .8)+
        theme_fivethirtyeight()+
        theme(legend.position = c(.8,.2))+
        facet_wrap(~factor(Month), ncol = 3)

# themes 
gg + theme_minimal()
gg + theme_bw()
gg + theme_light()
gg + theme_excel()    # ugly, isn"t it?
gg + theme_few()      # one of my favorites
gg + theme_economist()
gg + theme_wsj()
gg + theme_fivethirtyeight() # I love this one
gg + theme_solarized()
gg + theme_dark()
# ... feel free to test them all))


# colors ------------------------------------------------------------------

# Parameter "color" changes the color of lines and points
# Parameter "fill" changes the color of shapes (see the violin example)
# The way you override ggplot"s defaults is to use functions
# scale_color_[...] or scale_fill_[...] (use TAB to see options)
# I really recommend viridis colors
# NOTE: with viridis you need to know if you variable is continious or categotical
# The video on viridis https://youtu.be/xAoljeRJ3lU

# paletteer !!!
library(paletteer)
# https://github.com/EmilHvitfeldt/r-color-palettes



# fonts -------------------------------------------------------------------

# library(extrafont)
library(hrbrthemes)

gg + theme_minimal(base_family = font_rc)




# scale _ identity --------------------------------------------------------

# https://twitter.com/ikashnitsky/status/937786580231696384

n <- 1e2

tibble(x = runif(n),
       y = runif(n),
       size = runif(n, min = 4, max = 20)) %>% 
        ggplot(aes(x, y, size = size)) + 
        geom_point(color = "white", pch = 42)+
        scale_size_identity()+
        coord_cartesian(c(0, 1), c(0, 1))+
        theme_void()+
        theme(panel.background = element_rect(fill = "black"),
              plot.background = element_rect(fill = "black"))


# generate bubbles of random color and size
n <- sample(20:50, 1)

tibble(x = runif(n),
       y = runif(n),
       size = runif(n, min = 3, max = 20),
       color = rgb(runif(n),runif(n),runif(n))) %>% 
        ggplot(aes(x, y, size = size, color = color)) + 
        geom_point()+
        scale_color_identity()+
        scale_size_identity()+
        coord_cartesian(c(0,1),c(0,1))+
        theme_void()






# ggridges ----------------------------------------------------------------

# https://twitter.com/ikashnitsky/status/886978944985071616

library(ggridges)
library(wpp2015)

# get the UN country names
data(UNlocations)

countries <- UNlocations %>% pull(name) %>% paste

# data on male life expectancy at birth
data(e0M) 

e0M %>% 
        filter(country %in% countries) %>% 
        select(-last.observed) %>% 
        gather(period, value, 3:15) %>% 
        ggplot(aes(x = value, y = period %>% fct_rev()))+
        geom_density_ridges(aes(fill = period))+
        scale_fill_viridis_d(
                option = "B", direction = -1, begin = .1, end = .9
        )+
        labs(
                x = "Male life expectancy at birth",
                y = "Period",
                title = "Global convergence in male life expectancy at birth since 1950",
                subtitle = "UNPD World Population Prospects 2015 Revision, via wpp2015",
                caption = "ikashnitsky.github.io"
        )+
        theme_minimal(base_family =  "mono")+
        theme(legend.position = "none")




# label lines hack --------------------------------------------------------

# https://twitter.com/ikashnitsky/status/1045428469801385987
