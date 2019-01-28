################################################################################
#
# ikashnitsky.github.io 2017-12-10
# Data acquisition in R 3/4
# https://ikashnitsky.github.io/2017/data-acquisition-three
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#
################################################################################


# HMD --------------------------------------------------------------------------

# load required packages
library(HMDHFDplus)
library(tidyverse)
library(purrr)


# help function to list the available countries
country <- getHMDcountries()

# remove optional populations
opt_pop <- c("FRACNP", "DEUTE", "DEUTW", "GBRCENW", "GBR_NP")
country <- country[!country %in% opt_pop]

# temporary function to download HMD data for a simgle county (dot = input)
tempf_get_hmd <- . %>% readHMDweb("Exposures_1x1", ik_user_hmd, ik_pass_hmd) 


# download the data iteratively for all countries using purrr::map()
exposures <- country %>% map(tempf_get_hmd)

# data transformation to apply to each county dataframe
tempf_trans_data <- . %>% 
        select(Year, Age, Female, Male) %>% 
        filter(Year %in% 2012) %>%
        select(-Year) %>%
        transmute(age = Age, ratio = Male / Female * 100)

# perform transformation
df_hmd <- exposures %>% 
        map(tempf_trans_data) %>% 
        bind_rows(.id = "country")

# summarize all ages older than 90 (too jerky)
df_hmd_90 <- df_hmd %>% 
        filter(age %in% 90:110) %>% 
        group_by(country) %>% 
        summarise(ratio = ratio %>% mean(na.rm = T)) %>%
        ungroup() %>% 
        transmute(country, age = 90, ratio)

# insert summarized 90+
df_hmd_fin <- bind_rows(df_hmd %>% filter(!age %in% 90:110), df_hmd_90)

# finaly - plot
df_hmd_fin %>%  
        ggplot(aes(age, ratio, color = country, group = country))+
        geom_hline(yintercept = 100, color = "grey50", size = 1)+
        geom_line(size = 1)+
        scale_y_continuous(limits = c(0, 120), 
                           expand = c(0, 0), 
                           breaks = seq(0, 120, 20))+
        scale_x_continuous(limits = c(0, 90), 
                           expand = c(0, 0), 
                           breaks = seq(0, 80, 20))+
        facet_wrap(~country, ncol = 6)+
        theme_minimal(base_family = "mono", base_size = 15)+
        theme(legend.position = "none",
              panel.border = element_rect(size = .5, fill = NA, 
                                          color = "grey50"))+
        labs(x = "Age", 
             y = "Sex ratio, males per 100 females", 
             title = "Sex ratio in all countries from Human Mortality Database",
             subtitle = "HMD 2012, via HMDHFDplus by @timriffe1",
             caption = "ikashnitsky.github.io")

ggsave("hmd.png", width = 10, height = 12)




# wpp 2015 ---------------------------------------------------------------------
library(wpp2015)
library(tidyverse)
library(ggridges)
library(viridis)

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
        scale_fill_viridis(discrete = T, option = "B", direction = -1, 
                           begin = .1, end = .9)+
        labs(x = "Male life expectancy at birth",
             y = "Period",
             title = "Global convergence in male life expectancy at birth since 1950",
             subtitle = "UNPD World Population Prospects 2015 Revision, via wpp2015",
             caption = "ikashnitsky.github.io")+
        theme_minimal(base_family =  "mono", base_size = 14)+
        theme(legend.position = "none")

ggsave("wpp2015.png", width = 10, height = 7)



# ESS (Jorge) -------------------------------------------------------------
library(ess)
library(tidyverse) 

# help gunction to see the available countries
show_countries()

# check the available rounds for a selected country
show_country_rounds("Netherlands")

# get the full dataset of the last (8) round
df_ess <- ess_rounds(8, your_email =  ik_email) 

# select a variable and calculate mean value
df_ess_select <- df_ess %>% 
        bind_rows() %>% 
        select(idno, cntry, trstplc) %>% 
        group_by(cntry) %>% 
        mutate(avg = trstplc %>% mean(na.rm = T)) %>% 
        ungroup() %>% 
        mutate(cntry = cntry %>% as_factor() %>% fct_reorder(avg))

df_ess_select %>% 
        ggplot(aes(trstplc, fill = avg))+
        geom_histogram()+
        scale_x_continuous(limits = c(0, 11), breaks = seq(2, 10, 2))+
        scale_fill_gradient("Average\ntrust\nscore", 
                            low = "black", high = "aquamarine")+
        facet_wrap(~cntry, ncol = 6)+
        theme_minimal(base_family = "mono")+
        labs(x = "Trust score [0 -- 10]",
             y = "# of respondents",
             title = "Trust in police",
             subtitle = "ESS wave 8 2017, via ess by @cimentadaj",
             caption = "ikashnitsky.github.io")

ggsave("ess.png", width = 8, height = 6)




# American Census data ----------------------------------------------------

library(tidycensus)
library(tidyverse)
library(viridis)
library(janitor)
library(sf)
# to use geom_sf we need the latest development version of ggplot2
devtools::install_github("tidyverse/ggplot2", "develop")
library(ggplot2)


# you need a personal API key, available free at
# https://api.census.gov/data/key_signup.html
# normally, this key is to be stored in .Renviron

# see state and county codes and names
fips_codes %>% View

# the available variables
load_variables(year = 2015, dataset = "acs5") %>% View

# data on median age of population in Chicago
df_acs <- get_acs(
        geography = "tract",
        county = "Cook County",
        state = "IL",
        variables = "B01002_001E",
        year = 2015,
        key = ik_api_acs,
        geometry = TRUE
) %>% clean_names()


# map the data
df_acs %>% 
        ggplot()+
        geom_sf(aes(fill = estimate %>% 
                            cut(breaks = seq(20, 60, 10))), 
                color = NA)+
        scale_fill_viridis_d("Median age", begin = .4)+
        coord_sf(datum = NA)+
        theme_void(base_family =  "mono")+
        theme(legend.position = c(.15, .15))+
        labs(title = "Median age of population in Chicago\nby census tracts\n",
             subtitle = "ACS 2015, via tidycensus by @kyle_e_walker",
             caption = "ikashnitsky.github.io", 
             x = NULL, y = NULL)

ggsave("tidycensus.png", width = 6, height = 6)