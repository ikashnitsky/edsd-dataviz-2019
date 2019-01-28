#===============================================================================
# 2019-01-25 -- EDSD
# Pre-install packages
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


# First, install pacman to deal easier with other packages
# solution by Sacha Epskamp from: http://stackoverflow.com/a/9341833/4638884
if (!require('pacman', character.only = TRUE)) {
    install.packages('pacman', dep = TRUE)
    if (!require('pacman', character.only = TRUE))
        stop("Package not found")
}

# install packages
pacman::p_load(
    install = T, update = F, 
    tidyverse,
    magrittr,
    rio,
    janitor,
    plotly,
    paletteer, # all patettes in one place 
    ticolore,
    # ggplot extensions
    ggthemes,
    hrbrthemes,
    ggtern,
    ggridges,
    GGally,
    ggrepel,
    ggforce,
    treemap,
    geofacet,
    gghighlight,
    gganimate,
    tweenr,
    cowplot,
    # srpatial
    rgdal,
    rgeos,
    sf,
    raster,
    tmap,
    leaflet
)