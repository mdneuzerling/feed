library(tidyverse)
library(sqldf)
library(visdat)

feed_raw <- read_csv("C:/Users/mdneuzerling/Google Drive/Projects/feed/feed.csv")

#' `read_csv` does not convert strings to factors, but we do have a few factors
#' here. We manually specify them.
feed_raw <- feed_raw %>% mutate(
    `Area Abbreviation` = `Area Abbreviation` %>% as.factor,
    `Area Code` = `Area Code` %>% as.factor,
    `Area` = `Area` %>% as.factor,
    `Item Code` = `Item Code` %>% as.factor,
    `Item` = `Item` %>% as.factor,
    `Element Code` = `Element Code` %>% as.factor,
    `Element` = `Element` %>% as.factor
)

feed_raw <- feed_raw %>% mutate_if(
    vars(starts_with("Y") | starts_with(l)),
    as.factor
)

feed_raw %>% mutate_at(
    vars(-(starts_with("Y") && starts_with("l"))), 
    funs(as.factor)
) 

feed_raw %>% mutate_at(
    vars(-starts_with("Y"), -starts_with("l")), 
    funs(as.factor)
) 

#' We're also going to add an indexing column
feed_raw <- feed_raw %>% cbind(Id = seq(1, nrow(.)), .)

#' The initial wide format isn't very convenient for modelling, but it is 
#' appropriate for visualising the missing data.
#' Some missing data, but it seems that a row is either complete, or is missing
#' an entire string of years. It actually looks as though collection starts in 
#' different years for each item, but once collection starts it continues
#' uninterrupted. What happened in 1993?
feed_raw %>% vis_dat(warn_large_data = FALSE)

#' Gather all of those year columns into a simple Key/Value long format.
feed <- feed_raw %>% 
    gather(key = Year, value = Production, starts_with("Y")) %>% 
    mutate(
        Year = Year %>% substr(2, 5) %>% as.ordered
    )

#' We use a join (merge) to introduce a change column for each (Area, Item,
#' Element) tuple, so that we can track how much production has increased or
#' decreased since last year.
x <- feed %>% merge(
    feed,
    by = c("Area", "Item", "Element"),
    all.x = True
)

feed <- sqldf(
    "
    SELECT       CURRENTYEAR.*
                ,LASTYEAR.Production AS ProductionLastYear
                ,CURRENTYEAR.Production - LASTYEAR.Production as ProductionChange
    FROM        feed AS CURRENTYEAR
    LEFT JOIN   feed AS LASTYEAR
        ON      CURRENTYEAR.Area = LASTYEAR.Area
        AND     CURRENTYEAR.Item = LASTYEAR.Item
        AND     CURRENTYEAR.Element = LASTYEAR.Element
        AND     CURRENTYEAR.Year = LASTYEAR.Year + 1
    "
)

#' Change in food and feed production over the years.
feed %>%
    group_by(Year, Element) %>% 
    summarise(Production = Production %>% sum(na.rm = TRUE)) %>%
    ggplot(aes(x = Year, y = Production, group = Element, colour = Element)) +
    geom_line()

#' Change in production for each category, as a heat map.
feed %>% 
    group_by(Year, Item) %>%
    filter(!is.na(ProductionLastYear)) %>% 
    summarise(
        Production = Production %>% sum(na.rm = TRUE),
        ProductionLastYear = ProductionLastYear %>% sum(na.rm = TRUE)
    ) %>% 
    mutate(PercentProductionChange = 
               (Production - ProductionLastYear) / ProductionLastYear) %>% 
    ggplot(aes(x = Year, y = Item, fill = PercentProductionChange)) +
    geom_raster() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         limits = c(-0.5, 0.5))

















