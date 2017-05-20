library(moments)
library(varhandle)
library(lubridate)

pdata <- read.csv("train.csv", sep = ",", header = T)
hist(pdata$price_doc)

skewness(log(pdata$price_doc))
skewness(pdata$price_doc)
inspect.na(pdata)

pdata %>% 
        filter(build_year >1691 & build_year < 2018) %>% 
        ggplot(aes(x=build_year)) + geom_histogram(fill="red") + 
        geom_tile("Distribution of Building by Years")


pdata %>% 
        group_by(timestamp) %>%
        summarise(med_price = median(price_doc)) %>%
        ggplot(aes(x=timestamp, y=med_price)) +
        geom_line (color ="red") +
        geom_line(method="lm", color = "purple")

pdata %>% 
        mutate(month = month(timestamp)) %>%
        group_by(month) %>%
        summarize(med_price = median(price_doc)) %>%
        ggplot(aes(x=as.integer(month), y = med_price)) +
        geom_line(color='red', stat='identity') + 
        geom_point(color='red', size=2) + 
        scale_x_continuous(breaks=seq(1,12,1)) + 
        labs(x='Month', title='Price by month of year')

        