library(readxl)
library(tidyverse)
library(rvest)
library(scales)
library("ggrepel")
library(plotly)
library(ggimage)
library(jpeg)
library(gt)


url <- read_html("https://studyinthestates.dhs.gov/sevis-data-mapping-tool/october-2021-sevis-data-mapping-tool-data")
x <- html_table(url)[[1]]
names(x)<-as.character(x[1,])
oct2021<-x[-1,]%>%
  mutate(year=2021)

url <- read_html("https://studyinthestates.dhs.gov/sevis-data-mapping-tool/january-2020-sevis-data-mapping-tool-data")
jan2020 <- html_table(url)[[1]]%>%
  mutate(year=2019)


names(oct2021)[1]<-'Nationality'
names(jan2020)<-names(oct2021)


oct2021$active_students<-as.numeric(oct2021$active_students)
oct2021<-oct2021%>%
  mutate(x=active_students)
jan2020<-jan2020%>%mutate(y=active_students)

gr<-left_join(oct2021,jan2020,by='Nationality')%>%
  filter(x>y)%>%
  mutate(change=x-y)%>%
  select('Nationality','change')

de<-left_join(oct2021,jan2020,by='Nationality')%>%
  filter(x<y)%>%
  mutate(change=y-x)%>%
  select('Nationality','change') 



world <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")%>%
  mutate(Nationality=toupper(COUNTRY))

world$Nationality<-sub("CONGO, REPUBLIC OF THE","CONGO (BRAZZAVILLE)",world$Nationality)
world$Nationality<-sub("CONGO, DEMOCRATIC REPUBLIC OF THE","CONGO (KINSHASA)",world$Nationality)
world$Nationality<-sub("KOREA, SOUTH","SOUTH KOREA",world$Nationality)

world.gr<-world%>%
  left_join(gr,by='Nationality')
world.gr$hover <- with(world.gr, paste(COUNTRY,":increased",change))
plot_ly(world.gr, type='choropleth', locations=world.gr$CODE, z=world.gr$change, text=world.gr$hover, colors="YlGn",hoverinfo = "text")%>%
  layout(title = list(text = paste0("<br>",
                                    'Increased Enrollment of Students from These Countries<br>in 2021 Compared to 2019',
                                    '<br>')),
         annotations = 
           list(x = 1, y = 0.1,text = paste0("Data Source:www.studyinthestates.dhs,gov"),showarrow = F))
hoverinfo = "text"
world.de<-world%>%
  left_join(de,by='Nationality')
world.de$hover <- with(world.de, paste(COUNTRY,":decreased",change))
plot_ly(world.de, type='choropleth', locations=world.de$CODE, z=world.de$change, text=world.de$hover,hoverinfo = "text", colors="OrRd")%>%
  layout(title = list(text = paste0("<br>",
                                    'Decreased Enrollment of Students from These Countries<br>in 2021 Compared to 2019',
                                    '<br>')),
         annotations = 
           list(x = 1, y = 0.1, text = paste0("Data Source:www.studyinthestates.dhs,gov"),showarrow = F))




