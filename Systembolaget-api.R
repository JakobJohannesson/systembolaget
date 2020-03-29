#### Systembolaget API ####

## Laddar in paket ##
library(httr)
library(XML)
library(methods)
library(dplyr)
library(stringr)
library(ggplot2)

# tar bort vetenskapligt notering
options(scipen = 999)

# hämtar data från Systembolaget
skr<-GET(url = "https://www.systembolaget.se/api/assortment/products/xml", type="basic")
hej<-content(skr, "text")
xmldataframe <- xmlToDataFrame(hej)

df<-xmldataframe[-c(1,2),]

df<- df %>% select(Namn,Namn2,Prisinklmoms,PrisPerLiter,Volymiml,Varugrupp,Typ,
                   Stil,Forpackning, Forslutning, Ursprung, Ursprunglandnamn,
                   Producent, Leverantor, Argang, Alkoholhalt, SortimentText,
                   Ekologisk, Pant)



df$Prisinklmoms<-as.numeric(df$Prisinklmoms)
df$PrisPerLiter<-as.numeric(df$PrisPerLiter)
df$Volymiml<-as.numeric(df$Volymiml)
df$Volymiml<-df$Volymiml/1000

df$Alkoholhalt<-str_replace_all(df$Alkoholhalt, pattern = "%", "")
df$Alkoholhalt<-as.numeric(df$Alkoholhalt)
df$Alkoholhalt<-df$Alkoholhalt/100
df$APK<-((df$Volymiml*1000)*df$Alkoholhalt)/df$Prisinklmoms
df<-df %>% filter(APK>0.9) # tar bort allt med låg apk



df$Varugrupp<-as.factor(df$Varugrupp)

g <- ggplot(df, aes(Varugrupp,Alkoholhalt*100))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Systembolaget alkoholhalt per varugrupp",
       caption="Source: Systembolagets API",
       x="Varugrupp",
       y="Alkoholhalt")+ theme_bw() + 
  theme(axis.title.y = 
          element_text(angle = 0, 
                       hjust = 1, 
                       vjust = 0.5), 
        plot.title = 
          element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45),
        panel.grid.major.x = 
          element_blank(),
        panel.grid.minor.x = 
          element_blank(),
        panel.grid.major.y = 
          element_line(color = "dark gray")) 
