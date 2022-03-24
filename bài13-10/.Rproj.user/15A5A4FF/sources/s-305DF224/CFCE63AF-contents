#install treemapify
#install.packages("treemapify")

library(ggplot2)
library(dpyr)
library(mosaicData)
library(magrittr)
library(dplyr)
library(treemapify)

data("Marriage", package = "mosaicData")
ggplot(data = Marriage, mapping = aes(x = officialTitle))+
  geom_bar() -> p1 

#Xoay ngang biểu đồ (Cách 1)
p1 + coord_flip()

# Using the angle text(Rotation label) (Cách 2)
#angle góc quay
#hjust là điều chỉnh khoảng cách text với trục x hoặc y
p1 + theme(axis.text.x = element_text(angle=45, hjust = 1))

#using the staggered labels(gán từng giá trị vào tiêu đề)
lbls <- paste0(c("","\n"), levels(Marriage$officialTitle))
lbls

#Cách 3 xử lý bị đè label
ggplot(data = Marriage, mapping = aes(x = factor(officialTitle, labels = lbls))) +
  geom_bar(fill = "indianred3", color = "white") +
  labs(title = "Marriage by officiate",
       caption = "source = http://mosaicdat-web.org",
       y = "Frequency",
       x = "")

#Making tree map
plotdata <- Marriage %>% count(officialTitle)
ggplot(data = plotdata, mapping = aes(fill = officialTitle, area = n,
                                      label = officialTitle))+
  geom_treemap()+
  geom_treemap_text(color = "white", place = "centre")+
  labs(title = "Marriage by officiate",
       caption = "source = http://mosaicdata-web.org")+
  theme(legend.position = "none") #để các lengend = None(Xóa tiêu đề giá trị)
  
  
  
  
  
  