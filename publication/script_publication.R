## Pie chart for my last slide resumer
library(tidyverse)


setwd("~/Dropbox/dossiers_admin/dossier_CIRAD/Green2017/Audition/prez/publication/")

pub <- read.csv("curriculum.csv",header = T, sep = ",")

unique(pub$Extra)
# [1] conferencePaper bookSection     journalArticle  presentation    magazineArticle thesis 


pub$nb <- 1
## selction last 5 years 
sel <- pub$Publication.Year > (2017 - 5)
pub5 <- pub[sel,]

resum_pub <- pub5 %>%
  group_by(Extra, Publication.Year) %>%
  summarise(
    sum_pub = sum(nb),
    pct_pub = sum(nb) / 33 * 100)

resum_pub <- na.omit(resum_pub)

gg.hceres <- ggplot(data <- resum_pub)+
  geom_bar(aes(x = Extra, y = sum_pub, fill = Extra), stat="identity")+
  #coord_polar(theta='y')+
  theme_bw(base_size = 14)+
  theme(#panel.grid = element_blank(), ## remove guide lines
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks=element_blank(),  # the axis ticks
    axis.title=element_blank(),  # the axis labels
    legend.position="none", #top
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "", y = "Quantification", title = paste(sum(resum_pub$sum_pub, na.rm = T),"prod. scienctifique depuis 5 ans"))
  #scale_fill_hue(name="Classification\nHCERES (%)")
gg.hceres

ggsave("../img/publication_pie.png",gg.hceres)
