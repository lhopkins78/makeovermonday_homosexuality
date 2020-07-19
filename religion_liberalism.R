library("httr")
library("readxl")
library(tidyverse)
library(ggthemes)
library(ggsci)
library(scales)
library(ggrepel)
GET("https://query.data.world/s/n6rkuq7t6gguxwxlmg55uhw4ybyflx", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
df$Country <- c("Israel", "Czech Republic", "South Korea" , "Canada","United States", "Slovakia", "Greece", "Turkey",     
                "France", "Australia", "Poland", "United Kingdom", "Sweden",  "Hungary", "Germany", "Italy" ,      
                "Argentina", "Netherlands",  "Spain",  "Lithuania", "South Africa", "Brazil",  "Bulgaria", "Lebanon",     
                "Mexico"  )

god <- read_csv("god.csv")
test <- god[complete.cases(god),]
god_2013 <- god %>% select(Entity, Year, god_impt = "How important is God in your life?") %>%
  group_by(Entity) %>% top_n(1)

atheist <- read_csv("atheist.csv") %>% select(Entity, percent_atheist=`(%)`)
literacy <- read_csv("literacy.csv") %>% group_by(Entity, Year) %>% top_n(1) %>% ungroup() %>%
  select(Entity, `Literacy rate (%)`)
main_religion <- read_csv("main_religion.csv") %>% select(Entity, main_religion="X4")

df_religion <- df %>% left_join(god_2013_final, by=c("Country"="Entity")) %>%
  left_join(atheist, by=c("Country"="Entity")) %>%
  left_join(literacy, by=c("Country"="Entity")) %>%
  left_join(main_religion, by=c("Country"="Entity")) %>%
  mutate(diff=`Religion is very important`-`Religion is not very important`,
         label_pos = `Religion is very important`+((`Religion is not very important`-`Religion is very important`)/2))

df_chart <- df_religion %>%
  pivot_longer(cols=c("Religion is not very important", "Religion is very important"))

  ggplot(df_chart, aes(reorder(Country, value), value, col=name,
                              shape=main_religion, label=round(percent_atheist,0))) +
  geom_segment(data=df_religion, 
                              aes(x=reorder(Country, `Religion is not very important`), 
                              xend=Country, y=`Religion is very important`, 
                              yend=`Religion is not very important`), inherit.aes = F, col="grey81") +
  geom_point(size=4) +
  theme_few() +
  scale_color_lancet() +
  coord_flip() + 
  scale_y_continuous(labels=label_percent()) +
    theme(text=element_text(family="Avenir", size=15),
          plot.title=element_text(size=30)) +
    labs(title="The Global Divide on Homosexuality Persists",
         col="Religious outlook",
         subtitle="People who see religion as less important in their lives are more accepting of homosexuality",
         x="", y="% who say homosexuality should be accepted by society", shape="Main religion", 
         caption="Sources: Pew Research Center, Our World in Data")
  ggsave("pew.png", dpi="retina", height=8, width=12) 

df_religion[complete.cases(df_religion),]

df %>% anti_join(literacy, by=c("Country"="Entity"))
  