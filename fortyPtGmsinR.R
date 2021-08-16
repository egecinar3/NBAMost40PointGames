library(tidyverse)
library(nbastatR)
library(gt)

logs <- game_logs(seasons = 2000:2021, season_types = "Regular Season")

df <- logs %>% 
  filter(pts>=40) %>%
  group_by(urlPlayerHeadshot) %>%
  summarise(name = unique(namePlayer), no = n()) %>%
  arrange(desc(no)) %>%
  head(10)%>% 
  `colnames<-` (c(" ", "Player", "Number of 40 Point Games"))  %>%
  gt %>%
  text_transform(
    locations = cells_body(
      c(" ")
    ),
    fn = function(x) {
      web_image(
        url = x,
        height = 35
      )
    }
  ) %>%
  tab_header(
    title = md("**Most 40 Point Games**"),
    subtitle = md(
      "2000-2021 Regular Seasons"
    )
  ) %>%
  tab_options(
    table.background.color = "white",
    column_labels.font.size = 12,
    column_labels.font.weight = 'bold',
    row_group.font.weight = 'bold',
    row_group.background.color = "#E5E1D8",
    table.font.size = 10,
    heading.title.font.size = 22,
    heading.subtitle.font.size = 10,
    table.font.names = "Franklin Gothic Medium",
    data_row.padding = px(8),
    footnotes.padding = px(.5),
  )  %>%
  tab_source_note(source_note = md("Table: @egecinar3")) %>%
  gtsave("fortyPtGms.png")


