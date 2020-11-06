library(nbastatR)
library(dplyr)

shots2014 <- teams_shots(teams = c(a$nameTeam), seasons = 2014)
shots2015 <- teams_shots(teams = c(a$nameTeam), seasons = 2015)
shots2016 <- teams_shots(teams = c(a$nameTeam), seasons = 2016)
shots2017 <- teams_shots(teams = c(a$nameTeam), seasons = 2017)
shots2018 <- teams_shots(teams = c(a$nameTeam), seasons = 2018)
shots2019 <- teams_shots(teams = c(a$nameTeam), seasons = 2019)
shots2020 <- teams_shots(teams = c(a$nameTeam), seasons = 2020)
shotss = rbind(shots2014, shots2015, shots2016, shots2017, shots2018, shots2019, shots2020)

aaa = shotss %>% 
  filter(numberPeriod > 3 & typeShot == "3PT Field Goal") %>% 
  group_by(namePlayer) %>%
  count() %>% 
  filter(n > 650)

bbb = shotss %>% 
  filter(namePlayer %in% aaa$namePlayer & numberPeriod > 3 & typeShot == "3PT Field Goal" & isShotMade == T) %>% 
  group_by(namePlayer) %>% 
  count()

shotfg = left_join(bbb, aaa, by = "namePlayer")
colnames(shotfg) = c("Player_Name", "FG3M", "FG3A")
shotfg = shotfg %>% 
  mutate(FG3_PCT = FG3M/FG3A) %>% 
  arrange(desc(FG3A))

shotfg$FG3_PCT = as.numeric(format(shotfg$FG3_PCT, digits = 3))
str(shotfg$FG3_PCT)

library(gt)

gt_tbl = gt(tibble(shotfg))

gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = md("**3-Point Shooting in the 4th Quarter or Overtime**"),
    subtitle = md("Regular Season Data from 2013-14 to 2019-20, Min. 650 Attempts")
  ) %>%
  tab_source_note(
    source_note = md("Table: @burakcankoc")
  ) %>%
  tab_source_note(
    source_note = md("Source: nba.com/stats")
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "left", weight = "bold")
    ),
    locations = cells_body(
      columns = vars(Player_Name))
  ) %>%
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = 2:4) 
  ) %>%
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_column_labels(columns = 1) 
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(
      columns = vars(FG3_PCT))
  ) %>% 
  tab_style(
    style = cell_text(font = "BANGERS"),
    locations = cells_body(
      rows = T)
  )

gt_tbl
