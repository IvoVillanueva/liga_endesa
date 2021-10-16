
# librerias ---------------------------------------------------------------


library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)


# datos, tabla, nombres y logos-------------------------------------------------------------------

url <- "https://www.acb.com/resultadosClasificacion/ver" %>%
  read_html()

acb <- tibble(id_team = url %>% html_elements(".nombre_equipo a") %>%
                html_attr("href") ,
              tm =url %>% html_elements("a span.nombre_largo") %>%
                html_text("class"),
              abb = url %>% html_elements("a span.abreviatura") %>%
                html_text("class"),
              logo = url %>% html_elements("td.logo_equipo img") %>%
                html_attr("src")) %>%
  mutate(id_team = str_extract(id_team, "[0-9]+"),
         logo = paste0("https:", logo),
         tm_bref =case_when(
           tm == "Barça" ~ "FC Barcelona Lassa",
           tm == "Río Breogán" ~ "Rio Breogan",
           tm == "BAXI Manresa" ~ "Baxi Manresa",
           tm == "Gran Canaria" ~ "Herbalife Gran Canaria",
           tm == "Urbas Fuenlabrada" ~ "Montakit Fuenlabrada",
           tm ==  "Joventut Badalona" ~ "Divina Seguros Joventut",
           TRUE ~ tm
         )) %>%
  select(id_team, tm, tm_bref, abb, logo)

write.csv(acb, "acb.csv", row.names = FALSE)

teams <- read.csv("acb.csv")


url <- "https://www.basketball-reference.com/international/spain-liga-acb/2022-schedule.html"
url2 <- "https://www.basketball-reference.com/international/spain-liga-acb/2022.html"

# df schedule con los resultados-------------------------------------------------------------

df <- url %>%
  read_html() %>%
  html_element("#games") %>%
  html_table() %>%
  clean_names() %>%
  filter(date != "October") %>%
  mutate_at(vars("pts", "pts_2"), as.numeric) %>%
  mutate(
    result = pts - pts_2,
    ot = ifelse(ot == "", 0, ot)
  ) %>%
  select(!c(x, x_2, date, x_3), team_home = team, team_away = opp, score_home = pts, score_away = pts_2) %>%
  pivot_longer(contains("team"), names_to = "home_away", values_to = "team", names_prefix = ("team_")) %>%
  mutate(
    result = ifelse(home_away == "home", result, -result),
    win = ifelse(result > 0, 1, 0)) 

# df standings clasificicacion------------------------------------------------------------

standings <- url2 %>%
  read_html() %>%
  html_element("#spa_standings") %>%
  html_table() %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  mutate_at(vars("w":"pa_g"), as.numeric) %>%
  mutate(gb = ifelse(is.na(gb), 0, gb)) %>%
  select(team = x, w_l_percent:pa_g)

# join and crates list tables -------------------------------------------------------------

home <- df %>%
  dplyr::group_by(team) %>%
  filter(home_away == "home") %>%
  dplyr::summarize(home = list(score_home))

away <- df %>%
  dplyr::group_by(team) %>%
  filter(home_away == "away") %>%
  dplyr::summarize(away = list(score_away))


joined_df <- df %>%
  group_by(team) %>%
  summarise(
    Wins = length(win[win==1]),
    Losses = length(win[win==0]),
    outcomes = list(win),
    .groups = "drop") %>%
  left_join(teams, by = c("team" = "tm_bref")) %>%
  left_join(home) %>%
  left_join(away)

# gt table ----------------------------------------------------------------


standings %>%
  left_join(joined_df) %>%
  ungroup() %>%
  mutate(Pos = row_number()) %>%
  select(Pos, logo, tm, Wins, Losses, w_l_percent, gb, ps_g, pa_g, home, away, outcomes) %>%
  gt() %>%
  fmt_percent(
    columns = c(w_l_percent),
    decimals = 0
  ) %>%
  cols_label(
    Pos = gt::html("<span style='font-weight:bold;font-size:12px'>Rk</span>"),
    logo = "",
    tm = gt::html("<span style='font-weight:bold;font-size:12px'>Equipo</span>"),
    Wins = gt::html("<span style='font-weight:bold;font-size:12px'>Wins</span>"),
    Losses = gt::html("<span style='font-weight:bold;font-size:12px'>Losses</span>"),
    w_l_percent = gt::html("<span style='font-weight:bold;font-size:12px'>W-L%</span>"),
    gb = gt::html("<span style='font-weight:bold;font-size:12px'>DIF.</span>"),
    ps_g = gt::html("<span style='font-weight:bold;font-size:12.7px'>SCORE</span>"),
    pa_g = gt::html("<span style='font-weight:bold;font-size:12.7px'>AGAINST</span>"),
    home = gt::html("<span style='font-weight:bold;font-size:12.7px'>PUNTOS<br>LOCAL</span>"),
    away = gt::html("<span style='font-weight:bold;font-size:12.7px'>PUNTOS<br>VISITANTE</span>"),
    outcomes = gt::html("<span style='font-weight:bold;font-size:12.7px'>HISTORICO</span>")
  ) %>%
  tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Liga_Endesa_2019_logo.svg/800px-Liga_Endesa_2019_logo.svg.png'
               style='height:40px;'>"),
    subtitle = md(paste0("<b>Clasificación Temporada 2021-22 hasta el ", format(Sys.Date(), "%d %B, %Y</b>")))
  ) %>%
  tab_source_note(
    source_note = md("<div><b>Grafica por</b> : <i>\n Ivo Villanueva @elcheff<i>
                       <div><b>Datos por</b> : \n<i>@basketball_ref<i>")
  ) %>%
  gt_img_rows(logo, height = 30) %>%
  gt_plt_winloss(outcomes, max_wins = 9) %>%
  gt_sparkline(home) %>%
  gt_sparkline(away) %>%
  gt_theme_538() %>%
  tab_options(
    data_row.padding = px(3),
  ) %>%
  cols_align(
    align = "center",
    columns = c(Wins:outcomes)
  ) %>%
  gtsave("final_df.png")

# Ivo Villanueva ----------------------------------------------------------
