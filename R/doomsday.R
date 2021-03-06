library(tidyverse)
library(lubridate)
library(glue)

eleven_on_odd <- function(infrasecular){
  temp <- infrasecular
  if (temp %% 2 == 1) { temp <- temp + 11 }
  temp = temp / 2
  if (temp %% 2 == 1) { temp <- temp + 11 }
  temp = -temp %% 7
  return(temp)
}

doomsday <- function(year){
  
  century = floor(year/100)
  infrasecular = year - 100*century
  
  century_mod_4 = century %% 4
  if (century_mod_4 == 0) {century_rank = 2}
  else if (century_mod_4 == 1) {century_rank = 0}
  else if (century_mod_4 == 2) {century_rank = 5}
  else {century_rank = 3}
  
  decalage <- eleven_on_odd(infrasecular)
  
  doomsday = (decalage + century_rank) %% 7
  
  return(doomsday)
  
}

doomsdays_per_year <- function(start, end){
  
  dd_year <- matrix(0, nrow = 1, ncol = 7)
  week <- dd_year
  
  old = doomsday(start)
  dd_year[old+1] <- start
  
  row <- 1
  for (year in seq(start+1,end,1)){
    new = doomsday(year)
    if (new < old) { 
      dd_year <- rbind(dd_year, week)
      row = row + 1
    }
    dd_year[row, new+1] = year
    old <- new
  }
  
  return(dd_year)
  
}

df <- doomsdays_per_year(1898,2099)
df <- as_tibble(df) %>% 
  rename(Dimanche = V1,
         Lundi = V2,
         Mardi = V3,
         Mercredi = V4,
         Jeudi = V5,
         Vendredi = V6,
         Samedi = V7) %>% 
  mutate(
    Dimanche = if_else(Dimanche == 0, "->", as.character(Dimanche)),
    Lundi = if_else(Lundi == 0, "->", as.character(Lundi)),
    Mardi = if_else(Mardi == 0, "->", as.character(Mardi)),
    Mercredi = if_else(Mercredi == 0, "->", as.character(Mercredi)),
    Jeudi = if_else(Jeudi == 0, "->", as.character(Jeudi)),
    Vendredi = if_else(Vendredi == 0, "->", as.character(Vendredi)),
    Samedi = if_else(Samedi == 0, "->", as.character(Samedi))
  )

start <- 1898
end <- 2030
balise_day <- tibble(
  year = seq(from = start, to = end, by = 1)
) %>% 
  mutate(century = floor(year/100),
         infrasecular = year - 100*century,
         century_mod_4 = century %% 4,
         century_rank = case_when(
           century_mod_4 == 0 ~ 2,
           century_mod_4 == 1 ~ 0,
           century_mod_4 == 2 ~ 5,
           century_mod_4 == 3 ~ 3
         ),
         infrasecular_balise_day = map_dbl(infrasecular, eleven_on_odd),
         doomsday = (century_rank + infrasecular_balise_day) %% 7,
         doomsday_weekday = case_when(
           doomsday == 0 ~ "Dimanche",
           doomsday == 1 ~ "Lundi",
           doomsday == 2 ~ "Mardi",
           doomsday == 3 ~ "Mercredi",
           doomsday == 4 ~ "Jeudi",
           doomsday == 5 ~ "Vendredi",
           doomsday == 6 ~ "Samedi"
         )) %>% 
  select(year, doomsday) %>% 
  mutate(jump = if_else(year %% 4 == 0, TRUE, FALSE))



balise_century <- tribble(
  ~ Siècle, ~ Balise,
  1600, 2,
  1700, 0,
  1800, 5,
  1900, 3,
  2000, 2,
  2100, 0,
  2200, 5,
  2300, 3
)

weekday_name <- function(weekday) {
  
  if (weekday == 0) {name <- "Dimanche"}
  else if (weekday == 1) {name <- "Lundi"}
  else if (weekday == 2) {name <- "Mardi"}
  else if (weekday == 3) {name <- "Mercredi"}
  else if (weekday == 4) {name <- "Jeudi"}
  else if (weekday == 5) {name <- "Vendredi"}
  else if (weekday == 6) {name <- "Samedi"}
  
  return(name)
  
}

weekday <- function(my_date) {
  
  my_year <- year(my_date)
  my_month <- month(my_date)
  my_day <- day(my_date)
  
  my_doomsday <- doomsday(my_year)
  
  if (my_month == 0) {
    
  } else if (my_month == 1) {
    
  } else if (my_month == 2) {
    
  } else if (my_month == 3) {
    decalage <- (my_day - 0) %% 7
  } else if (my_month == 4) {
    decalage <- (my_day - 4) %% 7
  } else if (my_month == 5) {
    decalage <- (my_day - 9) %% 7
  } else if (my_month == 6) {
    decalage <- (my_day - 6) %% 7
  } else if (my_month == 7) {
    decalage <- (my_day - 11) %% 7
  } else if (my_month == 8) {
    decalage <- (my_day - 8) %% 7
  } else if (my_month == 9) {
    decalage <- (my_day - 5) %% 7
  } else if (my_month == 10) {
    decalage <- (my_day - 10) %% 7
  } else if (my_month == 11) {
    decalage <- (my_day - 7) %% 7
  } else if (my_month == 12) {
    decalage <- (my_day - 12) %% 7
  }
  
  weekday <- (my_doomsday + decalage) %% 7
  name <- weekday_name(weekday)
  
  return(glue("Le {my_day} {my_month} {my_year} est un {name}"))
  
}













