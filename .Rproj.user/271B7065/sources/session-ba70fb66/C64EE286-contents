library(nflfastR)
library(tidyverse)
library(stringr)


#Load & prune plays
season <- 2018
dat <- load_pbp(season)

# Remove penalties, special teams, spikes, kneels, timeouts
misc_rx <- regex(
  paste(
    c("field goal","extra point","punt","timeout","kneel","spike",
      "two-point","conversion","kicks off","kickoff"),
    collapse="|"
  ),
  ignore_case = TRUE
)

pbp <- dat %>%
  filter(
    penalty == 0,
    !str_detect(desc, misc_rx),
    play_type %in% c("run","pass") # keep scrimmage plays only
  )

#Build a clean roster lookup
# Defensive positions of interest
def_pos <- c("DE","DT","NT","DL","EDGE","DI",   # DL-ish
             "LB","ILB","OLB","MLB",           # LB
             "CB","FS","SS","S","NB","DB")     # DB

rost_raw <- fast_scraper_roster(season) %>%
  filter(position %in% def_pos | depth_chart_position %in% def_pos)

# Normalize last names to match pbp "[##-F.Last]" pattern and build lookup key
rost <- rost_raw %>%
  mutate(
    last_clean   = str_to_title(str_remove_all(last_name, "[^A-Za-z]")),
    first_init   = str_sub(str_to_upper(first_name), 1, 1),
    lookup_key   = paste0(jersey_number, "-", first_init, ".", last_clean),
    position_fin = coalesce(position, depth_chart_position)
  ) %>%
  # ensure unique rows per (team, lookup_key) for safe many-to-one joins
  distinct(team, lookup_key, .keep_all = TRUE) %>%
  select(team, gsis_id, position_fin, lookup_key)

# Helper to collapse positions into coarse groups
to_pos_group <- function(pos) {
  case_when(
    pos %in% c("DE","DT","NT","DL","EDGE","DI") ~ "DL",
    pos %in% c("LB","ILB","OLB","MLB")          ~ "LB",
    pos %in% c("CB","FS","SS","S","NB","DB")    ~ "DB",
    TRUE ~ NA_character_
  )
}


#Long form from ID columns (tackles, sacks, PD, etc.)

id_cols <- c(
  "solo_tackle_1_player_id","solo_tackle_2_player_id",
  "assist_tackle_1_player_id","assist_tackle_2_player_id",
  "qb_hit_1_player_id","qb_hit_2_player_id",
  "sack_player_id","half_sack_1_player_id","half_sack_2_player_id",
  "pass_defense_1_player_id","pass_defense_2_player_id",
  "forced_fumble_player_1_player_id","forced_fumble_player_2_player_id"
)

roles_long <- pbp %>%
  select(game_id, play_id, posteam, defteam, desc, play_type, epa, all_of(id_cols)) %>%
  pivot_longer(
    cols = all_of(id_cols),
    names_to = "role",
    values_to = "player_id",
    values_drop_na = TRUE
  ) %>%
  # Join by BOTH player id and defteam => prevents many-to-many mismatches
  left_join(
    rost %>% select(team, gsis_id, position_fin),
    by = c("player_id" = "gsis_id", "defteam" = "team"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    position        = position_fin,
    position_group  = to_pos_group(position)
  ) %>%
  select(game_id, play_id, posteam, defteam, desc, play_type, epa,
         role, player_id, position, position_group)

#Extract bracketed pressures: "[##-F.Last]"
extract_bracket_lookups <- function(x) {
  out <- str_extract_all(x, "\\[(\\d{1,2}-[A-Za-z]\\.[A-Za-z’'\\-]+)\\]")
  lapply(out, function(z) {
    z %>%
      str_remove_all("\\[|\\]") %>%
      str_replace_all("’", "'")
  })
}

pressures_long <- pbp %>%
  transmute(
    game_id, play_id, posteam, defteam, desc, play_type, epa,
    pressure_lookup = extract_bracket_lookups(desc)
  ) %>%
  unnest_longer(pressure_lookup) %>%
  filter(!is.na(pressure_lookup)) %>%
  # Join by (defteam, lookup_key)
  left_join(
    rost %>% select(team, lookup_key, position_fin),
    by = c("defteam" = "team", "pressure_lookup" = "lookup_key"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    role           = "pressure",
    player_id      = NA_character_,                  # not present in desc
    position       = position_fin,
    position_group = to_pos_group(position)
  ) %>%
  select(game_id, play_id, posteam, defteam, desc, play_type, epa,
         role, player_id, position, position_group, pressure_lookup)


#Final clean long data + play-level summary
defenders_long <- bind_rows(roles_long, pressures_long) %>%
  # Keep defensive positions only
  filter(!is.na(position_group)) %>%
  # If a player appears twice in same role (rare), keep a single row
  distinct(game_id, play_id, defteam, role, player_id, position, position_group,
           .keep_all = TRUE)
agg_def <- defenders_long %>%
  group_by(game_id, play_id, posteam, defteam) %>%
  summarise(
    n_defenders  = n(),
    DL_involved  = sum(position_group == "DL"),
    LB_involved  = sum(position_group == "LB"),
    DB_involved  = sum(position_group == "DB"),
    any_pressure = any(role == "pressure"),
    any_sack     = any(role %in% c("sack_player_id","half_sack_1_player_id","half_sack_2_player_id")),
    any_pd       = any(role %in% c("pass_defense_1_player_id","pass_defense_2_player_id")),
    any_tackle   = any(str_detect(role, "tackle")),
    .groups = "drop"
  )

# Build the play-level table from *all* scrimmage plays, then left-join aggregates
play_defense_summary <- pbp %>%
  # Keep helpful play-level fields (incl. touchdowns)
  transmute(
    game_id, play_id, posteam, defteam, desc, play_type, epa,
    touchdown = as.logical(touchdown),              # keep TD flag
    pass_touchdown = as.logical(pass_touchdown),
    rush_touchdown = as.logical(rush_touchdown),
    return_touchdown = as.logical(return_touchdown)
  ) %>%
  left_join(agg_def, by = c("game_id","play_id","posteam","defteam")) %>%
  # Fill plays with no defender rows (including TDs) with zeros/FALSE
  mutate(
    n_defenders  = coalesce(n_defenders, 0L),
    DL_involved  = coalesce(DL_involved, 0L),
    LB_involved  = coalesce(LB_involved, 0L),
    DB_involved  = coalesce(DB_involved, 0L),
    any_pressure = coalesce(any_pressure, FALSE),
    any_sack     = coalesce(any_sack, FALSE),
    any_pd       = coalesce(any_pd, FALSE),
    any_tackle   = coalesce(any_tackle, FALSE)
  )

write.csv(play_defense_summary, 'plays_with_positions.csv')
