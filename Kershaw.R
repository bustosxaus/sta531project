library(dplyr)
library(data.table)
library(lubridate)

# Reading in the data
pitchfx = as.data.frame(fread("Pitchfx.csv"))

# Converting game_date to a date
pitchfx = pitchfx %>%
  mutate(game_date = dmy(game_date))

# tabling the occurences of pitches
#as.data.frame(table(pitchfx$pitch_type))

# Creating a new categorical variable for runners on base
pitchfx = pitchfx %>%
  mutate(runners = paste0( ifelse(is.na(Runneron1st_ID) == FALSE, 1, 0 ),
                           ifelse(is.na(Runneron2nd_ID) == FALSE, 1, 0 ),
                           ifelse(is.na(Runneron3rd_ID) == FALSE, 1, 0 )) )

# Creating a new categorical variable for count
pitchfx = pitchfx %>%
  mutate(count = paste(pre_balls, pre_strikes, sep = "-"))

# Combining low-occurence pitches into "Other Category"
Other = c("", "AB", "FO", "IN", "PO", "SC", "UN", "EP")
Fastballs = c("FC", "FF", "FT", "FA")
pitchfx = pitchfx %>%
  mutate(new_pitch_type = ifelse(pitch_type %in% Other, "Other", 
                                 ifelse(pitch_type %in% Fastballs,"FB", pitch_type)))

# tabling the occurences of pitches after pitch_type update
# as.data.frame(table(pitchfx$new_pitch_type))

# Reading in data to get names from
pitching_stats = read.csv("Pitching_Statistics.csv", header=TRUE)

# Getting unique pitchers names
pitchers = pitching_stats %>%
  distinct(pitcher_id, Name) %>%
  select(pitcher_id, Name)

# Joining with pitchfx data to attach names
pitchfx = left_join(pitchfx, pitchers)

# Just Clayton Kershaw's pitches
kershaw = pitchfx %>%
  filter(Name == "Kershaw, Clayton") %>%
  arrange(game_date, at_bat_number)

# Removing intentional balls
kershaw = kershaw %>%
  filter(pitch_type != "IN")

# Creating a new variable for the pitch number of the game
pitchcounts = c()
pitchcounts[1] = 1
for(i in 2:nrow(kershaw))
{
  if(kershaw$game_id[i] == kershaw$game_id[i-1])
  {
    pitchcounts[i] = pitchcounts[i-1] + 1
  } else
  {
    pitchcounts[i] = 1
  }
}

kershaw = kershaw %>%
  mutate(pitch_count = pitchcounts)

# Creating a new variable for the previous pitch type
previous_pitch_type = c()
previous_pitch_type[1] = "start"
for(i in 2:nrow(kershaw))
{
  previous_pitch_type[i] = kershaw$pitch_type[i-1]
}

kershaw = kershaw %>%
  mutate(previous_pitch_type = previous_pitch_type)

# Creating a new variable for the previous event type
previous_event_type = c()
previous_event_type[1] = "start"
for(i in 2:nrow(kershaw))
{
  previous_event_type[i] = kershaw$event_type[i-1]
}

kershaw = kershaw %>%
  mutate(previous_event_type = previous_event_type)


