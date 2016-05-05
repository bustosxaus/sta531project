library(dplyr)
library(tables)

source("Kershaw.R")

# Table for count 
chart_count = table(kershaw$count, kershaw$pitch_type)
chart_count = as.data.frame.matrix(
  chart_count/rowSums(chart_count))[1:(nrow(chart_count) - 2), ]

# Table for pre_outs
chart_pre_outs = table(kershaw$pre_outs, kershaw$pitch_type)
chart_pre_outs = as.data.frame.matrix(
  chart_pre_outs/rowSums(chart_pre_outs))


# Table for inning
chart_inning = table(kershaw$inning, kershaw$pitch_type)
chart_inning = as.data.frame.matrix(
  chart_inning/rowSums(chart_inning))

# Table for top_inning_sw
chart_top_inning_sw = table(kershaw$top_inning_sw, kershaw$pitch_type)
chart_top_inning_sw = as.data.frame.matrix(
  chart_top_inning_sw/rowSums(chart_top_inning_sw))

# Table for bat_side
chart_bat_side = table(kershaw$bat_side, kershaw$pitch_type)
chart_bat_side = as.data.frame.matrix(
  chart_bat_side/rowSums(chart_bat_side))

# Table for prev_pitch_type
chart_prev_pitch_type = table(kershaw$prev_pitch_type, kershaw$pitch_type)
chart_prev_pitch_type = as.data.frame.matrix(
  chart_prev_pitch_type/rowSums(chart_prev_pitch_type))

