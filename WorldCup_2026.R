library(emoji)

create_html_row = function(data, color = "", widths = rep(0, length(data)), port = "", bold = FALSE){
  open_tds = rep("", length(data))
  for (i in c(1:length(data))){
    color_str = ""
    width_str = ""
    if (nchar(color) > 0){
      color_str = paste0(" bgcolor = \"", color, "\"")
    }
    if (widths[i] > 0){
      width_str = paste0(" width = \"", widths[i], "\"")
    }
    words = grepl("[a-z]", data[i], ignore.case = TRUE)
    away_goal = grepl("[0-9]+\ \\(a\\)", data[i])
    if (words & !away_goal){
      align_str = " align = \"left\""
    } else {
      align_str = ""
    }
    port_str = ""
    if (nchar(port) > 0){
      if (i == 1){
        port_str = paste0(" port = \"w",port,"\"")
      }
      if (i == length(data)){
        port_str = paste0(" port = \"e",port,"\"") 
      }
    }
    open_tds[i] = paste0("<td", color_str, width_str, align_str, port_str, ">")
  }
  rets = rep("", length(data))
  for (i in c(1:length(data))){
    if(bold){
      rets[i] = paste0(open_tds[i], "<b>", data[i], "</b></td>")
    } else {
      rets[i] = paste0(open_tds[i], data[i], "</td>")
    }
  }
  ret = paste0(unlist(rets), collapse = "")
  #data_elements = paste(unlist(lapply(data, function(x) paste0(open_td, x, "</td>"))), collapse = "")
  return (paste0("<tr>", ret, "</tr>\n"))
}

get_color = function(categories){
  colors = rep(NULL, length(categories))
  colors[categories == "advance"] = "#BBF3FF"
  colors[categories == "wc"] = "#BBF3BB"
  colors[categories == "dq"] = "#BBBBBB"
  colors[categories == "wd"] = "#BBBBBB"
  colors[categories == "gold"] = "gold"
  colors[categories == "silver"] = "#C0C0C0"
  colors[categories == "bronze"] = "#CC9966"
  return(colors)
}

create_table = function(dtf, categories, widths = rep(0, ncol(dtf)), bolds = NULL){
  if (is.null(bolds)){
    bolds = categories %in% c("advance", "wc", "gold", "bronze")
  }
  colors = get_color(categories)
  table_atts = "label = <<table border = '0' cellborder = '1' cellspacing = '0' bgcolor = '#f8f9fa'>\n"
  rows = rep("", nrow(dtf)+1)
  rows[1] = create_html_row(colnames(dtf), "#eaecf0", widths = widths, bold = TRUE)
  for (i in c(1:nrow(dtf))){
    if (is.na(colors[i])){
      rows[i+1] = create_html_row(dtf[i,], port = i, bold = bolds[i])
    } else {
      rows[i+1] = create_html_row(dtf[i,], colors[i], port = i, bold = bolds[i])
    }
  }
  table_end = "</table>>"
  row_string = paste(rows, collapse = "")
  
  
  return(paste0(table_atts,row_string, table_end))
}

get_country_name = function(country){
  country = trimws(strsplit(country, "\\(")[[1]][1])
  country = sub("Saint", "St.", country)
  country = sub(" and the ", " & ", country)
  country = sub(" and ", " & ", country)
  
  if (country == "USA"){
    return("United States")
  }
  if (country == "UAE"){
    return("United Arab Emirates")
  }
  if (country == "Hong Kong"){
    return("Hong Kong SAR China")
  }
  if (country == "Palestine"){
    return("Palestinian Territories")
  }
  if (country == "DR Congo"){
    return("Congo - Kinshasa")
  }
  if (country == "Congo"){
    return("Congo - Brazzaville")
  }
  if (country == "Myanmar"){
    return("Myanmar (Burma)")
  }
  if (country == "Tahiti"){
    return("French Polynesia") # Flag used by Fifa
  }
  if (country == "Ivory Coast"){
    return("Côte d’Ivoire")
  }
  if (country == "Macau"){
    return("Macao SAR China")
  }
  return(country)
}

get_single_flag = function(country){
  if (country == "Scotland"){
    return("🏴󠁧󠁢󠁳󠁣󠁴󠁿")
  }
  if (country == "England"){
    return("🏴󠁧󠁢󠁥󠁮󠁧󠁿")
  }
  if (country == "Wales"){
    return("🏴󠁧󠁢󠁷󠁬󠁳󠁿")
  }
  if (country == "Chinese Taipei"){
    return("") # Uses Chinese Taipei flag, no emoji. Could use Taiwan.
  }
  if (country == "Northern Ireland"){
    return("") # Uses the Ulster banner, no Emoji
  }
  if (country == "??"){
    return("")
  }
  return(flag(country))
}

add_flag = function(countries){
  country_names = sapply(countries, function(country) get_country_name(country))
  country_flags = sapply(country_names, get_single_flag)
  return(paste(country_flags, countries))
}

create_node = function(dtf, node_name, categories, widths = rep(0, ncol(dtf))){
  if (is.matrix(dtf)){
    dtf = as.data.frame(dtf)
  }
  gds = dtf$GD
  gds[gds == "-"] = 0
  gds = as.numeric(gds)
  if(sum(gds) != 0){
    stop("Goal difference does not even out")
  }
  
  teams = dtf$Team
  dtf$Team = add_flag(teams)
  
  return(paste0(node_name, "[shape = plaintext,\n", create_table(dtf, categories, widths), "]\n"))
}

create_group = function(Team, GD, Pts, group_let, widths = rep(0, 3), advance_teams = 3){
  dtf = cbind(Team, GD, Pts)
  group_name = paste0("Group", group_let)
  categories = c(rep("advance", advance_teams), rep("", length(Team) - advance_teams))
  return(create_node(dtf, group_name, categories, widths))
}

create_H2H = function(name, Team1, Team2, Score1, Score2, team1Advance, widths = rep(0,2), categories = NULL, wc = FALSE){
  Team = c(Team1, Team2)
  Score = c(Score1, Score2)
  dtf = cbind(Team, Score)
  
  if (is.null(categories)){
    if(wc){
      advance_cat = "wc"
    } else {
      advance_cat = "advance"
    }
    if(team1Advance){
      categories = c(advance_cat, "")
    } else {
      categories = c("", advance_cat)
    }
  } else {
    if(wc){
      categories["advance"] = "wc"
    }
  }
  create_node(dtf, name, categories, widths = widths)
}

create_two_leg = function(name, team1, team2, score11, score21, score12, score22, team1Advance, widths = rep(0,4), wc = FALSE, pks = NULL){
  Team = c(team1, team2)
  Game1 = c(score11, score21)
  Game2 = c(score12, score22)
  Total = c(score11 + score12, score21 + score22)
  
  if (!is.null(pks)){
    Total = paste0(Total, " (", pks, ")")
  }
  dtf = cbind(Team, Game1, Game2, Total)
  colnames(dtf) = c("Team", "Match 1", "Match 2", "Total")
  if(wc){
    advance_cat = "wc"
  } else {
    advance_cat = "advance"
  }
  if(team1Advance){
    categories = c(advance_cat, "")
  } else {
    categories = c("", advance_cat)
  }
  create_node(dtf, name, categories, widths = widths)
}

find_max_width = function(values, text_multiplier){
  text_width_max = text_multiplier * max(sapply(values, function(x) strwidth(x, family = "serif")))
  return(text_width_max + 44) # Approximate width of emoji flag, space, and spacing to edges of column
}

file_name = "WorldCup_2026.txt"

file_start = "digraph g{
edge [penwidth = 2]"
write(file_start, file_name)

# Sao Tome and Principe = 149 = 6 + 143
plot.new()
bold_multiplier = 1.1
width_multiplier = bold_multiplier * 142/strwidth("Sao Tome and Principe", family = "serif")
width_padding = 6 # Approximate width of column padding


gd_width = strwidth("+77") * width_multiplier + width_padding
pts_width = strwidth("77") * width_multiplier + width_padding
score_width = strwidth("1 (3)") * width_multiplier + width_padding
two_leg_score_width = strwidth("Match 1") * width_multiplier

Team = c("Argentina", "Ecuador", "Colombia", "Uruguay", 
         "Brazil", "Paraguay", "Bolivia", "Venezuela",
        "Peru", "Chile")
Pts = c(38, 29, 28, 28, 28, 28, 20, 18, 12, 11)
GD = c("+21", "+9", "+10", "+10", "+7", "+4", "-18", "-10", "-15", "-18")
categories = c(rep("wc", 6), "advance", rep("", 3))

write(create_node(cbind(Team, GD, Pts), "CMBRR", categories), file_name, append = TRUE)

afc_r1_teams = c("Afghanistan", "Mongolia",
                  "Maldives", "Bangladesh",
                  "Singapore", "Guam",
                  "Yemen", "Sri Lanka",
                  "Myanmar", "Macau",
                  "Cambodia", "Pakistan",
                  "Chinese Taipei", "Timor-Leste",
                  "Indonesia", "Brunei",
                  "Hong Kong", "Bhutan",
                  "Nepal", "Laos")

widths = c(find_max_width(afc_r1_teams, width_multiplier), rep(two_leg_score_width, 2), score_width)

write(create_two_leg("AFCR1M1", "Afghanistan", "Mongolia", 1, 0, 1, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M2", "Maldives", "Bangladesh", 1, 1, 1, 2, FALSE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M3", "Singapore", "Guam", 2, 1, 1, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M4", "Yemen", "Sri Lanka", 3, 0, 1, 1, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M5", "Myanmar", "Macau", 5, 1, 0, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M6", "Cambodia", "Pakistan", 0, 0, 0, 1, FALSE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M7", "Chinese Taipei", "Timor-Leste", 4, 0, 3, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M8", "Indonesia", "Brunei", 6, 0, 6, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M9", "Hong Kong", "Bhutan", 4, 0, 0, 2, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M10", "Nepal", "Laos", 1, 1, 1, 0, TRUE, widths), file_name, append = TRUE)

afc_r2_teams = c("Qatar", "Kuwait", "India", "Afghanistan",
                 "Japan", "North Korea", "Syria", "Myanmar",
                 "South Korea", "China", "Thailand", "Singapore",
                 "Oman", "Kyrgyzstan", "Malaysia", "Chinese Taipei",
                 "Iran", "Uzbekistan", "Turkmenistan", "Hong Kong", 
                 "Iraq", "Indonesia", "Vietnam", "Philippines",
                 "Jordan", "Saudi Arabia", "Tajikistan", "Pakistan",
                 "United Arab Emirates", "Bahrain", "Yemen", "Nepal",
                 "Australia", "Palestine", "Lebanon", "Bangladesh")

team_width = find_max_width(afc_r2_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)                    
categories = c("advance", "advance", "", "")

Team = c("Qatar", "Kuwait", "India", "Afghanistan")
GD = c("+15", "0", "-4", "-11")
Pts = c(16, 7, 5, 5)
write(create_node(cbind(Team, GD, Pts), "AFCR2GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Japan", "North Korea", "Syria", "Myanmar")
GD = c("+24", "+4", "-3", "-25")
Pts = c(18, 9, 7, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GB", categories, widths = widths), file_name, append = TRUE)

Team = c("South Korea", "China", "Thailand", "Singapore")
GD = c("+19", "0", "0", "-19")
Pts = c(16, 8, 8, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GC", categories, widths = widths), file_name, append = TRUE)

Team = c("Oman", "Kyrgyzstan", "Malaysia", "Chinese Taipei")
GD = c("+9", "+6", "0", "-15")
Pts = c(13, 11, 10, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR2GD", categories, widths = widths), file_name, append = TRUE)

Team = c("Iran", "Uzbekistan", "Turkmenistan", "Hong Kong")
GD = c("+12","+9", "-10", "-11")
Pts = c(14, 14, 2, 2)
write(create_node(cbind(Team, GD, Pts), "AFCR2GE", categories, widths = widths), file_name, append = TRUE)

Team = c("Iraq", "Indonesia", "Vietnam", "Philippines")
GD = c("+15", "0", "-4", "-11")
Pts = c(18, 10, 6, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GF", categories, widths = widths), file_name, append = TRUE)

Team = c("Jordan", "Saudi Arabia", "Tajikistan", "Pakistan")
GD = c("+12", "+9", "+4", "-25")
Pts = c(13, 13, 8, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR2GG", categories, widths = widths), file_name, append = TRUE)

Team = c("United Arab Emirates", "Bahrain", "Yemen", "Nepal")
GD = c("+14","+8", "-4", "-18")
Pts = c(16, 11, 5, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GH", categories, widths = widths), file_name, append = TRUE)

Team = c("Australia", "Palestine", "Lebanon", "Bangladesh")
GD = c("+22", "0", "-3", "-19")
Pts = c(18, 8, 6, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GI", categories, widths = widths), file_name, append = TRUE)
#end round 2

afc_r3_teams = c("Iran", "Uzbekistan", "United Arab Emirates", "Qatar", "Kyrgyzstan", "North Korea",
                 "South Korea", "Jordan", "Iraq", "Oman", "Palestine", "Kuwait", 
                 "Japan", "Australia", "Saudi Arabia", "Indonesia", "China", "Bahrain")

team_width = find_max_width(afc_r3_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)
categories = c("wc", "wc", "advance", "advance", "", "")

Team = c("Iran", "Uzbekistan", "United Arab Emirates", "Qatar", "Kyrgyzstan", "North Korea")
GD = c("+11", "+7", "+7", "-7", "-6", "-12")
Pts = c(23, 21, 15, 13, 8, 3)
write(create_node(cbind(Team, GD, Pts), "AFCR3GA", categories, widths = widths), file_name, append = TRUE)

Team = c("South Korea", "Jordan", "Iraq", "Oman", "Palestine", "Kuwait")
GD = c("+13", "+8", "0", "-5", "-3", "-13")
Pts = c(22, 16, 15, 11, 10, 5)
write(create_node(cbind(Team, GD, Pts), "AFCR3GB", categories, widths = widths), file_name, append = TRUE)

Team = c("Japan", "Australia", "Saudi Arabia", "Indonesia", "China", "Bahrain")
GD = c("+27", "+9", "-1", "-11", "-13", "-11")
Pts = c(23, 19, 13, 12, 9, 6)
write(create_node(cbind(Team, GD, Pts), "AFCR3GC", categories, widths = widths), file_name, append = TRUE)
#End AFC Round 3

afc_r4_teams = c("Qatar", "United Arab Emirates", "Oman",
                 "Saudi Arabia", "Iraq", "Indonesia")

team_width = find_max_width(afc_r4_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)
categories = c("wc", "advance", "")

Team = c("Qatar", "United Arab Emirates", "Oman")
GD = c("+1", "0", "-1")
Pts = c(4, 3, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR4GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Saudi Arabia", "Iraq", "Indonesia")
GD = c("+1", "+1", "-2")
Pts = c(4, 4, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR4GB", categories, widths = widths), file_name, append = TRUE)

widths = c(find_max_width(afc_r1_teams, width_multiplier), rep(two_leg_score_width, 2), score_width)
write(create_two_leg("AFCR5", "United Arab Emirates", "Iraq", 1, 1, 1, 2, FALSE, widths), file_name, append = TRUE)

caf_r1_teams = c("Egypt", "Burkina Faso", "Sierra Leone", "Guinea-Bissau", "Ethiopia", "Djibouti",
                 "Senegal", "DR Congo", "Sudan", "Togo", "Mauritania", "South Sudan",
                 "South Africa", "Nigeria", "Benin", "Lesotho", "Rwanda", "Zimbabwe",
                 "Cape Verde", "Cameroon", "Libya", "Angola", "Mauritius", "Eswatini",
                 "Morocco", "Niger", "Tanzania", "Zambia", "Congo", "Eritrea",
                 "Ivory Coast", "Gabon", "Gambia", "Kenya", "Burundi", "Seychelles",
                 "Algeria", "Uganda", "Mozambique", "Guinea", "Botswana", "Somalia",
                 "Tunisia", "Namibia", "Liberia", "Malawi", "Equatorial Guinea", "São Tomé and Príncipe",
                 "Ghana", "Madagascar", "Mali", "Comoros", "Central African Republic", "Chad")

team_width = find_max_width(caf_r1_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)

categories = c("wc", "", "", "", "", "")
Team = c("Egypt", "Burkina Faso", "Sierra Leone", "Guinea-Bissau", "Ethiopia", "Djibouti")
GD = c("+18", "+15", "+2", "-2", "-5", "-28")
Pts = c(26, 21, 15, 10, 9, 1)
write(create_node(cbind(Team, GD, Pts), "CAFR1GA", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "", "", "")
Team = c("Senegal", "DR Congo", "Sudan", "Togo", "Mauritania", "South Sudan")
GD = c("+19", "+9", "+2", "-5", "-9", "-16")
Pts = c(24, 22, 13, 8, 7, 5)
write(create_node(cbind(Team, GD, Pts), "CAFR1GB", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "", "", "")
Team = c("South Africa", "Nigeria", "Benin", "Lesotho", "Rwanda", "Zimbabwe")
GD = c("+6", "+7", "+1", "-3", "-4", "-7")
Pts = c(18, 17, 17, 12, 11, 5)
write(create_node(cbind(Team, GD, Pts), "CAFR1GC", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "", "", "")
Team = c("Cape Verde", "Cameroon", "Libya", "Angola", "Mauritius", "Eswatini")
GD = c("+8", "+12", "+2", "+1", "-10", "-13")
Pts = c(23, 19, 16, 12, 6, 3)
write(create_node(cbind(Team, GD, Pts), "CAFR1GD", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "", "", "", "", "wd")
Team = c("Morocco", "Niger", "Tanzania", "Zambia", "Congo", "Eritrea")
GD = c("+20", "+1", "-1", "0", "-20", "0")
Pts = c(24, 15, 10, 9, 1, 0)
write(create_node(cbind(Team, GD, Pts), "CAFR1GE", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "", "", "")
Team = c("Ivory Coast", "Gabon", "Gambia", "Kenya", "Burundi", "Seychelles")
GD = c("+25", "+13", "+9", "+4", "0", "-51")
Pts = c(26, 25, 13, 12, 10, 0)
write(create_node(cbind(Team, GD, Pts), "CAFR1GF", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "", "", "", "", "")
Team = c("Algeria", "Uganda", "Mozambique", "Guinea", "Botswana", "Somalia")
GD = c("+16", "+5", "-3", "+3", "-4", "-17")
Pts = c(25, 18, 18, 15, 10, 1)
write(create_node(cbind(Team, GD, Pts), "CAFR1GG", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "", "", "", "", "")
Team = c("Tunisia", "Namibia", "Liberia", "Malawi", "Equatorial Guinea", "São Tomé and Príncipe")
GD = c("+22", "+3", "+2", "+1", "-7", "-21")
Pts = c(28, 15, 15, 13, 11, 3)
write(create_node(cbind(Team, GD, Pts), "CAFR1GH", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "", "", "", "", "")
Team = c("Ghana", "Madagascar", "Mali", "Comoros", "Central African Republic", "Chad")
GD = c("+17", "+5", "+11", "-1", "-13", "-19")
Pts = c(25, 19, 18, 15, 8, 1)
write(create_node(cbind(Team, GD, Pts), "CAFR1GI", categories, widths = widths), file_name, append = TRUE)

#END Round 1 CAF
caf_r2_teams = c("Nigeria", "Gabon", "Cameroon", "DR Congo")
widths = c(find_max_width(caf_r2_teams, width_multiplier), rep(score_width, 3))
write(create_H2H("CAFR2S1", "Nigeria", "Gabon", "4", "1", TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("CAFR2S2", "Cameroon", "DR Congo", "0", "1", FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("CAFR2F", "Nigeria", "DR Congo", "1 (3)", "1 (4)", FALSE, widths = widths), file_name, append = TRUE)

europe_teams = c("Germany", "Slovakia", "Northern Ireland", "Luxembourg", 
                 "Switzerland", "Kosovo", "Slovenia", "Sweden", 
                 "Scotland", "Denmark", "Greece", "Belarus",
                 "France", "Ukraine", "Iceland", "Azerbaijan",
                 "Spain", "Turkey", "Georgia", "Bulgaria", 
                 "Portugal", "Ireland", "Hungary", "Armenia", 
                 "Netherlands", "Poland", "Finland", "Malta", "Lithuania",
                 "Austria", "Bosnia and Herzegovina", "Romania", "Cyprus", "San Marino",
                 "Norway", "Italy", "Israel", "Estonia", "Moldova",
                 "Belgium", "Wales", "North Macedonia", "Kazakhstan", "Liechtenstein",
                 "England", "Albania", "Serbia", "Latvia", "Andorra", 
                 "Croatia", "Czechia", "Faroe Islands","Montenegro", "Gibraltar")

team_width = find_max_width(europe_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)
widths = round(widths)

categories = c("wc", "advance", "advance", "")

Team = c("Germany", "Slovakia", "Northern Ireland", "Luxembourg")
GD = c("+13", "-2", "+1", "-12")
Pts = c(15, 12, 9, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGA", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "advance")
Team = c("Switzerland", "Kosovo", "Slovenia", "Sweden")
GD = c("+12", "+1", "-5", "-8")
Pts = c(14, 11, 4, 2)
write(create_node(cbind(Team, GD, Pts), "UEFAGB", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "")
Team = c("Scotland", "Denmark", "Greece", "Belarus")
GD = c("+6", "+9", "-2", "-13")
Pts = c(13, 11, 7, 2)
write(create_node(cbind(Team, GD, Pts), "UEFAGC", categories, widths = widths), file_name, append = TRUE)

Team = c("France", "Ukraine", "Iceland", "Azerbaijan")
GD = c("+12", "-1", "+2", "-13")
Pts = c(16, 10, 7, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGD", categories, widths = widths), file_name, append = TRUE)

Team = c("Spain", "Turkey", "Georgia", "Bulgaria")
GD = c("+19", "+5", "-8", "-16")
Pts = c(16, 13, 3, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGE", categories, widths = widths), file_name, append = TRUE)

Team = c("Portugal", "Ireland", "Hungary", "Armenia")
GD = c("+13", "+2", "+1", "-16")
Pts = c(13, 10, 8, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGF", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", rep("", 3))

Team = c("Netherlands", "Poland", "Finland", "Malta", "Lithuania")
GD = c("+23", "+7", "-6", "-15", "-9")
Pts = c(20, 17, 10, 5, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGG", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "advance", "", "")
Team = c("Austria", "Bosnia and Herzegovina", "Romania", "Cyprus", "San Marino")
GD = c("+18", "+10", "+9", "0", "-37")
Pts = c(19, 17, 13, 8, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGH", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", rep("", 3))
Team = c("Norway", "Italy", "Israel", "Estonia", "Moldova")
GD = c("+32", "+9", "-1", "-13", "-27")
Pts = c(24, 18, 12, 4, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGI", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "advance", "", "")
Team = c("Belgium", "Wales", "North Macedonia", "Kazakhstan", "Liechtenstein")
GD = c("+22", "+10", "+3", "-4", "-31")
Pts = c(18, 16, 13, 8, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGJ", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", rep("", 3))
Team = c("England", "Albania", "Serbia", "Latvia", "Andorra")
GD = c("+22", "+2", "-1", "-10", "-13")
Pts = c(24, 14, 13, 5, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGK", categories, widths = widths), file_name, append = TRUE)

Team = c("Croatia", "Czechia", "Faroe Islands","Montenegro", "Gibraltar")
GD = c("+22", "+10", "+2", "-9", "-25")
Pts = c(22, 16, 12, 9, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGL", categories, widths = widths), file_name, append = TRUE)

#END UEFA R1

uefa_r2_teams = c("Italy", "Denmark", "Turkey", "Ukraine",
                  "Poland", "Wales", "Czechia", "Slovakia",
                  "Ireland", "Albania", "Bosnia and Herzegovina", "Kosovo",
                  "Sweden", "Romania", "North Macedonia", "Northern Ireland")

widths = c(find_max_width(uefa_r2_teams, width_multiplier), rep(score_width, 3))

write(create_H2H("UEFAR2AS1", "Italy", "Northern Ireland", 2, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2AS2", "Wales", "Bosnia and Herzegovina", "1 (2)", "1 (4)", FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2AF", "Bosnia and Herzegovina", "Italy", "1 (4)", "1 (1)", TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)

write(create_H2H("UEFAR2BS1", "Ukraine", "Sweden", 1, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2BS2", "Poland", "Albania", 2, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2BF", "Sweden", "Poland", 3, 2, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)

write(create_H2H("UEFAR2CS1", "Turkey", "Romania", 1, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2CS2", "Slovakia", "Kosovo", 3, 4, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2CF", "Kosovo", "Turkey", 0, 1, FALSE, widths = widths, wc = TRUE), file_name, append = TRUE)

write(create_H2H("UEFAR2DS1", "Denmark", "North Macedonia", 4, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2DS2", "Czechia", "Ireland", "2 (4)", "2 (3)", TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2DF", "Czechia", "Denmark", "2 (3)", "2 (1)", TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)

#End UEFA R2
ccf_r1_teams = c("Anguilla", "Turks and Caicos Islands",
                 "U.S. Virgin Islands", "British Virgin Islands")
widths = c(find_max_width(ccf_r1_teams, width_multiplier), rep(score_width, 3))
write(create_two_leg("CCFR1M1", "Anguilla", "Turks and Caicos Islands", 0, 0, 1, 1, TRUE, widths = widths, pks = c(4,3)), file_name, append = TRUE)
write(create_two_leg("CCFR1M2", "U.S. Virgin Islands", "British Virgin Islands", 1, 1, 0, 0, FALSE, widths = widths, pks = c(2, 4)), file_name, append = TRUE)

ccf_r2_teams = c("Honduras", "Bermuda", "Cuba", "Cayman Islands", "Antigua and Barbuda", 
                 "Costa Rica", "Trinidad and Tobago", "Grenada", "Saint Kitts and Nevis", "Bahamas",
                 "Curaçao", "Haiti", "Saint Lucia", "Aruba", "Barbados",
                 "Panama", "Nicaragua", "Guyana", "Montserrat", "Belize", 
                 "Jamaica", "Guatemala", "Dominican Republic", "Dominica", "British Virgin Islands",
                 "Suriname", "El Salvador", "Puerto Rico", "Saint Vincent and the Grenadines", "Anguilla")

team_width = find_max_width(ccf_r2_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)
widths = round(widths)

categories = c(rep("advance", 2), rep("", 4))

Team = c("Honduras", "Bermuda", "Cuba", "Cayman Islands", "Antigua and Barbuda")
GD = c("+10", "+1", "+1", "-8", "-4")
Pts = c(12, 7, 6, 3, 1)
write(create_node(cbind(Team, GD, Pts), "CCFR2GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Costa Rica", "Trinidad and Tobago", "Grenada", "Saint Kitts and Nevis", "Bahamas")
GD = c("+16", "+9", "+4", "-8", "-21")
Pts = c(12, 7, 7, 3, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR2GB", categories, widths = widths), file_name, append = TRUE)

Team = c("Curaçao", "Haiti", "Saint Lucia", "Aruba", "Barbados")
GD = c("+13", "+4", "-4", "-7", "-6")
Pts = c(12, 9, 4, 2, 1)
write(create_node(cbind(Team, GD, Pts), "CCFR2GC", categories, widths = widths), file_name, append = TRUE)

Team = c("Panama", "Nicaragua", "Guyana", "Montserrat", "Belize")
GD = c("+9", "+5", "+2", "-7", "-9")
Pts = c(12, 9, 6, 3, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR2GD", categories, widths = widths), file_name, append = TRUE)

Team = c("Jamaica", "Guatemala", "Dominican Republic", "Dominica", "British Virgin Islands")
GD = c("+6", "+8", "+6", "-9", "-11")
Pts = c(12, 9, 6, 3, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR2GE", categories, widths = widths), file_name, append = TRUE)


Team = c("Suriname", "El Salvador", "Puerto Rico", "Saint Vincent and the Grenadines", "Anguilla")
GD = c("+8", "+5", "+8", "0", "-21")
Pts = c(10, 8, 7, 3, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR2GF", categories, widths = widths), file_name, append = TRUE)

ccf_r3_teams = c("Panama", "Suriname", "Guatemala", "El Salvador", 
                 "Curaçao", "Jamaica", "Trinidad and Tobago", "Bermuda", 
                 "Haiti", "Honduras", "Costa Rica", "Nicaragua")

team_width = find_max_width(ccf_r2_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)
widths = round(widths)

categories = c("wc", "advance", "", "")

Team = c("Panama", "Suriname", "Guatemala", "El Salvador")
GD = c("+5", "+3", "+1", "-9")
Pts = c(12, 9, 8, 3)
write(create_node(cbind(Team, GD, Pts), "CCFR3GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Curaçao", "Jamaica", "Trinidad and Tobago", "Bermuda")
GD = c("+10", "+8", "+1", "-19")
Pts = c(12, 11, 7, 1)
write(create_node(cbind(Team, GD, Pts), "CCFR3GB", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "", "", "")
Team = c("Haiti", "Honduras", "Costa Rica", "Nicaragua")
GD = c("+3", "+3", "+2", "-8")
Pts = c(11, 9, 7, 4)
write(create_node(cbind(Team, GD, Pts), "CCFR3GC", categories, widths = widths), file_name, append = TRUE)


#TODO
ofc_r1_teams = c("Cook Islands", "Tonga", 
                 "American Samoa", "Samoa")
team_width = find_max_width(ofc_r1_teams, width_multiplier)
widths = c(find_max_width(ofc_r1_teams, width_multiplier), rep(score_width, 3))
write(create_H2H("OFCR1S1", "Cook Islands", "Tonga", 1, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("OFCR1S2", "American Samoa", "Samoa", 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("OFCR1F", "Tonga", "Samoa", 1, 2, FALSE, widths = widths), file_name, append = TRUE)

ofc_r2_teams = c("New Caledonia", "Fiji", "Solomon Islands", "Papua New Guinea", 
                 "New Zealand", "Tahiti", "Vanuatu", "Samoa")

widths = c(find_max_width(ofc_r2_teams, width_multiplier), gd_width, pts_width)

categories = c("advance", "advance", "", "")
Team = c("New Caledonia", "Fiji", "Solomon Islands", "Papua New Guinea")
GD = c("+3", "+1", "-1", "-3")
Pts = c(7, 5, 3, 1)
write(create_node(cbind(Team, GD, Pts), "OFCR2GA", categories, widths = widths), file_name, append = TRUE)

Team = c("New Zealand", "Tahiti", "Vanuatu", "Samoa")
GD = c("+18", "+2", "-6", "-14")
Pts = c(9, 6, 3, 0)
write(create_node(cbind(Team, GD, Pts), "OFCR2GB", categories, widths = widths), file_name, append = TRUE)

ofc_r3_teams = c("New Caledonia", "Tahiti", 
                 "New Zealand", "Fiji")
team_width = find_max_width(ofc_r3_teams, width_multiplier)
widths = c(find_max_width(ofc_r3_teams, width_multiplier), rep(score_width, 3))

write(create_H2H("OFCR3S1", "New Caledonia", "Tahiti", 3, 0, TRUE, widths), file_name, append = TRUE)
write(create_H2H("OFCR3S2", "New Zealand", "Fiji", 7, 0, TRUE, widths), file_name, append = TRUE)
write(create_H2H("OFCR3F", "New Caledonia","New Zealand", 0, 3, FALSE, widths, wc = TRUE), file_name, append = TRUE)

#### TODO

icf_teams = c("New Caledonia", "Jamaica", 
                 "DR Congo", "Bolivia",
              "Suriname", "Iraq")
team_width = find_max_width(icf_teams, width_multiplier)
widths = c(find_max_width(icf_teams, width_multiplier), rep(score_width, 3))

write(create_H2H("ICF1R1", "New Caledonia", "Jamaica", 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("ICF1R2", "DR Congo", "Jamaica", 1, 0, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)

write(create_H2H("ICF2R1", "Bolivia", "Suriname", 2, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("ICF2R2", "Iraq", "Bolivia", 1, 0, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)

wc_final_teams = c("Mexico", "South Africa", "South Korea", "Czechia",
                   "Switzerland", "Canada", "Bosnia and Herzegovina", "Qatar",
                   "Brazil", "Morocco", "Scotland", "Haiti",
                   "United States", "Australia", "Paraguay", "Turkey",
                   "Germany", "Ivory Coast", "Ecuador", "Curaçao",
                   "Netherlands", "Japan", "Sweden", "Tunisia",
                   "Belgium", "Egypt", "Iran", "New Zealand",
                   "Spain", "Cape Verde", "Uruguay", "Saudi Arabia",
                   "France", "Norway", "Senegal", "Iraq",
                   "Argentina", "Austria", "Algeria", "Jordan",
                   "Colombia", "Portugal", "DR Congo", "Uzbekistan",
                   "England", "Croatia", "Ghana", "Panama")

team_width = find_max_width(wc_final_teams, width_multiplier)

widths = c(team_width, gd_width, pts_width)

Teams = c("Mexico", "South Africa", "South Korea", "Czechia")
GD = c("+6", "-1", "-1", "-4")
Pts = c(9, 4, 3, 1)
write(create_group(Teams, GD, Pts, "A", advance_teams = 2, widths = widths), file_name, append = TRUE)

Teams = c("Switzerland", "Canada", "Bosnia and Herzegovina", "Qatar")
GD = c("+4", "+5", "-1", "-8")
Pts = c(7,4,4,1)
write(create_group(Teams, GD, Pts, "B", widths), file_name, append = TRUE)

Teams = c("Brazil", "Morocco", "Scotland", "Haiti")
GD = c("+6", "+3", "-3", "-6")
Pts = c(7,7,3,0)
write(create_group(Teams, GD, Pts, "C", widths, advance_teams = 2), file_name, append = TRUE)

Teams = c("United States", "Australia", "Paraguay", "Turkey")
GD = c("+4", "0", "-2", "-2")
Pts = c(6,4,4,3)
write(create_group(Teams, GD, Pts, "D", widths), file_name, append = TRUE)

Teams = c("Germany", "Ivory Coast", "Ecuador", "Curaçao")
GD = c("+6", "+2", "0", "-8")
Pts = c(6,6,4,1)
write(create_group(Teams, GD, Pts, "E", widths), file_name, append = TRUE)

Teams = c("Netherlands", "Japan", "Sweden", "Tunisia")
GD = c("+6", "+4", "0", "-10")
Pts = c(7,5,4,0)
write(create_group(Teams, GD, Pts, "F", widths), file_name, append = TRUE)

Teams = c("Belgium", "Egypt", "Iran", "New Zealand")
GD = c("+4", "+2", "0", "-6")
Pts = c(5,5,3,1)
write(create_group(Teams, GD, Pts, "G", advance_teams = 2, widths), file_name, append = TRUE)

Teams = c("Spain", "Cape Verde", "Uruguay", "Saudi Arabia")
GD = c("+5", "0", "-1", "-4")
Pts = c(7,3,2,2)
write(create_group(Teams, GD, Pts, "H", advance_teams = 2, widths), file_name, append = TRUE)

Teams = c("France", "Norway", "Senegal", "Iraq")
GD = c("+8", "+1", "+2", "-11")
Pts = c(9,6,3,0)
write(create_group(Teams, GD, Pts, "I", widths), file_name, append = TRUE)

Teams = c("Argentina", "Austria", "Algeria", "Jordan")
GD = c("+7", "0", "-2", "-5")
Pts = c(9,4,4,0)
write(create_group(Teams, GD, Pts, "J", widths), file_name, append = TRUE)

Teams = c("Colombia", "Portugal", "DR Congo", "Uzbekistan")
GD = c("+3", "+5", "+1", "-9")
Pts = c(7,5,4,0)
write(create_group(Teams, GD, Pts, "K", widths), file_name, append = TRUE)

Teams = c("England", "Croatia", "Ghana", "Panama")
GD = c("+4", "0", "0", "-4")
Pts = c(7,6,4,0)
write(create_group(Teams, GD, Pts, "L", widths), file_name, append = TRUE)

knockout_teams = c("Germany", "Paraguay",
                   "France", "Sweden",
                   "South Africa", "Canada",
                   "Netherlands", "Morocco",
                   "Portugal", "Croatia", 
                   "Spain", "Austria",
                   "United States", "Bosnia and Herzegovina", 
                   "Belgium", "Senegal",
                   "Brazil", "Japan",
                   "Ivory Coast", "Norway", 
                   "Mexico", "Ecuador",
                   "England", "DR Congo",
                   "Argentina", "Cape Verde",
                   "Australia", "Egypt",
                   "Switzerland", "Algeria",
                   "Colombia", "Ghana")

 ko_team_width = find_max_width(knockout_teams, width_multiplier)
 ko_widths = c(ko_team_width, score_width)

 write(create_H2H( "R32M1", "Germany", "Paraguay", "1 (3)", "1 (4)", FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M2", "France", "Sweden", "3", "0", TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M3", "South Africa", "Canada", 0, 1, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M4", "Netherlands", "Morocco", "1 (2)", "1 (3)", FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M5", "Portugal", "Croatia", 2, 1, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M6", "Spain", "Austria", 3, 0, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M7", "United States", "Bosnia and Herzegovina", "2", "0", TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M8", "Belgium", "Senegal", 3, 2, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H( "R32M9", "Brazil", "Japan", 2, 1, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M10", "Ivory Coast", "Norway", 1, 2, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M11", "Mexico", "Ecuador", 2, 0, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M12", "England", "DR Congo", 2, 1, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M13", "Argentina", "Cape Verde", 3, 2, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M14", "Australia", "Egypt", "1 (2)", "1 (4)", FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M15", "Switzerland", "Algeria", 2, 0, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R32M16", "Colombia", "Ghana", 1, 0, TRUE, ko_widths), file_name, append = TRUE)
 
 write(create_H2H("R16M1", "Paraguay", "France", 0, 1, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M2", "Canada", "Morocco", 0, 3, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M3", "Portugal", "Spain", 0, 1, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M4", "United States", "Belgium", 1, 4, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M5", "Brazil", "Norway", 1, 2, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M6", "Mexico", "England", 2, 3, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M7", "Argentina", "Egypt", 3, 2, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("R16M8", "Switzerland", "Colombia", "0 (4)", "0 (3)", TRUE, ko_widths), file_name, append = TRUE)
# 
 write(create_H2H("QF1", "France", "Morocco", 2, 0, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("QF2", "Spain", "Belgium", 2, 1, TRUE, ko_widths), file_name, append = TRUE)
 write(create_H2H("QF3", "Norway", "England", 1, 2, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("QF4", "Argentina", "Switzerland", 3, 1, TRUE, ko_widths), file_name, append = TRUE)
# 
 write(create_H2H("SF1", "France", "Spain", 0, 2, FALSE, ko_widths), file_name, append = TRUE)
 write(create_H2H("SF2", "England", "Argentina", 1, 2, FALSE, ko_widths), file_name, append = TRUE)
# 
 write(create_H2H("P3", "France", "England", "4", "6", TRUE, ko_widths, categories = c("", "bronze")), file_name, append = TRUE)
 write(create_H2H("F", "Spain", "Argentina", "?", "?", TRUE, ko_widths, categories = c("gold", "silver")), file_name, append = TRUE)

#END World cup finals


file_end = '  graph [rankdir = "LR",
         newrank = "true"]

  subgraph cluster_Conmebol{
    label = "Confederación 
Sudamericana
 de Fútbol"
 #   label = "CONMEBOL"
    fontsize = 50
    CMBRR;
  }
  edge [color = "#800000"]
edge [color = "black"]
  
  subgraph cluster_AFC{
label = "Asian Football Confederation"
#label = "AFC"
fontsize = 50
    {rank = same; AFCR1M1; AFCR1M2; AFCR1M3; AFCR1M4; AFCR1M5; AFCR1M6; AFCR1M7; AFCR1M8; AFCR1M9; AFCR1M10;}
    
    {rank = same; AFCR2GA; AFCR2GB; AFCR2GC; AFCR2GD; AFCR2GE; AFCR2GF; AFCR2GG; AFCR2GH; AFCR2GI}
    

    
    {rank = same; AFCR3GA; AFCR3GB; AFCR3GC}
    {rank = same; AFCR4GA; AFCR4GB}
    
      edge[color = "#F58231"]
    AFCR1M1:e1 -> AFCR2GA:w4
    AFCR1M2:e2 -> AFCR2GI:w4
    AFCR1M3:e1 -> AFCR2GC:w4
    AFCR1M4:e1 -> AFCR2GH:w3
    AFCR1M5:e1 -> AFCR2GB:w4
    AFCR1M6:e2 -> AFCR2GG:w4
    AFCR1M7:e1 -> AFCR2GD:w4
    AFCR1M8:e1 -> AFCR2GF:w2
    AFCR1M9:e1 -> AFCR2GE:w4
    AFCR1M10:e1 -> AFCR2GH:w4
    
    AFCR2GA:e1 -> AFCR3GA:w4 [color = "#CB7C77"]
    AFCR2GA:e2 -> AFCR3GB:w6 [color = "#68D359"]
    AFCR2GB:e1 -> AFCR3GC:w1 [color = "#6B42C8"]
    AFCR2GB:e2 -> AFCR3GA:w6 [color = "#C9D73D"]
    AFCR2GC:e1 -> AFCR3GB:w1 [color = "#C555CB"]
    AFCR2GC:e2 -> AFCR3GC:w5 [color = "#AED688"]
    AFCR2GD:e1 -> AFCR3GB:w4 [color = "#502E71"]
    AFCR2GD:e2 -> AFCR3GA:w5 [color = "#C49A3F"]
    AFCR2GE:e1 -> AFCR3GA:w1 [color = "#6A7DC9"]
    AFCR2GE:e2 -> AFCR3GA:w2 [color = "#D7652D"]
    AFCR2GF:e1 -> AFCR3GB:w3 [color = "#7CD5C8"]
    AFCR2GF:e2 -> AFCR3GC:w4 [color = "#C5383C"]
    AFCR2GG:e1 -> AFCR3GB:w2 [color = "#507D41"]
    AFCR2GG:e2 -> AFCR3GC:w3 [color = "#CF4C8B"]
    AFCR2GH:e1 -> AFCR3GA:w3 [color = "#5D8D9C"]
    AFCR2GH:e2 -> AFCR3GC:w6 [color = "#722E41"]
    AFCR2GI:e1 -> AFCR3GC:w2 [color = "#C8B693"]
    AFCR2GI:e2 -> AFCR3GB:w5 [color = "#33333C"]
    
    AFCR3GA:e3 -> AFCR4GA:w2 [color = "#5D8D9C"] #UAE
    AFCR3GA:e4 -> AFCR4GA:w1 [color = "#CB7C77"] #Qatar
    AFCR3GB:e3 -> AFCR4GB:w2 [color = "#7CD5C8"] #Iraq
    AFCR3GB:e4 -> AFCR4GA:w3 [color = "#502E71"] #Oman
    AFCR3GC:e3 -> AFCR4GB:w1 [color = "#CF4C8B"] #Saudi Arabia
    AFCR3GC:e4 -> AFCR4GB:w3 [color = "#C5383C"] #Indonesia
    
    AFCR4GA:e2 -> AFCR5:w1 [color = "#5D8D9C"] #UAE
    AFCR4GB:e2 -> AFCR5:w2 [color = "#7CD5C8"] #Iraq

      edge[color = "black"]
  }
  subgraph cluster_CAF{
label = "Confederation of African Football"
#label = "CAF"
fontsize = 50
    {rank = same; CAFR1GA; CAFR1GE; CAFR1GG; CAFR1GH; CAFR1GI;}
    {rank = same; CAFR1GB; CAFR1GC; CAFR1GD; CAFR1GF;}
    {rank = same; CAFR2S1; CAFR2S2;}
    CAFR2F
  }
  CAFR1GC -> CAFR1GF [style = "invis"]

edge[color = "#3CB44B"]

    CAFR1GC:e2 -> CAFR2S1:w1
    CAFR1GF:e2 -> CAFR2S1:w2
    CAFR1GD:e2 -> CAFR2S2:w1
    CAFR1GB:e2 -> CAFR2S2:w2
    CAFR2S1:e1 -> CAFR2F:w1
    CAFR2S2:e2 -> CAFR2F:w2
    edge[color = "black"]
  
edge[color = "#3CB44B"]    
edge[color = "black"]
  
  subgraph cluster_OFC{
label = "Oceania Football Confederation"
#label = "OFC"
    fontsize = 50
    {rank = same; OFCR1S1; OFCR1S2}
    {rank = same; OFCR2GA; OFCR2GB}
    {rank = same; OFCR3S1; OFCR3S2}
    edge[color = "#0082c8"]
    OFCR1S1:e2 -> OFCR1F:w1
    OFCR1S2:e2 -> OFCR1F:w2
    OFCR1F:e2 -> OFCR2GB:w4
    OFCR2GA:e1 -> OFCR3S1:w1 [color = "#000000"]
    OFCR2GA:e2 -> OFCR3S2:w2 [color = "#FF6DB6"]
    OFCR2GB:e1 -> OFCR3S2:w1 [color = "#B66DFF"]
    OFCR2GB:e2 -> OFCR3S1:w2 [color = "#920000"]
    OFCR3S1:e1 -> OFCR3F:w1 [color = "#000000"]
    OFCR3S2:e1 -> OFCR3F:w2 [color = "#B66DFF"]
    edge[color = "black"]
  }
  
  subgraph cluster_UEFA{
label = "Union of European Football Associations"
#label = "UEFA"
fontsize = 50
    {rank = same; UEFAGA; UEFAGB; UEFAGC; UEFAGD; UEFAGE; UEFAGF; UEFAGG; UEFAGH; UEFAGI; UEFAGJ; UEFAGK; UEFAGL}
    {rank = same; UEFAR2AS1; UEFAR2AS2; UEFAR2BS1; UEFAR2BS2; UEFAR2CS1; UEFAR2CS2; UEFAR2DS1; UEFAR2DS2}
    {rank = same; UEFAR2AF; UEFAR2BF; UEFAR2CF; UEFAR2DF}


  edge[ color = "black"] 
    UEFAGA:e2 -> UEFAR2CS2:w1
    UEFAGA:e3 -> UEFAR2AS1:w2
    UEFAGB:e2 -> UEFAR2CS2:w2
    UEFAGB:e4 -> UEFAR2BS1:w2
    UEFAGC:e2 -> UEFAR2DS1:w1
    UEFAGD:e2 -> UEFAR2BS1:w1
    UEFAGE:e2 -> UEFAR2CS1:w1
    UEFAGF:e2 -> UEFAR2DS2:w2
    UEFAGG:e2 -> UEFAR2BS2:w1
    UEFAGH:e2 -> UEFAR2AS2:w2
    UEFAGH:e3 -> UEFAR2CS1:w2
    UEFAGI:e2 -> UEFAR2AS1:w1
    UEFAGJ:e2 -> UEFAR2AS2:w1
    UEFAGJ:e3 -> UEFAR2DS1:w2
    UEFAGK:e2 -> UEFAR2BS2:w2
    UEFAGL:e2 -> UEFAR2DS2:w1
    
    UEFAR2AS2:e2 -> UEFAR2AF:w1
    UEFAR2AS1:e1 -> UEFAR2AF:w2
    
    UEFAR2BS1:e2 -> UEFAR2BF:w1
    UEFAR2BS2:e1 -> UEFAR2BF:w2
    
    UEFAR2CS2:e2 -> UEFAR2CF:w1
    UEFAR2CS1:e1 -> UEFAR2CF:w2
    
    UEFAR2DS2:e1 -> UEFAR2DF:w1
    UEFAR2DS1:e1 -> UEFAR2DF:w2
    
    
   edge[color = "black"] 
  }
  edge[ color = "black"]  
  
  subgraph cluster_Concacaf{
label = "The Confederation of North,
Central American and Caribbean
Association Football"
#label = "CONCACAF"
fontsize = 50
    {rank = same; CCFR1M1; CCFR1M2;}
    {rank = same; CCFR2GA; CCFR2GB; CCFR2GC; CCFR2GD; CCFR2GE; CCFR2GF;}
    {rank = same; CCFR3GA; CCFR3GB; CCFR3GC;}


    edge[ color = "#000080"]
    CCFR1M1:e1 -> CCFR2GF:w5
    CCFR1M2:e2 -> CCFR2GE:w5
    
    CCFR2GA:e1 -> CCFR3GC:w2 [color = "#CB7C77"]
    CCFR2GA:e2 -> CCFR3GB:w4 [color = "#68D359"]
    CCFR2GB:e1 -> CCFR3GC:w3 [color = "#6B42C8"]
    CCFR2GB:e2 -> CCFR3GB:w3 [color = "#C9D73D"]
    CCFR2GC:e1 -> CCFR3GB:w1 [color = "#C555CB"]
    CCFR2GC:e2 -> CCFR3GC:w1 [color = "#AED688"]
    CCFR2GD:e1 -> CCFR3GA:w1 [color = "#502E71"]
    CCFR2GD:e2 -> CCFR3GC:w4 [color = "#C49A3F"]
    CCFR2GE:e1 -> CCFR3GB:w2 [color = "#6A7DC9"]
    CCFR2GE:e2 -> CCFR3GA:w3 [color = "#D7652D"] 
    CCFR2GF:e1 -> CCFR3GA:w2 [color = "#7CD5C8"]
    CCFR2GF:e2 -> CCFR3GA:w4 [color = "#C5383C"]

    edge[color = "black"]
    
  }
 subgraph cluster_icf {
     label = "Intercontinental Playoffs"
     fontsize = 50
     ICF1R1:e2 -> ICF1R2:w2
     ICF2R1:e1 -> ICF2R2:w2
     {rank = same; ICF1R1; ICF2R1}
 }
 {rank = same; CCFR1M1; UEFAGA}
# UEFAR2BF -> OFCR1S1 [ltail=cluster_UEFA, lhead=cluster_OFC];
 #UEFAR2CF -> AFCR1M7 [ltail=cluster_UEFA, lhead=cluster_AFC];
 {rank = same; AFCR1M7; OFCR1S1; CAFR1GA}
 {rank = same; CMBRR; AFCR5}
 edge [style = "invis"]
 UEFAR2DF -> OFCR1S1
 AFCR1M7 -> CAFR1GI
 CAFR2F -> CMBRR
 AFCR5 -> ICF2R1
 ICF1R2 -> GroupG
 CCFR3GA -> CAFR1GA

subgraph cluster_wcF {
     label = "World Cup Finals"
     fontsize = 50
    {rank = same; GroupL; GroupK; GroupJ; GroupI; GroupH; GroupG; GroupF; GroupE; GroupD; GroupC; GroupB; GroupA}
    {rank = same; R32M16; R32M15; R32M14; R32M13; R32M12; R32M11; R32M10; R32M9; R32M8; R32M7; R32M6; R32M5; R32M4; R32M3; R32M2; R32M1;}
    {rank = same; R16M8; R16M7; R16M6; R16M5; R16M4; R16M3; R16M2; R16M1;}
    {rank = same; QF4; QF3; QF2; QF1;}
    {rank = same; SF2; SF1}
    {rank = same; F; P3}
    edge [style = "invis"]
    
    GroupL -> GroupK -> GroupJ -> GroupI -> GroupH -> GroupG -> GroupF -> GroupE -> GroupD -> GroupC -> GroupB -> GroupA
    R32M16 -> R32M15 -> R32M14 -> R32M13 -> R32M12 -> R32M11 -> R32M10 -> R32M9 -> R32M8 -> R32M7 -> R32M6 -> R32M5 -> R32M4 -> R32M3 -> R32M2 -> R32M1
    R16M8 -> R16M7 -> R16M6 -> R16M5 -> R16M4 -> R16M3 -> R16M2 -> R16M1
    QF4 -> QF3 -> QF2 -> QF1
    SF2 -> SF1
    P3 -> F
    GroupF -> R32M8
    GroupG -> R32M9
    R32M16 -> P3
    R32M1 -> F
    
    edge [style = "visible"]
    
    R32M1:e2 -> R16M1:w1
    R32M2:e1 -> R16M1:w2
    R32M3:e2 -> R16M2:w1
    R32M4:e2 -> R16M2:w2
    R32M5:e1 -> R16M3:w1
    R32M6:e1 -> R16M3:w2
    R32M7:e1 -> R16M4:w1
    R32M8:e1 -> R16M4:w2
    R32M9:e1 -> R16M5:w1
    R32M10:e2 -> R16M5:w2
    R32M11:e1 -> R16M6:w1
    R32M12:e1 -> R16M6:w2
    R32M13:e1 -> R16M7:w1
    R32M14:e2 -> R16M7:w2
    R32M15:e1 -> R16M8:w1
    R32M16:e1 -> R16M8:w2
    
    R16M1:e2 -> QF1:w1
    R16M2:e2 -> QF1:w2
    R16M3:e2 -> QF2:w1
    R16M4:e2 -> QF2:w2
    R16M5:e2 -> QF3:w1
    R16M6:e2 -> QF3:w2
    R16M7:e1 -> QF4:w1
    R16M8:e1 -> QF4:w2
    
    QF1:e1 -> SF1:w1
    QF2:e1 -> SF1:w2
    QF3:e2 -> SF2:w1
    QF4:e1 -> SF2:w2
    
    SF1:e2 -> F:w1
    SF2:e2 -> F:w2
    
    SF1:e1 -> P3:w1
    SF2:e1 -> P3:w2
}

#225 + 259 + 99 + 90 + 18 + 204 + 4 + 104

graph [label = "2026 FIFA World Cup:
209 Countries, 1003 Matches, 1 Champion
Created by Jed Grabman",
         labelloc = "t",
fontsize = 200]
}'
write(file_end, file_name, append = TRUE)

