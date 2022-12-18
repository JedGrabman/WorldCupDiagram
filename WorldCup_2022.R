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
    return("French Polynesia")
  }
  if (country == "Ivory Coast"){
    return("Côte d’Ivoire")
  }
  if (country == "Macau"){
    return("Macao SAR China")
  }
  return(country)
}

add_flag = function(countries){
  country_names = sapply(countries, function(country) get_country_name(country))
  country_flag = flag(country_names)
  country_flag[country_names %in% c("Scotland", "England", "Wales", "Northern Ireland")] = flag("United Kingdom")
  
  return(paste(country_flag, countries))
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

create_group = function(Team, GD, Pts, group_let, widths = rep(0, 3)){
  dtf = cbind(Team, GD, Pts)
  group_name = paste0("Group", group_let)
  categories = c("advance", "advance", "", "")
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
  if ((Total[1] == Total[2]) & (score21 != score12)){
    if (score21 > score12){
      Total[2] = paste(Total[2], "(a)")
    } else {
      Total[1] = paste(Total[1], "(a)")
    }
  }
  
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

file_name = "WorldCup_2022.txt"

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

Team = c("Brazil", "Argentina", "Uruguay", "Ecuador",
        "Peru", "Colombia", "Chile", "Paraguay",
         "Bolivia", "Venezuela")
Pts = c(45, 39, 28, 26, 24, 23, 19, 16, 15, 10)
categories = c(rep("wc", 4), "advance", rep("", 5))

write(create_node(cbind(Team, Pts), "CMBRR", categories), file_name, append = TRUE)

wc_final_teams = c("Netherlands", "Senegal", "Ecuador", "Qatar",
                   "England", "USA", "Iran", "Wales",
                   "Argentina", "Poland", "Mexico", "Saudi Arabia",
                   "France", "Australia", "Tunisia", "Denmark",
                   "Japan", "Spain", "Germany", "Costa Rica",
                   "Morocco", "Croatia", "Belgium", "Canada",
                   "Brazil", "Switzerland", "Cameroon", "Serbia",
                   "Portugal", "South Korea", "Uruguay", "Ghana") 


team_width = find_max_width(wc_final_teams, width_multiplier)

widths = c(team_width, gd_width, pts_width)

Teams = c("Netherlands", "Senegal", "Ecuador", "Qatar")
GD = c("+4", "+1", "+1", "-6")
Pts = c(7,6,4,0)
write(create_group(Teams, GD, Pts, "A", widths), file_name, append = TRUE)

Teams = c("England", "USA", "Iran", "Wales")
GD = c("+7", "+1", "-3", "-5")
Pts = c(7,5,3,1)
write(create_group(Teams, GD, Pts, "B", widths), file_name, append = TRUE)

Teams = c("Argentina", "Poland", "Mexico", "Saudi Arabia")
GD = c("+3", "0", "-1", "-2")
Pts = c(6,4,4,3)
write(create_group(Teams, GD, Pts, "C", widths), file_name, append = TRUE)

Teams = c("France", "Australia", "Tunisia", "Denmark")
GD = c("+3", "-1", "0", "-2")
Pts = c(6,6,4,1)
write(create_group(Teams, GD, Pts, "D", widths), file_name, append = TRUE)

Teams = c("Japan", "Spain", "Germany", "Costa Rica")
GD = c("+1", "+6", "+1", "-8")
Pts = c(6,4,4,3)
write(create_group(Teams, GD, Pts, "E", widths), file_name, append = TRUE)

Teams = c("Morocco", "Croatia", "Belgium", "Canada")
GD = c("+3", "+3", "-1", "-5")
Pts = c(7,5,4,0)
write(create_group(Teams, GD, Pts, "F", widths), file_name, append = TRUE)

Teams = c("Brazil", "Switzerland", "Cameroon", "Serbia")
GD = c("+2", "+1", "0", "-3")
Pts = c(6,6,4,1)
write(create_group(Teams, GD, Pts, "G", widths), file_name, append = TRUE)

Teams = c("Portugal", "South Korea", "Uruguay", "Ghana")
GD = c("+2", "0", "0", "-2")
Pts = c(6,4,4,3)
write(create_group(Teams, GD, Pts, "H", widths), file_name, append = TRUE)

knockout_teams = wc_final_teams[c(TRUE, TRUE, FALSE, FALSE)]
ko_team_width = find_max_width(knockout_teams, width_multiplier)
ko_widths = c(ko_team_width, score_width)

write(create_H2H("R16M1", "Netherlands", "USA", 3, 1, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M2", "Argentina", "Australia", 2, 1, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M3", "Japan", "Croatia", "1 (1)", "1 (3)", FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M4", "Brazil", "South Korea", 4, 1, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M5", "England", "Senegal", 3, 0, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M6", "France", "Poland", 3, 1, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M7", "Morocco", "Spain", "0 (3)", "0 (0)", TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M8", "Portugal", "Switzerland", 6, 1, TRUE, ko_widths), file_name, append = TRUE)

write(create_H2H("QF1", "Netherlands", "Argentina", "2 (3)", "2 (4)", FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("QF2", "Croatia", "Brazil", "1 (4)", "1 (2)", TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("QF3", "England", "France", 1, 2, FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("QF4", "Morocco", "Portugal", 1, 0, TRUE, ko_widths), file_name, append = TRUE)

write(create_H2H("SF1", "Argentina", "Croatia", 3, 0, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("SF2", "France", "Morocco", 2, 0, TRUE, ko_widths), file_name, append = TRUE)

write(create_H2H("P3", "Croatia", "Morocco", "2", "1", TRUE, ko_widths, categories = c("bronze", "")), file_name, append = TRUE)
write(create_H2H("F", "Argentina", "France", "3 (4)", "3 (2)", TRUE, ko_widths, categories = c("gold", "silver")), file_name, append = TRUE)


afc_r1_teams =  c("Mongolia", "Brunei",
                  "Macau", "Sri Lanka",
                  "Laos", "Bangladesh", 
                  "Malaysia", "Timor-Leste",
                  "Cambodia", "Pakistan",
                  "Bhutan", "Guam")
widths = c(find_max_width(afc_r1_teams, width_multiplier), rep(two_leg_score_width, 2), score_width)

write(create_two_leg("AFCR1M1", "Mongolia", "Brunei", 2, 0, 1, 2, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M2", "Macau", "Sri Lanka", 1, 0, 0, 3, FALSE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M3", "Laos", "Bangladesh", 0, 1, 0, 0, FALSE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M4", "Malaysia", "Timor-Leste", 7, 1, 5, 1, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M5", "Cambodia", "Pakistan", 2, 0, 2, 1, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M6", "Bhutan", "Guam", 1, 0, 0, 5, FALSE, widths), file_name, append = TRUE)

afc_group_teams = c("Syria", "China", "Philippines", "Maldives", "Guam",
                    "Australia", "Kuwait", "Jordan", "Nepal", "Taiwan",
                    "Iran", "Iraq", "Bahrain", "Hong Kong", "Cambodia",
                    "Saudi Arabia", "Uzbekistan", "Palestine", "Singapore", "Yemen",
                    "Qatar (H)","Oman","India","Afghanistan","Bangladesh",
                    "Japan", "Tajikistan", "Kyrgyzstan", "Mongolia", "Myanmar",
                    "UAE", "Vietnam", "Malaysia", "Thailand", "Indonesia",
                    "South Korea", "Lebanon", "Turkmenistan", "Sri Lanka", "North Korea")

team_width = find_max_width(afc_group_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)                    
categories = c("advance", "advance", "", "", "")

Team = c("Syria", "China", "Philippines", "Maldives", "Guam")
GD = c("+15", "+27", "+1", "-13", "-30")
Pts = c(21, 19, 11, 7, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR2GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Iran", "Iraq", "Bahrain", "Hong Kong", "Cambodia")
GD = c("+30", "+10", "+11", "-9", "-42")
Pts = c(18, 17, 15, 5, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GC", categories, widths = widths), file_name, append = TRUE)

Team = c("Qatar (H)","Oman","India","Afghanistan","Bangladesh")
GD = c("+17","+10", "-1", "-10", "-16")
Pts = c(22, 18, 7, 6, 2)
write(create_node(cbind(Team, GD, Pts), "AFCR2GE", c("wc", "advance", "", "", ""), widths = widths), file_name, append = TRUE)

Team = c("UAE", "Vietnam", "Malaysia", "Thailand", "Indonesia")
GD = c("+16","+8", "-2", "0", "-22")
Pts = c(18, 17, 12, 9, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GG", categories, widths = widths), file_name, append = TRUE)

categories = c("advance", "advance", "", "", "wd")
Team = c("South Korea", "Lebanon", "Turkmenistan", "Sri Lanka", "North Korea (W)")
GD = c("+21","+3", "-3", "-21","-")
Pts = c(16, 10, 9, 0, "-")
write(create_node(cbind(Team, GD, Pts), "AFCR2GH", categories, widths = widths), file_name, append = TRUE)

categories = c("advance", "", "", "", "")

Team = c("Australia", "Kuwait", "Jordan", "Nepal", "Taiwan")
GD = c("+26", "+12", "+10", "-18", "-30")
Pts = c(24, 14, 14, 6, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR2GB", categories, widths = widths), file_name, append = TRUE)

Team = c("Saudi Arabia", "Uzbekistan", "Palestine", "Singapore", "Yemen")
GD = c("+18", "+9", "0", "-15", "-12")
Pts = c(20, 15, 10, 7, 5)
write(create_node(cbind(Team, GD, Pts), "AFCR2GD", categories, widths = widths), file_name, append = TRUE)

Team = c("Japan", "Tajikistan", "Kyrgyzstan", "Mongolia", "Myanmar")
GD = c("+44","+2", "+7", "-24", "-29")
Pts = c(24, 13, 10, 6, 6)
write(create_node(cbind(Team, GD, Pts), "AFCR2GF", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "wc", "advance", "", "", "")
Team = c("Iran", "South Korea", "UAE", "Iraq", "Syria", "Lebanon")
GD = c("+11","+10", "0", "-6", "-7", "-8")
Pts = c(25, 23, 12, 9, 6, 6)
write(create_node(cbind(Team, GD, Pts), "AFCR3GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Saudi Arabia", "Japan", "Australia", "Oman", "China", "Vietnam")
GD = c("+6","+8", "+6", "+1", "-10", "-11")
Pts = c(23, 22, 15, 14, 6, 4)
write(create_node(cbind(Team, GD, Pts), "AFCR3GB", categories, widths = widths), file_name, append = TRUE)

write(create_H2H("AFCR4", "UAE", "Australia", 1, 2, FALSE), file_name, append = TRUE)

caf_r1_teams = c("Ethiopia", "Lesotho",
                 "Somalia", "Zimbabwe",
                 "Eritrea", "Namibia",
                 "Burundi", "Tanzania",
                 "Djibouti", "Eswatini",
                 "Botswana", "Malawi",
                 "Gambia", "Angola",
                 "Liberia", "Sierra Leone",
                 "Mauritius", "Mozambique",
                 "Sao Tome and Principe", "Guinea-Bissau",
                 "South Sudan", "Equatorial Guinea",
                 "Comoros", "Togo",
                 "Chad", "Sudan",
                 "Seychelles", "Rwanda")

widths = c(find_max_width(caf_r1_teams, width_multiplier), rep(two_leg_score_width,2), score_width)

write(create_two_leg("CAFR1M1", "Ethiopia", "Lesotho", 0, 0, 1, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M2", "Somalia", "Zimbabwe", 1, 0, 1, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M3", "Eritrea", "Namibia", 1, 2, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M4", "Burundi", "Tanzania", 1, 1, 1, 1, FALSE, widths = widths, pks = c(0, 3)), file_name, append = TRUE)
write(create_two_leg("CAFR1M5", "Djibouti", "Eswatini", 2, 1, 0, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M6", "Botswana", "Malawi", 0, 0, 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M7", "Gambia", "Angola", 0, 1, 1, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M8", "Liberia", "Sierra Leone", 3, 1, 0, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M9", "Mauritius", "Mozambique", 0, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M10", "São Tomé and Príncipe", "Guinea-Bissau", 1, 1, 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M11", "South Sudan", "Equatorial Guinea", 1, 1, 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M12", "Comoros", "Togo", 1, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M13", "Chad", "Sudan", 1, 3, 0, 0, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M14", "Seychelles", "Rwanda", 0, 3, 0, 7, FALSE, widths = widths), file_name, append = TRUE)


caf_r2_teams = c("Algeria", "Burkina Faso", "Niger", "Djibouti",
                 "Tunisia", "Equatorial Guinea", "Zambia", "Mauritania",
                 "Nigeria", "Cape Verde", "Liberia", "Central African Republic",
                 "Cameroon", "Ivory Coast", "Mozambique", "Malawi",
                 "Mali", "Uganda", "Kenya", "Rwanda",
                 "Egypt", "Gabon", "Libya", "Angola",
                 "Ghana", "South Africa", "Ethiopia", "Zimbabwe",
                 "Senegal", "Togo", "Namibia", "Congo",
                 "Morocco", "Guinea-Bissau", "Guinea", "Sudan",
                 "DR Congo", "Benin", "Tanzania", "Madagascar"
                 )

widths = c(find_max_width(caf_r2_teams, width_multiplier), rep(score_width,3))
categories = c("advance", "", "", "")
Team = c("Algeria", "Burkina Faso", "Niger", "Djibouti")
GD = c("+21", "+8", "-4", "-25")
Pts = c(14, 12, 7, 0)
write(create_node(cbind(Team, GD, Pts), "CAFR2GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Tunisia", "Equatorial Guinea", "Zambia", "Mauritania")
GD = c("+9", "+1", "-1", "-9")
Pts = c(13, 11, 7, 2)
write(create_node(cbind(Team, GD, Pts), "CAFR2GB", categories, widths = widths), file_name, append = TRUE)

Team = c("Nigeria", "Cape Verde", "Liberia", "Central African Republic")
GD = c("+6", "+2", "-3", "-5")
Pts = c(13, 11, 6, 4)
write(create_node(cbind(Team, GD, Pts), "CAFR2GC", categories, widths = widths), file_name, append = TRUE)

Team = c("Cameroon", "Ivory Coast", "Mozambique", "Malawi")
GD = c("+9", "+7", "-6", "-10")
Pts = c(15, 13, 4, 3)
write(create_node(cbind(Team, GD, Pts), "CAFR2GD", categories, widths = widths), file_name, append = TRUE)

Team = c("Mali", "Uganda", "Kenya", "Rwanda")
GD = c("+11", "+1", "-5", "-7")
Pts = c(16, 9, 6, 1)
write(create_node(cbind(Team, GD, Pts), "CAFR2GE", categories, widths = widths), file_name, append = TRUE)

Team = c("Egypt", "Gabon", "Libya", "Angola")
GD = c("+6", "-1", "-3", "-2")
Pts = c(14, 7, 7, 5)
write(create_node(cbind(Team, GD, Pts), "CAFR2GF", categories, widths = widths), file_name, append = TRUE)

Team = c("Ghana", "South Africa", "Ethiopia", "Zimbabwe")
GD = c("+4", "+4", "-3", "-5")
Pts = c(13, 13, 5, 2)
write(create_node(cbind(Team, GD, Pts), "CAFR2GG", categories, widths = widths), file_name, append = TRUE)

Team = c("Senegal", "Togo", "Namibia", "Congo")
GD = c("+11", "-1", "-5", "-5")
Pts = c(16, 8, 5, 3)
write(create_node(cbind(Team, GD, Pts), "CAFR2GH", categories, widths = widths), file_name, append = TRUE)

Team = c("Morocco", "Guinea-Bissau", "Guinea", "Sudan")
GD = c("+19", "-6", "-6", "-7")
Pts = c(18, 6, 4, 3)
write(create_node(cbind(Team, GD, Pts), "CAFR2GI", categories, widths = widths), file_name, append = TRUE)

Team = c("DR Congo", "Benin", "Tanzania", "Madagascar")
GD = c("+6", "+1", "-2", "-5")
Pts = c(11, 10, 8, 4)
write(create_node(cbind(Team, GD, Pts), "CAFR2GJ", categories, widths = widths), file_name, append = TRUE)

caf_r3_teams = c("Egypt", "Senegal",
                 "Cameroon", "Algeria",
                 "Ghana", "Nigeria",
                 "DR Congo", "Morocco",
                 "Mali", "Tunisia")
widths = c(find_max_width(caf_r3_teams, width_multiplier), rep(two_leg_score_width, 2), score_width)

write(create_two_leg("CAFR3M1", "Egypt", "Senegal", 1, 0, 0, 1, FALSE, pks = c(1, 3), widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_two_leg("CAFR3M2", "Cameroon", "Algeria", 0, 1, 2, 1, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_two_leg("CAFR3M3", "Ghana", "Nigeria", 0, 0, 1, 1, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_two_leg("CAFR3M4", "DR Congo", "Morocco", 1, 1, 1, 4, FALSE, widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_two_leg("CAFR3M5", "Mali", "Tunisia", 0, 1, 0, 0, FALSE, widths = widths, wc = TRUE), file_name, append = TRUE)


write(create_H2H("ICF1", "Australia", "Peru", "0 (5)", "0 (4)", TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_H2H("ICF2", "Costa Rica", "New Zealand", 1, 0, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)

categories = c("wc", "advance", rep("", 4))

europe_teams = c("Serbia", "Portugal", "Ireland", "Luxembourg", "Azerbaijan",
                 "Spain", "Sweden", "Greece", "Georgia", "Kosovo",
                 "Switzerland", "Italy", "Northern Ireland", "Bulgaria", "Lithuania",
                 "France", "Ukraine", "Finland", "Bosnia and Herzegovina", "Kazakhstan",
                 "Belgium", "Wales", "Czechia", "Estonia", "Belarus",
                 "Denmark", "Scotland", "Israel", "Austria", "Faroe Islands", "Moldova",
                 "Netherlands", "Turkey", "Norway", "Montenegro", "Latvia", "Gibraltar",
                 "Croatia", "Russia", "Slovakia", "Slovenia", "Cyprus", "Malta",
                 "England", "Poland", "Albania", "Hungary", "Andorra", "San Marino",
                 "Germany", "Macedonia", "Romania", "Armenia", "Iceland", "Liechtenstein")

team_width = find_max_width(europe_teams, width_multiplier)
widths = c(team_width, gd_width, pts_width)
widths = round(widths)

Team = c("Serbia", "Portugal", "Ireland", "Luxembourg", "Azerbaijan")
GD = c("+9", "+11", "+3", "-10", "-13")
Pts = c(20, 17, 9, 9, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGA", categories, widths = widths), file_name, append = TRUE)

Team = c("Spain", "Sweden", "Greece", "Georgia", "Kosovo")
GD = c("+10", "+6", "0", "-6", "-10")
Pts = c(19, 15, 10, 7, 5)
write(create_node(cbind(Team, GD, Pts), "UEFAGB", categories, widths = widths), file_name, append = TRUE)

Team = c("Switzerland", "Italy", "Northern Ireland", "Bulgaria", "Lithuania")
GD = c("+13", "+11", "-1", "-8", "-15")
Pts = c(18, 16, 9, 8, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGC", categories, widths = widths), file_name, append = TRUE)

Team = c("France", "Ukraine", "Finland", "Bosnia and Herzegovina", "Kazakhstan")
GD = c("+15", "+3", "0", "-3", "-15")
Pts = c(18, 12, 11, 7, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGD", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", rep("advance", 2), rep("", 2))

Team = c("Belgium", "Wales", "Czechia", "Estonia", "Belarus")
GD = c("+19", "+5", "+5", "-12", "-17")
Pts = c(20, 15, 14, 4, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGE", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", "", "advance", "", "")

Team = c("Denmark", "Scotland", "Israel", "Austria", "Faroe Islands", "Moldova")
GD = c("+27", "+10", "+2", "+2", "-16", "-25")
Pts = c(27, 23, 16, 16, 4, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGF", categories, widths = widths), file_name, append = TRUE)

categories = c("wc", "advance", rep("", 4))

Team = c("Netherlands", "Turkey", "Norway", "Montenegro", "Latvia", "Gibraltar")
GD = c("+25", "+11", "+7", "-1", "-3", "-39")
Pts = c(23, 21, 18, 12, 9, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGG", categories, widths = widths), file_name, append = TRUE)

Team = c("Croatia", "Russia", "Slovakia", "Slovenia", "Cyprus", "Malta")
GD = c("+17", "+13", "+7", "+1", "-17", "-21")
Pts = c(23, 22, 14, 14, 5, 5)
write(create_node(cbind(Team, GD, Pts), "UEFAGH", categories, widths = widths), file_name, append = TRUE)

Team = c("England", "Poland", "Albania", "Hungary", "Andorra", "San Marino")
GD = c("+36", "+19", "0", "+6", "-16", "-45")
Pts = c(26, 20, 18, 17, 6, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGI", categories, widths = widths), file_name, append = TRUE)

Team = c("Germany", "North Macedonia", "Romania", "Armenia", "Iceland", "Liechtenstein")
GD = c("+32", "+12", "+5", "-11", "-6", "-32")
Pts = c(27, 18, 17, 12, 9, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGJ", categories, widths = widths), file_name, append = TRUE)

uefa_r2_teams = c("Portugal", "Scotland", "Italy", "Russia (DQ)", "Sweden", "Wales",
                  "Turkey", "Poland", "North Macedonia", "Ukraine")

widths = c(find_max_width(uefa_r2_teams, width_multiplier), rep(score_width, 3))

write(create_H2H("UEFAR2M1", "Scotland", "Ukraine", 1, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2M2", "Wales", "Austria", 2, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2M3", "Russia (DQ)", "Poland", "-", "-", FALSE, widths = widths, categories = c("dq", "advance")), file_name, append = TRUE)
write(create_H2H("UEFAR2M4", "Sweden", "Czechia", 1, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2M5", "Italy", "North Macedonia", 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_H2H("UEFAR2M6", "Portugal", "Turkey", 2, 0, TRUE, widths = widths), file_name, append = TRUE)

write(create_H2H("UEFAR3M1", "Wales", "Ukraine", 1, 0, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_H2H("UEFAR3M2", "Poland", "Sweden", 2, 0, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)
write(create_H2H("UEFAR3M3", "Portugal", "North Macedonia", 2, 0, TRUE, widths = widths, wc = TRUE), file_name, append = TRUE)


ccf_r1_teams = c("El Salvador", "Montserrat", "Antigua and Barbuda", "Grenada", "U.S. Virgin Islands",
                 "Canada", "Suriname", "Bermuda", "Aruba", "Cayman Islands",
                 "Curaçao", "Guatemala", "Cuba", "Saint Vincent and the Grenadines", "British Virgin Islands",
                 "Panama", "Dominican Republic", "Barbados", "Dominica", "Anguilla",
                 "Haiti", "Nicaragua", "Belize", "Turks and Caicos Islands", "Saint Lucia (W)",
                 "Saint Kitts and Nevis", "Trinidad and Tobago", "Puerto Rico", "Guyana", "Bahamas")

widths = c(find_max_width(ccf_r1_teams, width_multiplier), rep(score_width, 3))

categories = c(rep("advance", 1), rep("", 4))

Team = c("El Salvador", "Montserrat", "Antigua and Barbuda", "Grenada", "U.S. Virgin Islands")
GD = c("+12", "+5", "+1", "-3", "-15")
Pts = c(10, 8, 7, 3, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR1GA", categories, widths = widths), file_name, append = TRUE)

Team = c("Canada", "Suriname", "Bermuda", "Aruba", "Cayman Islands")
GD = c("+26", "+11", "-5", "-16", "-16")
Pts = c(12, 9, 4, 3, 1)
write(create_node(cbind(Team, GD, Pts), "CCFR1GB", categories, widths = widths), file_name, append = TRUE)

Team = c("Curaçao", "Guatemala", "Cuba", "Saint Vincent and the Grenadines", "British Virgin Islands")
GD = c("+14", "+14", "+4", "-13", "-19")
Pts = c(10, 10, 6, 3, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR1GC", categories, widths = widths), file_name, append = TRUE)

Team = c("Panama", "Dominican Republic", "Barbados", "Dominica", "Anguilla")
GD = c("+18", "+4", "0", "+1", "-23")
Pts = c(12, 7, 5, 4, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR1GD", categories, widths = widths), file_name, append = TRUE)

Team = c("Saint Kitts and Nevis", "Trinidad and Tobago", "Puerto Rico", "Guyana", "Bahamas")
GD = c("+6", "+5", "+8", "-4", "-15")
Pts = c(9, 8, 7, 3, 1)
write(create_node(cbind(Team, GD, Pts), "CCFR1GF", categories, widths = widths), file_name, append = TRUE)

categories = c(rep("advance", 1), rep("", 3), "wd")
Team = c("Haiti", "Nicaragua", "Belize", "Turks and Caicos Islands", "Saint Lucia (W)")
GD = c("+13", "+9", "0", "-22", "-")
Pts = c(9, 6, 3, 0, "-")
write(create_node(cbind(Team, GD, Pts), "CCFR1GE", categories, widths = widths), file_name, append = TRUE)

ccf_r2_teams = c("Saint Kitts and Nevis", "El Salvador",
                 "Haiti", "Canada",
                 "Panama", "Curaçao")
widths = c(find_max_width(ccf_r2_teams, width_multiplier), rep(two_leg_score_width, 2), score_width)

write(create_two_leg("CCFR2M1", "Saint Kitts and Nevis", "El Salvador", 0, 4, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M2", "Haiti", "Canada", 0, 1, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M3", "Panama", "Curaçao", 2, 1, 0, 0, TRUE, widths = widths), file_name, append = TRUE)

ccf_r3_teams = c("Canada", "Mexico", "USA", "Costa Rica", "Panama",
                 "Jamaica", "El Salvador", "Honduras")

Pts = c(28, 28, 25, 25, 21, 11, 10, 4)
Team = ccf_r3_teams
GD = c("+16", "+9", "+11", "+5", "-2", "-10", "-10", "-19")
categories = c(rep("wc", 3), "advance", rep("", 4))

write(create_node(cbind(Team, GD, Pts), "CCFRR", categories), file_name, append = TRUE)

ofc_teams = c("Tonga (W)",
              "Solomon Islands", "Tahiti", "Vanuatu (W)", "Cook Islands (W)",
              "New Zealand", "Papua New Guinea", "Fiji", "New Caledonia")

widths = c(find_max_width(ofc_teams, width_multiplier), gd_width, pts_width)
categories = c("wd", "advance")
write(create_H2H("OFCR1", "Tonga (W)", "Cook Islands", "-", "-", FALSE, widths, categories = categories), file_name, append = TRUE)

Team = c("Solomon Islands", "Tahiti", "Vanuatu (W)", "Cook Islands (W)")
GD = c("+2", "-2", "-", "-")
Pts = c(3, 0, "-", "-")
categories = c("advance", "advance", "wd", "wd")
write(create_node(cbind(Team, GD, Pts), "OFCR2GA", categories, widths = widths), file_name, append = TRUE)

Team = c("New Zealand", "Papua New Guinea", "Fiji", "New Caledonia")
GD = c("+11", "+1", "-4", "-8")
Pts = c(9, 6, 3, 0)
categories = c("advance", "advance", "", "")
write(create_node(cbind(Team, GD, Pts), "OFCR2GB", categories, widths = widths), file_name, append = TRUE)

write(create_H2H("OFCR3M1", "Solomon Islands", "Papua New Guinea", "3", "2", TRUE, widths), file_name, append = TRUE)
write(create_H2H("OFCR3M2", "New Zealand", "Tahiti", 1, 0, TRUE, widths), file_name, append = TRUE)
write(create_H2H("OFCR4M1", "Solomon Islands","New Zealand", 0, 5, FALSE, widths), file_name, append = TRUE)

file_end = '  graph [rankdir = "LR",
         newrank = "true"]

  #node[shape = rect]
  R16M1; R16M2; R16M3; R16M4; R16M5; R16M6; R16M7; R16M8;
  QF1; QF2; QF3; QF4;
  SF1; SF2; F;
  GroupA
  GroupB
  GroupC
  GroupD
  GroupE
  GroupF
  GroupG
  GroupH
  
  
  subgraph cluster_Conmebol{
    label = "Confederación 
Sudamericana
 de Fútbol"
 #   label = "CONMEBOL"
    fontsize = 50
    CMBRR;
  }
  edge [color = "#800000"]
 # CMBRR:e1 -> GroupG:w1
 # CMBRR:e2 -> GroupC:w1
 # CMBRR:e3 -> GroupH:w3
 # CMBRR:e4 -> GroupA:w3
  CMBRR:e5 -> ICF1:w2
edge [color = "black"]
  
  subgraph cluster_AFC{
label = "Asian Football Confederation"
#label = "AFC"
fontsize = 50
    {rank = same; AFCR1M1; AFCR1M2; AFCR1M3; AFCR1M4; AFCR1M5; AFCR1M6;}
    
    {rank = same; AFCR2GA; AFCR2GB; AFCR2GC; AFCR2GD; AFCR2GE; AFCR2GF; AFCR2GG; AFCR2GH}
    

    
    {rank = same; AFCR3GA; AFCR3GB;}
    
      edge[color = "#F58231"]
    AFCR1M1:e1 -> AFCR2GF:w4
    AFCR1M2:e2 -> AFCR2GH:w4
    AFCR1M3:e2 -> AFCR2GE:w5
    AFCR1M4:e1 -> AFCR2GG:w3
    AFCR1M5:e1 -> AFCR2GC:w5
    AFCR1M6:e2 -> AFCR2GA:w5
    
    AFCR2GA:e1 -> AFCR3GA:w5
    AFCR2GA:e2 -> AFCR3GB:w5
    AFCR2GB:e1 -> AFCR3GB:w3
    AFCR2GC:e1 -> AFCR3GA:w1
    AFCR2GC:e2 -> AFCR3GA:w4
    AFCR2GD:e1 -> AFCR3GB:w1

    AFCR2GE:e2 -> AFCR3GB:w4
    AFCR2GF:e1 -> AFCR3GB:w2
    AFCR2GG:e1 -> AFCR3GA:w3
    AFCR2GG:e2 -> AFCR3GB:w6
    AFCR2GH:e1 -> AFCR3GA:w2
    AFCR2GH:e2 -> AFCR3GA:w6
    
    AFCR3GA:e3 -> AFCR4:w1
    AFCR3GB:e3 -> AFCR4:w2
      edge[color = "black"]
  }
      edge[color = "#F58231"]
 # AFCR3GA:e1 -> GroupB:w3
 # AFCR3GA:e2 -> GroupH:w2
 # AFCR3GB:e1 -> GroupC:w4
 # AFCR3GB:e2 -> GroupE:w1
  AFCR4:e2 -> ICF1:w1
  
  subgraph cluster_CAF{
label = "Confederation of African Football"
#label = "CAF"
fontsize = 50
    {rank = same; CAFR1M1; CAFR1M2; CAFR1M3; CAFR1M4; CAFR1M5; CAFR1M6; CAFR1M7; CAFR1M8; CAFR1M9; CAFR1M10; CAFR1M11; CAFR1M12; CAFR1M13; }
    

    
    
    {rank = same; CAFR2GA; CAFR2GB; CAFR2GC; CAFR2GD; CAFR2GE; 
      CAFR2GF; CAFR2GG; CAFR2GH; CAFR2GI; CAFR2GJ;}
    
    {edge[style = invis;]
    CAFR2GH -> CAFR2GF;
    CAFR2GI -> CAFR2GJ;
    CAFR2GA -> CAFR2GD;
    CAFR2GC -> CAFR2GG;
    
    }
    

edge[color = "#3CB44B"]    
    CAFR1M1:e1 -> CAFR2GG:w3
    CAFR1M2:e2 -> CAFR2GG:w4
    CAFR1M3:e2 -> CAFR2GH:w3
    CAFR1M4:e2 -> CAFR2GJ:w3
    CAFR1M5:e1 -> CAFR2GA:w4
    CAFR1M6:e2 -> CAFR2GD:w4
    CAFR1M7:e2 -> CAFR2GF:w4
    CAFR1M8:e1 -> CAFR2GC:w3
    CAFR1M9:e2 -> CAFR2GD:w3
    CAFR1M10:e2 -> CAFR2GI:w2
    CAFR1M11:e2 -> CAFR2GB:w2
    CAFR1M12:e2 -> CAFR2GH:w2
    CAFR1M13:e2 -> CAFR2GI:w4
    CAFR1M14:e2 -> CAFR2GE:w4

    CAFR2GA:e1 -> CAFR3M2:w2
    CAFR2GB:e1 -> CAFR3M5:w2
    CAFR2GC:e1 -> CAFR3M3:w2
    CAFR2GD:e1 -> CAFR3M2:w1
    CAFR2GE:e1 -> CAFR3M5:w1
    CAFR2GF:e1 -> CAFR3M1:w1
    CAFR2GG:e1 -> CAFR3M3:w1
    CAFR2GH:e1 -> CAFR3M1:w2
    CAFR2GI:e1 -> CAFR3M4:w2
    CAFR2GJ:e1 -> CAFR3M4:w1
    edge[color = "black"]
  }
edge[color = "#3CB44B"]    
#  CAFR3M1:e2 -> GroupA:w2
#  CAFR3M2:e1 -> GroupG:w3
#  CAFR3M3:e1 -> GroupH:w4
#  CAFR3M4:e2 -> GroupF:w1
#  CAFR3M5:e2 -> GroupD:w3
edge[color = "black"]
  
  subgraph cluster_OFC{
label = "Oceania Football Confederation"
#label = "OFC"
    fontsize = 50
    {rank = same; OFCR2GA; OFCR2GB}
    {rank = same; OFCR3M1; OFCR3M2}
    edge[color = "#0082c8"]
    OFCR1:e2 -> OFCR2GA:w4
    OFCR2GA:e1 -> OFCR3M1:w1
    OFCR2GA:e2 -> OFCR3M2:w2
    OFCR2GB:e1 -> OFCR3M2:w1
    OFCR2GB:e2 -> OFCR3M1:w2
    OFCR3M1:e1 -> OFCR4M1:w1
    OFCR3M2:e1 -> OFCR4M1:w2
    edge[color = "black"]
  }
  
  subgraph cluster_UEFA{
label = "Union of European Football Associations"
#label = "UEFA"
fontsize = 50
    {rank = same; UEFAGA; UEFAGB; UEFAGC; UEFAGD; UEFAGE; UEFAGF; UEFAGG; UEFAGH; UEFAGI; UEFAGJ}
    {rank = same; UEFAR2M1; UEFAR2M2; UEFAR2M3; UEFAR2M4; UEFAR2M5; UEFAR2M6}
    {rank = same; UEFAR3M1; UEFAR3M2; UEFAR3M3}
    
    
    {
        edge[style = invis;]
        UEFAR2M5 -> UEFAR2M6
    }

  edge[ color = "black"] 
    UEFAGA:e2 -> UEFAR2M6:w1
    UEFAGB:e2 -> UEFAR2M4:w1
    UEFAGC:e2 -> UEFAR2M5:w1
    UEFAGD:e2 -> UEFAR2M1:w2
    UEFAGE:e2 -> UEFAR2M2:w1
    UEFAGE:e3 -> UEFAR2M4:w2
    UEFAGF:e2 -> UEFAR2M1:w1
    UEFAGF:e4 -> UEFAR2M2:w2
    UEFAGG:e2 -> UEFAR2M6:w2
    UEFAGH:e2 -> UEFAR2M3:w1
    UEFAGI:e2 -> UEFAR2M3:w2
    UEFAGJ:e2 -> UEFAR2M5:w2
    
    UEFAR2M1:e2 -> UEFAR3M1:w2
    UEFAR2M2:e1 -> UEFAR3M1:w1
    UEFAR2M3:e2 -> UEFAR3M2:w1
    UEFAR2M4:e1 -> UEFAR3M2:w2
    UEFAR2M5:e2 -> UEFAR3M3:w2
    UEFAR2M6:e1 -> UEFAR3M3:w1
    
   edge[color = "black"] 
  }
  edge[ color = "black"]  
  
  subgraph cluster_Concacaf{
label = "The Confederation of North,
Central American and Caribbean
Association Football"
#label = "CONCACAF"
fontsize = 50
    {rank = same; CCFR1GA; CCFR1GB; CCFR1GC; CCFR1GD; CCFR1GE; CCFR1GF;}



    {rank = same; CCFR2M1; CCFR2M2; CCFR2M3;}
    
        {edge[style = invis;]
    CCFR2M1 -> CCFR2M3;
    
    }


    edge[ color = "#000080"]
    CCFR1GA:e1 -> CCFR2M1:w2
    CCFR1GB:e1 -> CCFR2M2:w2
    CCFR1GC:e1 -> CCFR2M3:w2
    CCFR1GD:e1 -> CCFR2M3:w1
    CCFR1GE:e1 -> CCFR2M2:w1
    CCFR1GF:e1 -> CCFR2M1:w1
    
    CCFR2M1:e1 -> CCFRR:w7
    CCFR2M2:e2 -> CCFRR:w1
    CCFR2M3:e1 -> CCFRR:w5
    edge[color = "black"]
    
  }
  edge[color = "#000080"]
  CCFRR:e4 -> ICF2:w1
  edge[color = "black"]
  OFCR4M1:e2 -> ICF2:w2 [color = "#0082c8"]
  
  node[style = invis;]
  edge[style = invis;]
  in1 -> in2 -> in3 -> in4 -> in5
  node[style = ""]
  edge[style = ""]
  
  {rank = same; ICF1; ICF2; in3}
  {rank = same; GroupA; GroupB; GroupC; GroupD; GroupE; GroupF; GroupG; GroupH; in5}
  {rank = same; GroupA -> GroupB -> GroupC -> GroupD -> GroupE -> GroupF -> GroupG -> GroupH [style = invis]; in5}

  
  UEFAR3M2 -> AFCR1M3 [style = "invis"]
 
subgraph cluster_wcF {
    label = "World Cup Finals"
    fontsize = 50
  GroupA:e1 -> R16M1:w1
  GroupA:e2 -> R16M5:w2 [color = "#3CB44B"]
  GroupB:e1 -> R16M5:w1
  GroupB:e2 -> R16M1:w2 [color = "#000080"]
  GroupC:e1 -> R16M2:w1 [color = "#800000"]
  GroupC:e2 -> R16M6:w2
  GroupD:e1 -> R16M6:w1
  GroupD:e2 -> R16M2:w2 [color = "#f58231"]
  GroupE:e1 -> R16M3:w1 [color = "#f58231"]
  GroupE:e2 -> R16M7:w2
  GroupF:e1 -> R16M7:w1 [color = "#3CB44B"]
  GroupF:e2 -> R16M3:w2
  GroupG:e1 -> R16M4:w1 [color = "#800000"]
  GroupG:e2 -> R16M8:w2
  GroupH:e1 -> R16M8:w1
  GroupH:e2 -> R16M4:w2 [color = "#f58231"]
  
  R16M1:e1 -> QF1:w1
  R16M2:e1 -> QF1:w2 [color = "#800000"]
  R16M3:e2 -> QF2:w1
  R16M4:e1 -> QF2:w2 [color = "#800000"]
  R16M5:e1 -> QF3:w1
  R16M6:e1 -> QF3:w2
  R16M7:e1 -> QF4:w1 [color = "#3CB44B"]
  R16M8:e1 -> QF4:w2
  QF1:e2 -> SF1:w1 [color = "#800000"]
  QF2:e1 -> SF1:w2
  QF3:e2 -> SF2:w1
  QF4:e1 -> SF2:w2 [color = "#3CB44B"]
  SF1:e1 -> F:w1
  SF2:e1 -> F:w2
  SF1:e2 -> P3:w1
  SF2:e2 -> P3:w2
  {
    rank = same; 
    edge[style = "invis"]
    GroupH -> GroupG -> GroupF -> GroupE -> GroupD -> GroupC -> GroupB -> GroupA
  }
  {
    rank = same; 
    edge[style = "invis"]
    R16M8 -> R16M4 
    R16M7 -> R16M3
    R16M6 -> R16M2 
    R16M5 -> R16M1
  }
  {
    rank = same; 
    edge[style = "invis"]
    QF4 -> QF2
    QF3 -> QF1
  }
  {
    rank = same;
    edge[style = "invis"]
    SF2 -> SF1
  }
}

graph [label = "2022 FIFA World Cup:\\n206 Countries\\n929 Matches\\n1 Champion\\nCreated by Jed Grabman",
         labelloc = "t",
fontsize = 200]
}'
write(file_end, file_name, append = TRUE)

