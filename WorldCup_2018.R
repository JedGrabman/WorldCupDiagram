create_html_row = function(data, color = "", widths = rep(0, length(data)), port = ""){
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
    words = grepl("[a-z]", data[i])
    if (words){
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
    rets[i] = paste0(open_tds[i], data[i], "</td>")
  }
  ret = paste0(unlist(rets), collapse = "")
  #data_elements = paste(unlist(lapply(data, function(x) paste0(open_td, x, "</td>"))), collapse = "")
  return (paste0("<tr>", ret, "</tr>\n"))
}


create_table = function(dtf, colors = rep(FALSE, nrow(dtf)), color_name = "#BBF3BB", widths = rep(0, ncol(dtf))){
  table_atts = "label = <<table border = '0' cellborder = '1' cellspacing = '0'>\n"
  rows = rep("", nrow(dtf)+1)
  rows[1] = create_html_row(colnames(dtf), widths = widths)
  for (i in c(1:nrow(dtf))){
    if (colors[i]){
      rows[i+1] = create_html_row(dtf[i,], color_name, port = i)
    } else {
      rows[i+1] = create_html_row(dtf[i,], port = i)
    }
  }
  table_end = "</table>>"
  row_string = paste(rows, collapse = "")
  
  
  return(paste0(table_atts,row_string, table_end))
}

create_node = function(dtf, node_name, colors, color_name = "#BBF3BB", widths = rep(0, ncol(dtf))){
  return(paste0(node_name, "[shape = plaintext,\n", create_table(dtf, colors, color_name, widths), "]\n"))
}

create_group = function(Teams, GD, Pts, group_let, widths = rep(0, 3)){
  dtf = cbind(Teams, GD, Pts)
  group_name = paste0("Group", group_let)
  colors = c(TRUE, TRUE, FALSE, FALSE)
  return(create_node(dtf, group_name, colors, color_name = "#BBF3BB", widths))
}

create_H2H = function(name, Team1, Team2, Score1, Score2, team1Advance, widths = rep(0,2)){
  Team = c(Team1, Team2)
  Score = c(Score1, Score2)
  dtf = cbind(Team, Score)
  if (team1Advance){
    colors = c(TRUE, FALSE)
  } else {
    colors = c(FALSE, TRUE)
  }
  create_node(dtf, name, colors, widths = widths)
}

create_two_leg = function(name, team1, team2, score11, score21, score12, score22, team1Advance, widths = rep(0,4)){
  Team = c(team1, team2)
  Game1 = c(score11, score21)
  Game2 = c(score12, score22)
  Total = c(score11 + score12, score21 + score22)
  dtf = cbind(Team, Game1, Game2, Total)
  colnames(dtf) = c("Team", "M1", "M2", "Total")
  if (team1Advance){
    colors = c(TRUE, FALSE)
  } else {
    colors = c(FALSE, TRUE)
  }
  create_node(dtf, name, colors, widths = widths)
}

find_max_width = function(values){
  return(max(sapply(values, function(x) strwidth(x))))
}

file_name = "WorldCup_2022.txt"

file_start = "digraph g{
edge [penwidth = 2]"
write(file_start, file_name)


width_multiplier = 340/72
plot.new()
gd_width = strwidth("+77") * width_multiplier
pts_width = strwidth(77) * width_multiplier
score_width = strwidth("1 (3)") * width_multiplier
two_leg_score_width = strwidth(1) * width_multiplier

Pos = c(1:10)
Team = c("Brazil", "Uruguay", "Argentina", "Colmbia",
         "Peru", "Chile", "Paraguay", "Ecuador",
         "Bolivia", "Venezuela")
Pts = c(41, 31, 28, 27, 26, 26, 24, 20, 14, 12)
colors = c(rep(TRUE, 5), rep(FALSE, 5))

write(create_node(cbind(Pos, Team, Pts), "CMBRR", colors), file_name, append = TRUE)
            
wc_final_teams = c("Uruguay", "Russia", "Saudi Arabia", "Egypt",
                   "Spain", "Portugal", "Iran", "Morocco",
                    "France", "Denmark", "Peru", "Australia",
                    "Croatia", "Argentina", "Nigeria", "Iceland",
                    "Brazil", "Switzerland", "Serbia", "Costa Rica",
                    "Sweden", "Mexico", "South Korea", "Germany",
                    "Belgium", "England", "Tunisia", "Panama",
                    "Colombia", "Japan", "Senegal", "Poland") 


team_width = find_max_width(wc_final_teams) * width_multiplier

widths = c(team_width, gd_width, pts_width)

Teams = c("Uruguay", "Russia", "Saudi Arabia", "Egypt")
GD = c("+4", "+5", "-5", "-4")
Pts = c(9,6,3,0)
write(create_group(Teams, GD, Pts, "A", widths), file_name, append = TRUE)

Teams = c("Spain", "Portugal", "Iran", "Morocco")
GD = c("+1", "+1", "0", "-2")
Pts = c(5,5,4,1)
write(create_group(Teams, GD, Pts, "B", widths), file_name, append = TRUE)

Teams = c("France", "Denmark", "Peru", "Australia")
GD = c("+2", "+1", "0", "-3")
Pts = c(7,5,3,1)
write(create_group(Teams, GD, Pts, "C", widths), file_name, append = TRUE)

Teams = c("Croatia", "Argentina", "Nigeria", "Iceland")
GD = c("+6", "-2", "-1", "-3")
Pts = c(9,4,3,1)
write(create_group(Teams, GD, Pts, "D", widths), file_name, append = TRUE)

Teams = c("Brazil", "Switzerland", "Serbia", "Costa Rica")
GD = c("+4", "+1", "-2", "-3")
Pts = c(7,5,3,1)
write(create_group(Teams, GD, Pts, "E", widths), file_name, append = TRUE)

Teams = c("Sweden", "Mexico", "South Korea", "Germany")
GD = c("+3", "-1", "0", "-2")
Pts = c(6,6,3,3)
write(create_group(Teams, GD, Pts, "F", widths), file_name, append = TRUE)

Teams = c("Belgium", "England", "Tunisia", "Panama")
GD = c("+7", "+5", "-3", "-9")
Pts = c(9,6,3,0)
write(create_group(Teams, GD, Pts, "G", widths), file_name, append = TRUE)

Teams = c("Colombia", "Japan", "Senegal", "Poland")
GD = c("+3", "0", "0", "-3")
Pts = c(6,4,4,3)
write(create_group(Teams, GD, Pts, "H", widths), file_name, append = TRUE)

knockout_teams = wc_final_teams[c(TRUE, TRUE, FALSE, FALSE)]
ko_team_width = find_max_width(knockout_teams) * width_multiplier
ko_widths = c(ko_team_width, score_width)

write(create_H2H("R16M1", "Uruguay", "Portugal", 2, 1, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M2", "France", "Argentina", 4, 3, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M3", "Brazil", "Mexico", 2, 0, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M4", "Belgium", "Japan", 3, 2, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M5", "Spain", "Russia", "1 (3)", "1 (4)", FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M6", "Croatia", "Denmark", "1 (3)", "1 (2)", TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M7", "Sweden", "Switzerland", 1, 0, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("R16M8", "Colombia", "England", "1 (3)", "1 (4)", FALSE, ko_widths), file_name, append = TRUE)

write(create_H2H("QF1", "Uruguay", "France", 0, 2, FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("QF2", "Brazil", "Belgium", 1, 2, FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("QF3", "Russia", "Croatia", "2 (3)", "2 (4)", FALSE, ko_widths), file_name, append = TRUE)
write(create_H2H("QF4", "Sweden", "England", 0, 2, FALSE, ko_widths), file_name, append = TRUE)

write(create_H2H("SF1", "France", "Belgium", 1, 0, TRUE, ko_widths), file_name, append = TRUE)
write(create_H2H("SF2", "Croatia", "England", 2, 1, TRUE, ko_widths), file_name, append = TRUE)

write(create_H2H("F", "France", "Croatia", 2, 1, TRUE, ko_widths), file_name, append = TRUE)

afc_r1_teams =  c("India", "Nepal",
                  "Yemen", "Pakistan",
                  "Timor-Leste (DQ)", 
                  "Cambodia", "Macau",
                  "Chinese Taipei", "Brunei",
                  "Sri Lanka", "Bhutan")
widths = c(find_max_width(afc_r1_teams) * width_multiplier, rep(two_leg_score_width, 3))

write(create_two_leg("AFCR1M1", "India", "Nepal", 2, 0, 0, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M2", "Yemen", "Pakistan", 3, 1, 0, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M3", "Timor-Leste (DQ)", "Mongolia", 0, 3, 0, 3, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M4", "Cambodia", "Macau", 3, 0, 1, 1, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M5", "Chinese Taipei", "Brunei", 0, 1, 2, 0, TRUE, widths), file_name, append = TRUE)
write(create_two_leg("AFCR1M6", "Sri Lanka", "Bhutan", 0, 1, 1, 2, FALSE, widths), file_name, append = TRUE)

colors = c(TRUE, TRUE, FALSE, FALSE, FALSE)

afc_group_teams = c("Saudi Arabia", "United Arab Emirates", "Palestine", "Malaysia", "Timor-Leste",
                    "Qatar", "China", "Hong Kong", "Maldives", "Bhutan",
                    "Japan", "Syria", "Singapore", "Afghanistan", "Cambodia",
                    "Thailand","Iraq", "Vietnam", "Chinese Taipei", "Indonesia (DQ)",
                    "Australia","Jordan","Kyrgyzstan","Tajikistan","Bangladesh",
                    "Iran", "Oman", "Turkmenistan", "Guam", "India",
                    "South Korea", "Lebanon", "Kuwait", "Myanmar", "Laos",
                    "Uzbekistan", "North Korea", "Philippines", "Bahrain", "Yemen")

team_width = find_max_width(afc_group_teams) *width_multiplier
widths = c(team_width, gd_width, pts_width)                    
                    
Team = c("Saudi Arabia", "United Arab Emirates", "Palestine", "Malaysia", "Timor-Leste")
GD = c("+24", "+23", "+19", "-22", "-44")
Pts = c(20, 17, 14, 6, 0)

write(create_node(cbind(Team, GD, Pts), "AFCR2GA", colors, widths = widths), file_name, append = TRUE)

Team = c("Qatar", "China", "Hong Kong", "Maldives", "Bhutan")
GD = c("+25", "+26", "+8", "-12", "-47")
Pts = c(21, 17, 14, 6, 0)

write(create_node(cbind(Team, GD, Pts), "AFCR2GC", colors, widths = widths), file_name, append = TRUE)

Team = c("Japan", "Syria", "Singapore", "Afghanistan", "Cambodia")
GD = c("+27", "+15", "0", "-16", "-26")
Pts = c(22, 18, 10, 9, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR2GE", colors, widths = widths), file_name, append = TRUE)

Team = c("Thailand","Iraq", "Vietnam", "Chinese Taipei", "Indonesia (DQ)")
GD = c("+8", "+7", "-1", "-14", "0")
Pts = c(14, 12, 7, 0, 0)
write(create_node(cbind(Team, GD, Pts), "AFCR2GF", colors, widths = widths), file_name, append = TRUE)

colors = c(TRUE, FALSE, FALSE, FALSE, FALSE)

Team = c("Australia","Jordan","Kyrgyzstan","Tajikistan","Bangladesh")
GD = c("+25","+14", "+2", "-11", "-30")
Pts = c(21, 16,14, 5, 1)
write(create_node(cbind(Team, GD, Pts), "AFCR2GB", colors, widths = widths), file_name, append = TRUE)

Team = c("Iran", "Oman", "Turkmenistan", "Guam", "India")
GD = c("+23","+4", "-1", "-13", "-13")
Pts = c(20, 14,13, 7, 3)
write(create_node(cbind(Team, GD, Pts), "AFCR2GD", colors, widths = widths), file_name, append = TRUE)

Team = c("South Korea", "Lebanon", "Kuwait", "Myanmar", "Laos")
GD = c("+27","+6", "+2", "-12", "-23")
Pts = c(24, 11,10, 8, 4)
write(create_node(cbind(Team, GD, Pts), "AFCR2GG", colors, widths = widths), file_name, append = TRUE)

Team = c("Uzbekistan", "North Korea", "Philippines", "Bahrain", "Yemen")
GD = c("+13","+6", "-4", "0","-15")
Pts = c(21, 16,10, 9, 3)
write(create_node(cbind(Team, GD, Pts), "AFCR2GH", colors, widths = widths), file_name, append = TRUE)

colors = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
Team = c("Iran", "South Korea", "Syria", "Uzbekistan", "China", "Qatar")
GD = c("+8","+1", "+1", "-1", "-2", "-7")
Pts = c(22, 15, 13, 13, 12, 7)
write(create_node(cbind(Team, GD, Pts), "AFCR3GA", colors, widths = widths), file_name, append = TRUE)

colors = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
Team = c("Japan", "Saudi Arabia", "Australia", "United Arab Emirates", "Iraq", "Thailand")
GD = c("+10","+7", "+5", "-3", "-1", "-18")
Pts = c(20, 19, 19, 13, 11, 2)
write(create_node(cbind(Team, GD, Pts), "AFCR3GB", colors, widths = widths), file_name, append = TRUE)

write(create_two_leg("AFCR4", "Syria", "Australia", 1, 1, 1, 2, FALSE), file_name, append = TRUE)

ofc_teams = c("Samoa", "American Samoa", "Cook Islands", "Tonga",
              "Papua New Guinea", "New Caledonia", "Tahiti",
              "New Zealand", "Solomon Islands", "Fiji", "Vanuatu")

widths = c(find_max_width(ofc_teams) * width_multiplier, gd_width, pts_width)
Team = c("Samoa", "American Samoa", "Cook Islands", "Tonga")
GD = c("+3", "+2", "+2", "-7")
Pts = c(6, 6, 6, 0)
write(create_node(cbind(Team, GD, Pts), "OFCR1", c(TRUE, FALSE, FALSE, FALSE), widths = widths), file_name, append = TRUE)

Team = c("Papua New Guinea", "New Caledonia", "Tahiti", "Samoa")
GD = c("+8", "+7", "+4", "-19")
Pts = c(5, 5, 5, 0)
write(create_node(cbind(Team, GD, Pts), "OFCR2GA", c(TRUE, TRUE, FALSE, FALSE), widths = widths), file_name, append = TRUE)

Team = c("New Zealand", "Solomon Islands", "Fiji", "Vanuatu")
GD = c("+8", "-1", "-2", "-5")
Pts = c(9, 3, 3, 3)
write(create_node(cbind(Team, GD, Pts), "OFCR2GB", c(TRUE, TRUE, FALSE, FALSE), widths = widths), file_name, append = TRUE)

Team = c("New Zealand", "New Caledonia", "Fiji")
GD = c("+6", "-1", "-5")
Pts = c(10, 5, 1)
write(create_node(cbind(Team, GD, Pts), "OFCR3GA", c(TRUE, FALSE, FALSE), widths = widths), file_name, append = TRUE)

Team = c("Solomon Islands", "Tahiti", "Papua New Guinea")
GD = c("0", "+3", "-3")
Pts = c(9, 6, 3)
write(create_node(cbind(Team, GD, Pts), "OFCR3GB", c(TRUE, FALSE, FALSE), widths = widths), file_name, append = TRUE)

widths = c(find_max_width(c("New Zealand", "Solomon Islands")) * width_multiplier, rep(score_width,3))
write(create_two_leg("OFCR4", "New Zealand", "Solomon Islands", 6, 1, 2, 2, TRUE), file_name, append = TRUE)

caf_r1_teams = c("Somalia", "Niger",
                 "South Sudan", "Mauritania",
                 "Gambia", "Namibia",
                 "Sao Tome and Principe", "Ethiopia",
                 "Chad", "Sierra Leonne",
                 "Comoros", "Lesotho",
                 "Djibouti", "Swaziland",
                 "Eritrea", "Botswana",
                 "Seychelles", "Burundi",
                 "Liberia", "Guinea-Bissau",
                 "Central African Republic", "Madagascar",
                 "Maurtius", "Kenya",
                 "Tanzania", "Malawi")

widths = c(find_max_width(caf_r1_teams) * width_multiplier, rep(score_width,3))

write(create_two_leg("CAFR1M1", "Somalia", "Niger", 0, 2, 0, 4, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M2", "South Sudan", "Mauritania", 1, 1, 0, 4, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M3", "Gambia", "Namibia", 1, 1, 1, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M4", "Sao Tome and Principe", "Ethiopia", 1, 0, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M5", "Chad", "Sierra Leonne", 1, 0, 1, 2, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M6", "Comoros", "Lesotho", 0, 0, 1, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M7", "Djibouti", "Swaziland", 0, 6, 1, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M8", "Eritrea", "Botswana", 0, 2, 1, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M9", "Seychelles", "Burundi", 0, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M10", "Liberia", "Guinea-Bissau", 1, 1, 3, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M11", "Central African Republic", "Madagascar", 0, 3, 2, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M12", "Maurtius", "Kenya", 2, 5, 0, 0, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR1M13", "Tanzania", "Malawi", 2, 0, 0, 1, TRUE, widths = widths), file_name, append = TRUE)


caf_r2_teams = c("Niger", "Cameroon",
                 "Mauritania", "Tunisia",
                 "Namibia", "Gunea",
                 "Ethiopia", "Congo",
                 "Chad", "Egypt",
                 "Comoros", "Ghana",
                 "Swaziland", "Nigeria",
                 "Botswana", "Mali",
                 "Burundi", "DR Congo",
                 "Liberia", "Ivory Coast",
                 "Madagascar", "Senegal",
                 "Kenya", "Cape Verde",
                 "Tanzania", "Algeria",
                 "Sudan", "Zambia",
                 "Libya", "Rwanda",
                 "Morocco", "Equatorial Gunea",
                 "Mazombique", "Gabon",
                 "Benin", "Burkina Faso",
                 "Togo", "Uganda",
                 "Angola", "South Africa")

write(create_two_leg("CAFR2M1", "Niger", "Cameroon", 0, 3, 0, 0, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M2", "Mauritania", "Tunisia", 1, 2, 1, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M3", "Namibia", "Gunea", 0, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M4", "Ethiopia", "Congo", 3, 4, 1, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M5", "Chad", "Egypt", 1, 0, 0, 4, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M6", "Comoros", "Ghana", 0, 0, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M7", "Swaziland", "Nigeria", 0, 0, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M8", "Botswana", "Mali", 2, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M9", "Burundi", "DR Congo", 2, 3, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M10", "Liberia", "Ivory Coast", 0, 1, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M11", "Madagascar", "Senegal", 2, 2, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M12", "Kenya", "Cape Verde", 1, 0, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M13", "Tanzania", "Algeria", 2, 2, 0, 7, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M14", "Sudan", "Zambia", 0, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M15", "Libya", "Rwanda", 1, 0, 3, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M16", "Morocco", "Equatorial Gunea", 2, 0, 0, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M17", "Mazombique", "Gabon", 1, 0, 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M18", "Benin", "Burkina Faso", 2, 1, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M19", "Togo", "Uganda", 0, 1, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CAFR2M20", "Angola", "South Africa", 1, 3, 0, 1, FALSE, widths = widths), file_name, append = TRUE)

colors = c(TRUE, FALSE, FALSE, FALSE)
widths = c(find_max_width(caf_r2_teams) * width_multiplier, gd_width, pts_width)


Team = c("Tunisia", "DR Congo", "Libya", "Guinea")
GD = c("+7", "+7", "-6", "-8")
Pts = c(14, 13, 4, 3)
write(create_node(cbind(Team, GD, Pts), "CAFR3GA", colors, widths = widths), file_name, append = TRUE)

Team = c("Nigeria", "Zambia", "Cameroon", "Algeria")
GD = c("+5", "+1", "-2", "-4")
Pts = c(13, 8, 7, 4)

write(create_node(cbind(Team, GD, Pts), "CAFR3GB", colors, widths = widths), file_name, append = TRUE)

Team = c("Morocco", "Ivory Coast", "Gabon", "Mali")
GD = c("+11", "+2", "-5", "-8")
Pts = c(12, 8, 6, 4)
write(create_node(cbind(Team, GD, Pts), "CAFR3GC", colors), file_name, append = TRUE)

Team = c("Senegal", "Burkina Faso", "Cape Verde", "South Africa")
GD = c("+7", "+4", "-8", "-3")
Pts = c(14, 9, 6, 4)
write(create_node(cbind(Team, GD, Pts), "CAFR3GD", colors, widths = widths), file_name, append = TRUE)

Team = c("Egypt", "Uganda", "Ghana", "Congo")
GD = c("+4", "+1", "+2", "-7")
Pts = c(13, 9,7, 2)
write(create_node(cbind(Team, GD, Pts), "CAFR3GE", colors, widths = widths), file_name, append = TRUE)

icf_teams = c("Honduras", "Australia", "New Zealand", "Peru")
widths = c(find_max_width(icf_teams) * width_multiplier, rep(score_width, 3))

write(create_two_leg("ICF1", "Honduras", "Australia", 0, 0, 1, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("ICF2", "New Zealand", "Peru", 0, 0, 0, 2, FALSE, widths = widths), file_name, append = TRUE)

colors = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)

europe_teams = c("France", "Sweden", "Netherlands", "Bulgaria", "Luxembourg", "Belarus",
                 "Portugal", "Switzerland", "Hungary", "Faroe Islands", "Latvia", "Andorra",
                 "Germany", "Northern Ireland", "Czech Republic", "Norway", "Azerbaijan", "San Marino",
                 "Serbia", "Ireland", "Wales", "Austria", "Georgia", "Moldova",
                 "Spain", "Italy", "Albania", "Israel", "Macedonia", "Liechtenstein",
                 "England", "Slovakia", "Scotland", "Slovenia", "Lithuania", "Malta",
                 "Belgium", "Greece", "Bosnia and Herzegovina", "Estonia", "Cyprus", "Gibraltar",
                 "Iceland", "Croatia", "Ukraine", "Turkey", "Finland", "Kosovo",
                 "Poland", "Denmark", "Montenegro", "Romania", "Armenia", "Kazakhstan")

team_width = find_max_width(europe_teams) * width_multiplier
widths = c(team_width, gd_width, pts_width)
widths = round(widths)



Team = c("France", "Sweden", "Netherlands", "Bulgaria", "Luxembourg", "Belarus")
GD = c("+12", "+17", "+9", "-5", "-18", "-15")
Pts = c(23, 19, 19, 13, 6, 5)
write(create_node(cbind(Team, GD, Pts), "UEFAGA", colors, widths = widths), file_name, append = TRUE)

Team = c("Portugal", "Switzerland", "Hungary", "Faroe Islands", "Latvia", "Andorra")
GD = c("+28", "+16", "0", "-12", "-11", "-21")
Pts = c(27, 27, 13, 9, 7, 4)
write(create_node(cbind(Team, GD, Pts), "UEFAGB", colors, widths = widths), file_name, append = TRUE)

Team = c("Germany", "Northern Ireland", "Czech Republic", "Norway", "Azerbaijan", "San Marino")
GD = c("+39", "+11", "+7", "+1", "-9", "-49")
Pts = c(30, 19, 15, 13, 10, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGC", colors, widths = widths), file_name, append = TRUE)

Team = c("Serbia", "Ireland", "Wales", "Austria", "Georgia", "Moldova")
GD = c("+10", "+6", "+7", "+2", "-6", "-19")
Pts = c(21, 19, 17, 15, 5, 2)
write(create_node(cbind(Team, GD, Pts), "UEFAGD", colors, widths = widths), file_name, append = TRUE)

Team = c("Poland", "Denmark", "Montenegro", "Romania", "Armenia", "Kazakhstan")
GD = c("+14", "+12", "+8", "+2", "-16", "-20")
Pts = c(25, 20, 16, 13, 7, 3)
write(create_node(cbind(Team, GD, Pts), "UEFAGE", colors, widths = widths), file_name, append = TRUE)

Team = c("Spain", "Italy", "Albania", "Israel", "Macedonia", "Liechtenstein")
GD = c("+33", "+13", "-3", "-5", "0", "-38")
Pts = c(28, 23, 13, 12, 11, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGG", colors, widths = widths), file_name, append = TRUE)

Team = c("Belgium", "Greece", "Bosnia and Herzegovina", "Estonia", "Cyprus", "Gibraltar")
GD = c("+37", "+11", "+11", "-6", "-9", "-44")
Pts = c(28, 19, 17, 11, 10, 0)
write(create_node(cbind(Team, GD, Pts), "UEFAGH", colors, widths = widths), file_name, append = TRUE)

Team = c("Iceland", "Croatia", "Ukraine", "Turkey", "Finland", "Kosovo")
GD = c("+9", "+11", "+4", "+1", "-4", "-21")
Pts = c(22, 20, 17, 15, 9, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGI", colors, widths = widths), file_name, append = TRUE)

colors = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
Team = c("England", "Slovakia", "Scotland", "Slovenia", "Lithuania", "Malta")
GD = c("+15", "+10", "+5", "+5", "-13", "-22")
Pts = c(26, 18, 18, 15, 6, 1)
write(create_node(cbind(Team, GD, Pts), "UEFAGF", colors, widths = widths), file_name, append = TRUE)

uefap_teams = c("Northern Ireland", "Switzerland",
                "Croatia", "Greece",
                "Denmark", "Ireland",
                "Sweden", "Italy")

widths = c(find_max_width(uefap_teams) * width_multiplier, rep(score_width, 3))

write(create_two_leg("UEFAP1", "Northern Ireland", "Switzerland", 0, 1, 0, 0, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("UEFAP2", "Croatia", "Greece", 4, 1, 0, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("UEFAP3", "Denmark", "Ireland", 0, 0, 5, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("UEFAP4", "Sweden", "Italy", 1, 0, 0, 0, TRUE, widths = widths), file_name, append = TRUE)

ccf_r1_teams = c("Bahamas", "Bermuda", 
                 "British Virgin Islands", "Dominica",
                 "Barbados", "U.S. Virgin Islands",
                 "Saint Kitts and Nevis", "Turks and Caicos Islands",
                 "Nicaragua", "Anguilla",
                 "Belize", "Cayman Islands",
                 "Curacao", "Montserrat")
widths = c(find_max_width(ccf_r1_teams) * width_multiplier, rep(score_width, 3))


write(create_two_leg("CCFR1M1" , "Bahamas", "Bermuda", 0, 5, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR1M2" , "British Virgin Islands", "Dominica", 2, 3, 0, 0, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR1M3" , "Barbados", "U.S. Virgin Islands", 0, 1, 4, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR1M4" , "Saint Kitts and Nevis", "Turks and Caicos Islands", 6, 2, 6, 2, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR1M5" , "Nicaragua", "Anguilla", 5, 0, 3, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR1M6" , "Belize", "Cayman Islands", 0, 0, 1, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR1M7" , "Curacao", "Montserrat", 2, 1, 2, 2, TRUE, widths = widths), file_name, append = TRUE)

widths = c(strwidth("Saint Vincent and the Grenadines") * width_multiplier, rep(score_width, 3))

write(create_two_leg("CCFR2M1", "Saint Vincent and the Grenadines", "Guyana", 2, 2, 4, 4, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M2", "Antigua and Barbuda", "Saint Lucia", 1, 3, 4, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M3", "Puerto Rico", "Grenada", 1, 0, 0, 2, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M4", "Dominica", "Canada", 0, 2, 0, 4, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M5", "Dominican Republic", "Belize", 1, 2, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M6", "Guatemala", "Bermuda", 0, 0, 1, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M7", "Aruba", "Barbados", 0, 2, 3, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M8", "Saint Kitts and Nevis", "El Salvador", 2, 2, 1, 4, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M9", "Curacao", "Cuba", 0, 0, 1, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR2M10", "Nicaragua", "Suriname", 1, 0, 3, 1, TRUE, widths = widths), file_name, append = TRUE)

write(create_two_leg("CCFR3M1", "Curacao", "El Salvador", 0, 1, 0, 1, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR3M2", "Canada", "Belize", 3, 0, 1, 1, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR3M3", "Grenada", "Haiti", 1, 3, 0, 3, FALSE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR3M4", "Jamaica", "Nicaragua", 2, 3, 2, 0, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR3M5", "Saint Vincent and the Grenadines", "Aruba", 2, 0, 1, 2, TRUE, widths = widths), file_name, append = TRUE)
write(create_two_leg("CCFR3M6", "Antigua and Barbuda", "Guatemala", 1, 0, 0, 2, FALSE, widths = widths), file_name, append = TRUE)

colors = c(TRUE, TRUE, FALSE, FALSE)

widths = c(strwidth("Saint Vincent and Grenadines") * width_multiplier, gd_width, pts_width)

Team = c("Mexico", "Honduras", "Canada", "El Salvador")
GD = c("+12", "0", "-3", "-9")
Pts = c(16, 8, 7, 2)
write(create_node(cbind(Team, GD, Pts), "CCFR4GA", colors, widths = widths), file_name, append = TRUE)

Team = c("Costa Rica", "Panama", "Haiti", "Jamaica")
GD = c("+8", "+2", "-2", "-8")
Pts = c(16, 10, 4, 4)
write(create_node(cbind(Team, GD, Pts), "CCFR4GB", colors, widths = widths), file_name, append = TRUE)

Team = c("United States", "Trinidad and Tobago", "Guatemala", "Saint Vincent and Grenadines")
GD = c("+17", "+4", "+7", "-28")
Pts = c(13, 11, 10, 0)
write(create_node(cbind(Team, GD, Pts), "CCFR4GC", colors, widths = widths), file_name, append = TRUE)

ccf_gteams = c("Mexico", "Costa Rica", "Panama", "Honduras", "United States", "Trinidad and Tobago")
widths = c(find_max_width(ccf_gteams) * width_multiplier, gd_width, pts_width)

colors = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
Team = c("Mexico", "Costa Rica", "Panama", "Honduras", "United States", "Trinidad and Tobago")
GD = c("+9", "+6", "-1", "-6", "+4", "-12")
Pts = c(21, 16, 13, 13, 12, 6)
write(create_node(cbind(Team, GD, Pts), "CCFR5", colors), file_name, append = TRUE)

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
 #   label = "Confederación Sudamericana de Fútbol"
    label = "CONMEBOL"
    fontsize = 50
    CMBRR;
  }
  edge [color = "#800000"]
  CMBRR:e2 -> GroupA:w1
  CMBRR:e3 -> GroupD:w2
  CMBRR:e1 -> GroupE:w1
  CMBRR:e4 -> GroupH:w1
  CMBRR:e5 -> ICF2:w2
edge [color = "black"]
  
  subgraph cluster_AFC{
#label = "Asian Football Confederation"
label = "AFC"
fontsize = 50
    {rank = same; AFCR1M1; AFCR1M2; AFCR1M3; AFCR1M4; AFCR1M5; AFCR1M6;}
    
    {rank = same; AFCR2GA; AFCR2GB; AFCR2GC; AFCR2GD; AFCR2GE; AFCR2GF; AFCR2GG; AFCR2GH}
    

    
    {rank = same; AFCR3GA; AFCR3GB;}
    
      edge[color = "#F58231"]
    AFCR1M1:e1 -> AFCR2GD:w5
    AFCR1M2:e1 -> AFCR2GH:w5
    AFCR1M3:e1 -> AFCR2GA:w5
    AFCR1M4:e1 -> AFCR2GE:w5
    AFCR1M5:e1 -> AFCR2GF:w4
    AFCR1M6:e2 -> AFCR2GC:w5
    
    AFCR2GA:e1 -> AFCR3GB:w2
    AFCR2GA:e2 -> AFCR3GB:w4
    AFCR2GB:e1 -> AFCR3GB:w3
    AFCR2GC:e1 -> AFCR3GA:w6
    AFCR2GC:e2 -> AFCR3GA:w5
    AFCR2GD:e1 -> AFCR3GA:w1
    AFCR2GE:e2 -> AFCR3GA:w3
    AFCR2GE:e1 -> AFCR3GB:w1
    AFCR2GF:e1 -> AFCR3GB:w6
    AFCR2GF:e2 -> AFCR3GB:w5
    AFCR2GG:e1 -> AFCR3GA:w2
    AFCR2GH:e1 -> AFCR3GA:w4
    
    AFCR3GA:e3 -> AFCR4:w1
    AFCR3GB:e3 -> AFCR4:w2
      edge[color = "black"]
  }
      edge[color = "#F58231"]
  AFCR3GA:e1 -> GroupB:w3
  AFCR3GA:e2 -> GroupF:w3
  AFCR3GB:e1 -> GroupH:w2
  AFCR3GB:e2 -> GroupA:w3
  AFCR4:e2 -> ICF1:w2
  edge[color = "black"]
  
  subgraph cluster_CAF{
#label = "Confederation of African Football"
label = "CAF"
fontsize = 50
    {rank = same; CAFR1M1; CAFR1M2; CAFR1M3; CAFR1M4; CAFR1M5; CAFR1M6; CAFR1M7; CAFR1M8; CAFR1M9; CAFR1M10; CAFR1M11; CAFR1M12; CAFR1M13; }
    

    
    
    {rank = same; CAFR2M1; CAFR2M2; CAFR2M3; CAFR2M4; CAFR2M5; 
      CAFR2M6; CAFR2M7; CAFR2M8; CAFR2M9; CAFR2M10; 
      CAFR2M11; CAFR2M12; CAFR2M13; CAFR2M14; CAFR2M15;
      CAFR2M16; CAFR2M17; CAFR2M18; CAFR2M19; CAFR2M20;}
    
    
    
    
    {rank = same; CAFR3GA; CAFR3GB; CAFR3GC; CAFR3GD; CAFR3GE;}
    

edge[color = "#3CB44B"]    
    CAFR1M1:e2 -> CAFR2M1:w1
    CAFR1M2:e2 -> CAFR2M2:w1
    CAFR1M3:e2 -> CAFR2M3:w1
    CAFR1M4:e2 -> CAFR2M4:w1
    CAFR1M5:e1 -> CAFR2M5:w1
    CAFR1M6:e1 -> CAFR2M6:w1
    CAFR1M7:e2 -> CAFR2M7:w1
    CAFR1M8:e2 -> CAFR2M8:w1
    CAFR1M9:e2 -> CAFR2M9:w1
    CAFR1M10:e1 -> CAFR2M10:w1
    CAFR1M11:e2 -> CAFR2M11:w1
    CAFR1M12:e2 -> CAFR2M12:w1
    CAFR1M13:e1 -> CAFR2M13:w1

    CAFR2M1:e2 -> CAFR3GB:w3
    CAFR2M2:e2 -> CAFR3GA:w1
    CAFR2M3:e2 -> CAFR3GA:w4
    CAFR2M4:e2 -> CAFR3GE:w4
    CAFR2M5:e2 -> CAFR3GE:w1
    CAFR2M6:e2 -> CAFR3GE:w3
    CAFR2M7:e2 -> CAFR3GB:w1
    CAFR2M8:e2 -> CAFR3GC:w4
    CAFR2M9:e2 -> CAFR3GA:w2
    CAFR2M10:e2 -> CAFR3GC:w2
    CAFR2M11:e2 -> CAFR3GD:w1
    CAFR2M12:e2 -> CAFR3GD:w3
    CAFR2M13:e2 -> CAFR3GB:w4
    CAFR2M14:e2 -> CAFR3GB:w2
    CAFR2M15:e1 -> CAFR3GA:w3
    CAFR2M16:e1 -> CAFR3GC:w1
    CAFR2M17:e2 -> CAFR3GC:w3
    CAFR2M18:e2 -> CAFR3GD:w2
    CAFR2M19:e2 -> CAFR3GE:w2
    CAFR2M20:e2 -> CAFR3GD:w4
    edge[color = "black"]
  }
edge[color = "#3CB44B"]    
  CAFR3GA:e1 -> GroupG:w3
  CAFR3GB:e1 -> GroupD:w3
  CAFR3GC:e1 -> GroupB:w4
  CAFR3GD:e1 -> GroupH:w3
  CAFR3GE:e1 ->  GroupA:w4
edge[color = "black"]
  
  subgraph cluster_OFC{
#label = "Oceania Football Confederation"
label = "OFC"
    fontsize = 50
    {rank = same; OFCR2GA; OFCR2GB}
    {rank = same; OFCR3GA; OFCR3GB}
    edge[color = "#0082c8"]
    OFCR1:e1 -> OFCR2GA:w4
    OFCR2GA:e1 -> OFCR3GB:w3
    OFCR2GA:e2 -> OFCR3GA:w2
    OFCR2GA:e3 -> OFCR3GB:w2
    OFCR2GB:e1 -> OFCR3GA:w1
    OFCR2GB:e2 -> OFCR3GB:w1
    OFCR2GB:e3 -> OFCR3GA:w3
    OFCR3GA:e1 -> OFCR4:w1
    OFCR3GB:e1 -> OFCR4:w2
    edge[color = "black"]
  }
  
  subgraph cluster_UEFA{
#label = "Union of European Football Associations"
label = "UEFA"
fontsize = 50
    {rank = same; UEFAGA; UEFAGB; UEFAGC; UEFAGD; UEFAGE; UEFAGF; UEFAGG; UEFAGH; UEFAGI;}



    {rank = same; UEFAP1; UEFAP2; UEFAP3; UEFAP4;}

  edge[ color = "black"]  
    UEFAGB:e2 -> UEFAP1:w2
    UEFAGC:e2 -> UEFAP1:w1
    
    UEFAGH:e2 -> UEFAP2:w2
    UEFAGI:e2 -> UEFAP2:w1
    
    UEFAGD:e2 -> UEFAP3:w2
    UEFAGE:e2 ->  UEFAP3:w1
    
    UEFAGA:e2 -> UEFAP4:w1
    UEFAGG:e2 -> UEFAP4:w2
   edge[color = "black"] 
  }
  edge[ color = "black"]  
  UEFAGA:e1 -> GroupC:w1
  UEFAGB:e1 -> GroupB:w2
  UEFAGC:e1 -> GroupF:w4
  UEFAGD:e1 -> GroupE:w3
  UEFAGE:e1 -> GroupH:w4
  UEFAGF:e1 -> GroupG:w2
  UEFAGG:e1 -> GroupB:w1
  UEFAGH:e1 -> GroupG:w1
  UEFAGI:e1 -> GroupD:w4
  UEFAP1:e2 -> GroupE:w2
  UEFAP2:e1 -> GroupD:w1
  UEFAP3:e1 -> GroupC:w2
  UEFAP4:e1 -> GroupF:w1
edge[color = "black"]
  
  subgraph cluster_Concacaf{
#label = "The Confederation of North,\\nCentral American and Caribbean\\nAssociation Football"
label = "CONCACAF"
fontsize = 50
    {rank = same; CCFR1M1; CCFR1M2; CCFR1M3; CCFR1M4; CCFR1M5; CCFR1M6; CCFR1M7;}



    {rank = same; CCFR2M1; CCFR2M2; CCFR2M3; CCFR2M4; CCFR2M5; CCFR2M6; CCFR2M7; CCFR2M8; CCFR2M9; CCFR2M10;}


    
{rank = same; CCFR3M1 -> CCFR3M2 [style = "invis"]; CCFR3M3; CCFR3M4; CCFR3M5 -> CCFR3M6 [style = "invis"]}


    {rank = same; CCFR4GA; CCFR4GB; CCFR4GC;}



    edge[ color = "#000080"]
    CCFR1M1:e2 -> CCFR2M6:w2
    CCFR1M2:e2 -> CCFR2M4:w1
    CCFR1M3:e1 -> CCFR2M7:w2
    CCFR1M4:e1 -> CCFR2M8:w1
    CCFR1M5:e1 -> CCFR2M10:w1
    CCFR1M6:e1 -> CCFR2M5:w2
    CCFR1M7:e1 -> CCFR2M9:w1
    
    CCFR2M1:e1 -> CCFR3M5:w1
    CCFR2M2:e1 -> CCFR3M6:w1
    CCFR2M3:e2 -> CCFR3M3:w1
    CCFR2M4:e2 -> CCFR3M2:w1
    CCFR2M5:e2 -> CCFR3M2:w2
    CCFR2M6:e1 -> CCFR3M6:w2
    CCFR2M7:e1 -> CCFR3M5:w2
    CCFR2M8:e2 -> CCFR3M1:w2
    CCFR2M9:e1 -> CCFR3M1:w1
    CCFR2M10:e1 -> CCFR3M4:w2
    
    CCFR3M1:e2 -> CCFR4GA:w4
    CCFR3M2:e1 -> CCFR4GA:w3
    CCFR3M3:e2 -> CCFR4GB:w3
    CCFR3M4:e1 -> CCFR4GB:w4
    CCFR3M5:e1 -> CCFR4GC:w4
    CCFR3M6:e2 -> CCFR4GC:w3
    
    CCFR4GA:e1 -> CCFR5:w1
    CCFR4GA:e2 -> CCFR5:w4
    CCFR4GB:e1 -> CCFR5:w2
    CCFR4GB:e2 -> CCFR5:w3
    CCFR4GC:e1 -> CCFR5:w5
    CCFR4GC:e2 -> CCFR5:w6
    edge[color = "black"]
    
  }
  edge[color = "#000080"]
  CCFR5:e1 -> GroupF:w2
  CCFR5:e2 -> GroupE:w4
  CCFR5:e3 -> GroupG:w4
  CCFR5:e4 -> ICF1:w1
edge[color = "black"]
  
  OFCR4:e -> ICF2:w [color = "#0082c8"]
  
  node[style = invis;]
  edge[style = invis;]
  in1 -> in2 -> in3 -> in4 -> in5
  node[style = ""]
  edge[style = ""]
  
  {rank = same; ICF1; ICF2; in3}
  {rank = same; GroupA; GroupB; GroupC; GroupD; GroupE; GroupF; GroupG; GroupH; in5}
#  {rank = same; GroupA -> GroupB -> GroupC -> GroupD -> GroupE -> GroupF -> GroupG -> GroupH [style = invis]; in5}
#  {rank = same; R16M1 -> R16M2 -> R16M3 -> R16M4 -> R16M5 -> R16M6 -> R16M7 -> R16M8 [style = invis]}
#  {rank = same; QF1 -> QF2 -> QF3 -> QF4 [style = invis]}
#  {rank = same; SF1 -> SF2[style = invis]}
{rank = same; UEFAGA; CCFR3M1}
  
  {rank = same; 	CMBRR; AFCR4; CAFR3GE; UEFAP4; CCFR5; OFCR4; in1}
  
  ICF1:e2 -> GroupC:w4 [color = "#f58231"]
  ICF2:e2 -> GroupC:w3 [color = "#800000"]
  
  GroupA:e1 -> R16M1:w1 [color = "#800000"]
  GroupA:e2 -> R16M5:w2
  GroupB:e1 -> R16M5:w1
  GroupB:e2 -> R16M1:w2
  GroupC:e1 -> R16M2:w1
  GroupC:e2 -> R16M6:w2
  GroupD:e1 -> R16M6:w1
  GroupD:e2 -> R16M2:w2 [color = "#800000"]
  GroupE:e1 -> R16M3:w1 [color = "#800000"]
  GroupE:e2 -> R16M7:w2
  GroupF:e1 -> R16M7:w1
  GroupF:e2 -> R16M3:w2 [color = "#000080"]
  GroupG:e1 -> R16M4:w1
  GroupG:e2 -> R16M8:w2
  GroupH:e1 -> R16M8:w1 [color = "#800000"]
  GroupH:e2 -> R16M4:w2 [color = "#f58231"]
  
  R16M1:e1 -> QF1:w1 [color = "#800000"]
  R16M2:e1 -> QF1:w2
  R16M3:e1 -> QF2:w1 [color = "#800000"]
  R16M4:e1 -> QF2:w2
  R16M5:e2 -> QF3:w1
  R16M6:e1 -> QF3:w2
  R16M7:e1 -> QF4:w1
  R16M8:e2 -> QF4:w2
  QF1:e2 -> SF1:w1
  QF2:e2 -> SF1:w2
  QF3:e2 -> SF2:w1
  QF4:e2 -> SF2:w2
  SF1:e1 -> F:w1
  SF2:e1 -> F:w2

Author[label ="Created by\\nJed Grabman",
       shape = "plaintext"
       fontsize = 20]
{rank = same; F; Author}
OFCR3GB -> Author[style = "invis"]
graph [label = "2018 FIFA World Cup:\\n210 Countries\\n935 Matches\\n1 Champion",
         labelloc = "t",
fontsize = 200]
}'
write(file_end, file_name, append = TRUE)
