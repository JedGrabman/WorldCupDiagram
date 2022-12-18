 # Frequently Asked Questions

## What is this?

The goal of this project was to create a diagram summarizing the entire 2022 FIFA World Cup, including qualification stages.

## What do you mean by “qualification stages”? Doesn’t the World Cup only have 32 teams?

The final stage of the World Cup only has 32 teams, however far more than 32 nations would like to participate. One spot is automatically reserved for the host nation, but every other team must earn their way to the final stage through a qualifying tournament organized by their continental confederation. The final stage with 32 teams is sometimes referred to as the “World Cup finals” for this reason.

## How does this have 206 countries? Doesn’t the U.N. only have <200 countries?

Most of the discrepancy is due to nations or territories that are represented by broader political groups in the UN. For example, England and Scotland are not on the list of [U.N. member states](https://www.un.org/en/about-us/member-states), as they are both represented by the United Kingdom. Similarly, teams such as Guam or Tahiti are recognized by FIFA, despite officially being part of the U.S and France (respectively) for U.N. purposes.

## What’s a continental confederation?

There are 6 continental confederations that organize and represent national football organizations in different regions of the world. They are:

* Confederation of North, Central America and Caribbean Association Football (CONCACAF)
* Confederación Sudamericana de Fútbol (CONMEBOL)
* Union of European Football Associations (UEFA)
* Asian Football Confederation (AFC)
* Confederation of African Football (CAF)
* Oceania Football Confederation (OFC)

## How many spots does each confederation get and how is it determined?

First, the host is automatically granted a spot in the final stage.

Each confederation had the following number of additional spots that could be earned through their continental qualifying stage:

**Confederation**|**Spots**
--|--
AFC|4 or 5
CAF|5
CONCACAF|3 or 4
CONMEBOL|4 or 5
OFC|0 or 1
UEFA|13

For teams with a variable number of spots, one team from that confederation needed to participate in a playoff with a team from a different confederation to determine who would advance. For example, Peru placed 5th in CONMEBOL qualifying and Australia placed 5th in AFC qualifying, so the two teams then played each other to determine who would be in the World Cup finals.

The number of spots each confederation receives is at the discretion of FIFA.

## Why are the tournament structures so different in each confederation?

While there are some restrictions, each confederation is given broad latitude on how to structure their own tournament.

## Why do some teams have to play more rounds, even within a single confederation?

Confederations are allowed to give higher ranked teams byes to later rounds. However, the teams receiving the byes must be the highest ranked teams in the confederation as determined by the FIFA world rankings.

## It looks like there’s a lot of different structures for how teams can face each other. What are all the options?

There are essentially 2 structures:

* Head-to-head with 1 team advancing. This can either be a single game, or a two-leg match with 1 game hosted by each country. In a two-leg match, the aggregate number of goals decides the winner. If the aggregate total is the same, the team that scored more goals away from home is the winner. Extra time and a shootout is used if needed.
* Group round robin. Every team plays every other team in the group (either once, or home and away). Teams are awarded 3 points for a win and 1 point for a draw. The teams with the most points advance. If teams are tied on points, the first tie-breaker is goal differential (goals for - goals against).

## How are groups determined for round robins?

Generally, the groups are selected in ways to try to ensure each group is of approximately equal strength. Imagine there are 20 teams that need to be divided into 5 groups of 4 teams. Typically, the teams will be ranked and each group will have one team ranked in the top 5, one team ranked 6-10, and so on. This helps ensure the groups are approximately equal in strength

## What does (W), (DQ) mean next to a team’s name?

It means they withdrew (W) or were ejected (DQ) from the tournament.

## Why did various teams withdraw?

[American Samoa](https://www.reuters.com/lifestyle/sports/oceania-mini-tournament-decide-world-cup-playoff-qualifier-2021-11-29/) \- Covid-19 travel restrictions

[Cook Islands](https://www.rnz.co.nz/international/pacific-news/463905/cook-islands-withdraw-from-fifa-world-cup-qualifiers) \- “the bulk of their squad tested positive for Covid-19”

[North Korea](https://apnews.com/article/north-korea-middle-east-health-coronavirus-pandemic-world-cup-e9694d13f52ea8b51274f1f94fe8a5ed) \- Reportedly over Covid-19 concerns

[Saint Lucia](https://www.fifa.com/fifaplus/en/articles/when-st-lucias-world-cup-pull-out-left-hearts-broken) \- Covid logistical concerns

[Samoa](https://www.reuters.com/lifestyle/sports/oceania-mini-tournament-decide-world-cup-playoff-qualifier-2021-11-29/) \- Covid-19 travel restrictions

[Tonga](https://www.fifa.com/tournaments/mens/worldcup/qatar2022/news/tonga-withdraw-from-fifa-world-cup-qatar-2022-tm-ofc-preliminary-competition) \- [2022 Hunga Tonga–Hunga Haʻapai eruption and tsunami](https://en.wikipedia.org/wiki/2022_Hunga_Tonga%E2%80%93Hunga_Ha%CA%BBapai_eruption_and_tsunami)

[Vanuatu](https://www.rnz.co.nz/international/pacific-news/463655/vanuatu-withdraw-from-football-world-cup-qualifiers) \- “a majority of their players and staff testing positive for Covid-19”

## Why was Russia ejected from UEFA’s qualifying tournament?

[Russia was suspended by FIFA and UEFA](https://www.fifa.com/tournaments/mens/worldcup/qatar2022/media-releases/fifa-uefa-suspend-russian-clubs-and-national-teams-from-all-competitions) following the [2022 Russian invasion of Ukraine](https://en.wikipedia.org/wiki/2022_Russian_invasion_of_Ukraine)

## Why do more teams advance from some groups than others?

**AFC:** The top 5 runner-ups in round 2 were granted advancement to round 3. The runner-ups were ranked by points and goal differential. However, because North Korea withdrew from group H, that group only had 4 participating teams, compared to 5 for every other group. Therefore, for the purposes of calculating the relative rankings of runner-up teams, matches against the 5th place team in the group were excluded for all other groups.

**UEFA:** The top two UEFA [Nations League group winners](https://en.wikipedia.org/wiki/2020%E2%80%9321_UEFA_Nations_League) that had not already qualifying for round 2 were granted advancement

## Why isn’t the UEFA Nations League in the diagram if it’s part of qualifiers?

Despite having an impact on the qualification tournament, matches in the Nations League were not considered to be qualifying matches for ranking purposes. In some confederations, a team’s ranking affected whether it received a bye. Every match can affect a team’s ranking, but that doesn’t make every match part of the qualifying tournament in those confederations. It is a similar principle. 

## Where can I find more specific information on the qualifying process?
I recommend the Wikipedia article ["2022 FIFA World Cup qualification"](https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_qualification)

## What did you use to make this?

**Sources:** Primarily [Wikipedia](https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_qualification)

**Software:** The diagram is written in [DOT and rendered using Graphviz](https://graphviz.org/). I used [https://dreampuf.github.io/GraphvizOnline](https://dreampuf.github.io/GraphvizOnline) for rendering. Most of the nodes in the graph are simply [HTML tables](https://www.w3schools.com/html/html_tables.asp). In principle, this could all be done by hand, but I’ve written [some code in R](https://github.com/JedGrabman/WorldCupDiagram/blob/main/WorldCup_2022.R) to streamline the process.

## Can I use your code?
Generally, yes. Please refer to the [License](https://github.com/JedGrabman/WorldCupDiagram/blob/main/LICENSE) for full details.

## Your code has a problem - will you fix it?

This project is a low priority for me in terms of optimizing / maintaining, as it’s something I only do once every 4 years. Feel free to report issues on Github, but don’t expect a prompt resolution. It is open source though, so feel free to fork it or submit pull requests if you’d like.

## Your diagram has an error - will you fix it?

I expect that I’ve missed something while transcribing data about hundreds of teams. I can’t update the original post, but if you report an error, I’ll keep a list and make an update after a sufficient enough time has passed that I’m unlikely to get more reports.

## What known errors are there?

* The flags of Scotland, Northern Ireland, Wales and England show up as the UK.
   * This is a shockingly annoying technical issue. The flags are emoji, but don’t generally show up in Windows. They will mostly render when using Firefox, except for the flags mentioned above. These countries are also regions of the UK and the emoji are constructed differently. I chose to use the UK flag to have something, but it’s not perfect. It does get around the fact that major vendors haven’t implemented a [Northern Ireland flag emoji](https://emojipedia.org/flag-for-northern-ireland-gbnir/).
* The flag of Tahiti is actually French Polynesia
   * I could not find a Tahiti flag emoji. Similarly to using the UK flag for its constituent nations, I chose to use French Polynesia as the closest substitute for Tahiti. It has the added benefit of looking similar to the Tahiti flag, but it is not the flag of Tahiti and I unfortunately do not have a solution at this time.

## Have you done this for past World Cups?

[2014 Reddit post](https://www.reddit.com/r/soccer/comments/2akovc/203_teams_883_matches_1_champion_a_diagram/)

[2018 Reddit post](https://www.reddit.com/r/soccer/comments/8ywhjs/210_teams_935_matches_1_champion_a_diagram/)

As you can see, I’ve experimented with the exact format.

## How can I contact you?

If you want to discuss this diagram publicly, you may comment on the [reddit post](https://reddit.com/user/aeouo/comments/zp5sf0/206_teams_929_matches_1_champion_diagrams_showing/). I’ll be notified for top-level comments. This is preferred for general questions about the diagram and the World Cup qualification process.

You can also message [u/aeouo](https://www.reddit.com/user/aeouo) on Reddit.

If you have code improvement suggestions, See “Your code has X problem - will you fix it?”

If you want to email me, use [JedGWC@gmail.com](mailto:JedGWC@gmail.com) (for Jed G. World Cup). 
