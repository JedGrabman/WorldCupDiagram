 # Frequently Asked Questions

## What is this?

The month long tournament that takes place every 4 years to crown the World Cup champion is only the final stage of a much longer tournament.  

After every World Cup, I like to create a graphic that shows the entire process, including qualification stages.

## How does this have 209 countries? I'm pretty sure there aren't that many countries in the U.N.

Most of the discrepancy is due to nations or territories that are usually represented by broader political groups in political contexts like the UN. For example, England and Scotland are not on the list of [U.N. member states](https://www.un.org/en/about-us/member-states), as they are both represented by the United Kingdom. Similarly, teams such as Guam or Tahiti are recognized by FIFA, despite officially being part of the U.S and France (respectively) for U.N. purposes.

## How does qualification work?

Each host of a World Cup is automatically reserved a spot in the finals. Every other team must earn their way through a qualifying tournament organized by their continental confederation. The final stage is sometimes referred to as the “World Cup finals” for this reason.

## What’s a continental confederation?

There are 6 continental confederations that organize and represent national football organizations in different regions of the world. They are:

* Confederation of North, Central America and Caribbean Association Football (CONCACAF)
* Confederación Sudamericana de Fútbol (CONMEBOL)
* Union of European Football Associations (UEFA)
* Asian Football Confederation (AFC)
* Confederation of African Football (CAF)
* Oceania Football Confederation (OFC)

## How many spots does each confederation get and how is it determined?

First, all hosts are automatically granted a spot in the final stage.

Each confederation had a number of reserved spots in the World Cup finals. Additionally, confederations may have additional inter-continental playoff spots, where teams between different confederations play to determine which ones will advance to the World Cup finals.

There were a total of 3 spots for the hosts (all from CONCACAF), 43 spots from confederation tournaments. 6 teams qualified for the inter-continental playoffs, where the top 2 teams would advance to the World Cup finals.

The breakdown of direct spots + inter-continental playoff spots was as follows:

**Confederation**|**Spots**
--|--
AFC|8+1
CAF|9+1
CONCACAF|3+2
CONMEBOL|6+1
OFC|1+1
UEFA|16+0

The number of spots each confederation receives is at the discretion of FIFA.

## Why are the tournament structures so different in each confederation?

While there are some restrictions, each confederation is given broad latitude on how to structure their own tournament and they often make adjustments each cycle.

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

## Which teams did not participate in the tournament?

* [Russia was suspended by FIFA and UEFA](https://www.fifa.com/tournaments/mens/worldcup/qatar2022/media-releases/fifa-uefa-suspend-russian-clubs-and-national-teams-from-all-competitions) following the [2022 Russian invasion of Ukraine](https://en.wikipedia.org/wiki/2022_Russian_invasion_of_Ukraine)
* [Eritrea withdrew from the tournament before playing any games](https://inside.fifa.com/tournaments/mens/worldcup/canadamexicousa2026/news/statement-on-fifa-world-cup-2026-tm-preliminary-competition)

## Why do more teams advance from some groups than others?

**CAF:** The top 4 runner-ups in round 1 were granted advancement to round 2. The runner-ups were ranked by points and goal differential. Due to Eritrea's withdrawal, games against the last placed team in each group were excluded from the calculation.

**UEFA:** The top four UEFA [Nations League group winners](https://en.wikipedia.org/wiki/2020%E2%80%9321_UEFA_Nations_League) that had not already qualified for round 2 were granted advancement

## Why isn’t the UEFA Nations League in the diagram if it’s part of qualifiers?

Despite having an impact on the qualification tournament, matches in the Nations League were not considered to be qualifying matches for ranking purposes. In some confederations, a team’s ranking affected whether it received a bye. Every match can affect a team’s ranking, but that doesn’t make every match part of the qualifying tournament in those confederations. It is a similar principle. 

## Where can I find more specific information on the qualifying process?
I recommend the Wikipedia article ["2026 FIFA World Cup qualification"](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_qualification)

## What did you use to make this?

**Sources:** Primarily [Wikipedia](https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_qualification)

**Software:** The diagram is written in [DOT and rendered using Graphviz](https://graphviz.org/). I used [https://dreampuf.github.io/GraphvizOnline](https://dreampuf.github.io/GraphvizOnline) for rendering. Most of the nodes in the graph are simply [HTML tables](https://www.w3schools.com/html/html_tables.asp). In principle, this could all be done by hand, but I’ve written [some code in R](https://github.com/JedGrabman/WorldCupDiagram/blob/main/WorldCup_2026.R) to streamline the process.

## Can I use your code?
Generally, yes. Please refer to the [License](https://github.com/JedGrabman/WorldCupDiagram/blob/main/LICENSE) for full details.

## Can I repost these images?
Yes, but you must prominently give me credit. Keeping my name on the main graphic is sufficient.

## Your code has a problem - will you fix it?

This project is a low priority for me in terms of optimizing / maintaining, as it’s something I only do once every 4 years. Feel free to report issues on Github, but don’t expect a prompt resolution. It is open source though, so feel free to fork it or submit pull requests if you’d like.

## Your diagram has an error - will you fix it?

I expect that I’ve missed something while transcribing data about hundreds of teams. Feel free to open a Github issue (or if you found this through a reddit post I've made, leave a comment there). Honestly, this project only gets any attention once every 4 years so I might not fix it. But, I'll probably make a note for the 2030 World Cup.

## What's up with the flags of Northern Ireland and Chinese Taipei?

This was rendered using emoji flags. There is no official emoji for these two teams, so I couldn't render them in SVG format. I plan to manually edit them in PNG format, using the flags listed on the official FIFA website.

## Have you done this for past World Cups?

[2014 Reddit post](https://www.reddit.com/r/soccer/comments/2akovc/203_teams_883_matches_1_champion_a_diagram/)

[2018 Reddit post](https://www.reddit.com/r/soccer/comments/8ywhjs/210_teams_935_matches_1_champion_a_diagram/)

[2022 Reddit post](https://www.reddit.com/r/soccer/comments/zp6bos/oc_206_teams_929_matches_1_champion_diagrams/)

As you can see, I’ve experimented with the exact format.

## How can I contact you?

Generally, issues or contact through Github is preferred, but I know many users come here through reddit. You can also message [my account there](https://www.reddit.com/user/aeouo).

If you have code improvement suggestions, See “Your code has a problem - will you fix it?”

If you want to email me, use [JedGWC@gmail.com](mailto:JedGWC@gmail.com)
