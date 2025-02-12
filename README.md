# Rax_Investment_Calculator

A small project building a shiny application that performs a number of calculations pertaining to the Real sports mobile app. The virtual currency in the app, rax, can be used to buy Player and Team cards, sometimes referred to as "passes". Additionally, you can use rax to buy packs of play rating cards (and for some sports, individual cards) to upgrade your Player and Team passes. There are four main types of packs a user can buy: General (200 rax for 6 play rating cards and a booster), Starter (100 rax for 3 play rating cards and 1 booster), Yesterday packs (250 rax for 5 play rating cards of players that played the previous day), and Game (minimum 800 rax for 6 play rating cards of players that that will play in the specific game you buy). Player and Team passes earn rax depending on quality of performance, which the user then uses to invest in more passes or into card packs.

This app calculates how much rax needs to be invested into Player and Team passes to reach each rarity level in an attempt to identify which pass rarities provide the best value for your investment. The calculator is an estimation, as I do not have the average sum of play rating card values for each type of pack, but the user can test different values. They can also input the initial Player and Team card rax costs, choose the type of pack, and choose the target rarity to see how many packs must be bought and for how much rax. 

Also included is a table that shows the net profit a user can expect from On This Day (OTD) rax -  a feature that allows a user to continue collecting rax from player performances on the same date in the years following that performance, up to a maximum cap. Considering most non-elite players and teams do not reach the rax caps during the season, the table provides a good estimation of how valuable it is to invest in each rarity for the majority of players and teams on the Real app.

I am currently working on Calculator 2.0 that will include additional features such: as combined earnings of player and team cards whose play ratings count toward each other, upgrade costs when starting at non-zero player ratings, and allowing for the input of multiple players when upgrading team cards and providing an optimal upgrade path to reach the target team card rarity.

The link to the Shiny app in its current state is below!

https://derkrez.shinyapps.io/Rax_Investment_Calculator/
