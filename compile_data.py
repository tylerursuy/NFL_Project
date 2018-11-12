import pandas as pd
import numpy as np
import math
import sys

nflplays = sys.argv[1]
spreads = sys.argv[2]

nfl = pd.read_csv(nflplays, low_memory=False)
spread = pd.read_csv(spreads)

print('csvs read')

#Tyler's part
teams = {"ARI": "Arizona Cardinals", "ATL": "Atlanta Falcons", "BAL": "Baltimore Ravens",
         "BUF": "Buffalo Bills", "CAR": "Carolina Panthers", "CHI": "Chicago Bears",
         "CIN": "Cincinnati Bengals", "CLE": "Cleveland Browns", "DAL": "Dallas Cowboys",
         "DEN": "Denver Broncos", "DET": "Detroit Lions", "GB": "Green Bay Packers",
         "HOU": "Houston Texans", "IND": "Indianapolis Colts", "JAX": "Jacksonville Jaguars",
         "JAC": "Jacksonville Jaguars", "KC": "Kansas City Chiefs", "LA": "Los Angeles Rams",
         "LAC": "Los Angeles Chargers", "MIA": "Miami Dolphins", "MIN": "Minnesota Vikings",
         "NE": "New England Patriots", "NO": "New Orleans Saints", "NYG": "New York Giants",
         "NYJ": "New York Jets", "OAK": "Oakland Raiders", "PHI": "Philadelphia Eagles",
         "PIT": "Pittsburgh Steelers", "SD": "San Diego Chargers", "SEA": "Seattle Seahawks",
         "SF": "San Francisco 49ers", "STL": "St. Louis Cardinals", "TB": "Tampa Bay Buccaneers",
         "TEN": "Tennessee Titans", "WAS": "Washington Redskins"}

"""Get desired features to include"""
nfl_teams = nfl[["Date", "GameID", "HomeTeam", "AwayTeam"]]
spread_games = list()
for game in range(len(spread["spread_favorite"])):
    if not math.isnan(spread["spread_favorite"][game]):
        if int(spread["schedule_date"][game][6:10]) > 2009:
            spread_games.append(spread.iloc[game])
    else:
        pass
spread_games = pd.DataFrame(spread_games)

"""Clean up data to prepare for merging"""
spread_dates_list = list()
for date in spread_games["schedule_date"]:
    spread_dates_list.append(date)
for date in range(len(spread_dates_list)):
    spread_dates_list[date] = spread_dates_list[date][6:10] + "/" + \
                              spread_dates_list[date][0:2] + "/" + \
                              spread_dates_list[date][3:5]
spread_games["schedule_date"] = spread_dates_list
nfl_dates_list = list()
for date in nfl_teams["Date"]:
    nfl_dates_list.append(date.replace("-", "/"))
nfl_teams.loc[:]['Date'] = nfl_dates_list
nfl_dates_set = set()
for date in nfl_dates_list:
    nfl_dates_set.add(date)
spread_dates = set(spread_dates_list)

"""Merge data sets"""
accounted_dates = nfl_dates_set.intersection(spread_dates)
nfl_df = pd.DataFrame(list(accounted_dates), columns=["Date"])
nfl_teams = np.array(nfl_teams)
unique_games = set()
for game in nfl_teams:
    unique_games.add(tuple(game))
matched_games = set()
for game in unique_games:
    if game[0] in accounted_dates:
        matched_games.add(game)
accounted_spread_games = spread_games[["schedule_date", "team_home", "team_away",
                                       "score_home", "score_away", "team_favorite_id",
                                       "spread_favorite", "over_under_line"]]
accounted_spread_array = np.array(accounted_spread_games)
matched_scores = list()
for game in accounted_spread_array:
    for g in matched_games:
        if game[0] == g[0] and game[1] == teams[g[2]] and game[2] == teams[g[3]]:
            game = tuple(game) + (g[1],)
            matched_scores.append(tuple(game))
new_df = pd.DataFrame(matched_scores, columns=["Date", "Home Team", "Away Team",
                                               "Home Score", "Away Score", "Favorite Team ID",
                                               "Spread Favorite", "Over Under", "gameid"])
new_df = new_df.set_index('gameid')

print("Tyler's part done")

#Eddie's part
nfl = nfl.sort_values(['Date', 'GameID', 'TimeSecs'], ascending=False)
nfl = nfl[0 <= nfl.TimeSecs]
nfl.index = range(nfl.shape[0])

# add fumbles, sacks, penalty yardage
gameid = []; hometm = []; awaytm = []; homesc = []; awaysc = []; hmhalfsc = []; awhalfsc = []; ot = []
hpassyds = [0];
hpatts = [0];
hypa = [];
hcomp = [0];
hcpct = [];
hints = [0];
hrushyds = [0];
hratts = [0];
hypr = []
apassyds = [0];
apatts = [0];
aypa = [];
acomp = [0];
acpct = [];
aints = [0];
arushyds = [0];
aratts = [0];
aypr = []
hmadj = 0;
awadj = 0
for i in range(nfl.shape[0]):
    if nfl.qtr[i] == 4 and not nfl.qtr[i + 1] == 4:
        if nfl.qtr[i + 1] == 1:
            ot.extend([0])
        else:
            ot.extend([1])
        offscore = nfl.PosTeamScore[i - 1]  # score after 4 quarters
        defscore = nfl.DefTeamScore[i - 1]
        if nfl.posteam[i] == nfl.HomeTeam[i]:
            homesc.extend([offscore + hmadj])
            awaysc.extend([defscore + awadj])
        else:
            homesc.extend([defscore + hmadj])
            awaysc.extend([offscore + awadj])
        hmadj = 0;
        awadj = 0  # will be uncommented
    if nfl.qtr[i] == 2 and not nfl.qtr[i + 1] == 2:  # 4 to be changed to 2
        gameid.extend([nfl.GameID[i]])
        hometm.extend([nfl.HomeTeam[i]])
        awaytm.extend([nfl.AwayTeam[i]])
        hypa.extend([hpassyds[-1] / hpatts[-1]]);
        hcpct.extend([hcomp[-1] / hpatts[-1]])
        hypr.extend([hrushyds[-1] / hratts[-1]])
        hpassyds.extend([0]);
        hpatts.extend([0]);
        hcomp.extend([0]);
        hints.extend([0])
        hrushyds.extend([0]);
        hratts.extend([0])
        aypa.extend([apassyds[-1] / apatts[-1]]);
        acpct.extend([acomp[-1] / apatts[-1]])
        aypr.extend([arushyds[-1] / aratts[-1]])
        apassyds.extend([0]);
        apatts.extend([0]);
        acomp.extend([0]);
        aints.extend([0])
        arushyds.extend([0]);
        aratts.extend([0])

        offscore = nfl.PosTeamScore[i + 1]  # halftime score
        defscore = nfl.DefTeamScore[i + 1]
        if nfl.posteam[i + 1] == nfl.HomeTeam[i]:
            hmhalfsc.extend([offscore + hmadj])
            awhalfsc.extend([defscore + awadj])
        else:
            hmhalfsc.extend([defscore + hmadj])
            awhalfsc.extend([offscore + awadj])
        # hmadj=0; awadj=0 #should be commented out
    if nfl.qtr[i] in [1, 2, 3, 4]:  # need all 4 quarters for full time score
        if nfl.Touchdown[i] == 1:
            j = 1
            while not isinstance(nfl.posteam[i + j], str):
                j += 1
            if not isinstance(nfl.ExPointResult[i + j], str) and not isinstance(nfl.TwoPointConv[i + j], str) and \
                    nfl['Challenge.Replay'][i] == 0:
                if nfl.HomeTeam[i] == nfl.DefensiveTeam[i]:
                    hmadj += 1
                    if nfl.ScoreDiff[i] == nfl.ScoreDiff[i + j]:
                        hmadj += 6
                else:
                    awadj += 1
                    if nfl.ScoreDiff[i] == nfl.ScoreDiff[i + j]:
                        awadj += 6
    if nfl.qtr[i] in [1, 2]:  # to be changed to [1,2]
        if nfl.PassAttempt[i] == 1 and not nfl.PlayType[i] == 'No Play':
            if nfl.posteam[i] == nfl.HomeTeam[i]:
                hpatts[-1] += 1
                if nfl.InterceptionThrown[i] == 1:
                    hints[-1] += 1
                elif nfl.PassOutcome[i] == 'Complete':
                    hpassyds[-1] += nfl.AirYards[i] + nfl.YardsAfterCatch[i]
                    hcomp[-1] += 1
            elif nfl.posteam[i] == nfl.AwayTeam[i]:
                apatts[-1] += 1
                if nfl.InterceptionThrown[i] == 1:
                    aints[-1] += 1
                elif nfl.PassOutcome[i] == 'Complete':
                    apassyds[-1] += nfl.AirYards[i] + nfl.YardsAfterCatch[i]
                    acomp[-1] += 1
        elif nfl.RushAttempt[i] == 1 and not nfl.PlayType[i] == 'No Play':
            if nfl.posteam[i] == nfl.HomeTeam[i]:
                hratts[-1] += 1
                if nfl['Challenge.Replay'][i] == 1:
                    hrushyds[-1] += (nfl.yrdline100[i] - nfl.yrdline100[i + 1])
                else:
                    hrushyds[-1] += nfl['Yards.Gained'][i]
            elif nfl.posteam[i] == nfl.AwayTeam[i]:
                aratts[-1] += 1
                if nfl['Challenge.Replay'][i] == 1:
                    arushyds[-1] += (nfl.yrdline100[i] - nfl.yrdline100[i + 1])
                else:
                    arushyds[-1] += nfl['Yards.Gained'][i]

hpassyds = hpassyds[:-1]; hpatts = hpatts[:-1]; hcomp = hcomp[:-1]; hints = hints[:-1]; hrushyds = hrushyds[:-1]; hratts = hratts[:-1]
apassyds = apassyds[:-1]; apatts = apatts[:-1]; acomp = acomp[:-1]; aints = aints[:-1]; arushyds = arushyds[:-1]; aratts = aratts[:-1]

wrangled = pd.DataFrame({'gameid':gameid, 'home':hometm, 'away':awaytm, 'homesc':homesc, 'awaysc':awaysc,
                         'hmhalfsc':hmhalfsc, 'awhalfsc':awhalfsc, 'ot':ot,
                         'hpyd':hpassyds, 'hpatt':hpatts, 'hcomp':hcomp, 'hypa':hypa, 'hcomppct':hcpct, 'hint':hints,
                         'hryd':hrushyds, 'hratt':hratts, 'hypr':hypr,
                         'apyd':apassyds, 'apatt':apatts, 'acomp':acomp, 'aypa':aypa, 'acomppct':acpct, 'aint':aints,
                         'aryd':arushyds, 'aratt':aratts, 'aypr':aypr})
wrangled = wrangled.set_index('gameid')

merged = new_df.merge(wrangled, how='outer', on='gameid')
merged.to_csv('nfl_cleaned.csv')