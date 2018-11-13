import pandas as pd
import numpy as np
import math

nfl = pd.read_csv("data/NFL_Plays.csv", low_memory=False)
spread = pd.read_csv("data/spreadspoke_scores.csv", low_memory=False)

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
        if int(spread["schedule_date"][game][6:10]) >= 2009:
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
spread_games = spread_games.drop(columns=["schedule_date"])
spread_games["schedule_date"] = spread_dates_list
nfl_dates_list = list()
for date in nfl_teams["Date"]:
    nfl_dates_list.append(date.replace("-", "/"))
nfl_teams = nfl_teams.drop(columns=["Date"])
nfl_teams["Date"] = nfl_dates_list
cols = nfl_teams.columns.tolist()
cols = cols[-1:] + cols[:-1]
nfl_teams = nfl_teams[cols]
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
                                               "Spread Favorite", "Over Under", "Game ID"])
new_cols = new_df.columns.tolist()
new_cols = new_cols[-1:] + new_cols[:-1]
new_df = new_df[new_cols]
