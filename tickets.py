import sys
import requests
import json
import csv
from time import time
import urllib2
import webbrowser


####

### NOTE need to specify team to get data for
team = "Golden State Warriors"

# read list of teams and home arenas
f = open('arenas_red.csv', 'rU')
csv_f = csv.reader(f)
stadiums = []
for row in csv_f:
	stadiums.append({'stadium': row[0], 'team': row[1], 'code': row[2]})
######################
#search Stub hub for games
ind = next(index for (index, stadiums) in enumerate(stadiums) if stadiums['team'] == team)
url = "https://www.stubhub.com/listingCatalog/select?q=stubhubDocumentType:event%20AND%20description:%22"+team.replace(' ', '%20')+"%22%20AND%20ancestorGenreDescriptions:NBA&wt=json&indent=on"
webbrowser.open_new_tab(url)
response = urllib2.urlopen(url)
data = json.load(response)
## save data
filename = stadiums[ind]['code']+'.json'
with open(filename, 'w') as outfile:
	json.dump(data, outfile)
#########
with open(filename) as data_file:
	data = json.load(data_file)
games = data['response']['docs']
games_list = []
stadium = stadiums[ind]['stadium']
for idx, game in enumerate(games):
	if game['venue_name'] == stadium:
		temp = {}
		temp['event_id'] = game['event_id']
		temp['home'] = game['act_primary']
		temp['away'] = game['act_secondary']
		temp['date'] = game['event_date_local']
		games_list.append(temp)
##########
## grab ticket info using event IDs
tickets_list = []
csvfile = stadiums[ind]['code']+'.csv'
csvfields = ['home', 'away', 'date','location', 'row', 'qty', 'price']
for idx in enumerate(games_list):
	print(idx)[0]
	t = int(time())
	event = games_list[idx[0]]['event_id']
	url = 'http://www.stubhub.com/ticketAPI/restSvc/event/' + event + '/sort/price/0?ts=' + str(t) + '000'
	# make get request to stubhub.com and take the result in json
	response = requests.get(url)
	response_dict = response.json()
	tickets = response_dict['eventTicketListing']['eventTicket']
	# select location, row, quantity and price of the tickets
	for ticket in tickets:
		d = {}
		d['home'] = games_list[idx[0]]['home']
		d['away'] = games_list[idx[0]]['away']
		d['date'] = games_list[idx[0]]['date']
		d['location'] = ticket['va']
		d['row'] = ticket['rd']
		d['qty'] = ticket['qt']
		d['price'] = ticket['cp']
		tickets_list.append(d)
# write result into csv file
with open(csvfile, 'wb') as f:
	d_writer = csv.DictWriter(f, csvfields)
	d_writer.writer.writerow(csvfields)
	d_writer.writerows(tickets_list)


