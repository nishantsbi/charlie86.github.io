from nytimesarticle import articleAPI
import time

API_KEY = 'd0ca6e84f73f40a2b9cc4e7b24dc5674'

api = articleAPI(API_KEY)

articles = api.search(q = 'Trump',
	fq = {'headline':'Trump', 'source':['Reuters', 'AP', 'The New York Times']})

def parse_articles(articles):
	news = []
	for i in articles['response']['docs']:
		dic = {}
		dic['id'] = i['_id']
		if i['abstract'] is not None:
			dic['abstract'] = i['abstract'].encode('utf8')
		dic['headline'] = i['headline']['main'].encode('utf8')
		dic['desk'] = i['news_desk']
		dic['date'] = i['pub_date'][0:10] # cutting time of day.
		dic['section'] = i['section_name']
		if i['snippet'] is not None:
			dic['snippet'] = i['snippet'].encode('utf8')
		dic['source'] = i['source']
		dic['type'] = i['type_of_material']
		dic['url'] = i['web_url']
		dic['word_count'] = i['word_count']
		# locations
		locations = []
		for x in range(0,len(i['keywords'])):
			if 'glocations' in i['keywords'][x]['name']:
				locations.append(i['keywords'][x]['value'])
		dic['locations'] = locations
		# subject
		subjects = []
		for x in range(0,len(i['keywords'])):
			if 'subject' in i['keywords'][x]['name']:
				subjects.append(i['keywords'][x]['value'])
		dic['subjects'] = subjects
		news.append(dic)
	return(news) 

def get_articles(date,query):
	all_articles = []
	for i in range(0,100): #NYT limits pager to first 100 pages. But rarely will you find over 100 pages of results anyway.
		articles = api.search(q = query,
			fq = {'source':['Reuters','AP', 'The New York Times']},
			begin_date = date + '0101',
			end_date = date + '1231',
			sort='oldest',
			page = str(i))
		articles = parse_articles(articles)
		all_articles = all_articles + articles
		time.sleep(1)
	return(all_articles)

trump_all = []
# for i in 2017:
	# print 'Processing' + str(i) + '...'
trump_year = get_articles(str(2017),'Trump')
trump_all = trump_all + trump_year

import csv
keys = trump_all[0].keys()
with open('trump-mentions.csv', 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(trump_all)