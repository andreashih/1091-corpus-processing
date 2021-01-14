import json

with open("Boy-Girl-5073-5173.json", "r", encoding = "UTF-8") as f:
    bg_articles = json.load(f)['articles']

bg_content = []
for a in bg_articles[0:1600]:
    bg_content.append(a['content'])
    
with open('bg_content.txt', 'w', encoding='utf-8') as f:
    for item in bg_content:
        f.write("%s\n" % item)