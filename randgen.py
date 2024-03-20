
# { "state": "Mexico", "region": "South", "pop": 2312312322, "pop_male": 3123123, "pop_female": 123123 }
import json
import random
import urllib.request
states = ['Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming']
regions = ["South", "West", "Southwest", "North", "East", "Northeast", "Middle", "Midwest", "Mideast", "Nowhere"]

word_site = "https://www.mit.edu/~ecprice/wordlist.10000"
response = urllib.request.urlopen(word_site)
txt = response.read()
WORDS = txt.splitlines()

print(random.choice(states))

# print(WORDS[:10])

data = []
for i in range(10000):
    data.append({"state": random.choice(states),
                 "region": random.choice(regions),
                 "name": f"{random.choice(WORDS).decode().capitalize()} {random.choice(WORDS).decode().capitalize()}",
                 "pop": random.randint(1000000, 1000000000),
                 "pop_male": random.randint(10000, 1000000),
                 "pop_female": random.randint(10000, 1000000)})

f = open("./cities.json", "w")
f.write(json.dumps(data))
f.close()
                 
