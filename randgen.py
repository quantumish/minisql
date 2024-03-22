import json
import sys
import random
import urllib.request
import time

WORDS = [l.strip() for l in open("./words.txt").readlines()]

def cities():
    states = ['Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming']
    regions = ["South", "West", "Southwest", "North", "East", "Northeast", "Middle", "Midwest", "Mideast", "Nowhere"]


    print(random.choice(states))

    # print(WORDS[:10])

    data = []
    for i in range(10000):
        data.append({"state": random.choice(states),
                     "region": random.choice(regions),
                     "name": f"{random.choice(WORDS).capitalize()} {random.choice(WORDS).capitalize()}",
                     "pop": random.randint(1000000, 1000000000),
                     "pop_male": random.randint(10000, 1000000),
                     "pop_female": random.randint(10000, 1000000)})

    f = open("./cities.json", "w")
    f.write(json.dumps(data))
    f.close()

class Customer:
    def __init__(self):
        self.past_actions = []
        self.name = f"{random.choice(WORDS).capitalize()} Corp."
        self.headcount = random.randint(10, 100000)
    
    def next(self):
        self.headcount = random.randint(10, 100000)
        if self.past_actions == []:
            self.past_actions.append("new")
            return "new"
        possible = []
        if "new" in self.past_actions:
            if "buy" not in self.past_actions:
                self.past_actions.append("buy")
                return "buy"
        if "buy" in self.past_actions:
            if "return" not in self.past_actions:
                self.past_actions.append(random.choice(["return", "complain"]))
            else:
                self.past_actions.append("leave")
        return self.past_actions[-1]

def customers(seq=True): 
    data = []
    actions = ["buy", "new", "return", "complain", "leave"]
    users = [Customer() for i in range(100)]
    t = int(time.time())
    for i in range(1000):
        t += random.randint(600, 604800 * 3)
        user = random.choice(users)
        entry = {}
        if not seq:
            entry["time"] = t
        entry.update({ "action": user.next(),
                       "name": user.name,            
                       "headcount": user.headcount })
        data.append(entry)
        if data[-1]["action"] == "leave":
            users.remove(user)
            users.append(Customer())

    f = open(f"./{'seq' if seq else ''}users.json", "w")
    f.write(json.dumps(data))
    f.close()
    
if sys.argv[1] == "cities":
    cities()
elif sys.argv[1] == "sequsers":    
    customers()
elif sys.argv[1] == "users":
    customers(seq=False)
