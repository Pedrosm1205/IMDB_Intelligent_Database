import csv
import sys

csv.field_size_limit(sys.maxsize)

movie_facts = []
rating_facts = []
genre_facts = []
actor_facts = []
name_facts = []
birth_facts = []
death_facts = []
region_facts = []
language_facts = []

# Filter settings
MIN_YEAR = 2000
MIN_VOTES = 1

# Store filtered titles
ratings = {}

# Load ratings (to find popular movies)
with open("imdb_tables/title.ratings.tsv", encoding='utf-8') as f:
    reader = csv.DictReader(f, delimiter='\t')
    for row in reader:
        if row['numVotes'] != '\\N' and int(row['numVotes']) >= MIN_VOTES:
            ratings[row['tconst']] = float(row['averageRating'])

# Convert basics
with open("imdb_tables/title.basics.tsv", encoding='utf-8') as basics, open("imdb_kb.pl", "w", encoding='utf-8') as pl:
    
    reader = csv.DictReader(basics, delimiter='\t')
    for row in reader:
        tconst = row['tconst']
        if tconst not in ratings:
            continue
        if row['titleType'] != 'movie' or row['isAdult'] != '0':
            continue
        if row['startYear'] == '\\N' or int(row['startYear']) < MIN_YEAR:
            continue

        title = row['primaryTitle'].replace("'", "\\'")
        year = row['startYear']
        runtime = row['runtimeMinutes'] if row['runtimeMinutes'] != '\\N' else 'null'
        genres = row['genres'].replace('-', '_').lower().split(',') if row['genres'] != '\\N' else []

        movie_facts.append(f"movie('{tconst}', '{title}', {year}, {runtime}).\n")
        rating_facts.append(f"rating('{tconst}', {ratings[tconst]}).\n")
        for g in genres:
            genre_facts.append(f"genre('{tconst}', {g}).\n")



written_actors = set()

with open("imdb_tables/title.principals.tsv", encoding='utf-8') as f, open("imdb_tables/name.basics.tsv", encoding='utf-8') as names, open("imdb_kb.pl", "a", encoding='utf-8') as pl:
    name_map = {}
    name_reader = csv.DictReader(names, delimiter='\t')
    for row in name_reader:
        nconst = row['nconst']
        name = row['primaryName'].replace("'", "\\'")
        birth = row['birthYear'] if row['birthYear'] != '\\N' else 'null'
        death = row['deathYear'] if row['deathYear'] != '\\N' else 'null'
        name_map[nconst] = (name, birth, death)

    reader = csv.DictReader(f, delimiter='\t')
    for row in reader:
        tconst = row['tconst']
        nconst = row['nconst']
        if tconst not in ratings:
            continue
        if row['category'] not in ('actor', 'actress'):
            continue

        if nconst in name_map:
            name, birth, death = name_map[nconst]
            actor_facts.append(f"actor('{tconst}', '{nconst}').\n")

            if nconst not in written_actors:
                name_facts.append(f"name('{nconst}', '{name}').\n")
                birth_facts.append(f"birth('{nconst}', {birth}).\n")
                death_facts.append(f"death('{nconst}', {death}).\n")
                written_actors.add(nconst)




# Add regions and languages for each title from title.akas.tsv
with open("imdb_tables/title.akas.tsv", encoding='utf-8') as akas, open("imdb_kb.pl", "a", encoding='utf-8') as pl:

    reader = csv.DictReader(akas, delimiter='\t')
    for row in reader:
        tconst = row['titleId']
        if tconst not in ratings:
            continue  # Only process known/filtered titles

        region = row['region']
        language = row['language']

        if region != '\\N':
            region_facts.append(f"region('{tconst}', '{region}').\n")
        if language != '\\N':
            language_facts.append(f"language('{tconst}', '{language}').\n")


with open("imdb_kb.pl", "w", encoding='utf-8') as pl:
    for section in [
        movie_facts, rating_facts, genre_facts,
        actor_facts, name_facts, birth_facts, death_facts,
        region_facts, language_facts
    ]:
        pl.writelines(section)
