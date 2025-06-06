import tkinter as tk
from tkinter import ttk,simpledialog, messagebox, scrolledtext
import spacy  
import nltk
from nltk import word_tokenize, pos_tag, ne_chunk
from nltk.tree import Tree
from pyswip import Prolog

nlp = spacy.load("en_core_web_md")  # index e melhorar queries ordem de pesquisa

patterns = {
    "actor_movies": ("In which movies did the actor [PERSON] participate?",1,0,0),
    "movies_by_year": ("Which movies were made in [YEAR]?",0,0,1),
    "person_birth": ("When [PERSON] was born?",1,0,0),
    "movies_by_actor_count": ("How many movies did [PERSON] participated?",1,0,0),  
    "movie_genres": ("What are the genres of [MOVIE]?",0,1,0),
    "movie_cast": ("Who acted in [MOVIE]?",0,1,0),
    "movie_year": ("When was [MOVIE] released?",0,1,0),
    "actor_rating": ("What is the average rating for [PERSON]?",1,0,0),
    "actor_age_release": ("How old was [PERSON] when [MOVIE] was released?",1,1,0),
    "top_actors": ("Give me names of very good actors.",0,0,0),
    "dead_movies": ("What movies have all actors already dead?",0,0,0),
    "worked_together": ("Which actors worked with [Person]?",1,0,0),
    "commom_movies": ("In which movies did [PERSON] and [PERSON] both act?",2,0,0),
    "actor_death:": ("When did [PERSON] died?",1,0,0),
    "actor_movies_year": ("What movies did [PERSON] act in during [YEAR]?",1,0,1),
    "actors_born_same_year": ("Which actors were born in the same year as [PERSON]?",1,0,0),
    "actor_languages": ("What languages does [PERSON] represent?",1,0,0),
    "erdos": ("Erdos number between [PERSON] and [PERSON]",2,0,0),
    "top_movies_actor": ("What are the best movies that [Person] acted",1,0,0),
    "only_one_lang_actors": ("Actors that only speak one language",0,0,0),
    "actor_most_genre": ("What is the most commom genre for [PERSON]",1,0,0),
    "actor_genre_movie": ("Give a genre and movie for [PERSON]",1,0,0)
}

def extract_persons(text):
    doc = nlp(text)
    for ent in doc.ents:
        print(ent.text,ent.label_)
    persons = [ent.text for ent in doc.ents if ent.label_ == "PERSON"]
    return list(set(persons)) 

def extract_movies(text):
    doc = nlp(text)
    return [ent.text for ent in doc.ents if ent.label_ in ("WORK_OF_ART", "EVENT")]

def similarity(a, b):
    doc1 = nlp(a)
    doc2 = nlp(b)
    return doc1.similarity(doc2)

def match_pattern(user_input,nperson,nmovie,nyear):
    patterns_list = list(patterns.values())
    similarities = [similarity(user_input, p) if ps == nperson and nmovie == ms and ys == nyear else 0 for (p, ps, ms, ys) in patterns_list]
    best_index = similarities.index(max(similarities))
    best_key = list(patterns.keys())[best_index]
    return best_key

def query_prolog(query_string):
    print(f"Prolog query: {query_string}")
    results = prolog.query(query_string)
    return results

def ask_birth_year(person, options, parent):
    dialog = tk.Toplevel(parent)
    dialog.title(f"Which {person} are you referring to? Please identify their birth year.")
    dialog.geometry("500x250") 
    dialog.grab_set()

    label = ttk.Label(dialog, text=f"Select birth year for {person}:", font=("Segoe UI", 13, "bold"))
    label.pack(pady=20)

    year_var = tk.StringVar()
    combo = ttk.Combobox(dialog, textvariable=year_var, values=options, state="readonly", font=("Segoe UI", 12), width=30)
    combo.pack(pady=10)
    combo.current(0)

    selected = {"year": None}

    def confirm():
        selected["year"] = combo.get()
        dialog.destroy()

    confirm_button = ttk.Button(dialog, text="OK", command=confirm)
    confirm_button.pack(pady=15)

    parent.wait_window(dialog)
    return selected["year"]



def build_prolog_query(user_input,gui_root):
    print(user_input)
    persons = extract_persons(user_input)
    check_person = ""
    idx_check = 0
    for person in persons:
        same_name = list(query_prolog(f"findall(Year,(name(AID,'{person}'),birth(AID,Year)),List)."))
        if same_name and len(list(filter(lambda x: x != 'null', same_name[0]['List']))) > 1:
            options = [year for year in same_name[0]['List'] if year != 'null']
            year = ask_birth_year(person, options, gui_root)
            if not year or not year.isdigit():
                return "Query canceled."
            if idx_check > 0:
                check_person += f"birth(AID1,{year}),"
            else:
                check_person = f"birth(AID,{year}),"
        idx_check += 1


    print("people:",persons)
    movies = extract_movies(user_input)
    print("movies:",movies)
    years = [token for token in word_tokenize(user_input) if token.isdigit() and len(token) == 4]
    print("years:",years)
    pattern = match_pattern(user_input,len(persons),len(movies),len(years))
    print("pattern:",pattern)

    if pattern == "actor_movies":
        return f"name(AID, '{persons[0]}'),"+check_person+f"findall(Title,(actor(TID,AID),movie(TID,Title,_,_)),List)."
    elif pattern == "movies_by_year":
        return f"findall(Movie,movie(_,Movie, {years[0]},_),List)."
    elif pattern == "person_birth":
        return f"name(AID,'{persons[0]}'),birth(AID,Year)."
    elif pattern == "movies_by_actor_count":
        return f"name(AID, '{persons[0]}'),"+check_person+f"findall(TID,actor(TID,AID),List),length(List,Count)."
    elif pattern == "movie_genres":
        return f"movie(TID,'{movies[0]}',_,_),"+f"findall(G,genre(TID,G),List)."
    elif pattern == "movie_cast":
        return f"movie(TID,'{movies[0]}',_,_),"+f"findall(Name,(actor(TID,AID),name(AID,Name)),List)."
    elif pattern == "movie_year":
        return f"movie(_,'{movies[0]}',Year,_)."
    elif pattern == "actor_rating": 
        return f"average_actor_rating('{persons[0]}', AvgRounded)."
    elif pattern == "actor_age_release":
        return f"actor_age_at_release('{persons[0]}','{movies[0]}',Age)."
    elif pattern == "top_actors": 
        return f"setof(Name,AID^(name(AID,Name),top_actor(AID)),List)."
    elif pattern == "dead_movies": 
        return f"setof(Title,TID^X^Y^Z^(movie(TID,Title,X,Y),actor(TID,Z),dead_movie(TID)),List)."
    elif pattern == "worked_together": 
        return f"name(AID, '{persons[0]}'),"+check_person+f"setof(Actor,CAID^(collab(AID,CAID),name(CAID,Actor)),List)."
    elif pattern == "commom_movies": 
        return f"name(AID, '{persons[0]}'),name(AID1,'{persons[1]}'),"+check_person+f"setof(Title,TID^(collab_movie(AID,AID1,TID),movie(TID,Title,_,_)),List)."    
    elif pattern == "actor_death":
        return f"name(AID,'{persons[0]}'),"+check_person+"death(AID,Year)."
    elif pattern == "actor_movies_year": 
        return f"name(AID, '{persons[0]}'),"+check_person+f"setof(Title,TID^(actor(TID,AID),movie(TID,Title,{years[0]},_)),List)."
    elif pattern == "actors_born_same_year":
        return f"name(AID, '{persons[0]}'),"+check_person+f"findall(Name,(birth(AID1,Year),birth(AID,Year),AID \= AID1,name(AID1,Name)),List)."
    elif pattern == "actor_languages": 
        return f"name(AID, '{persons[0]}'),"+check_person+f"setof(Language,TID^(actor(TID,AID),language(TID,Language)),List)."
    elif pattern == "erdos":
        return f"name(AID, '{persons[0]}'),name(AID1,'{persons[1]}')"+check_person+"erdos(AID,AID1,E)."
    elif pattern == "top_movies_actor": #!
        return f"name(AID, '{persons[0]}'),"+check_person+f"actor_top_movies(AID,List)."
    elif pattern == "only_one_lang_actors": 
        return "findall(Name,(name(AID,Name),only_lang_actor(AID)),List)."
    elif pattern == "actor_most_genre":
        return f"name(AID, '{persons[0]}'),"+check_person+"mc_genre_actor(AID,Genre)."
    elif pattern == "actor_genre_movie": 
        return f"name(AID, '{persons[0]}'),"+check_person+"genre_movie_actor(AID,List)."
    else:
        return "Sorry, I couldn't understand your question."


prolog = Prolog()
prolog.consult("start.pl")


def run_gui():
    root = tk.Tk()
    root.title("Intelligent Database")
    root.geometry("1200x800") 
    root.configure(bg="#f0f4f7")

    style = ttk.Style(root)
    style.theme_use("clam")

    style.configure("TLabel", background="#f0f4f7", font=("Segoe UI", 13))
    style.configure("TButton", font=("Segoe UI", 13), padding=8)
    style.configure("TEntry", font=("Segoe UI", 13))
    style.configure("Output.TFrame", background="#ffffff", borderwidth=1, relief="solid")

    title_label = ttk.Label(root, text="Ask Anything About Movies or Actors:", font=("Segoe UI", 18, "bold"))
    title_label.pack(pady=(20, 10))

    input_frame = ttk.Frame(root)
    input_frame.pack(pady=10)

    user_input_box = ttk.Entry(input_frame, width=100, font=("Segoe UI", 13))
    user_input_box.pack(side=tk.LEFT, padx=(10, 10), ipady=8) 

    def handle_query():
        user_input = user_input_box.get()
        if user_input.lower() == "exit":
            root.destroy()
            return

        query_string = build_prolog_query(user_input, root)
        output_area.insert(tk.END, f"\nYou asked: {user_input}\n")

        if "Sorry" in query_string or "Query canceled" in query_string:
            output_area.insert(tk.END, f" {query_string}\n\n")
        else:
            results = list(query_prolog(query_string))
            if not results:
                output_area.insert(tk.END, "No results found.\n\n")
            else:
                for res in results:
                    val = list(res.values())[-1]
                    if isinstance(val, (int, float, str)):
                        output_area.insert(tk.END, f"→ {val}\n")
                    else:
                        for r in val[:50]:
                            output_area.insert(tk.END, f"→ {r}\n")
                output_area.insert(tk.END, "\n")

        output_area.see(tk.END)
        user_input_box.delete(0, tk.END)

    ask_button = ttk.Button(input_frame, text="Ask", command=handle_query)
    ask_button.pack(side=tk.LEFT, padx=(0, 10))

    output_frame = ttk.Frame(root, style="Output.TFrame")
    output_frame.pack(padx=20, pady=20, fill=tk.BOTH, expand=True)

    output_area = scrolledtext.ScrolledText(
        output_frame,
        wrap=tk.WORD,
        font=("Consolas", 14),  
        bg="#fdfdfd",
        borderwidth=0
    )
    output_area.pack(fill=tk.BOTH, expand=True)

    root.mainloop()


if __name__ == "__main__":
    run_gui()
    
