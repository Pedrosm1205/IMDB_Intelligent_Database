:- dynamic region/2.
:- dynamic actor/2.
:- dynamic birth/2.
:- dynamic death/2.
:- dynamic genre/2.
:- dynamic language/2.
:- dynamic rating/2.
:- dynamic name/2.
:- dynamic movie/4.

clean_region :- 
    retract(region(_, _)),
    fail.
clean_region.

clean_actor :-
    actor(TID,AID),
    (should_remove_actor(AID);should_remove_movie(TID)),
    retract(actor(TID,AID)),
    fail.
clean_actor.

should_remove_actor(AID) :- \+ (name(AID,_)).

should_remove_movie(TID) :- \+ (movie(TID,_,_,_)).

clean_birth :-
    birth(AID, Year),
    should_remove_actor(AID),
    retract(birth(AID, Year)),
    fail.
clean_birth.

clean_death :-
    death(AID, Year),
    should_remove_actor(AID),
    retract(death(AID, Year)),
    fail.
clean_death.

clean_genre :-
    genre(TID, Genre),
    should_remove_movie(TID),
    retract(genre(TID, Genre)),
    fail.
clean_genre.

clean_language :-
    language(TID, Lang),
    should_remove_movie(TID),
    retract(language(TID, Lang)),
    fail.
clean_language.

clean_rating :-
    rating(TID, Rating),
    should_remove_movie(TID),
    retract(rating(TID, Rating)),
    fail.
clean_rating.

clean :-
    clean_region,
    clean_actor,
    clean_birth,
    clean_death,
    clean_genre,
    clean_language,
    clean_rating.
