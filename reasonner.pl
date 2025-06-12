:- dynamic edge/2.

% idade do actor na estreia de um filme
actor_age_at_release(Title, Name, Age) :-
    movie(TID, Title, ReleasedYear, _),
    actor(TID, AID),
    name(AID, Name),
    birth(AID, BirthYear),
    Age is ReleasedYear - BirthYear.

% media dos filmes em que um ator participou
average_actor_rating(Name, AvgRounded) :-
    name(AID, Name),
    findall(R, (actor(TID, AID), rating(TID, R)), Ratings),
    Ratings \= [],
    sum_list(Ratings, Sum),
    length(Ratings, Count),
    Avg is Sum / Count,
    Temp is Avg * 100,
    round(Temp, TempRounded),
    AvgRounded is TempRounded / 100.

% predicado se um ator esta vivo
alive(AID) :- death(AID,null).
dead(AID) :- death(AID,Year), Year \= null.

check_dead([]).
check_dead([AID|Actors]) :- dead(AID),check_dead(Actors).

% predicado se um filme tem todos os seus atores mortos
dead_movie(TID) :- findall(AID,actor(TID,AID),Actors),check_dead(Actors).

% top actor só participa em que filmes de rating elevado
top_actor(AID) :- actor(_,AID), \+ (actor(TID,AID),movie(TID,_,_,_),rating(TID,R), R < 7.5).

% filmes que dois atores colaboraram
collab_movie(AID,CAID,TID) :- actor(TID,AID),actor(TID,CAID).

collab(AID,CAID) :- actor(TID,AID),actor(TID,CAID),AID \= CAID.

% usar bfs - edge -> collab
erdos(X,Y,E) :- bfs(Y,5,[[X]],Path),!,length(Path,Len),E is Len-1.
erdos(_,_,E) :- E = "No connection".

bfs(Goal,_,[[Goal|Path]|_],[Goal|Path]).
bfs(Goal,Limit,[Path|Paths],Sol) :- length(Path,L),
                                    L =< Limit, 
                                    expand(Path,ExpPaths),
                                    append(Paths,ExpPaths,Paths2),
                                    bfs(Goal,Limit,Paths2,Sol),!.

check_collab(AID,CAID) :- (edge(AID,CAID) -> true ; 
                           actor(TID,AID),actor(TID,CAID),AID \= CAID,
                           assertz(edge(AID,CAID)),
                           assertz(edge(CAID,AID))
                           ).


expand([First|Path],ExpPaths) :- findall([Next,First|Path],(check_collab(First,Next),not(member(Next,[First|Path]))),ExpPaths).


actor_top_movies(AID,List) :- setof(Title,TID^(actor(TID,AID),rating(TID,R),R>=7.5,movie(TID,Title,_,_)),List).


only_lang_actor(AID) :- setof(Lang,TID^(actor(TID,AID),language(TID,Lang)),List),length(List,1).



actor_genres(AID,Genres) :- findall(Genre,(actor(TID,AID),genre(TID,Genre)),Genres).
actor_set_genres(AID,Genres) :- setof(Genre,TID^(actor(TID,AID),genre(TID,Genre)),Genres).

count_genres([], []).
count_genres([G|Gs], Counts) :-
    count_genres(Gs, RestCounts),
    ( select((G, N), RestCounts, RestWithoutG) ->
        N1 is N + 1,
        Counts = [(G, N1) | RestWithoutG]
    ; Counts = [(G, 1) | RestCounts]
    ).

max_genre([(Genre, Count)], Genre, Count).
max_genre([(G1, C1)|Rest], MaxGenre, MaxCount) :-
    max_genre(Rest, G2, C2),
    ( C1 >= C2 -> (MaxGenre = G1, MaxCount = C1)
                ; (MaxGenre = G2, MaxCount = C2)
    ).

mc_genre_actor(AID,Genre) :- actor_genres(AID,Genres),count_genres(Genres, CountList),max_genre(CountList, Genre, _).



set_movies(_,[],[]).
set_movies(AID,[Genre|Genres],[[Genre,Title]|List]) :- (actor(TID,AID),genre(TID,Genre),movie(TID,Title,_,_)),!,set_movies(AID,Genres,List).
genre_movie_actor(AID,List) :- actor_set_genres(AID,Genres),set_movies(AID,Genres,List).