
suits([clubs, diamonds, hearts, spades]).

ranks([deuce, three, four, five, six, seven, eight, nine, ten, jack, queen, king, ace]).

suit(X) :- suits(S), member(X, S).
rank(X) :- ranks(S), member(X, S).
card((R, S)) :- rank(R), suit(S).



indexOf([Element|_], Element, 0). % Element in first index
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1), % Check in the tail of the list
  Index is Index1+1.  % and increment the resulting index


less_than_one(R1,R2) :-
	ranks(R),
	(indexOf(R,R1,RankValue1)),
	(indexOf(R,R2,RankValue2)),
	CheckValue is RankValue2 + 1,
	RankValue1 = CheckValue.

not_less_than_one(R1,R2) :-
	ranks(R),
	(indexOf(R,R1,RankValue1)),
	(indexOf(R,R2,RankValue2)),
	CheckValue is RankValue2 + 1,
	RankValue1 \== CheckValue.




greater_suit(S1, S2) :-
	suits(S),
    (indexOf(S,S1,SuitValue1)),
    (indexOf(S,S2,SuitValue2)),
	SuitValue1 > SuitValue2.

greater_rank(R1, R2) :-
    ranks(R),
    (indexOf(R,R1,RankValue1)),
    (indexOf(R,R2,RankValue2)),
	RankValue1 > RankValue2.


not_equal_suit(S1, S2) :-
	suits(S),
    (indexOf(S,S1,SuitValue1)),
    (indexOf(S,S2,SuitValue2)),
	SuitValue1 \== SuitValue2.

not_equal_rank(R1, R2) :-
    ranks(R),
    (indexOf(R,R1,RankValue1)),
    (indexOf(R,R2,RankValue2)),
	RankValue1 \== RankValue2.



equal_suit(S1, S2) :-
	suits(S),
    (indexOf(S,S1,SuitValue1)),
    (indexOf(S,S2,SuitValue2)),
	SuitValue1 = SuitValue2.


equal_rank(R1, R2) :-
    ranks(R),
    (indexOf(R,R1,RankValue1)),
    (indexOf(R,R2,RankValue2)),
	RankValue1 = RankValue2.


/* ORDER: "spades, hearts, diamonds, clubs" */
/* Succeeds if S1 is a lower suit than the suit S2. */
/* Is S1 < S2? */
lower_suit(S1, S2) :-
	suits(S),
    (indexOf(S,S1,SuitValue1)),
    (indexOf(S,S2,SuitValue2)),
	SuitValue1 < SuitValue2.


/* ORDER: "Ace = Highest, Deuce = Lowest" */
/* Succeeds if R1 is a lower rank than the rank R2. */

lower_rank(R1, R2) :-
    ranks(R),
    (indexOf(R,R1,RankValue1)),
    (indexOf(R,R2,RankValue2)),
	RankValue1 < RankValue2.


/* Succeeds if card (R1, S1) is lower than card (R2, S2) in the hand ordering.
*/
lower_card((R1, S1), (R2, S2)) :-

    %C1 rank AND suit lower than C2
    lower_rank(R1, R2),
    lower_suit(S1, S2);

    %C1 rank equal,suit lower than C2
    equal_rank(R1, R2),
    lower_suit(S1, S2);

    %%C1 suit eqial, rank lower than C2
    equal_suit(S1, S2),
    lower_rank(R1, R2);

    %C1 rank lower, suit greater than C2
    lower_rank(R1, R2),
    greater_suit(S1, S2).



len([], LenResult):-
    LenResult is 0.

len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.


sorted_hand(H, N) :-
    length(H,2),
    [FirstCard|TotalDeck] = H,
    [SecondCard|_] = TotalDeck,
    lower_card((SecondCard),(FirstCard));

    length(H,N),
    [FirstCard|TotalDeck] = H,
    [SecondCard|_] = TotalDeck,
    lower_card((SecondCard),(FirstCard)),
    len(TotalDeck,ListLength),
    sorted_hand(TotalDeck, ListLength).



/*
goal(Stuff):-
  do_something(X),
  if_something(Y)-> do_this(Y) ; true,
  always_do_this(Z).
*/




check_four(H) :-




	[(FirstCardRank,_)|TotalDeck] = H,
	[(SecondCardRank,_)|TotalDeck2] = TotalDeck,
	[(ThirdCardRank,_)|TotalDeck3] = TotalDeck2,
	[(FourthCardRank,_)|TotalDeck4] = TotalDeck3,
	[(FifthCardRank,_)|_] = TotalDeck4,

	FirstCardRank = SecondCardRank,
	SecondCardRank = ThirdCardRank,
	ThirdCardRank = FourthCardRank;





	[(FirstCardRank,_)|TotalDeck] = H,
	[(SecondCardRank,_)|TotalDeck2] = TotalDeck,
	[(ThirdCardRank,_)|TotalDeck3] = TotalDeck2,
	[(FourthCardRank,_)|TotalDeck4] = TotalDeck3,
	[(FifthCardRank,_)|_] = TotalDeck4,

	SecondCardRank = ThirdCardRank,
	ThirdCardRank = FourthCardRank,
	FourthCardRank = FifthCardRank.


%four_of_kind([(five,hearts),(four,spades),(four,hearts),(four,diamonds),(four,clubs)]).
%four_of_kind([(four,spades),(four,hearts),(four,diamonds),(four,clubs),(three,hearts)]).
%trace,findall(H, four_of_kind(H), _L), length(_L, Len).

four_of_kind(H) :-


  len(H,ListLength),
  MaxLength = 5,
  ListLength == MaxLength,!,
  sorted_hand(H, ListLength),
  check_four(H).






check_house(H) :-


    %This was made in an attempt to see if the
    %recursion version was faster or slower because
    % the test banks never completed.
    %I did NOT see that the tests were bugged




	%First three same, last two pair
	%This is how it's setup
	%[(FirstCardRank,FirstCardSuit)|TotalDeck] = H,
    %[(SecondCardRank,SecondCardSuit)|NextPair] = TotalDeck,
    %[(ThirdCardRank,ThirdCardSuit)|FinalTwoCardCheck] = NextPair,

    [(FirstCardRank,_)|TotalDeck] = H,
    [(SecondCardRank,_)|NextPair] = TotalDeck,
    [(ThirdCardRank,_)|FinalTwoCardCheck] = NextPair,
    FirstCardRank == SecondCardRank,
    ThirdCardRank == SecondCardRank,
    %Last two cards
    [(FirstCardRank2,_)|TotalDeck2] = FinalTwoCardCheck,
    [(SecondCardRank2,_)|_] = TotalDeck2,
    FirstCardRank2 == SecondCardRank2;


    %OR First 2 pair, last three same (reverse of above)

    [(FirstCardRank2,_)|TotalDeck2] = H,
    [(SecondCardRank2,_)|LastThree] = TotalDeck2,
    FirstCardRank2 == SecondCardRank2,


    %[(FirstCardRank,FirstCardSuit)|TotalDeck] = LastThree,
    %[(SecondCardRank,SecondCardSuit)|NextPair] = TotalDeck,
    %[(ThirdCardRank,ThirdCardSuit)|FinalTwoCardCheck] = NextPair,

    [(FirstCardRank,_)|TotalDeck] = LastThree,
    [(SecondCardRank,_)|NextPair] = TotalDeck,
    [(ThirdCardRank,_)|_] = NextPair,
    FirstCardRank == SecondCardRank,
    ThirdCardRank == SecondCardRank.




full_house(H) :-

    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,
    sorted_hand(H, ListLength),
    check_house(H).






%Checks to see if every value has the SAME suit
check_suits(H) :-


    %[(FirstCardRank,FirstCardSuit)|TotalDeck] = H,
    %[(SecondCardRank,SecondCardSuit)|_] = TotalDeck,
    length(H,2),
    [(_,FirstCardSuit)|TotalDeck] = H,
    [(_,SecondCardSuit)|_] = TotalDeck,
    equal_suit(FirstCardSuit,SecondCardSuit);


    [(_,FirstCardSuit)|TotalDeck] = H,
    [(_,SecondCardSuit)|_] = TotalDeck,
    equal_suit(FirstCardSuit,SecondCardSuit),
    check_suits(TotalDeck).







%check_order([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(deuce,hearts)]).

%check_not_less_than_one_order([(three, clubs),  (three, clubs),  (three, clubs),  (three, clubs)]).
%check_less_than_one_order([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(deuce,hearts)]).


%%check_not_less_than_one_order([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(deuce,hearts)]).
%check_flush([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(deuce,hearts)]).

%check_flush([(nine, clubs), (five, clubs),  (four, clubs),  (three, clubs),  (deuce, clubs)]).
%straight_flush([(five, clubs), (four, clubs),  (three, clubs),  (deuce, clubs),  (ace, clubs)]).
%check_straight_flush([(five, clubs), (four, clubs),  (three, clubs),  (deuce, clubs),  (ace, clubs)]).
%check_straight_flush([(nine, clubs), (five, clubs),  (four, clubs),  (three, clubs),  (deuce, clubs)]).



check_less_than_one_order(H) :-
    length(H,2),
    ([(TheRank,_)|RestOfCards]) = H,
    ([(TheRank2,_)|_]) = RestOfCards,
    less_than_one(TheRank,TheRank2);

    length(H,2),
    ([(TheRank,_)|RestOfCards]) = H,
    ([(TheRank2,_)|_]) = RestOfCards,
    TheRank1 = deuce,
    TheRank2 = ace;



    ([(TheRank,_)|RestOfCards]) = H,
    ([(TheRank2,_)|_]) = RestOfCards,
    less_than_one(TheRank,TheRank2),
    check_less_than_one_order(RestOfCards).



sorted_hand_flush(H, N) :-
    length(H,2),
    [FirstCard|TotalDeck] = H,
    [SecondCard|_] = TotalDeck,
    lower_card((SecondCard),(FirstCard));

    length(H,2),
    [(FirstCard,_)|TotalDeck] = H,
    [(SecondCard,_)|_] = TotalDeck,
    FirstCard = deuce,
    SecondCard = ace;

    length(H,N),
    [FirstCard|TotalDeck] = H,
    [SecondCard|_] = TotalDeck,
    
    lower_card((SecondCard),(FirstCard)),
    len(TotalDeck,ListLength),
    sorted_hand_flush(TotalDeck, ListLength).






check_straight_flush(H) :-
	check_suits(H),
        check_less_than_one_order(H).

check_valid_ranks_and_suits(H) :-
    length(H,2),
    ([(TheRank,TheSuit)|RestOfCards]) = H,
    ([(TheRank2,TheSuit2)|_]) = RestOfCards,
    suits(S),
    member(TheSuit,S),
    member(TheSuit2,S),
    ranks(R),
    member(TheRank,R),   
    member(TheRank2,R);   

    ([(TheRank,TheSuit)|RestOfCards]) = H,
    suits(S),
    member(TheSuit,S),
    ranks(R),
    member(TheRank,R),
    check_valid_ranks_and_suits(RestOfCards).

straight_flush(H) :-
    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,
    check_straight_flush(H),
    sorted_hand_flush(H, ListLength), check_valid_ranks_and_suits(H).
   



%check_ranks_not_consecutive([(five,hearts),(four,hearts),(three,hearts),(deuce,hearts),(nine,hearts)]).
%check_ranks_not_consecutive([(nine,hearts),(eight,hearts),(seven,hearts),(six,hearts),(five,hearts)]).
%check_ranks_not_consecutive([(five,hearts),(four,hearts),(three,hearts),(deuce,hearts),(ace,hearts)]).
%check_ranks_not_consecutive([(ace,hearts),(king,hearts),(queen,hearts),(jack,hearts),(ten,hearts)]).
%check_ranks_not_consecutive([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(deuce,hearts)]).
%check_ranks_not_consecutive([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(three,hearts)]).
%sorted_hand([(six,hearts),(five,hearts),(four,hearts),(three,hearts),(three,hearts)],5).




check_ranks_not_consecutive(H) :- 
  
    [(FirstCardRank,_)|TotalDeck] = H,
    [(SecondCardRank,_)|_] = TotalDeck,
    not_less_than_one(FirstCardRank,SecondCardRank);
   
    length(H,2),
    [(FirstCardRank,_)|TotalDeck] = H,
    [(SecondCardRank,_)|_] = TotalDeck,
    not_less_than_one(FirstCardRank,SecondCardRank),
    fail;

    [(FirstCardRank,_)|TotalDeck] = H,
    [(SecondCardRank,_)|_] = TotalDeck,
    less_than_one(FirstCardRank,SecondCardRank),
    check_ranks_not_consecutive(TotalDeck).



check_flush(H) :-

	check_suits(H),
        check_ranks_not_consecutive(H).

%flush([(five,hearts),(four,hearts),(three,hearts),(deuce,hearts),(nine,hearts)]).
%flush([(nine,hearts),(eight,hearts),(seven,hearts),(six,hearts),(five,hearts)]).



flush(H) :-
    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,  
    sorted_hand(H, ListLength),
    check_flush(H),
    check_valid_ranks_and_suits(H).

















%straight_sorted_hand([(five,hearts),(four,hearts),(three,hearts),(deuce,hearts),(ace,hearts)]).
%straight_sorted_hand([(five,hearts),(four,hearts),(three,hearts),(deuce,hearts),(ace,hearts)]).
%straight_sorted_hand([(ace,hearts),(king,hearts),(three,hearts),(deuce,hearts),(ace,hearts)]).
%straight_sorted_hand([(ace,hearts),(king,hearts),(queen,hearts),(jack,hearts),(ten,hearts)]).


straight_sorted_hand(H) :-
    length(H,2),
    ([(TheRank,_)|RestOfCards]) = H,
    ([(TheRank2,_)|_]) = RestOfCards,
    less_than_one(TheRank,TheRank2);

    %This part makes sure that if the last two cards are an ace and deuce, it's OKAY instead of failing
    length(H,2),
    ([(TheRank,_)|RestOfCards]) = H,
    ([(TheRank2,_)|_]) = RestOfCards,
    TheRank = deuce,
    TheRank2 = ace;

    ([(TheRank,_)|RestOfCards]) = H,
    ([(TheRank2,_)|_]) = RestOfCards,
    less_than_one(TheRank,TheRank2),
    straight_sorted_hand(RestOfCards).












%check_suits_all_different([(ace,hearts),(king,hearts),(queen,hearts),(jack,hearts),(ten,clubs)]).

check_suits_all_different(H) :- 
  

    [(_,FirstCardSuit)|TotalDeck] = H,
    [(_,SecondCardSuit)|_] = TotalDeck,

    not_equal_suit(FirstCardSuit,SecondCardSuit);
   
    length(H,2),
    [(_,FirstCardSuit)|TotalDeck] = H,
    [(_,SecondCardSuit)|_] = TotalDeck,

    not_equal_suit(FirstCardSuit,SecondCardSuit),
    fail;

    [(_,FirstCardSuit)|TotalDeck] = H,
    [(_,SecondCardSuit)|_] = TotalDeck,

    equal_suit(FirstCardSuit,SecondCardSuit),
    check_suits_all_different(TotalDeck).



check_if_straight(H) :-	
	check_suits_all_different(H),		%makes sure every suit is different / min. of 2 different suits	
	check_less_than_one_order(H).

straight(H) :-
    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,
    sorted_hand_flush(H, ListLength),
    
    check_if_straight(H), check_valid_ranks_and_suits(H).











%This program I found online counts the total occurances on a specified element in the given list
%Called by going count(ElementToLookFor,TheList,FinalTotalCount).
count(_, [], 0).
count(X, [(X,_) | T], N) :-
  !, count(X, T, N1),
  N is N1 + 1.
count(X, [_ | T], N) :-
  count(X, T, N).




/*
    [(FirstCardRank,_)|TotalDeck] = H,
    [(FirstCardRank1,_)|TotalDeck1] = TotalDeck,
    [(FirstCardRank2,_)|TotalDeck2] = TotalDeck1,
    [(FirstCardRank3,_)|TotalDeck3] = TotalDeck2,
    [(FirstCardRank4,_)|TotalDeck4] = TotalDeck3,
*/


last_two_not_pair(H) :-

    [(_)|TotalDeck] = H,
    [(_)|TotalDeck1] = TotalDeck,
    [(_)|TotalDeck2] = TotalDeck1,
    [(FirstCardRank3,_)|TotalDeck3] = TotalDeck2,
    [(FirstCardRank4,_)|_] = TotalDeck3,    
    not_equal_rank(FirstCardRank3,FirstCardRank4);
    fail.

first_two_not_pair(H) :-

    [(FirstCardRank,_)|TotalDeck] = H,
    [(FirstCardRank1,_)|TotalDeck1] = TotalDeck,
    not_equal_rank(FirstCardRank,FirstCardRank1);  
    fail.



find_three_pairs(H) :-



    [(FirstCardRank,_)|TotalDeck] = H,
    count(FirstCardRank,TotalDeck,Count1),
    Count1 == 2,
    last_two_not_pair(H),!;
     
    [(FirstCardRank,_)|TotalDeck] = H,
    count(FirstCardRank,TotalDeck,Count1),
    Count1 == 2,
    not(last_two_not_pair(H))
    -> fail,!;
     

    [(_)|TotalDeck] = H,
    [(FirstCardRank1,_)|TotalDeck1] = TotalDeck,
    count(FirstCardRank1,H,Count2),
    Count2 == 3,!;

   


    [(_)|TotalDeck] = H,
    [(_)|TotalDeck1] = TotalDeck,
    [(FirstCardRank2,_)|TotalDeck2] = TotalDeck1,
    count(FirstCardRank2,H,Count3), 
    Count3 == 3,
    first_two_not_pair(H),!;

    [(_)|TotalDeck] = H,
    [(_)|TotalDeck1] = TotalDeck,
    [(FirstCardRank2,_)|TotalDeck2] = TotalDeck1,
    count(FirstCardRank2,H,Count3), 
    Count3 == 3,
    not(first_two_not_pair(H))
    -> fail,!.




%FAILS
%three_of_kind([(four,spades),(three,spades),(three,hearts),(three,diamonds),(three,clubs)]).
%three_of_kind([(four,spades),(four,hearts),(four,diamonds),(three,spades),(three,clubs)]).
%three_of_kind([(four,spades),(four,hearts),(four,diamonds),(four,clubs),(three,clubs)]).
%three_of_kind([(nine,spades),(nine,hearts),(five,spades),(five,hearts),(five,clubs)]).

%PASS
%three_of_kind([(four,spades),(three,spades),(three,hearts),(three,diamonds),(deuce,clubs)]).
%three_of_kind([(four,spades),(four,hearts),(four,diamonds),(three,spades),(deuce,clubs)]).
%three_of_kind([(nine,spades),(seven,hearts),(five,spades),(five,hearts),(five,clubs)]).


three_of_kind(H) :-
    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,
    sorted_hand(H, ListLength),

    find_three_pairs(H),
    check_valid_ranks_and_suits(H).
    


find_two_pairs(H) :-


    [(FirstCardRank,_)|TotalDeck] = H,			%GET RANK OF 1ST CARD		
    count(FirstCardRank,H,Count1),			%COUNT TOTAL OF 1ST CARD		
    (Count1 == 2),					%IF IT'S 2, THEN GOOD PAIR	
    [(_)|TotalDeck1] = TotalDeck,			%
    [(FirstCardRank1,_)|TotalDeck2] = TotalDeck1,	%GET RANK OF 3RD CARD
    count(FirstCardRank1,H,Count2),			%COUNT TOTAL OF 3RD CARD		
    (Count2 == 2),!;					%IF IT'S 2, THEN GOOD PAIR	


		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [(FirstCardRank,_)|TotalDeck] = H,			%GET RANK OF 1ST CARD						
    [(_)|TotalDeck1] = TotalDeck,				
    [(_)|TotalDeck2] = TotalDeck1,			
    [(FirstCardRank2,_)|TotalDeck3] = TotalDeck2,	%GET RANK OF 4TH CARD
%   [(_)|TotalDeck4] = TotalDeck3,			
    count(FirstCardRank,H,Count1),			%COUNT TOTAL OF 1ST CARD		
    (Count1 == 2),					%IF IT'S 2, THEN GOOD PAIR	
    count(FirstCardRank2,H,Count3),			%COUNT TOTAL OF 4TH CARD	
    (Count3 == 2),!;					%IF IT'S 2, THEN GOOD PAIR	
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   
    [(_)|TotalDeck] = H,						
    [(FirstCardRank,_)|TotalDeck1] = TotalDeck,		%GET RANK OF 2ND CARD	
    [(_)|TotalDeck2] = TotalDeck1,
    [(FirstCardRank2,_)|TotalDeck3] = TotalDeck2,	%GET RANK OF 4TH CARD
    count(FirstCardRank,H,Count1),			%COUNT TOTAL OF 2ND CARD	
    (Count1 == 2),    					%IF IT'S 2, THEN GOOD PAIR	
    count(FirstCardRank2,H,Count2),			%COUNT TOTAL OF 4TH CARD	
    (Count2 == 2),!.					%IF IT'S 2, THEN GOOD PAIR	



%two_pair([(nine,spades),(seven,hearts),(five,spades),(five,hearts),(five,clubs)]).
%two_pair([(nine,spades),(nine,hearts),(five,spades),(five,hearts),(five,clubs)]).
%two_pair([(nine,spades),(seven,hearts),(five,spades),(five,hearts),(five,clubs)]).

%PASS
%two_pair([(nine,spades),(nine,hearts),(six,spades),(five,hearts),(five,clubs)]).

two_pair(H) :-
    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,
    sorted_hand(H, ListLength),
    find_two_pairs(H),
    check_valid_ranks_and_suits(H).









find_one_pair(H) :-
    length(H,2),
    [(FirstCardRank,_)|TotalDeck] = H,
    count(FirstCardRank,H,Count1),
    (Count1 == 2),
    !;


    [(FirstCardRank,_)|TotalDeck] = H,
    count(FirstCardRank,H,Count1),
    (Count1 == 2)->
    true,!;
    [(_)|TotalDeck] = H,
    find_one_pair(TotalDeck).



%%I didn't have time to check, but I'm assuming that if a hand does NOT HAVE A:
%	two pairs
%	three-of-a-kind
%	four-of-a-kind
%	Then every card rank is unique except for 1 single pair

one_pair(H) :-
    len(H,ListLength),
    MaxLength = 5,
    ListLength == MaxLength,!,
    sorted_hand(H, ListLength),

    not(full_house(H)),
    not(four_of_kind(H)),
    not(three_of_kind(H)),
    not(two_pair(H)),

    find_one_pair(H),
    check_valid_ranks_and_suits(H).












/* Query: query to test
   X: Term to solve in query
   Inf: number of inferences needed to execute the query
   Res: the variable representing the list of all solutions
   Test: query that must be true for Res for test to pass
*/
test(Query, X, Inf, Res, Test) :-
    statistics(inferences, I1),
    call(findall(X, Query, Res)),
    statistics(inferences, I2),
    Inf is I2 - I1,
    call(Test) -> 
    	(write('success '), write(Inf), nl, !) ;
    	(write('failure '), write(Res), nl, fail).
test(_, _, 0, _, _).


test_all(Inf) :-
    write('1. Card comparisons: '),
    /* How many cards are between eight of spades and jack of diamonds? */
	test((lower_card(C, (jack, diamonds)), lower_card((eight, spades), C)),
         C, Inf1, R1, length(R1, 9)),
    
    write('2. Sorting your hand: '),
    /* How many sorted five-card poker hands have the queen of hearts and then
     * the six of diamonds specifically as the fourth card? */ 
    test((H1 = [_, _, _, (six, diamonds), _], sorted_hand(H1, 5), member((queen, hearts), H1)),
         H1, Inf2, R2, length(R2, 8976)),
    
    write('3. Four of a kind: '),
    /* How many four of kind hands contain the jack of diamonds? */
    test((four_of_kind(H2), member((jack, diamonds), H2)), H2, Inf3, R3, length(R3, 60)),
    
    write('4. Full house: '),
    /* How many full houses have a diamond as second card, and jack of spades as third? */
    test((H3=[_, (_, diamonds), (jack, spades), _, _], full_house(H3)), H3, Inf4, R4,
         length(R4, 18)), 
    
    write('5. Flush: '),
    /* How many flushes have a ten as second card, and a six as a third card? */
    test((H4=[_, (ten, _), (six, _), _, _], flush(H4)), H4, Inf5, R5, length(R5, 96)),
    
    write('6. Straight: '),
    /* How many straights start and end with a diamond? */
    test((H5=[(_,diamonds),_,_,_,(_, diamonds)], straight(H5)), H5, Inf6, R6, length(R6, 630)),
    
    write('7. Straight flush: '),
    /* How many straight flushes do not contain an eight? */
    test((straight_flush(H6), not(member((eight, _), H6))), H6, Inf7, R7, length(R7, 20)),
    
    write('8. Three of a kind: '),
    /* How many three of a kind hands do not contain any spades? */
    test((three_of_kind(H7), not(member((_, spades), H7))), H7, Inf8, R8, length(R8, 7722)),
    
    write('9. One pair: '),
    /* How many hands that have one pair have the suit pattern HSHSH? */
    test((H8=[(_,hearts), (_,spades), (_,hearts), (_, spades), (_, hearts)], one_pair(H8)),
         H8, Inf9, R9, length(R9, 1430)),
    
    write('10. Two pair: '),
    /* How many sorted two pair hands have the suit pattern C*C*H ? */
    test((H9 = [(_, clubs),(_, _),(_, clubs),(_, _),(_, hearts)], two_pair(H9)), H9, Inf10,
         R10, length(R10, 858)),
    
    /* Total inferences */
    Inf is Inf1 + Inf2 + Inf3 + Inf4 + Inf5 + Inf6 + Inf7 + Inf8 + Inf9 + Inf10.


test_all2(Inf) :-
    write('1. Card comparisons: '),
    /* How many sorted hands contain at least one card from each suit? */
	test((sorted_hand(H1,5), memberchk((_,spades),H1), memberchk((_,hearts),H1), memberchk((_,diamonds),H1), (memberchk((_,clubs),H1))),
         H1, Inf1, R1, length(R1, 685464)),
    
    write('2. No pairs: '),
    /* How many hands made of cards lower than nine don't contain any pairs? */
    test((lower_rank(R, nine),H11=[(R,_)|_],sorted_hand(H11, 5), not(one_pair(H11) ; two_pair(H11) ; three_of_kind(H11) ; four_of_kind(H11) ; full_house(H11))),
         H11, Inf2, R2, length(R2, 21504)),
    
    write('3. Sorted hand: '),
    /* No sorted hand contains its first card later again. */
    test((sorted_hand([H2|T],5), member(H2, T)), H2, Inf3, R3, length(R3, 0)),
    
    write('4. Full house: '),
    /* How many full houses have seven of hearts as the middle card? */
    test((H3=[_, _, (seven, hearts), _, _], full_house(H3)), H3, Inf4, R4,
         length(R4, 42)), 
    
    write('5. Flush: '),
    /* How many inferences are needed to find out that a flush can't start and end with two different suits ? */
    test((HH = [(_,spades),_,_,_,(_,clubs)], flush(HH)),HH,Inf5,R5,length(R5, 0)),
    
    write('6. Straight: '),
    /* How many straights start with jack of spades? */
    test((H5=[(jack,spades),_,_,_,_], straight(H5)), H5, Inf6, R6, length(R6, 255)),
    
    write('7. Four of a kind: '),
    /* How many four of a kinds do not contain an eight? */
    test((four_of_kind(H6), not(member((eight, _), H6))), H6, Inf7, R7, length(R7, 528)),
    
    write('8. Three of a kind: '),
    /* How many three of a kinds have some higher and some lower additional card? */
    test( (H7 = [_,(R,_),(R,_),(R,_),_], three_of_kind(H7)), H7, Inf8, R8, length(R8, 18304)),
    
    write('9. One pair: '),
    /* No one pair is also two pair. */
    test((H8=[(_,diamonds),_,_,_,_],one_pair(H8), two_pair(H8)),
         H8, Inf9, R9, length(R9, 0)),
    
    write('10. High card: '),
    /* Using only cards lower than ten, how many hands have nothing better than a high card? */
    test((lower_rank(R, ten), X = [(R,_)|_],sorted_hand(X, 5), not(one_pair(X) ; two_pair(X) ; three_of_kind(X) ;
                                                                   four_of_kind(X) ; full_house(X) ; flush(X) ; straight(X) ;
                                                                   straight_flush(X))), X, Inf10,
         R10, length(R10, 53040)),
    
    /* Total inferences */
    Inf is Inf1 + Inf2 + Inf3 + Inf4 + Inf5 + Inf6 + Inf7 + Inf8 + Inf9 + Inf10.
