%% SUBMITTED BY ASHWIN VIJAYAKUMAR(50249042) , ARUN KRISHNAMURTHY(50247445)
%%
% ===========================================================
% Main loop:
% 1. Repeat "input-response" cycle until input starts with "bye"
%    Each "input-response" cycle consists of:
%		1.1 Reading an input string and convert it to a %tokenized list
%		1.2 Processing tokenized list
% ===========================================================

chat:-
 repeat,
   readinput(Input),
   process(Input),
  (Input = [bye| _] ),!.


% ===========================================================
% Read input:
% 1. Read char string from keyboard.
% 2. Convert char string to atom char list.
% 3. Convert char list to lower case.
% 4. Tokenize (based on spaces).
% ===========================================================

readinput(TokenList):-
   read_line_to_codes(user_input,InputString),
   string_to_atom(InputString,CharList),
   string_lower(CharList,LoweredCharList),
   tokenize_atom(LoweredCharList,TokenList).


% ===========================================================
%  Process tokenized input
% 1. Parse morphology and syntax, to obtain semantic representation
% 2. Evaluate input in the model
% If input starts with "bye" terminate.
% ===========================================================

process(Input):-
	parse(Input,SemanticRepresentation),
	modelchecker(SemanticRepresentation,Evaluation),
	respond(Evaluation),!,
	nl,nl.

process([bye|_]):-
   write('> bye!').


% ===========================================================
%  Parse:
% 1. Morphologically parse each token and tag it.
% 2. Add semantic representation to each tagged token
% 3. Obtain FOL representation for input sentence
% ===========================================================

%parse(Input, SemanticRepresentation):-
parse(String,S) :-
 leftcorner_recognize(S,String,[]).
 %numbervars(S,0,_),
 %write(S).


leftcorner_recognize(Cat,[Word|StringIn],StringOut) :-
 stem(Word,Stemmed), %Stem the word
 lex(WCat,Stemmed),
 complete(Cat,WCat,StringIn,StringOut).

complete(Cat,Cat,String,String).
complete(Cat,SubCat,StringIn,StringOut) :-
 rule(LHS,[SubCat|Cats]),
 matches(Cats,StringIn,String1),
 complete(Cat,LHS,String1,StringOut).

matches([],String,String).

matches([Cat|Cats],StringIn,StringOut) :-
 leftcorner_recognize(Cat,StringIn,String1),
 matches(Cats,String1,StringOut).



% ===========================================================
% Grammar
% 1. List of lemmas
% 2. Lexical items
% 3. Phrasal rules
% ===========================================================

% --------------------------------------------------------------------
% Lemmas are uninflected, except for irregular inflection
% lemma(+Lemma,+Category)
% --------------------------------------------------------------------

%Determiners exists
lemma(a,dtexists).
lemma(an,dtexists).
lemma(some,dtexists).

%Determiner forall
lemma(each,dtforall).
lemma(all,dtforall).
lemma(every,dtforall).

%Determiner the
lemma(the,dt).

%Proper nouns
lemma(mia,pn).
lemma(tom,pn).
lemma(sue,pn).
lemma(martin,pn).

%pronouns(similar to proper nouns)
lemma(i,pn).
lemma(he,pn).

%nouns
lemma(box,n).
lemma(bowl,n).
lemma(egg,n).
lemma(juice,n).
lemma(milk,n).
lemma(lettuce,n).
lemma(water,n).
lemma(bread,n).
lemma(chicken,n).
lemma(beef,n).
lemma(lamb,n).
lemma(meat,n).
lemma(bread,n).
lemma(salmon,n).
lemma(apple,n).
lemma(fish,n).
lemma(ham,n).
lemma(refrigerator,n).
lemma(freezer,n).
lemma(yesterday,n).
lemma(today,n).
lemma(tomorrow,n).


%quantifier lemmas
lemma(one,quant).
lemma(two,quant).
lemma(three,quant).
lemma(four,quant).
lemma(five,quant).
lemma(six,quant).
lemma(seven,quant).
lemma(eight,quant).
lemma(nine,quant).
lemma(ten,quant).
lemma(many,quant).
lemma(some,quant).

%adjectives
lemma(big,adj).
lemma(small,adj).
lemma(large,adj).
lemma(fresh,adj).
lemma(stale,adj).
lemma(old,adj).
lemma(smelly,adj).
lemma(red,adj).
lemma(blue,adj).
lemma(white,adj).
lemma(green,adj).
lemma(black,adj).
lemma(yellow,adj).
lemma(new,adj).
lemma(old,adj).

%Transitive verbs
lemma(ate,tv).
lemma(eat,tv).
lemma(like,tv).
lemma(drink,tv).
lemma(drank,tv).
lemma(drunk,tv).
lemma(drinking,tv).
lemma(contain,tv).
lemma(has,tv).
lemma(find,tv).
lemma(have,tv).
lemma(saw,tv).
lemma(see,tv).
lemma(belong,tv).

%Ditransitive verbs
lemma(put,dtv).

%Intransitive verbs
lemma(expire,iv).
%lemma(contains,iv) %to allow for questions

%Prepositions
lemma(in,p).
lemma(inside,p).
lemma(under,p).
lemma(near,p).
lemma(on,p).

%Be
lemma(is,be).
lemma(are,be).
lemma(was,be).

%Auxiliary words
lemma(did,aux).
lemma(does,aux).
lemma(is,aux).
lemma(can,aux).
lemma(will,aux).


%Vacuous There
lemma(there,there).

%Relative clauses
lemma(that,rec).

%Vacuous preposition
lemma(to,vacp).
lemma(in,vac_pp).
lemma(inside,vac_pp).
lemma(under,vac_pp).
lemma(near,vac_pp).
lemma(on,vac_pp).


%Questions
lemma(who,who).
lemma(which,which).
lemma(what,what).
lemma(where,where).



% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------


%Proper noun 
lex(pn((Lemma^X)^X),Lemma):-
 lemma(Lemma,pn).

%Noun
lex(n(X^P),Lemma):-
	lemma(Lemma,n),
	P=.. [Lemma,X].


%The
lex(dt((X^P)^(X^Q)^R),the):-
 %lemma(the,dt),
 R=.. [the,X,and(P,Q)].

%Adjective
lex(adj((X^P)^X^and(P,Q)),Lemma):-
 lemma(Lemma,adj),
 Q=.. [Lemma,X].


%Prepositions
lex(p((Y^R)^Q^(X^P)^and(P,Q)),Lemma):-
 lemma(Lemma,p),
 R =.. [Lemma,X,Y].

%Numerical quantifiers
lex(quant((X^P)^(X^Q)^R),Lemma):-
 lemma(Lemma,quant),
 R =.. [Lemma,X,(and(P,Q))].


%Universal quantifier
lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Lemma):-
		lemma(Lemma,dtforall).

%Existential quantifier
lex(dt((X^P)^(X^Q)^exists(X,and(P,Q))),Lemma):-
 lemma(Lemma,dtexists).


%Intransitive verb
lex(iv(X^P,[]),Lemma):-
 lemma(Lemma,iv),
 P=.. [Lemma,X].


%Transitive verb
lex(tv(X^Y^Z,[]),Lemma):-
 lemma(Lemma,tv),
 Z=.. [Lemma,X,Y].

%Ditransitive verbs
lex(dtv(X^Y^Z^P),Lemma):-
 lemma(Lemma,dtv),
 P =.. [Lemma,X,Y,Z].

%Questions
lex(whpr((X^P)^q(X,and(person(X),P))),Lemma):-
    lemma(Lemma,who).

lex(whpr((X^P)^q(X,and(object(X),P))),Lemma):-
    lemma(Lemma,what).

lex(whpr((X^P)^q(X,and(object(X),P))),Lemma):-
    lemma(Lemma,which).

lex(whpr((X^P)^q(X,and(location(X),P))),Lemma):-
    lemma(Lemma,where).

%Auxiliary word
lex(aux,Lemma):-
 lemma(Lemma,aux).

%There
lex(there,there).

%Belong
%lex(belong,Lemma):-
%    lemma(Lemma,belong).

%Of
lex(of,Lemma):-
    lemma(Lemma,of).


%Be like words
lex(be,Lemma):-
 lemma(Lemma,be).

%Relative
lex(rec,Lemma):-
 lemma(Lemma,rec).

%Vacuous preposition
lex(vacp,Lemma):-
 lemma(Lemma,vacp).
lex(vac_pp,Lemma):-
 lemma(Lemma,vac_pp).


% ...

% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------


% Stemmer implemented using word atom_concat

stem(Word,X):-
 atom_concat(X,Y,Word),
 lemma(X,_).

% ...

% --------------------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------

%rule(s(A^B^P,[]),[np(A),tv(X^Y^P,[]),np(B)]).
%rule(ynq(Z),[be, np(X^Y),pp((X^Y)^Z)]).
%rule(ynq(Y),[be, np(X^Y),adj(X)]).

%rule for dtv
rule(np(A),[vac_pp,np(A)]).
rule(vp(P^B, []), [dtv(P^X^Y^C), np((X^A)^B),np((Y^C)^A)]).

%rule for phrases such as "the ham in the box"
rule(s(Z),[np(X^Y),be,pp((X^Y)^Z)]).

rule(Z, [whpr((X^Y)^Z) , ynq(Y)]).  
rule(Z, [whpr((B^C)^Z),be,pp((A^B)^C)]).
rule(s(Y,WH),[np(X^Y),vp(X,WH)]).

%Rule for determiners
rule(np(C),[dt(A^C),n(A)]). 

%rule for quantification
rule(np(C),[quant(A^C),n(A)]).

%rule for phrases such as "the box contains the ham"
rule(vp(X^K,[]),[tv(X^Y,[]),np(Y^K)]).

%rule for prepositional phrases
rule(n(X^Z),[n(X^Y),pp((X^Y)^Z)]).
rule(pp(Z),[p(X^Y^Z),np(X^Y)]).

rule(np(A),[pn(A)]). 
rule(np((X^Q)^exists(X,and(P,Q))), [n(X^P)]).

%To handle transitive verbs in the form of "Does the ham....contain?"
rule(iv(X^Z, [Y]), [tv(X^Y^Z, [])]).
rule(tv(Y^Z, [X]), [tv(X^Y^Z, [])]).

%rule for adjectives
rule(n(Y),[adj(X^Y),n(X)]).

%rule(ynq(Y^Z^P),[aux, np(X^Y),tv(Y^Z^P,[])]).
%rule(inv_s(B,[X]),[whpr(X^Y),aux,pp(A^B)]).

%Whpr rules
rule(vp(X,WH),[iv(X,WH)]).
rule(Y,[whpr(X^Y),vp(X,[])]).
%rule(inv_s(X,Y^Z),[whpr(Y^Z), inv_s(Y,X)]).     %%%%
rule(inv_s(Y,[WH]),[aux, np(X^Y),vp(X,[WH])]).
rule(Z,[whpr((X^Y)^Z), inv_s(Y,[X])]).
rule(ynq(Y),[aux, np(X^Y),vp(X,[])]).
rule(ynq(C) ,[be,np(B^C)]).


%Added Whpr rules
%rule(inv_s(Y,X),[aux, np(X^Y),tv(X^Y^P,[])]).
%rule(inv_s(X,A^B),[whpr(X^Y),aux,pp(A^B)]).
%rule(Z,[whpr(X^Y^Z),ynq(Y^Z^P)]).               %%%%
%rule(ynq(Y),[be, np(X^Y),vp(X,[])]).

%rule(inv_s(B),[whpr(X^Y),vp(A^B)]).


%Rules for relative clauses
rule(rc(Y,[A]),[rec,np(X^Y),vp(X^A,[])]).
%rule(np(B^Y^Z),[np(A^B),rc(Z,[Y]),np(X^Y),tv(Z,[])]).
%rule(s(),[relc(),vp(,[])]).
%rule(s(),[relc(),pp((X^Y)^Z)]).
rule(n(X^and(Y, Z)), [n(X^Y), rc(X^Z, [])]).
rule(n(X^and(Y, Z)), [n(X^Y), rc(Z, [X])]).
rule(rc(P, [X]),[rec,s(P,[X])]).

%Rule for vacuous there, is ...
rule(np(X),[vacp,np(X)]).
rule(be,[be,there]).
rule(s(Y),[there,be,np(X^Y)]).


% ===========================================================
%  Modelchecker:
%  1. If input is a declarative, check if true
%  2. If input is a yes-no question, check if true
%  3. If input is a content question, find answer
% ===========================================================
%





model([box1, box2, box3, box4, egg1, egg2, ham1, ham2, bowl1, freezer1, sue1, sam1, container1, container2, shelf1, shelf2, shelf3, banana1, watermelon1, popsicle1, milk1],
      [[object, [box1, box2, box3, box4, egg1, egg2, ham1, ham2, bowl1, freezer1,container1, container2, shelf1, shelf2, shelf3, banana1, watermelon1, popsicle1, milk1]],
      [location, [box1, box2, box3, box4, bowl1, freezer1, container1, container2, shelf1, shelf2, shelf3]],
      [person, [sue1, sam1]],
      [box, [box1, box2, box3, box4]],
      [ham, [ham1, ham2]],
      [meat, [ham1, ham2]],
      [freezer, [freezer1]],
      [milk, [milk]],
      [almond, [milk1]],
      [skim, [milk1]],
      [container, [container1, container2]],
      [top, [shelf3]],
      [middle, [shelf1]],
      [bottom, [shelf2]],
      [sue, [sue1]],
      [sam, [sam1]],
      [blue, [box1]],
      [white, [box2, container1]],
      [green, [box3]],
      [yellow, [box4]],
      [bowl, [bowl1]],
      [egg, [egg1, egg2]],
      [banana, [banana1]],
      [watermelon, [watermelon1]],
      [popsicle, [popsicle1]],
      [belong, [box4, sue]],
      [contain, [[box1, ham1], [freezer, box4], [box1, egg1], [box1, egg2], [box1, egg1], [box1, egg2], [banana1, container1], [container1, shelf2]]]
      ]).

%=====================================================
%%Wrapper for sat/3

modelchecker(SemanticRepresentation,Y):-
 sat([],SemanticRepresentation,Y).

%Return if true
modelchecker(s(A,[]),[true_in_model]):-
 sat([],A,_),!.

%Return if false
modelchecker(s(A,[]),[false_in_model]).


%Return true if the ynq evaluates to true
modelchecker( ynq( A ), [yes_to_question]):-
   sat([],A,_),!.

%Return false if ynq evaluates to false
modelchecker(ynq(_),[no_to_question]).

%Question rules
modelchecker( q(_,B), [invalid_question]).

%modelchecker(q(A,B),[R]):-
% sat([],exists(A,B),[_|P]), %returns the answer to the question
% model(_,P),!.                       %extract answer from model
% member() 




% ==================================================
% Function i
% Determines the value of a variable/constant in an assignment G
% ==================================================

i(Var,G,Value):-
    var(Var),
    member([Var2,Value],G),
    Var == Var2.

i(C,_,Value):-
   nonvar(C),
   f(C,Value).


% ==================================================
% Function F
% Determines if a value is in the denotation of a Predicate/Relation
% ==================================================

f(Symbol,Value):-
   model(_,F),
    member([Symbol,ListOfValues],F),
    member(Value,ListOfValues).


% ==================================================
% Extension of a variable assignment
% ==================================================

extend(G,X,[ [X,Val] | G]):-
   model(D,_),
   member(Val,D).


% ==================================================
% Existential quantifier
% ==================================================

sat(G1,exists(X,Formula),G3):-
   extend(G1,X,G2),
   sat(G2,Formula,G3).



% ==================================================
% Definite quantifier (semantic rather than pragmatic account)
% ==================================================

 sat(G1,the(X,and(A,B)),G3):-
   sat(G1,exists(X,and(A,B)),G3),
   i(X,G3,Value),
   \+ ( ( sat(G1,exists(X,A),G2), i(X,G2,Value2), \+(Value = Value2)) ).




% ==================================================
% Negation
% ==================================================

sat(G,not(Formula2),G):-
   \+ sat(G,Formula2,_).

% ==================================================
% Universal quantifier
% ==================================================

sat(G, forall(X,Formula2),G):-
  sat(G,not( exists(X,not(Formula2) ) ),G).


% ==================================================
% Conjunction
% ==================================================

sat(G1,and(Formula1,Formula2),G3):-
  sat(G1,Formula1,G2),
  sat(G2,Formula2,G3).


% ==================================================
% Disjunction
% ==================================================


sat(G1,or(Formula1,Formula2),G2):-
  ( sat(G1,Formula1,G2) ;
    sat(G1,Formula2,G2) ).


% ==================================================
% Implication
% ==================================================

sat(G1,imp(Formula1,Formula2),G2):-
   sat(G1,or(not(Formula1),Formula2),G2).


% ===================================================
% Questions
% ==================================================



% ==================================================
% Predicates
% ==================================================

sat(G,Predicate,G):-
   Predicate =.. [P,Var],
   \+ (P = not),
   i(Var,G,Value),
   f(P,Value).

% ==================================================
% Two-place Relations
% ==================================================

sat(G,rec,G):-
   Rel =.. [R,Var1,Var2],
   \+ ( member(R,[exists,forall,and,or,imp,the]) ),
   i(Var1,G,Value1),
   i(Var2,G,Value2),
   f(R,[Value1,Value2]).


% ===========================================================
%  Respond
%  For each input type, react appropriately.
% ===========================================================


% Declarative statement is true
respond(Evaluation) :-
    Evaluation = [true_in_model],
    write('That is correct'),!.

% Declarative statement is not true
respond(Evaluation) :-
    Evaluation = [false_in_model],
    write('That is not correct'),!.

% Yes-NO question returns true
respond(Evaluation) :-
    Evaluation = [yes_to_question],
    write('Yes').

% Yes-No question returns false
respond(Evaluation) :-
    Evaluation = [no_to_question],
    write('No').

% Questions returning true value
respond(Evaluation) :-
    Evaluation = [answer],
    write(answer).

%Question returning false
respond(Evaluation) :-
    Evaluation = [invalid_question],
    write('You have entered an invalid question').

























