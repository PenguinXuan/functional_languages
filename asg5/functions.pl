not( X ) :- X, !, fail.
not( _ ).

degrees_to_radians(degmin(Degrees, Minutes), Radians) :-
   Decimal_degrees is Degrees + Minutes / 60,
   Radians is Decimal_degrees * pi / 180.

haversine(Lat1, Lon1, Lat2, Lon2, Distance) :-
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    A is sin(Dlat / 2) ** 2 
         + cos(Lat1) * cos(Lat2)
         * sin(Dlon / 2) ** 2,
    C is 2 * atan2(sqrt(A), sqrt(1 - A)),
    Distance is 3961 * C.

distance(Airport1, Airport2, Distance) :-
    airport(Airport1, _, Lat1, Lon1),
    airport(Airport2, _, Lat2, Lon2),
    degrees_to_radians(Lat1, Rlat1),
    degrees_to_radians(Lat2, Rlat2),
    degrees_to_radians(Lon1, RLon1),
    degrees_to_radians(Lon2, RLon2),
    haversine(Rlat1, RLon1, Rlat2, RLon2, Distance).

timeInHours(time(Hours, Minutes), 
            DepartureTimeInHours) :-
    DepartureTimeInHours is Hours + Minutes / 60.

timeTraveled(Distance, Hours) :-
    Hours is Distance / 500.

flightTime(Departure, Arrival, TimeInHours) :-
    distance(Departure, Arrival, Distance),
    timeTraveled(Distance, Hours),
    TimeInHours is Hours.

printDigits(Digits) :-
    (Digits < 10 -> write(0), write(Digits); write(Digits)).

printTime(TimeInHours) :-
    Hours is floor(TimeInHours * 60) // 60,
    Minutes is floor(TimeInHours * 60) mod 60,
    printDigits(Hours), write(':'), printDigits(Minutes).

writepath([]) :-
    nl.

 writepath([flight(Node, Next, time(Hours, Minutes)) | Tail]) :-
    airport(Node, Departure, _, _),
    airport(Next, Arrival, _, _),
    write(' '), write('depart '), write(Node),
    write(' '), write(Departure), write(' '),
    timeInHours(time(Hours, Minutes), DepartureTimeInHours),
    printTime(DepartureTimeInHours),nl,
    write(' '), write('arrive '), write(Next),
    write(' '), write(Arrival), write(' '),
    flightTime(Node, Next, TimeInHours),
    ArrivalTime is DepartureTimeInHours + TimeInHours,
    printTime(ArrivalTime), nl,
    writepath(Tail).

listpath(Node, End, [flight(Node, Next, Time) | Outlist]) :-
   not(Node = End),
   flight(Node, Next, Time),
   listpath(Next, End, [flight(Node, Next, Time)], Outlist).

listpath(Node, Node, _, []).

listpath(Node, End,
        [flight(PrevNode, PrevArrival, time(H1, M1)) | Tried], 
        [flight(Node, Next, time(H2, M2)) | List]) :-
   flight(Node, Next, time(H2, M2)),
   flightTime(PrevNode, PrevArrival, T1),
   timeInHours(time(H1, M1), DepartureTimeInHours),
   ArrivalTime is DepartureTimeInHours + T1,
   timeInHours(time(H2, M2), NextDepartureTimeInHours),
   TransitTime is ArrivalTime + 0.5,
   TransitTime =< NextDepartureTimeInHours,
   flightTime(Node, Next, T2),
   T2 + NextDepartureTimeInHours < 24.0,
   TempTried = append([flight(PrevNode, 
                       PrevArrival, time(H1, M1))], Tried),
   not(member(Next, TempTried)),
   not(Next = PrevArrival),
   listpath(Next, End, 
           [flight(Node, Next, time(H2,M2)) | TempTried], List).

fly(Departure, Arrival) :-
    listpath(Departure, Arrival, List),
    writepath(List), !.

fly(Departure, Departure) :-
    write('Error: departure and arrival are the same airport.'), nl,
    !,fail.

fly(_, _) :-
    write('Error: nonexistent airports.'), nl,
    !,fail.