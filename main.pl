%****************************************************************************************
%  Thesis Work
%
%   Author:        Madeleine Malmsten
%   E-mail:        info@malmsten.eu
%   WWW:           http://www.malmsten.eu
%   Copyright (C): 2015, Madeleine Malmsten
%
%****************************************************************************************


:- module(chat_server,
	  [ server/0,
	    server/1,
	    create_chat_room/0
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(debug)).
:- use_module(library(http/http_files)).

:- use_module(hub).
:- use_module(library(threadutil)).

:- use_module(library(error)).

:- dynamic event/12.


server :-
	server(80).

server(Port) :-
	(   debugging(chat),
	    current_prolog_flag(gui, true)
	->  prolog_ide(thread_monitor)
	;   true
	),
	create_chat_room,
	http_server(http_dispatch, [port(Port)]),
	init_events("event.pl"),
	alarm(10,tracker(), _, [remove(true)]),
	alarm(1,sensorsimulator(), _, [remove(true)]),
	asserta(event(0,0,unknown,0)),
	asserta(status(0)).
	
%****************************************************************************************
%	Debug. (Restart Prolog after being used)
%		thread_signal(chatroom, (attach_console, trace)).
%****************************************************************************************	



%:- http_handler(root(.), serve_files, [prefix]).
:- http_handler(root(.), http_reply_from_files('assets', []), [prefix]).
:- http_handler(root(event),
		http_upgrade_to_websocket(
		    accept_chat,
		    [ guarded(false),
		      subprotocols([chat])
		    ]),
		[ id(chat_websocket)
		]).


serve_files(Request) :-
	 http_reply_from_files('assets', [], Request).
serve_files(Request) :-
	  http_404([], Request).


accept_chat(WebSocket) :-
	hub_add(chat, WebSocket, _Id).


:- dynamic
	visitor/1.

create_chat_room :-
	hub_create(chat, Room, _{}),
	thread_create(chatroom(Room), _, [alias(chatroom)]).

chatroom(Room) :-
	thread_get_message(Room.queues.event, Message),
	debug(chat, 'Got message ~p', [Message]),
	handle_message(Message, Room),
	chatroom(Room).



%****************************************************************************************
% Sensor simulator
%****************************************************************************************

sensorsimulator() :-
	X is random(2),
	debug(chat, 'Simulator says ~p', [X]),
	retractall(status(_)),
	asserta(status(X)),
	event(Eventint1,_,_,_),
	assertevents(X,Eventint1),
	alarm(10,sensorsimulator(), _Id, [remove(true)]).

%****************************************************************************************
% Tracker
% This predicate keeps tracking the event clauses for unusual things that should not happen or
% for things that has not happened but should happen.
%****************************************************************************************

tracker() :-
	format(atom(Javascript), 'correctPattern();', []), % remove brokenPattern
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	fail.

% If the stove is off, predicate tracking if event has happened about the same time before
% no matter which week or weekday. If true, send a warning to GUI.
tracker() :-
	status(0),	
	get_time(Timestamp),
	debug(chat, 'Trackertime ~p', [Timestamp]),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	findall(Starttime,(
		event(0,Starttime,Message,Endtime),
		\+(Message == ignore),
		checktime(Starttime, Endtime, Currenthour, Currentminute) % At this time of day the stove is usually on, but since status is zero it is not on now
		), List),
	length(List,Length),
	Length > 1,	
	debug(chat, 'Found ~p events (must be more than 1)', [Length]),
	format(atom(Javascript), 'brokenPattern();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),	
	debug(chat, 'Borde maskinen inte vara på???', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% All good
tracker() :-
	status(0),	
	debug(chat, 'All good', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.


% If the stove is on, predicate tracking if event
% has happened about the same time before (+/- 30 minutes) and consider weekday
tracker() :-
	status(1),	
	get_time(Timestamp),
	debug(chat, 'Trackertime ~p', [Timestamp]),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	event(0,Starttime,Message,Endtime),
	\+(Message == ignore),
	checkweeknumber(Timestamp, Starttime),
	checkdayofweek(Timestamp, Starttime),
	checktime(Starttime, Endtime, Currenthour, Currentminute),
	debug(chat, 'This event has happened this time before considering time of day, week number AND weekday', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider week number
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_Message,_Endtime),Elist),
	length(Elist,Length),
	Length < 40,
	get_time(Timestamp),
	debug(chat, 'Trackertime ~p', [Timestamp]),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	event(0,Starttime,Message,Endtime),
	\+(Message == ignore) ,
	checkdayofweek(Timestamp, Starttime),
	checktime(Starttime, Endtime, Currenthour, Currentminute),
	debug(chat, 'This event has happened this time before considering time of day AND weekday', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider week number OR weekday
tracker() :-
	status(1),
	findall(Starttime,event(0,Starttime,_Message,_Endtime),Elist),
	length(Elist,Length),
	Length < 25,
	get_time(Timestamp),
	debug(chat, 'Trackertime ~p', [Timestamp]),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	event(0,Starttime,Message,Endtime),
	\+(Message == ignore) ,
	checktime(Starttime, Endtime, Currenthour, Currentminute),
	debug(chat, 'This event has happened this time before considering time of day', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.



% TODO! Lägg till en findall
% If the stove is on, calculate if event is at least 50% longer than normal consider time of day, odd/even week AND weekday
tracker() :-
	status(1),	
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,_Endtime),
	event(0,Starttime,Oldmessage,Endtime),
	\+(Thismessage == ignore),
	\+(Oldmessage == ignore),
	checkweeknumber(Currentstarttime, Starttime),
	checkdayofweek(Currentstarttime, Starttime),
	Currentsession = Timestamp - Currentstarttime,
	Eventsession = Endtime - Starttime,
	Currentsession < Eventsession * 1.5,
	debug(chat, 'Not longer time running than normal considering time of day AND weekday', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider odd/even week
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_,_),Elist),
	length(Elist,Length),
	Length < 40,
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,_Endtime),
	event(0,Starttime,Oldmessage,Endtime),
	\+(Thismessage == ignore),
	\+(Oldmessage == ignore),
	checkdayofweek(Currentstarttime, Starttime),
	Currentsession = Timestamp - Currentstarttime,
	Eventsession = Endtime - Starttime,
	Currentsession < Eventsession * 1.5,
	debug(chat, 'Not longer time running than normal considering time of day AND weekday', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider odd/even week OR weekday
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_,_),Elist),
	length(Elist,Length),
	Length < 30,
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,_Endtime),
	event(0,Starttime,Oldmessage,Endtime),
	\+(Thismessage == ignore),
	\+(Oldmessage == ignore),
	Currentsession = Timestamp - Currentstarttime,
	Eventsession = Endtime - Starttime,
	Currentsession < Eventsession * 1.5,
	debug(chat, 'Not longer time running than normal considering time of day', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider weekday, odd/even week OR time
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_,_),Elist),
	length(Elist,Length),
	Length < 15,
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,_Endtime),
	event(0,Starttime,Oldmessage,Endtime),
	\+(Thismessage == ignore),
	\+(Oldmessage == ignore),
	Currentsession = Timestamp - Currentstarttime,
	Eventsession = Endtime - Starttime,
	Currentsession < Eventsession * 1.5,
	debug(chat, 'Not longer time running than normal', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Fail backtracking
tracker() :-
	status(1),	
	debug(chat, 'Tracker FAIL!', []),
	format(atom(Javascript), 'brokenPattern();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),	
	alarm(10,tracker(), _Id, [remove(true)]),!.


%****************************************************************************************
% Check time, check week
%****************************************************************************************
checktime(Starttime, Endtime, Currenthour, Currentminute) :-
	stamp_date_time(Starttime, date(_, _, _, Eventstarthour, Eventstartminute, _, _, _, _), 'UTC'),
	stamp_date_time(Endtime, date(_, _, _, Eventendhour, Eventendminute, _, _, _, _), 'UTC'),
	Startofevent = Eventstarthour * 60 + Eventstartminute,
	Endofevent = Eventendhour * 60 + Eventendminute,
	Nowinminutes = Currenthour * 60 + Currentminute,
	Nowinminutes > Startofevent - 30,
	Nowinminutes < Endofevent + 30.

checkweeknumber(Timestamp, Starttime) :-
	format_time(atom(TWN), '%V', Timestamp), % Set week number and check odd/ even
	atom_number(TWN, Thisweeknumber),
	format_time(atom(EWD), '%V', Starttime),
	atom_number(EWD, Eventweeknumber),
	Evenodd is Thisweeknumber + Eventweeknumber,
	Evenodd mod 2 =:= 0.

checkdayofweek(Timestamp, Starttime) :-
	format_time(atom(TDW), '%u', Timestamp),
	atom_number(TDW, Thisdow),
	format_time(atom(EWD), '%u', Starttime),
	atom_number(EWD, Eventdow),
	Thisdow == Eventdow.

%****************************************************************************************
% The time status predicate (recursive), that will be runned every second to update the GUI
% If the stove is off, calculate and print next start time no matter which week or weekday.
%****************************************************************************************
timestatus(Client) :-
	status(0),
	get_time(Now),
	debug(chat, 'Time status 0', []),
	setof(Seconds,(
		event(0,Starttime,Message,_Endtime),
		\+(Message == ignore),
		stamp_date_time(Starttime, date(_, _, _, Eventstarthour, Eventstartminute, Eventstartsecond, _, _, _), 'UTC'),
		stamp_date_time(Now, date(_, _, _, Timestamphour, Timestampminute, Timestampsecond, _, _, _), 'UTC'),
		Nowseconds = ((Timestamphour * 60) + Timestampminute) * 60 + Timestampsecond,
		Eventseconds = ((Eventstarthour * 60) + Eventstartminute) * 60 + Eventstartsecond,
		Eventseconds > Nowseconds,
		Seconds is Eventseconds - Nowseconds
	), List),
	length(List,Length),
	debug(chat, 'Length is ~p', [Length]),	
	reverse(List,List1),
	[Head|_Tail] = List1,
    format(atom(Javascript), 'nextStart("~f")', [Head]),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	alarm(1,timestatus(Client), _Id, [remove(true)]).

timestatus(Client) :-
	status(1),
	get_time(Now), 
	debug(chat, 'Time status 1', []),
	event(_Status,Oldtime,_Message,_),
	Timestamp is Now - Oldtime, 
	stamp_date_time(Timestamp, date(_, _, _, H, M, S, _, _, _), 'UTC'),
    format(atom(Javascript), 'document.getElementById("info").innerHTML = "~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~0f~2+";', [H,M,S]),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	alarm(1,timestatus(Client), _Id, [remove(true)]).

timestatus(Client) :-
	debug(chat, 'Time status fails', []),
    format(atom(Javascript), 'document.getElementById("info").innerHTML = "00:00:00";', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	alarm(1,timestatus(Client), _Id, [remove(true)]).

%****************************************************************************************
% Get message from GUI
%****************************************************************************************

handle_message(Message, Room) :- 
	websocket{client:Client,data:Data,format:string,hub:chat,opcode:text} :< Message,
	atom_string(Data1,Data),
	json:atom_json_dict(Data1, Json, []),
	handle_json_message(Json, Client, Room).
handle_message(Message, Room) :- 
	websocket{client:Client,data:Data,format:string,hub:chat,opcode:text} :< Message,
	json:atom_json_dict(Data, Json, []),
	handle_json_message(Json, Client, Room).
handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).


%****************************************************************************************
% Make JSON from message
%****************************************************************************************

handle_json_message(_{pid:"event",type:"make",values:[]}, _Client, _Room) :- % Web page opened
	debug(chat, 'Make recieved.', []),
	format(atom(Javascript), 'login();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).

handle_json_message(_{status:Status,id:1992}, _Client, _Room) :- % Connection from Raspberry Pi with id 1
	debug(chat, 'Connection from Raspberry Pi. ~p', [Status]). 

handle_json_message(_{pid:"chat",type:"post",values:[Input]}, Client, _Room) :- % Successfull sign in
	Input == "login1992",
	alarm(1,timestatus(Client), _Id, [remove(true)]),
	format(atom(Javascript), 'loginSuccess();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).

handle_json_message(_{pid:"chat",type:"post",values:[Input]}, Client, _Room) :- % Sign in fails!!!
	Input == "login",
	format(atom(Javascript), 'loginFail();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).


handle_json_message(_{pid:"chat",type:"post",values:[Input]}, Client, Room) :- % Turn off stove
	Input == "stop",
	Event = event(Status,Oldtime,_Message,Endtime),
	retract(Event),
	Updateevent = event(Status,Oldtime,ignore,Endtime),
	asserta(Updateevent),
	format(atom(Json), '{command : stop}', []),
	hub_broadcast(Room.name, websocket{client:Client,data:Json,format:string,hub:chat,opcode:text}),
	format(atom(Javascript), 'printMessage("stop");', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),!.

handle_json_message(_{pid:"chat",type:"post",values:[Input]}, Client, _Room) :- % Ignore this event in list
	Input == "ignore",
	Event = event(Status,Oldtime,_Message,Endtime),
	retract(Event),
	Updateevent = event(Status,Oldtime,ignore,Endtime),
	asserta(Updateevent),
	format(atom(Javascript), 'printMessage("ignore");', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),!.

handle_json_message(_{pid:"chat",type:"post",values:[Input]}, Client, _Room) :- % The event is correct
	Input == "correct",
	format(atom(Javascript), 'printMessage("correct");', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),!.

handle_json_message(_, _, _) :-
	debug(chat, 'Ooops', []).

%****************************************************************************************
% Add event to event clauses list (in event.pl) if status have changed
%****************************************************************************************

assertevents(Eventint,Eventint) :-
	debug(chat, 'Same as before', []).

assertevents(1, _Eventint) :-
		debug(chat, 'New eventint where status is 1', []),
		get_time(Timestamp),
		Evt = event(1,Timestamp,unknown,0),
		debug(chat, 'Starttime ~p', [Timestamp]),
		asserta(Evt).
		
assertevents(0, _Eventint) :-
		debug(chat, 'New eventint where status is 0', []),
		Event = event(1,Oldtime,Message,0),
		retract(Event),
		get_time(Timestamp),
		Updateevent = event(0,Oldtime,Message,Timestamp),
		debug(chat, 'Starttime ~p', [Oldtime]),
		debug(chat, 'Endtime ~p', [Timestamp]),
		asserta(Updateevent).
		%open('event.pl', append, Stream),
		%write(Stream, Updateevent),
		%write(Stream, '.'),
		%nl(Stream),
		%flush_output(Stream),
		%debug(chat, 'Closing ~p', [Stream]),
		%close(Stream).
		
%****************************************************************************************
% Read events from file
%****************************************************************************************

init_events(File) :-
        retractall(event(_,_,_,_)),
        open(File, read, Stream),
        call_cleanup(load_event(Stream),
                     close(Stream)).

load_event(Stream) :-
        read(Stream, T0),
        load_event(T0, Stream).

load_event(end_of_file, _) :- !.
load_event(event(Status,Startstamp,Normal,Endstamp), Stream) :- !,
        assertz(event(Status,Startstamp,Normal,Endstamp)),
        read(Stream, T2),
        load_event(T2, Stream).
load_event(Term, _Stream) :-
        type_error(event, Term).