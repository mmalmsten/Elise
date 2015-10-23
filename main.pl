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
	init_emails("email.pl"),
	alarm(10,tracker(), _, [remove(true)]),
	alarm(1,sensorsimulator(), _, [remove(true)]),
	asserta(event(0,0,unknown,0)),
	asserta(email("test@easyrider.nu")),
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
	%debug(chat, 'Got message ~p', [Message]),
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
	hub_broadcast(chat, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	fail.

% If the stove is off, predicate tracking if event has happened about the same time before
% no matter which week or weekday. If true, send a warning to GUI.
tracker() :-
	status(0),	
	get_time(Timestamp),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	findall(Starttime,(
		event(0,Starttime,Message,Endtime),
		\+(Message == ignore),
		checktime(Starttime, Endtime, Currenthour, Currentminute) % At this time of day the stove is usually on, but since status is zero it is not on now
		), List),
	length(List,Length),
	Length > 1,	
	format(atom(Javascript), 'brokenPattern();', []),
	hub_broadcast(chat, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
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
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	event(0,Starttime,Message,Endtime),
	\+(Message == ignore) ,
	checktime(Starttime, Endtime, Currenthour, Currentminute),
	debug(chat, 'This event has happened this time before considering time of day', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.



% If the stove is on, calculate if event is at least 50% longer than normal consider time of day, odd/even week AND weekday
tracker() :-
	status(1),	
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,Currentendtime),
	\+(Thismessage == ignore),
	findall(Starttime,(
		event(0,Starttime,Oldmessage,Endtime),
		\+(Oldmessage == ignore),
		checkweeknumber(Currentstarttime, Starttime),
		checkdayofweek(Currentstarttime, Starttime),
		checktime(Starttime, Endtime, Currentstarttime, Currentendtime),
		Currentsession = Timestamp - Currentstarttime,
		Eventsession = Endtime - Starttime,
		Currentsession < Eventsession * 1.5
	), _List),
	debug(chat, 'Not longer time running than normal considering time of day AND weekday', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider odd/even week
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_,_),Elist),
	length(Elist,Length),
	Length < 40,
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,Currentendtime),
	\+(Thismessage == ignore),
	findall(Starttime,(
		event(0,Starttime,Oldmessage,Endtime),
		\+(Oldmessage == ignore),
		checkdayofweek(Currentstarttime, Starttime),
		checktime(Starttime, Endtime, Currentstarttime, Currentendtime),
		Currentsession = Timestamp - Currentstarttime,
		Eventsession = Endtime - Starttime,
		Currentsession < Eventsession * 1.5
	), _List),
	debug(chat, 'Not longer time running than normal considering time of day AND weekday', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider odd/even week OR weekday
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_,_),Elist),
	length(Elist,Length),
	Length < 30,
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,Currentendtime),
	\+(Thismessage == ignore),
	findall(Starttime,(
		event(0,Starttime,Oldmessage,Endtime),
		\+(Oldmessage == ignore),
		checktime(Starttime, Endtime, Currentstarttime, Currentendtime),
		Currentsession = Timestamp - Currentstarttime,
		Eventsession = Endtime - Starttime,
		Currentsession < Eventsession * 1.5
	), _List),
	debug(chat, 'Not longer time running than normal considering time of day', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Same as above, but do NOT consider weekday, odd/even week OR time
tracker() :-
	status(1),	
	findall(Starttime,event(0,Starttime,_,_),Elist),
	length(Elist,Length),
	Length < 15,
	get_time(Timestamp),
	event(1,Currentstarttime,Thismessage,_Currentendtime),
	\+(Thismessage == ignore),
	findall(Starttime,(
		event(0,Starttime,Oldmessage,Endtime),
		\+(Oldmessage == ignore),
		Currentsession = Timestamp - Currentstarttime,
		Eventsession = Endtime - Starttime,
		Currentsession < Eventsession * 1.5
	), _List),
	debug(chat, 'Not longer time running than normal', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Fail backtracking
tracker() :-
	status(1),	
	debug(chat, 'Tracker FAIL!', []),
	format(atom(Javascript), 'brokenPattern();', []),
	hub_broadcast(chat, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
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
	status(1),
	get_time(Now), 
	debug(chat, 'timestatus 1', []),
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

handle_json_message(_{pid:"event",type:"make",values:[]}, Client, _Room) :- % Web page opened
	debug(chat, 'Make recieved ~p.', [Client]),
	format(atom(Javascript), 'login();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).

handle_json_message(_{status:0,id:_Id}, _Client, _Room) :- % Connection from Raspberry Pi 
	retractall(status(_)),
	asserta(status(0)),
	event(Eventint1,_,_,_),
	assertevents(0,Eventint1),
	debug(chat, 'Connection from Raspberry Pi. 0 ~p ~p', [0,Eventint1]). 

handle_json_message(_{status:_Status,id:_Id}, _Client, _Room) :- % Connection from Raspberry Pi 
	debug(chat, 'Status 1 from pi', []),
	retractall(status(_)),
	asserta(status(1)),
	event(Eventint1,_,_,_),
	assertevents(1,Eventint1),
	debug(chat, 'Connection from Raspberry Pi. ~p ~p', [1,Eventint1]). 

handle_json_message(_{pid:"chat",type:"post",values:["login",Email,"1992"]}, Client, _Room) :- % Successfull sign in
	sub_string(Email, _, _, _, "@"),
	assertemails(Email),
	alarm(1,timestatus(Client), _Id, [remove(true)]),
	format(atom(Javascript), 'loginSuccess();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).

handle_json_message(_{pid:"chat",type:"post",values:["login",_Email,_Pass]}, Client, _Room) :- % Sign in fails!!!
	format(atom(Javascript), 'loginFail();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).

handle_json_message(_{pid:"chat",type:"post",values:["stop",_Email,"1992"]}, Client, Room) :-
	Event = event(Status,Oldtime,_Message,Endtime),
	retract(Event),
	Updateevent = event(Status,Oldtime,ignore,Endtime),
	asserta(Updateevent),
	format(atom(Json), '$', []), % Send a $ to Raspberry Pi
	hub_broadcast(Room.name, websocket{client:Client,data:Json,format:string,hub:chat,opcode:text}),
	format(atom(Javascript), 'printMessage("stop");', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),!.

handle_json_message(_{pid:"chat",type:"post",values:["ignore",_Email,"1992"]}, Client, _Room) :-
	Event = event(Status,Oldtime,_Message,Endtime),
	retract(Event),
	Updateevent = event(Status,Oldtime,ignore,Endtime),
	asserta(Updateevent),
	format(atom(Javascript), 'printMessage("ignore");', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),!.

handle_json_message(_{pid:"chat",type:"post",values:["correct",_Email,"1992"]}, Client, _Room) :-
	format(atom(Javascript), 'printMessage("correct");', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),!.

handle_json_message(_, _, _) :-
	debug(chat, 'Ooops', []).

%****************************************************************************************
% Add event to event clauses list (in event.pl) if status have changed
%****************************************************************************************

assertevents(Eventint,Eventint).

assertevents(1, _Eventint) :-
		debug(chat, 'assertevents status 1', []),
		get_time(Timestamp),
		Evt = event(1,Timestamp,unknown,0),
		asserta(Evt).
		
assertevents(0, _Eventint) :-
		debug(chat, 'assertevents status 0', []),
		Event = event(1,Oldtime,Message,0),
		retract(Event),
		get_time(Timestamp),
		Updateevent = event(0,Oldtime,Message,Timestamp),
		asserta(Updateevent),
		open('event.pl', append, Stream),
		write(Stream, Updateevent),
		write(Stream, '.'),
		nl(Stream),
		flush_output(Stream),
		debug(chat, 'Closing ~p', [Stream]),
		close(Stream).
		
%****************************************************************************************
% If not already added, add email to email clauses list (later saved to email.pl)
%****************************************************************************************

assertemails(Email) :-
		email(Email),
		debug(chat, 'Email already exists', []).

assertemails(Email) :-
		Mail = email(Email),
		asserta(Mail),
		open('email.pl', append, Stream),
		write(Stream, 'email("'),
		write(Stream, Email),
		write(Stream, '").'),
		nl(Stream),
		flush_output(Stream),
		debug(chat, 'Closing ~p', [Stream]),
		close(Stream),
		debug(chat, 'New email added', []).

assertemails(_Email) :-
		debug(chat, 'Bad email', []).
		
		
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


%****************************************************************************************
% Read emails from file
%****************************************************************************************

init_emails(File) :-
        retractall(email(_)),
        open(File, read, Stream),
        call_cleanup(load_email(Stream),
        			 close(Stream)).

load_email(Stream) :-
        read(Stream, T0),
        load_email(T0, Stream).

load_email(end_of_file, _) :- !.
load_email(email(Email), Stream) :- !,
        assertz(email(Email)),
        read(Stream, T2),
        load_email(T2, Stream).
load_email(Term, _Stream) :-
        type_error(email, Term).