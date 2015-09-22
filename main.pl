%****************************************************************************************
%  Examensarbete
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
%****************************************************************************************

% Funktion som trackar om händelsen ägt rum ungefär samma tid (+/- 30min t.ex.), oavsett datum
tracker() :-
	status(1),	
	get_time(Timestamp),
	debug(chat, 'Trackertime ~p', [Timestamp]),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	event(0,Starttime,Message,Endtime),
	\+(Message == ignore) ,
	stamp_date_time(Starttime, date(_, _, _, Eventstarthour, Eventstartminute, _, _, _, _), 'UTC'),
	stamp_date_time(Endtime, date(_, _, _, Eventendhour, Eventendminute, _, _, _, _), 'UTC'),
	Startofevent = Eventstarthour * 60 + Eventstartminute,
	Endofevent = Eventendhour * 60 + Eventendminute,
	Nowinminutes = Currenthour * 60 + Currentminute,
	Nowinminutes > Startofevent - 30,
	Nowinminutes < Endofevent + 30,
	debug(chat, 'Success ~p', [Nowinminutes]),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Om maskinen är av, en funktion som trackar om händelsen ägt rum ungefär samma tid (+/- 30min t.ex.), oavsett datum, och skickar varning till GUI om det är så
tracker() :-
	status(0),	
	get_time(Timestamp),
	debug(chat, 'Trackertime ~p', [Timestamp]),
	stamp_date_time(Timestamp, date(_, _, _, Currenthour, Currentminute, _, _, _, _), 'UTC'),
	event(0,Starttime,Message,Endtime),
	\+(Message == ignore) ,
	stamp_date_time(Starttime, date(_, _, _, Eventstarthour, Eventstartminute, _, _, _, _), 'UTC'),
	stamp_date_time(Endtime, date(_, _, _, Eventendhour, Eventendminute, _, _, _, _), 'UTC'),
	Startofevent = Eventstarthour * 60 + Eventstartminute,
	Endofevent = Eventendhour * 60 + Eventendminute,
	Nowinminutes = Currenthour * 60 + Currentminute,
	Nowinminutes > Startofevent,
	Nowinminutes < Endofevent,
	format(atom(Javascript), 'brokenPattern();', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),	
	debug(chat, 'Borde maskinen inte vara på???', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.

% Om maskinen är på, funktion som trackar om maskinen varit på längre tid än normalt
tracker() :-
	status(1),	
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

% All good
tracker() :-
	status(0),	
	debug(chat, 'All good', []),
	alarm(10,tracker(), _Id, [remove(true)]),!.


%****************************************************************************************
% The time status predicate (recursive), that will be runned every second to update the GUI
%****************************************************************************************
timestatus(Client) :-
	status(0),
	format(atom(Javascript), 'document.getElementById("info").innerHTML = "00:00:00";', []),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	%debug(chat, 'Sending ~p', [Javascript]),
	alarm(1,timestatus(Client), _Id, [remove(true)]).

timestatus(Client) :-
	get_time(Now), 
	event(_Status,Oldtime,_Message,_),
	Timestamp is Now - Oldtime, 
	stamp_date_time(Timestamp, date(_, _, _, H, M, S, _, _, _), 'UTC'),
	%debug(chat, 'Running alarm', []),
    format(atom(Javascript), 'document.getElementById("info").innerHTML = "~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~0f~2+";', [H,M,S]),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	%debug(chat, 'Sending ~p', [Javascript]),
	alarm(1,timestatus(Client), _Id, [remove(true)]).

%****************************************************************************************
% Get message from GUI
%****************************************************************************************

handle_message(Message, Room) :- 
	websocket{client:Client,data:Data,format:string,hub:chat,opcode:text} :< Message, !,
	atom_string(Data1,Data),
	json:atom_json_dict(Data1, Json, []),
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
	alarm(1,timestatus(Client), _Id, [remove(true)]),
	debug(chat, 'Make recieved.', []).
handle_json_message(_{pid:"chat",type:"post",values:[""]}, _Client, _Room) :- 
	debug(chat, 'Empty Post recieved', []).
handle_json_message(_{pid:"chat",type:"post",values:["update"]}, Client, Room) :- 
	consult("chat.pl"),
	format(atom(Javascript), 'updatechat(~p);', ["uppdatering av servern klar"]),
	hub_broadcast(Room.name, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	debug(chat, 'Reload chat.pl', []).
handle_json_message(_{pid:"chat",type:"post",values:["clear"]}, Client, Room) :- 
	format(atom(Clearchat), 'var element = document.getElementById("newchatbox");while (element.firstChild) element.removeChild(element.firstChild);',[]),
	hub_broadcast(Room.name, websocket{client:Client,data:Clearchat,format:string,hub:chat,opcode:text}),
	debug(chat, 'Clear All Post recieved', []).
handle_json_message(_{pid:"chat",type:"post",values:["admin",Message,_]}, _Client, _Room) :- 
	debug(chat, 'Clear A certain Post recieved, ~p', [Message]).
handle_json_message(_{pid:"chat",type:"post",values:[Message]}, Client, _Room) :- 
	format(atom(Javascript), 'updatechat(~p);', [Message]),
	%hub_broadcast(Room.name, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}),
	debug(chat, 'Post recieved ~p', [Message]).

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