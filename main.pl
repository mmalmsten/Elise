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
	http_server(http_dispatch, [port(Port)]).
	%thread_signal(chatroom, (attach_console, trace)).



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
	utterance/3,
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

handle_json_message(_{pid:"event",type:"make",values:[]}, _Client, _Room) :- % Web page opened
	debug(chat, 'Make recieved.', []).
handle_json_message(_{pid:"chat",type:"post",values:[""]}, _Client, _Room) :- 
	debug(chat, 'Empty Post recieved', []).
handle_json_message(_{pid:"chat",type:"post",values:["update"]}, Client, Room) :- 
	consult("chat.pl"),
	format(atom(Javascript), 'updatechat(~p);', ["uppdatering av servern klar"]),
	hub_broadcast(Room.name, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	debug(chat, 'Reload chat.pl', []).
handle_json_message(_{pid:"chat",type:"post",values:["clear"]}, Client, Room) :- 
	retractall(utterance(_,_,_)),
	format(atom(Clearchat), 'var element = document.getElementById("newchatbox");while (element.firstChild) element.removeChild(element.firstChild);',[]),
	hub_broadcast(Room.name, websocket{client:Client,data:Clearchat,format:string,hub:chat,opcode:text}),
	debug(chat, 'Clear All Post recieved', []).
handle_json_message(_{pid:"chat",type:"post",values:["admin",Message,_]}, _Client, _Room) :- 
	retractall(utterance(_,Message,_)),
	debug(chat, 'Clear A certain Post recieved, ~p', [Message]).
handle_json_message(_{pid:"chat",type:"post",values:[Message]}, Client, Room) :- 
	format(atom(Javascript), 'updatechat(~p);', [Message]),
	assertz(utterance(Message)),
	hub_broadcast(Room.name, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	debug(chat, 'Post recieved ~p', [Message]).

%****************************************************************************************
% Debug: Is the machine on or off?
%****************************************************************************************
handle_json_message(_{pid:"data",type:"post",values:[Data]}, _Client, _Room) :- 
	atom_number(Data, X),
	checkthenumber(X),
	debug(chat, 'Data recieved ~p', [Data]).

	checkthenumber(X) :- X > 0,
		debug(chat, 'Stove on - ~p', [X]).
	checkthenumber(X) :- X < 1,
		debug(chat, 'Stove off - ~p', [X]).