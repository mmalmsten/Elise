var ws;

function ready(){

	// Sensor simulator
	setInterval(function(){
		var data = Math.floor((Math.random() * 2));
		ws.send('{\"pid\" : \"data\",\"type\" : \"post\",\"values\" : ["' + data + '"]}');
	},1000);
	// End of simulator

	// Websocket connection to main.pl
	if ('MozWebSocket' in window) {
		WebSocket = MozWebSocket;
	}
	if ('WebSocket' in window) {
		ws = new WebSocket('ws://' + window.location.host + '/event');
		ws.onopen = function() {
			ws.send('{\"pid\" : \"event\",\"type\" : \"make\",\"values\" : []}');
		};
		ws.onmessage = function (evt) {
			if (evt.data) {
				var str = evt.data;
				eval(str);
			}
		};
		ws.onclose = function() {
		};
	} else {
		// browser does not support websockets
	}
}

function chat(message){
		ws.send('{\"pid\" : \"chat\",\"type\" : \"post\",\"values\" : ["' + message + '"]}');
}

function updatechat(message){
	alert(message);
}

function printMessage(data){
	document.getElementById("info").innerHTML = data;
}