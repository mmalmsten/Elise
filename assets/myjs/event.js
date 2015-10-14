var ws;

function ready(){

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

function printMessage(data){
	var message;
	if(data == "stop") {
		message = "Spisen har nu stängts av, och den här händelsen ignoreras vid framtida beräkningar."		
	} else if(data == "ignore") {
		message = "Den här händelsen ignoreras vid framtida beräkningar."
	} else if(data == "correct") {
		message = "Ooops! Ursäkta att jag störde!"		
	}
	$("#printmessage #message").html(message);
    $("#printmessage").fadeIn(500);
    $("#printmessage").animate({marginTop: "20vh"},1000);
	$("body").click(function(){
	    $("#printmessage").fadeOut(500);
    	$("#printmessage").animate({marginTop: "0vh"},1000);
	});
}

function zeroPad(num) {
  var zero = 2 - num.toString().length + 1;
  return Array(+(zero > 0 && zero)).join("0") + num;
}
function nextStart(time){
	var hours = Math.floor(time / 3600);
	time = time - hours * 3600;
	var minutes = Math.floor(time / 60);
	var seconds = (time - minutes * 60).toFixed();
	var finalTime = zeroPad(hours)+":"+zeroPad(minutes)+":"+zeroPad(seconds);
	document.getElementById("info").innerHTML = finalTime;
}

function login(){
    $("#signin").fadeIn(500);
    $(".form-horizontal").animate({marginTop: "10vh"},1000);

	$(".close").click(function(){
	    $("#signin").fadeOut(500);
    	$(".form-horizontal").animate({marginTop: "0vh"},1000);
	});
}

function loginSuccess(){
    $("#signin").fadeOut(500);
	$(".form-horizontal").animate({marginTop: "0vh"},1000);
}

function loginFail(){
	var text = 'Ooops! Det verkar som att fel lösenord, eller en ogiltig e-mail angivits.';
	var div = document.createElement('div');
	div.className = 'alert';
	div.id = 'signin-message';
	div.innerHTML = text;
	document.getElementById("signin-message-placeholder").appendChild(div);
	login();
}

function brokenPattern(){
    $(".correct").fadeIn(1000);
    $(".correct .title").animate({marginTop: "0"},1000);
    $('.correct .info .btn').attr("disabled", false);
	$(".correct button").click(function(){
	    correctPattern();
	});    
}

function correctPattern(){
    $(".correct").fadeOut(1000);
    $(".correct .title").animate({marginTop: "-150px"},1000);
    $('.correct .info .btn').attr("disabled", true);
}

function send(message){
	var email = document.getElementById("inputEmail").value;
	var password = document.getElementById("inputPassword").value;
	ws.send('{\"pid\" : \"chat\",\"type\" : \"post\",\"values\" : ["' + message + '","' + email +'","' + password +'"]}');
}

//login(); 
//brokenPattern();
//correctPattern();