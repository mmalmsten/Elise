function simpleDatepicker(name){
	var i;
	var sel;
	var year = (new Date()).getFullYear();

	var months = [
	  {val : 01, text: 'Januari'},
	  {val : 02, text: 'Februari'},
	  {val : 03, text: 'Mars'},
	  {val : 04, text: 'April'},
	  {val : 05, text: 'Maj'},
	  {val : 06, text: 'Juni'},
	  {val : 07, text: 'Juli'},
	  {val : 08, text: 'August'},
	  {val : 09, text: 'September'},
	  {val : 10, text: 'Oktober'},
	  {val : 11, text: 'Novemeber'},
	  {val : 12, text: 'December'}
	];

	sel = $('<select class="form-control" id="day">').appendTo("#datepicker"+name);
	for (i = 1; i < 32; i++) {
		sel.append($("<option>").attr('value',pad(i)).text(i));
	};

	sel = $('<select class="form-control" id="month">').appendTo("#datepicker"+name);
	$(months).each(function() {
	 sel.append($("<option>").attr('value',this.val).text(this.text));
	});

	sel = $('<select class="form-control" id="year">').appendTo("#datepicker"+name);
	for (i = year; i > year-6; i--) {
		sel.append($("<option>").attr('value',i).text(i));
	};

	sel = $('<select class="form-control" id="hour">').appendTo("#datepicker"+name);
	for (i = 0; i < 24; i++) {
		sel.append($("<option>").attr('value',pad(i)).text(pad(i)));
	};

	sel = $('<select class="form-control" id="minute">').appendTo("#datepicker"+name);
	for (i = 0; i < 61; i++) {
		sel.append($("<option>").attr('value',pad(i)).text(pad(i)));
	};
}

function pad(num) {
    var s = num+"";
    while (s.length < 2) s = "0" + s;
    return s;
}