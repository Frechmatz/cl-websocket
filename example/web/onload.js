
document.addEventListener(
    'DOMContentLoaded',
    function() {
	var webSocketClient = new WebSocketClient();
	document.getElementById('connect-button').addEventListener(
	    'click',
	    function(evt) {
		webSocketClient.connect();
	    });
	document.getElementById('close-button').addEventListener(
	    'click',
	    function(evt) {
		webSocketClient.close(1000, 'Bye!');
	    });
	document.getElementById('close-button-no-status').addEventListener(
	    'click',
	    function(evt) {
		webSocketClient.close(null);
	    });
	document.getElementById('send-short-message-button').addEventListener(
	    'click',
	    function(evt) {
		webSocketClient.send(
		    'Hi there, greetings from your browser. '+
			'To keep things interesting here some special characters: '+
			'ÄÖÜäöüß€');
	    });
	document.getElementById('send-trigger-close-button').addEventListener(
	    'click',
	    function(evt) {
		webSocketClient.send(
		    'CLOSE');
	    });
	document.getElementById('send-long-message-button').addEventListener(
	    'click',
	    function(evt) {
		var str = '';
		for (var i = 0; i < 10000; i++) {
		    str += '0123456789012345678901234567890123456789012345678901234567890123456789';
		}
		webSocketClient.send(str);
	    });
	document.getElementById('send-binary-message-button').addEventListener(
	    'click',
	    function(evt) {
		var length = 999;
		var buffer = new ArrayBuffer(length);
		var view   = new Int8Array(buffer);
		var arr = [];
		for( var i = 0; i < length; i++) {
		    arr.push(67);
		}
		view.set(arr);
		webSocketClient.send(buffer);
	    });
    },
    false);
