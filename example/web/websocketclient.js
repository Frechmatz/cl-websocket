var WebSocketClient = function() {
    var websocket = null;

    var writeToConsole = function(message) {
	var element = document.getElementById('output-area');
	element.value = (element.value.length > 0 ? (element.value + '\n') : '') + message;
	element.scrollTop = element.scrollHeight;
    };

    function onOpen(evt) {
	writeToConsole('onOpen');
    };

    function onClose(evt) {
	writeToConsole('onClose (statusCode: ' + evt.code + ' reason: "' + evt.reason + '" wasClean: ' + evt.wasClean + ')');
    }

    function onMessage(evt) {
	writeToConsole('onMessage');
	writeToConsole(evt.data + ' (length:' + (evt.data instanceof Blob ? evt.data.size : evt.data.length) + ')');
    }

    function onError(evt) {
	writeToConsole('onError');
    }

    this.send = function(message) {
	websocket.send(message);
    }

    this.close = function( statusCode, reason ) {
	var tmp = websocket;
	websocket = null;
	if( tmp) {
	    writeToConsole('Closing socket. Status=' + statusCode + ' Reason=' + reason);
	    if (statusCode != null) {
		tmp.close(statusCode, reason);
	    } else {
		tmp.close();
	    }
	}
    };
    this.connect = function() {
	writeToConsole('Connecting...');
	var url = 'ws://' + location.hostname + ':7998/test?name=olli'; 
	websocket = new WebSocket(url);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) };
    };

    
};
