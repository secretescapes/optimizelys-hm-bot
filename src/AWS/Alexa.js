'use strict';
var Alexa = require("alexa-sdk");

exports.handler = function(event, context, callback) {
    var alexa = Alexa.handler(event, context);
    alexa.registerHandlers(handlers);
    alexa.execute();
};

exports.tell = function(alexa, msg){
    alexa.emit(':tell', msg)
}


// var handlers = {
//     'LaunchRequest': function () {
//         this.emit('SayHello');
//     },
//     'HelloWorldIntent': function () {
//         this.emit('SayHello')
//     },
//     'SayHello': function () {
//         this.emit(':tell', 'Hello World!');
//     }
// };

exports.convert =  function(handler){
    handler(this)
}

