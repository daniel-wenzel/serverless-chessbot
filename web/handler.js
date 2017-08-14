'use strict';

const exec = require('child_process').exec;

module.exports.move = (event, context, callback) => {
  let sendResponse = sendResponseInit(callback)

  if (!event.queryStringParameters) {
    return sendResponse(400, "please specify color and fen in parameters")
  }
  let color = event.queryStringParameters.color
  let fen = event.queryStringParameters.fen

  if (!color || (color != "w" && color != "b")) {
    return sendResponse(401, "please specify a color in the parameter ('w' or 'b')")
  }
  if (!fen) {
    return sendResponse(401, "please specify a fen string in the parameter")
  }
  //let color = "b"
  //let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  exec(`./ai/bin/bot ${fen} ${color}`, (err, stdout, stderr) => {
    if (err) {
      return sendResponse(500, err)
    }


    // the *entire* stdout and stderr (buffered)
    return sendResponse(200, stdout)
    //res.status(200).send(stdout)
  });

  // Use this code if you don't use the http event with the LAMBDA-PROXY integration
  // callback(null, { message: 'Go Serverless v1.0! Your function executed successfully!', event });
};

function sendResponseInit(callback) {
  return (statusCode, message) => {
    callback(null, {
        statusCode: statusCode,
        body: JSON.stringify(message)
    })
  }
}
