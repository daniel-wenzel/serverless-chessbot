console.log("hello world")

const express = require('express')
const app = express()
const { exec } = require('child_process');
const games = {}

app.get('/', (req, res) => {
  res.sendFile('main.html', {root: './public'})
})

app.listen(3000, () => {
  console.log('Example app listening on port 3000!')
})

app.use(express.static('public'));

app.get('/api/getMove/:color/:fen*', (req, res) => {
  let fen = req.path.substring(req.path.indexOf(req.params.fen), req.path.length)
  let color = req.params.color
  console.log(fen)
  console.log(color)
  if (!fen) {
    res.status(401).send("no fen string provided")
  }
  if (!color) {
    res.status(401).send("no color provided")
  }
  //fen contains backslashes so we parse it out quick and dirty
  exec(`./ai/bin/bot ${fen} ${color}`, (err, stdout, stderr) => {
    if (err) {
      // node couldn't execute the command
      return;
    }


    // the *entire* stdout and stderr (buffered)
    res.status(200).send(stdout)
  });

})
