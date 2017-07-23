function aiMove() {
  var truncatedFen = game.fen().match(/(\w+\/)+\w*/)[0]
  var color = game.turn()

  ajaxMove(truncatedFen, color)
}

function ajaxMove(truncatedFen, color) {
  var xhttp = new XMLHttpRequest();
//  game.move("e6")
  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      var move=parseMove(this.responseText)
        game.move(move)
        updateStatus();
        board.position(game.fen());
      //console.log(game.move(move, {sloppy: true}))
    }
  };
  xhttp.open("GET", "api/getMove/"+color+"/"+truncatedFen, true);
  xhttp.send();
}

function parseMove(move) {
  let parts=move.split(' ')
  return {
    from: parts[0].trim(),
    to: parts[1].trim(),
    promotion: 'q' // NOTE: always promote to a queen for example simplicity
  }
}
