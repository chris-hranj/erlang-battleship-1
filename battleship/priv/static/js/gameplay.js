$(document).ready(function() {
  var cells = document.getElementsByClassName("opponent-cell");
  var coordinate = document.getElementById("coordinate");

  var myFunction = function() {
    var attribute = this.getAttribute("id");
    coordinate.innerHTML = attribute;

  };

  for(var i=0;i<cells.length;i++){
    cells[i].addEventListener('click', myFunction, false);
  }

  var gameId = $("#game_id").val();
  var player = $("#player").val();
  function update_data() {
    var url = "/game/get_data/" + gameId + "/" + player;
    $.ajax(url, {
      success: function(data, code, xhr) {

        console.log(data);

        //Update the turn label
        var turnText;
        if (data.turn === player)
          turnText = "It is your turn";
        else
          turnText = "It is your opponent's turn";
        $(".player_turn").find("b").text(turnText);

        //TODO:Check for the winner

        for (var i = 0; i < data.board.length; i++) {
          ship = data.board[i];
          for (var j = 0; j < ship.coord_list.length; j++) {
            var coord_rec = ship.coord_list[j];
            var cellId = row_to_char(coord_rec.coord.row) + (coord_rec.coord.column-1);
            if (coord_rec.hit_status === "hit")
              $("#player-board").find("#"+cellId).addClass("hit");
            else if (coord_rec.hit_status === "none")
              $("#player-board").find("#"+cellId).addClass("ship");
          }
        }

      }});
  }
  update_data();
});

function row_to_char(row) {
  var str = "ABCDEFGHIJ";
  return str.charAt(row - 97);
}
