var cells = document.getElementsByClassName("opponent-cell");
var coordinate = document.getElementById("coordinate");

var myFunction = function() {
  var attribute = this.getAttribute("id");
  coordinate.innerHTML = attribute;

};

for(var i=0;i<cells.length;i++){
  cells[i].addEventListener('click', myFunction, false);
}
