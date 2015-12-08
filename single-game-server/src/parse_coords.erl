-module(parse_coords).
-export([parse/1, formatList/2]).

parse(StringCoord) ->
  StringList = string:tokens(StringCoord, ", "),
  formatList(StringList, []).


formatList([Head | List], Formatted) ->
  X = string:sub_string(Head, 1, 1),
  Y = string:sub_string(Head, 2, 2),
  FinalX = getChar(X),
  {FinalY,_} = string:to_integer(Y),
  
  Coords = {FinalX, FinalY},
  NewFormatted = lists:append(Formatted, [Coords]),
  formatList(List, NewFormatted);
formatList([], Formatted) -> Formatted.

getChar(Char) ->
  case Char of
    "a" -> $a;
    "b" -> $b;
    "c" -> $c;
    "d" -> $d;
    "e" -> $e;
    "f" -> $f;
    "g" -> $g;
    "h" -> $h;
    "i" -> $i;
    "j" -> $j
  end.
