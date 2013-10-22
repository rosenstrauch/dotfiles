%Override mcd function
function mcd() {
  mkdir -p "$1" && cd "$1";
}
