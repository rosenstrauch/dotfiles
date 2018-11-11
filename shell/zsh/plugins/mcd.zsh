# mcd make dir and enter it
# # Create directory and move into

mcd() { [ -n "$1" ] && mkdir -p "$1" && cd "$1"}
