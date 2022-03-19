
#!/bin/sh
# Select lines from the middle of a file.
# Usage: bash middle.sh filename end_line numlines
head -n "$2" "$1" | tail -n "$3"

