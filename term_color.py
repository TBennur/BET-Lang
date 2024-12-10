import sys

# ANSI color codes
RED = "\033[31m"
WHITE = "\033[37m"
RESET = "\033[0m"

try:
    for line in sys.stdin:  # Read input line by line
        for char in line.strip():  # Process each character
            if char == "1":
                print(f"{RED}{char}{RESET}", end="")
            elif char == "0":
                print(f"{WHITE}{char}{RESET}", end="")
            elif char == ",":
                continue
            else:
                print(char, end="")  # Print other characters as-is
        print()  # Newline after processing the line
except KeyboardInterrupt:
    sys.exit(0)  # Gracefully handle Ctrl+C
