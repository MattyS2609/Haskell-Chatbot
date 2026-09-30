# LGPT

LGPT (the Lightly Generalised Parsing Task) is a Haskell command-line chatbot
for the CS141 Functional Programming course at the University of Warwick. It
uses Megaparsec to interpret natural-language requests and responds through an
interactive terminal REPL.

## Features

- Greets the user and handles unrecognised requests.
- Answers questions about today, tomorrow, and how long ago a date was.
- Evaluates arithmetic expressions written in words, including `plus`, `minus`,
	and `times`.
- Stores calculated values and remembered facts for use later in the session.
- Queries the football-data.org API for a Premier League team's next fixture or
	league position.
- Formats numbers, dates, fixtures, and league standings for terminal output.

## Project Structure

- `app/Main.hs` starts the application and initialises the terminal and
	environment configuration.
- `src/LGPT/TUI.hs` contains the REPL, request parsers, and chatbot responses.
- `src/LGPT/Calculator.hs` parses and evaluates arithmetic expressions.
- `src/LGPT/Numbers.hs` parses and prints numbers in words.
- `src/LGPT/Football.hs` handles football API requests and response formatting.
- `src/LGPT/Helpers.hs` provides shared parser, terminal, and state helpers.
- `test/Spec.hs` contains the test suite.

## Running

Build and run the terminal chatbot with Stack:

```text
stack build
stack exec lgpt-tui
```

Football queries require an `API_KEY` environment variable for the
football-data.org API. Other chatbot features work without it.