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

## Commands

Enter one request at a time at the chatbot prompt. Requests are case-sensitive
and must use the punctuation shown below.

| Request | Example |
| --- | --- |
| Greeting | `Hello` |
| Current day | `What day is it?` |
| Tomorrow's day | `What day is it tomorrow?` |
| Days since a date | `How long ago was 2024-01-01?` |
| Calculate an expression | `What is twenty plus five times two?` |
| Store a calculated value | `Let total equal ten plus five.` |
| Remember a fact | `Remember that Matthew is a Haskell programmer.` |
| Recall a fact | `Tell me about Matthew.` |
| Next Premier League fixture | `Who is Chelsea playing next?` |
| Premier League position | `What position are Chelsea in the Premier League?` |

Calculations support the `plus`, `minus`, and `times` operators, and numbers
must be written in words. A calculated result can be reused with the variable
name `that` or with a name assigned using `Let`. Football commands require an
`API_KEY` for football-data.org, which should be stored in a .env file.

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