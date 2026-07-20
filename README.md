# Worms

Implementation of the classic *Worms* game written entirely in **Haskell**, using the **Gloss** graphics library. Turn-based artillery gameplay with destructible terrain, projectile physics and collision handling, built on an immutable game state. Developed for the Laboratórios de Informática / Programação I course at the University of Minho.

## Authors

Group project developed by:

- **Gonçalo Pereira** — @pereiravp
- **David Mimoso** — @davidmimoso

## Overview

The project reimplements the core mechanics of *Worms* under a strictly functional paradigm. The game state is modelled immutably and advanced through pure transformations driven by player input and the passage of time. The main development effort went into:

- Representation and manipulation of the game state.
- Terrain modelling, movement, and collision resolution.
- Projectile physics and terrain destruction.
- A turn system managing multiple worms and weapons (bazooka, dynamite, mine, jetpack, among others).
- A graphical front end with a main menu, map selection, and win screen.

## Screenshots

Main menu:

<!-- Replace with a screenshot of the menu, e.g. docs/screenshots/menu.png -->
![Main menu](docs/screenshots/menu.png)

Gameplay:

<!-- Replace with a screenshot of the gameplay, e.g. docs/screenshots/gameplay.png -->
![Gameplay](docs/screenshots/gameplay.png)

## Known Limitations

The menu exposes a **Bots** option and the codebase includes the scaffolding for a one-human-versus-three-bots mode (the `ConfigBot` configuration, bot naming, and per-turn bot detection). However, the AI decision logic was never implemented: when it is a bot's turn the game returns the state unchanged, so CPU-controlled worms take no action and the turn does not progress meaningfully. As a result the bot mode is effectively non-functional and its gameplay is poor. Human-versus-human play works as intended.

## Requirements

- GHC (GHC2021)
- Cabal
- Gloss / gloss-juicy (resolved automatically by Cabal)

## Building and Running

```bash
cd 2025l1g019
cabal build
cabal run worms-game
```

Image assets (`.bmp`) are loaded relative to the `assets/` directory, so the game must be run from `2025l1g019/`.

### Controls

| Key            | Action                       |
| -------------- | ---------------------------- |
| Left / Right   | Move the worm                |
| Space          | Fire weapon / use jetpack    |
| P              | Pass the turn                |
| Menu arrows    | Navigate menu options        |

## Testing, Coverage and Documentation

The project ships feedback executables for each task (`t1-feedback` through `t4-feedback`).

```bash
cd 2025l1g019

# Run the tests for a task (Task 1 shown as an example)
cabal run t1-feedback

# Measure test coverage (Task 1 shown as an example)
./runcoverage.sh t1

# Generate documentation with Haddock
cabal haddock-project
```

## Project Structure

```
LP/
├── 2025l1g019/
│   ├── app/            Graphical game (Main, Worms, Desenhar, Eventos, Tempo)
│   ├── lib/            Task logic (Tarefa1..4 and supporting modules)
│   ├── test/           Tests and oracles for each task
│   ├── assets/         Game images (.bmp)
│   ├── worms.cabal     Cabal project configuration
│   └── README.md       Interpreter, testing and documentation notes
├── docs/
│   └── screenshots/    Menu and gameplay screenshots
└── README.md           This file
```

## Academic Context

Developed for the Laboratórios de Informática / Programação I course, Software Engineering degree, University of Minho (2024/2025).
