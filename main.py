"""
Echo of Wumpus - Main Entry Point
A stealth horror game where you must collect treasure and escape from the Wumpus.
"""

from core.game import Game


def main():
    """Main entry point for the game"""
    game = Game()
    game.run()


if __name__ == "__main__":
    main()
