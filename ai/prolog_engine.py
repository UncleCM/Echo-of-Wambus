"""Base Prolog engine wrapper for PySwip"""
from pyswip import Prolog


class PrologEngine:
    """Low-level Prolog engine wrapper"""
    
    def __init__(self, knowledge_base_file="game_logic.pl"):
        """
        Initialize Prolog engine
        
        Args:
            knowledge_base_file: Path to .pl file to consult
        """
        self.available = False
        self.prolog = None
        
        try:
            self.prolog = Prolog()
            self.prolog.consult(knowledge_base_file)
            self.available = True
            self._query("init_game")  # Initialize game state
            print("[PrologEngine] Initialized successfully")
        except Exception as e:
            print(f"[PrologEngine] Failed to initialize: {e}")
            print("[PrologEngine] Running with fallback collision system")
            self.prolog = None
            self.available = False
    
    def _query(self, query_str):
        """
        Execute a Prolog query
        
        Args:
            query_str: Prolog query string
            
        Returns:
            list: Query results (list of dicts)
        """
        if not self.available or self.prolog is None:
            return []
        try:
            return list(self.prolog.query(query_str))
        except Exception as e:
            print(f"[PrologEngine] Query failed: {query_str} -> {e}")
            return []
    
    def is_available(self):
        """Check if Prolog engine is available"""
        return self.available
