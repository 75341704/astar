# astar

A rough implementation of (what I remembered off the top of my head of)
the A* heuristic search algorithm (https://en.wikipedia.org/wiki/A*_search_algorithm).

This implementation is very naive (often re-orders the list, often
recalculates the heuristic and actual values, and keeps a full history
of visited states to prevent cycles), but still solves the
'maximum discount' problem fairly quickly.
