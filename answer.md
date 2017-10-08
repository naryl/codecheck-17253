初めまして、アレキサンダーです。

The task is obviously a game with two players and full information about game
state. I had two ideas. The first one in the morning, which I implemented, the
other one about 40 minutes before deadline. :( I can try to implement it too
and we can discuss it during the interview.

The first idea is a simple depth-first search. We find all possible valid
answers and see what both players can do next after each of them. If the next
player has no valid moves, then we win and should do that move, if the enemy
will have a move which would leave us without moves then it's a losing move and
we should not make it. Apply this logic recursively to come up with the next
move which would leave the enemy without winning moves.

I chose to implement it in Common Lisp first. It was used for AI research
decades ago but it's mostly irrelevant now. Still, it's the best language for
rapid prototyping I know. (Better than Python or JS, I could have used those too)

There are two optimizations I made. Firstly there may be words which can never
come up in a game. They form an isolated graph and the entry point (the first
word the framework receives) is in another graph. I just remove them in the
framework before sending the words to AIs. Secondly, each word can have up to
10000 characters and using them all in recursion is too much of an overkill. I
could have shortened them to just the first and last letters but we'll need
full words to output to console anyway, so I made the algorithm (and recursion)
use integer indexes and only store full words once in the whole program.

There's also an edge case when all words start and end in the same letter. It
would lead to O(n!) space and time complexity with this algorithm so I made a
hack to stop it from recursing too deep and wide (at the same time). The hack
won't be needed for the second algorithm.
