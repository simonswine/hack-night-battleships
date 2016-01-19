# Battleships

Start the server:

```
stack build && stack exec battleships-exe
```

Connect using telnet:

```
~/battleships $ telnet 127.0.0.1 4000                   | ~/battleships $ telnet 127.0.0.1 4000
Trying 127.0.0.1...                                     | Trying 127.0.0.1...
Connected to 127.0.0.1.                                 | Connected to 127.0.0.1.
Escape character is '^]'.                               | Escape character is '^]'.
What is your name? Matt                                 | What is your name? Chris
Welcome, Matt!                                          | Welcome, Chris!
Type /help to list commands.                            | Type /help to list commands.
*** Matt has connected                                  | *** Chris has connected
*** Chris has connected                                 | *** let the game begin!
*** let the game begin!                                 | To play: Chris
To play: Chris                                          | + + . +
+ + . +                                                 | . . . +
. . . +                                                 | . . + .
. . + .                                                 | . . + .
. . + .                                                 | 
                                                        | ? ? ? ?
? ? ? ?                                                 | ? ? ? ?
? ? ? ?                                                 | ? ? ? ?
? ? ? ?                                                 | ? ? ? ?
? ? ? ?                                                 | /fire 1 2
*** Chris fired at (1, 2). It's a hit!                  | *** Chris fired at (1, 2). It's a hit!
To play: Matt                                           | To play: Matt
+ x . +                                                 | + + . +
. . . +                                                 | . . . +
. . + .                                                 | . . + .
. . + .                                                 | . . + .
                                                        | 
? ? ? ?                                                 | ? x ? ?
? ? ? ?                                                 | ? ? ? ?
? ? ? ?                                                 | ? ? ? ?
? ? ? ?                                                 | ? ? ? ?
```
