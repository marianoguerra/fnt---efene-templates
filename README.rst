fnt - efene template
====================

hopefuly an implementation of jquery template[1] for erlang/efene

[1] http://api.jquery.com/category/plugins/templates/

why it's awesome?
-----------------

* compile to erlang bytecode for awesome speed
* hand made lexer and parser for awesome speed
* uses io lists (no string concatenation) for awesome speed
* write your templates once, run them in the server and the browser
* jquery templates awesomeness
* erlang/efene awesomness

is it everything awesome?
-------------------------

nope

* work in progress
* currently supports

  * if/else
  * each
  * html
  * ${}

* to implement

  * tmpl
  * wrap

* needs documentation
* needs tests
  
  * to check compliance with js semantics
  * to check that we get the same results as jquery-tmpl
  * to check that it works in all cases

hey! you are the efene guy? why is it in erlang then?
-----------------------------------------------------

I don't want to push efene as a dependency, but as I said efene is awesome so go and check it out!


