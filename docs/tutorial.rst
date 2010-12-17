.. _tutorial:

fnt tutorial
------------

download and compile
....................

to build fnt you will need git and a recent version of erlang with support for leex and yecc

in a debian based system you install those like this::

        sudo apt-get install git-core erlang-nox

adapt to your package manager if not running a debian based system

::

        git clone https://github.com/marianoguerra/fnt---efene-templates.git fnt
        cd fnt/src
        ./build.sh
        cd ../..

template hello world
....................

let's start with a simple hello world program using fnt.

open a file called hello.fnt and type the following::

        hello world!

save it and close it.

now we will compile and erlang module called *tpls* that will contain a
function called *hello* that will render that template.

we will do this from the erlang interactive console.

we will start it passing the path where the fnt modules are located::

        erl -pa fnt/ebin/

now we will compile the template::

        Erlang R13B03 (erts-5.7.4) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

        Eshell V5.7.4  (abort with ^G)
        1> fnt:build(tpls, [{hello, "hello.fnt"}]).
        .:2: Warning: variable 'Context' is unused
        ok


the first two lines are the welcome banner of the erlang interactive console, then we run the following code::

        fnt:build(tpls, [{hello, "hello.fnt"}]).

this runs the build function from fnt and passes as first argument the name of
the module that will be created as atom and as second parameter a
`property list`__ that has as key the function name as atom and as value the
name of the template to compile.

__ http://www.erlang.org/doc/man/proplists.html

the warning that follows appears because the template doesn't contain any
template tag and the *Context* variable isn't used, this warning doesn't do any
harm and should appear in more complex templates.

you can pass more than one key, value pair to build a module with multiple templates.

now let's test our template module::

        2> Out = tpls:hello([]).
        ["hello world!\n"]

        3> io:format(Out).
        hello world!
        ok
 
the first line::

        Out = tpls:hello([]).

stores the result of calling tpls:hello with the Context parameter as [] in the variable Out.

the Context parameter is the parameter that contain the values of the variables
that will be used in the template, since we don't have any variable in the
template we pass an empty list.

in the following line we print the content of Out::

        3> io:format(Out).

and we get the expected result.

hello ${name}
.............

a template with a constant string isn't much fun, now we will start using some fnt features.

first we will create a template that will print hello and the name passed in the context parameter.

open a file called helloname.fnt and type the following::

        hello ${name}!

save it and close it.

now open the erlang shell again::

        erl -pa fnt/ebin/

and type::

        fnt:build(tpls, [{hello, "hello.fnt"}, {hello_name, "helloname.fnt"}]).

now let's try our module::

        6> tpls:hello_name([{name, "mariano"}]).
        ["hello ","mariano","!\n"]
        7> OutName = tpls:hello_name([{name, "mariano"}]).
        ["hello ","mariano","!\n"]
        8> io:format(OutName).
        hello mariano!
        ok
        9> OutName1 = tpls:hello_name([]).                
        ["hello ","undefined","!\n"]
        10> io:format(OutName1).           
        hello undefined!
        ok

.. note::

        if you didn't stopped the erlang shell between examples you will need
        to reload the tpls module to see the changes using *l(tpls).*

as you can see, we pass the value of the name variable as a proplist, if not defined it will use a default value.

.. note::

        the default value for undefined values may change in the future to make it compatible with jquery-templates

you may have noted that the name of the function doesn't have to match the name of the file.

another thing to note is the value of the result returned by the functions, for example::

        ["hello ","mariano","!\n"]

this is not a string but a list of strings, more commonly known in erlang as
iolist, it avoids doing costly string concatenations providing efficient
template functions.

conditionals
............

let's move to more complex example, let's say we want to give a special greeting to the admin user, a warning to the and a standar greeting to the other users.

for that we need to use conditionals.

open a file called greet.fnt in your text editor and type::

        {{if name == "admin"}} well hello sir {{else name == "root"}} you shouldn't be using the root user {{else}} welcome ${name}! {{/if}}

now compile the module and test it::

        1> fnt:build(tpls, [{hello, "hello.fnt"}, {hello_name, "helloname.fnt"}, {greet, "greet.fnt"}]).
        .:2: Warning: variable 'Context' is unused
        ok
        2> io:format(tpls:greet([{name, "admin"}])).
         well hello sir
        ok
        3> io:format(tpls:greet([{name, "root"}])).
         you shouldn't be using the root user
        ok
        4> io:format(tpls:greet([{name, "mariano"}])).
         welcome mariano!
        ok

the syntax of the if/else tags is explained in the jquery-template documentation but it's really similar to any programming language,
fnt allows complex expressions to be evaluated with the syntax of javascript (boolean, aritmethic and binary expressions allowed).

let's add a secret message for the user "mariano" to show the simplest case of conditionals, modify the greet.fnt file to::

        {{if name == "admin"}} well hello sir {{else name == "root"}} you shouldn't be using the root user {{else}} welcome ${name}! {{/if}}

        {{if name == "mariano"}}hello mariano, you have superpowers!{{/if}}


in this case we don't have else parts, we can also have just if/else or if/else/else/else ad infinitum ;)

recompile, reload and test the example::

        4> fnt:build(tpls, [{hello, "hello.fnt"}, {hello_name, "helloname.fnt"}, {greet, "greet.fnt"}]).
        .:2: Warning: variable 'Context' is unused
        ok
        5> l(tpls).
        {module,tpls}
        6> io:format(tpls:greet([{name, "mariano"}])).
         welcome mariano!

        hello mariano, you have superpowers!
        ok
        7> io:format(tpls:greet([{name, "marianoguerra"}])).
         welcome marianoguerra!


        ok

looping
.......

if we want to generate some content based on a list we must use the *each* tag.

let's create a list of items from a list of strings, open a file calles list and type the following::

        {{each item}} ${$index}: ${$value}
        {{/each}}

now compile and run::

        1> fnt:build(tpls, [{hello, "hello.fnt"}, {hello_name, "helloname.fnt"}, {greet, "greet.fnt"}, {list, "list.fnt"}]). 
        .:2: Warning: variable 'Context' is unused
        ok
        2> l(tpls).
        {module,tpls}
        3> io:format(tpls:list([{item, ["eggs", "spam", "bacon"]}])).
         0: eggs
         1: spam
         2: bacon

        ok

in the each loop there are two special variables $index (the index of the current value on the list) and $value (the value of the item).

note that newlines are preserved in the output.

escaping HTML
.............

until now we were using fnt as a simple template library without writting HTML, let's try the last example with some changes::

        4> io:format(tpls:list([{item, ["<eggs>", "<spam>", "<bacon>"]}])).
         0: &lt;eggs&gt;
         1: &lt;spam&gt;
         2: &lt;bacon&gt;

        ok

by default when you put the value of a variable in the template the HTML entities are escaped, to avoid this we can use {{html}} to print the content as is.

create a file called insecure.fnt and type::

        this is some content without escaping: {{html value}}
        the same content escaped: ${value}

compile, reload and test::

        5> fnt:build(tpls, [{hello, "hello.fnt"}, {hello_name, "helloname.fnt"}, {greet, "greet.fnt"}, {list, "list.fnt"}, {insecure, "insecure.fnt"}]).
        .:2: Warning: variable 'Context' is unused
        ok

        6> l(tpls).                                                                                                                                     
        {module,tpls}

        7> io:format(tpls:insecure([{value, "<h1>hello</h1>"}])).
        this is some content without escaping: <h1>hello</h1>
        the same content escaped: &lt;h1&gt;hello&lt;/h1&gt;
        ok

.. warning::

        use {{html}} with care to avoid cross site scripting (XSS) attacks 

some low level fun
..................

if you want to know what kind of code does fnt generates you can see it.

it may be also useful to debug errors in fnt or in your templates.

let's see the generated code for the module we have until now::

        10> io:format(fnt:to_erlang(tpls, [{hello, "hello.fnt"}, {hello_name, "helloname.fnt"}, {greet, "greet.fnt"}, {list, "list.fnt"}, {insecure, "insecure.fnt"}])).
        -module(tpls).

        -export([hello/1, hello_name/1, greet/1, list/1,
                 insecure/1]).

        hello(Context) -> ["hello world!\n"].

        hello_name(Context) ->
            ["hello ", fnt:escape(fnt:get(Context, [name])), "!\n"].

        greet(Context) ->
            [case fnt:get(Context, [name]) == "admin" of
               true -> " well hello sir ";
               _ ->
                   case fnt:get(Context, [name]) == "root" of
                     true -> " you shouldn't be using the root user ";
                     _ ->
                         [" welcome ", fnt:escape(fnt:get(Context, [name])),
                          "! "]
                   end
             end,
             "\n\n",
             case fnt:get(Context, [name]) == "mariano" of
               true -> "hello mariano, you have superpowers!";
               _ -> ""
             end,
             "\n"].

        list(Context) ->
            [fnt:each(fnt:get(Context, [item]),
                      fun (Index, Value) ->
                              [" ", fnt:escape(Index), ": ", fnt:escape(Value), "\n"]
                      end),
             "\n"].

        insecure(Context) ->
            ["this is some content without escaping: ",
             fnt:get(Context, [value]),
             "\nthe same content escaped: ",
             fnt:escape(fnt:get(Context, [value])), "\n"].

this is all for now, have fun building templates!
