About fnt - efene templates
---------------------------

first the most important, fnt code is hosted at github here:

https://github.com/marianoguerra/fnt---efene-templates

you are invited to contribute by testing it, reporting bugs, improving the
documentation, spreading the word and the most awesome of all, by helping with
the development.

fnt (efene templates) is an erlang module that allows to compile text templates
into erlang modules to be used in the erlang platform (erlang, efene, reia,
LFE, etc.)

fnt uses the syntax of `jquery templates`__ , this has several advantages, first
you have to learn only one template syntax for server and client side
templates, second you can reuse the templates to create server and client
generated pages, to improve page crawling by search engines and provide non
javascript versions of your site without duplicating effort. Also, you have
the benefit of more documentation and examples of the template syntax.

__ http://api.jquery.com/category/plugins/templates/

fnt templates are compiled into bytecode, you get fast page generation
since the template is translated into an optimized function that avoids string
concatenations.

your next step should be following the :ref:`tutorial`

