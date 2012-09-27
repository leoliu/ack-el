==========================================
 Emacs Interface to command-line tool ack
==========================================
 
From http://betterthangrep.com/

    ack is a tool like grep, designed for programmers with large trees
    of heterogeneous source code.

    ack is written purely in Perl, and takes advantage of the power of
    Perl's regular expressions.

Feature requests and bug reports are welcome. Thanks.

Install
-------

Place ``ack.el`` in the ``load-path`` and add to your init file::

  (require 'ack)

or::

 (autoload 'ack "ack" nil t)

Completion
~~~~~~~~~~

Place ``pcmpl-ack.el`` in the ``load-path`` and add::

  (autoload 'pcomplete/ack "pcmpl-ack")

to your init file. After this you will be able complete ``ack``
options while ``M-x ack`` or in shell/eshell.

Usage
-----

#. ``M-x ack`` and provide a pattern to search.
#. ``C-u M-x ack`` like ``M-x ack`` but allow you to select a
   directory to search.

While reading ack command and args from the minibuffer, the following
key bindings may be useful:

#. ``M-I`` => insert a template for case-insensitive file name search
#. ``TAB`` => completion for ack options
