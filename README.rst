==========================================
 Emacs Interface to command-line tool ack
==========================================
 
From http://betterthangrep.com/

    ack is a tool like grep, designed for programmers with large trees
    of heterogeneous source code.

    ack is written purely in Perl, and takes advantage of the power of
    Perl's regular expressions.

Feature requests and bug reports are welcome. Thanks.

Features
--------

- Neither ``--nogroup`` nor ``--noheading`` is required
- Handle colors using the standard library ``ansi-color.el``
- Completion for ack options while reading from the minibuffer
- Support both emacs 23 and 24

Screenshots
-----------

.. figure:: http://i.imgur.com/a72Ap.png
   :width: 400 px
   :target: http://i.imgur.com/a72Ap.png
   :alt: ack-emacs23-1.png

   ``ack --column 'ack is.*tool'``

.. figure:: http://i.imgur.com/U2vFz.png
   :width: 400 px
   :target: http://i.imgur.com/U2vFz.png
   :alt: ack-emacs23-2.png

   ``ack --column --nogroup --nocolor 'ack is.*tool'``

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
  (autoload 'pcomplete/ack-grep "pcmpl-ack")

to your init file. After this you will be able complete ``ack``
options while ``M-x ack`` or in shell/eshell.

Usage
-----

- ``M-x ack`` and provide a pattern to search.
- ``C-u M-x ack`` like ``M-x ack`` but allow you to select a
  directory to search.

While reading ack command and args from the minibuffer, the following
key bindings may be useful:

- ``M-I`` => insert a template for case-insensitive file name search
- ``TAB`` => completion for ack options

Contributors
------------
Phillip Lord
