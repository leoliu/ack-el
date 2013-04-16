==========================================
 Emacs Interface to command-line tool ack
==========================================

From http://betterthangrep.com/

    ack is a tool like grep, designed for programmers with large trees
    of heterogeneous source code.

    ack is written purely in Perl, and takes advantage of the power of
    Perl's regular expressions.

This package is part of `GNU ELPA <http://elpa.gnu.org>`_.

Patches, feature requests and bug reports are welcome. Thanks.

Features
--------

- Neither ``--nogroup`` nor ``--noheading`` is required
- Handle colors using the standard library ``ansi-color.el``
- Completion for ack options while reading from the minibuffer
- Support ``git grep``, ``hg grep`` and ``bzr grep``

Screenshots
-----------

.. figure:: http://i.imgur.com/mrk8k.png
   :width: 400 px
   :target: http://i.imgur.com/mrk8k.png
   :alt: ack-git-grep.png

   ``git --no-pager grep -n --color 'hg grep'``

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

Completion (optional)
~~~~~~~~~~~~~~~~~~~~~

UPDATE: ``pcmpl-ack.el`` is merged into emacs trunk and should appear
in emacs 24.4.

Place ``pcmpl-ack.el`` in the ``load-path`` and add::

  (autoload 'pcomplete/ack "pcmpl-ack")
  (autoload 'pcomplete/ack-grep "pcmpl-ack")

to your init file. After this you will be able complete ``ack``
options while ``M-x ack`` or in shell/eshell.

Usage
-----

- Type ``M-x ack`` and provide a pattern to search.
- Type ``C-u M-x ack`` to search from current project root.
- Type ``C-u C-u M-x ack`` to interactively choose a directory to search.

While reading ack command and args from the minibuffer, the following
key bindings may be useful:

- ``M-I`` => insert a template for case-insensitive file name search
- ``M-G`` => insert a template for ``git grep``, ``hg grep`` or ``bzr grep``
- ``M-Y`` => grab the symbol at point from the window before entering
  the minibuffer
- ``TAB`` => completion for ack options

Emacs23
-------

Check out the `emacs23
<https://github.com/leoliu/ack-el/tree/emacs23>`_ branch.

Bugs
----

https://github.com/leoliu/ack-el/issues

Contributors
------------
Phillip Lord
