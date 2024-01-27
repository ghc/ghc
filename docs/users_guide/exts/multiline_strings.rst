.. _multiline-strings:

Multiline string literals
-------------------------

.. extension:: MultilineStrings
    :shortdesc: Enable multiline string literals.

    :since: 9.12.1

    Enable multiline string literals.

With this extension, GHC now recognizes multiline string literals with ``"""`` delimiters. Indentation is automatically stripped, and gets desugared to normal string literals, so it works as expected for ``OverloadedStrings`` and any other functionality. The indentation that is stripped can be informally defined as "The longest prefix of whitespace shared by all lines in the string, excluding the first line and any whitespace-only lines".

Normal string literals are lexed, then string gaps are collapsed, then escape characters are resolved. Multiline string literals add the following post-processing steps between collapsing string gaps and resolving escape characters:

#. Split the string by newlines

#. Replace leading tabs with spaces up to the next tab stop

#. Remove common whitespace prefix in every line

#. If a line only contains whitespace, remove all of the whitespace

#. Join the string back with ``\n`` delimiters

#. If the first character of the string is a newline, remove it

Examples
~~~~~~~~

.. code-blocks use plain text because the Haskell syntax for pygments doesn't
   support multiline strings yet. Remove if/when pygments adds multiline
   strings to Haskell

+-----------------------+------------------------+---------------------------+
| Expression            | Output                 | Notes                     |
+=======================+========================+===========================+
| .. code-block:: text  | .. code-block::        |                           |
|                       |                        |                           |
|    """                |       "Line 1\n"       |                           |
|    Line 1             |    ++ "Line 2\n"       |                           |
|    Line 2             |    ++ "Line 3\n"       |                           |
|    Line 3             |                        |                           |
|    """                |                        |                           |
+-----------------------+------------------------+---------------------------+
| .. code-block:: text  | .. code-block::        |                           |
|                       |                        | Characters on the same    |
|    """Test            |       "Test\n"         | line as the delimiter are |
|    Line 1             |    ++ "Line 1\n"       | still included            |
|    Line 2             |    ++ "Line 2\n"       |                           |
|    Line 3             |    ++ "Line 3\n"       |                           |
|    """                |                        |                           |
+-----------------------+------------------------+---------------------------+
| .. code-block:: text  | .. code-block::        |                           |
|                       |                        | Omit the trailing newline |
|    """                |       "Line 1\n"       | with string gaps          |
|    Line 1             |    ++ "Line 2\n"       |                           |
|    Line 2             |    ++ "Line 3"         |                           |
|    Line 3\            |                        |                           |
|    \"""               |                        |                           |
+-----------------------+------------------------+---------------------------+
| .. code-block:: text  | .. code-block::        |                           |
|                       |                        | Double quotes don't need  |
|    """                |       "\"Hello\"\n"    | to be escaped unless      |
|    "Hello"            |    ++ "\"\"\"\n"       | they're triple quoted     |
|    \"\"\"             |    ++ "\"\"\"\n"       |                           |
|    \"""               |                        |                           |
|    """                |                        |                           |
+-----------------------+------------------------+---------------------------+
| .. code-block:: text  | .. code-block::        |                           |
|                       |                        | Only common indentation   |
|    """                |       "<div>\n"        | is stripped               |
|      <div>            |    ++ "  <p>ABC</p>\n" |                           |
|        <p>ABC</p>     |    ++ "</div>\n"       |                           |
|      </div>           |                        |                           |
|    """                |                        |                           |
+-----------------------+------------------------+---------------------------+
| .. code-block:: text  | .. code-block::        |                           |
|                       |                        | Use ``\&`` to keep        |
|    """                |       "  Line 1\n"     | leading indentation for   |
|      \&  Line 1       |    ++ "  Line 2\n"     | each line                 |
|      \&  Line 2       |    ++ "  Line 3\n"     |                           |
|      \&  Line 3       |                        |                           |
|    """                |                        |                           |
+-----------------------+------------------------+---------------------------+
