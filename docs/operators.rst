=========
Operators
=========

.. list-table::
    :header-rows: 1
    :widths: 10 10 60 10 10

    * - Precedence
      - Operator
      - Description
      - Associativity
      - Chain

    * - 1
      - ``-a``
      - Unary minus
      - right to left
      - all

    * - 1
      - ``!a``
      - Logical NOT
      - right to left
      - all

    * - 2
      - ``a / b``
      - Integer division
      - left to right
      - XEC, BCH, XPI

    * - 2
      - ``a % b``
      - Modulo
      - left to right
      - XEC, BCH, XPI
    * - 2
      - ``a * b``
      - Multiplication
      - left to right
      - BCH

    * - 3
      - ``a + b``
      - Integer addition
      - left to right
      - all

    * - 3
      - ``a - b``
      - Integer subtraction
      - left to right
      - all

    * - 4
      - ``a . b``
      - bytes arrays concatenation
      - left to right
      - XEC, BCH, XPI

    * - 5
      - ``a << b``
      - Raw left shift
      - left to right
      - XPI

    * - 5
      - ``a >> b``
      - Raw right shift
      - left to right
      - XPI

    * - 6
      - ``a < b``
      - Less than
      - left to right
      - all

    * - 6
      - ``a <= b``
      - Less than or equal
      - left to right
      - all

    * - 6
      - ``a > b``
      - Greater than
      - left to right
      - all

    * - 6
      - ``a >= b``
      - Greater than or equal
      - left to right
      - all

    * - 7
      - ``a == b``
      - Equal
      - left to right
      - all

    * - 7
      - ``a != b``
      - Not equal
      - left to right
      - all

    * - 7
      - ``a === b``
      - Numeric and equal
      - left to right
      - all

    * - 7
      - ``a !== b``
      - Numeric and not equal
      - left to right
      - all

    * - 8
      - ``a & b``
      - Bitwise AND
      - left to right
      - all

    * - 9
      - ``a ^ b``
      - Bitwise XOR
      - left to right
      - all

    * - 10
      - ``a | b``
      - Bitwise OR
      - left to right
      - all

    * - 11
      - ``a && b``
      - Bolean AND

        *Both a and b are always evaluated.*

      - left to right
      - all

    * - 12
      - ``a || b``
      - Boolean OR

        *Both a and b are always evaluated.*
      - left to right
      - all

    * - 13
      - ``a @ b``
      - Split bytes array ``a`` at position ``b``.
      - none
      - all

    * - 14
      - ``cond ? t : f``
      - If ``cond`` is ``true`` returns ``t``, otherwise ``f``.
      - right to left
      - all
