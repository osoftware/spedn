=========
Operators
=========

.. list-table::
    :header-rows: 1
    :widths: 15 15 55 15

    * - Precedence
      - Operator
      - Description
      - Associativity

    * - 1
      - ``-a``
      - Unary minus
      - right to left

    * - 1
      - ``!a``
      - Logical NOT
      - right to left

    * - 2
      - ``a / b``
      - Integer division
      - left to right

    * - 2
      - ``a % b``
      - Modulo
      - left to right

    * - 3
      - ``a + b``
      - Integer addition
      - left to right

    * - 3
      - ``a - b``
      - Integer subtraction
      - left to right

    * - 4
      - ``a . b``
      - bytes arrays concatenation
      - left to right

    * - 5
      - ``a < b``
      - Less than
      - left to right

    * - 5
      - ``a <= b``
      - Less than or equal
      - left to right

    * - 5
      - ``a > b``
      - Greater than
      - left to right

    * - 5
      - ``a >= b``
      - Greater than or equal
      - left to right

    * - 6
      - ``a == b``
      - Equal
      - left to right

    * - 6
      - ``a != b``
      - Not equal
      - left to right

    * - 6
      - ``a === b``
      - Numeric and equal
      - left to right

    * - 6
      - ``a !== b``
      - Numeric and not equal
      - left to right

    * - 7
      - ``a & b``
      - Bitwise AND
      - left to right

    * - 8
      - ``a ^ b``
      - Bitwise XOR
      - left to right

    * - 9
      - ``a | b``
      - Bitwise OR
      - left to right

    * - 10
      - ``a && b``
      - Bolean AND

        *Note: Both a and b are always evaluated.*

      - left to right

    * - 11
      - ``a || b``
      - Boolean OR

        *Note: Both a and b are always evaluated.*
      - left to right

    * - 12
      - ``a @ b``
      - Split bytes array ``a`` at position ``b``.
      - none
