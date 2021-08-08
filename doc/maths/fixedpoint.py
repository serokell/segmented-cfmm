from typing import Optional, Union

class Fixedpoint:
  def __init__(self: 'Fixedpoint', mantissa: int, exponent: int, error: Optional['Fixedpoint'] = None) -> 'Fixedpoint':
    self._mantissa = mantissa
    self._exponent = exponent
    if error is not None:
      assert error._error.is_exact_zero(), "Fixedpoint error must be exact"
      self._error = error
    else:
      if getattr(Fixedpoint, 'zero', None) is None:
        # HACK: we are the zero!
        assert mantissa == 0 and exponent == 0
        self._error = self
        Fixedpoint.zero = self
      self._error = Fixedpoint.zero

  def __str__(self: 'Fixedpoint') -> str:
    s = self._mantissa.__repr__() + '×2^{' + self._exponent.__repr__() + '}'
    if not self._error.is_exact_zero():
      s += ' ± (' + self._error.__str__() + ')'
    return s

  def is_zero(self: 'Fixedpoint') -> bool:
    return self._mantissa == 0 and self._exponent == 0

  def is_exact_zero(self: 'Fixedpoint') -> bool:
    return self is Fixedpoint.zero or (self.is_zero() and self._error.is_exact_zero())

  def __add__(self: 'Fixedpoint', other: Union['Fixedpoint', int]) -> 'Fixedpoint':
    if type(other) is Fixedpoint:
      if other.is_exact_zero():
        return self
      if self.is_exact_zero():
        return other
      # preserve the better precision (the smaller exponent)
      (small, big) = (self, other) if self._exponent <= other._exponent else (other, self)
      mantissa = small._mantissa + big._mantissa * pow(2, big._exponent - small._exponent)
      return Fixedpoint(mantissa, small._exponent, self._error + other._error)
    else:
      raise TypeError("Fixedpoint +: unsupported right operand type")

  def __neg__(self: 'Fixedpoint') -> 'Fixedpoint':
    return Fixedpoint(-self._mantissa, self._exponent, self._error)

  def __pos__(self: 'Fixedpoint') -> 'Fixedpoint':
    return self

  def __sub__(self: 'Fixedpoint', other: 'Fixedpoint') -> 'Fixedpoint':
    return self.__add__(other.__neg__())

  def __mul__(self: 'Fixedpoint', other: Union['Fixedpoint', int]) -> 'Fixedpoint':
    if type(other) is Fixedpoint:
      if self.is_exact_zero() or other.is_exact_zero():
        return Fixedpoint.zero
      mantissa = self._mantissa * other._mantissa
      exponent = self._exponent + other._exponent
      error  = Fixedpoint(self._mantissa, self._exponent) * other._error
      error += Fixedpoint(other._mantissa, other._exponent) * self._error
      error += self._error * other._error
      return Fixedpoint(mantissa, exponent, error)
    else:
      raise TypeError("Fixedpoint *: unsupported right operand type")

  def __int__(self: 'Fixedpoint') -> int:
    return self._mantissa * pow(2, self._exponent)

  def __float__(self: 'Fixedpoint') -> float:
    return self._mantissa * pow(2, self._exponent)

# Such imperative programming
# There is a hack in the constructor that assumes that the first value ever created is THE zero.
Fixedpoint.zero = Fixedpoint(0, 0)
