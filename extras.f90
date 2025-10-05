! ============================
! extras.f90
! Extra math utilities in a separate module.
! ============================

MODULE ExtraMath
  IMPLICIT NONE
CONTAINS

  REAL FUNCTION Square(x)
    REAL, INTENT(IN) :: x
    Square = x * x
  END FUNCTION Square

END MODULE ExtraMath

